##[PUR]=group
##recon_file=vector

library(grid)
library(gridExtra)
library(rasterVis)
library(ggplot2)
library(RColorBrewer)
library(rtf)
library(foreign)

#Read LUMENS log file
LUMENS_log <- as.data.frame(Sys.info())
OS <- substr(as.character(LUMENS_log[2,1]), 1, 2)
username <- as.character(LUMENS_log[6,1])
if(OS == "XP") {
  user_path<-paste("C:/Documents and Settings/All Users", sep="")
} else {
  user_path<-paste("C:/Users/Public", sep="")
}
LUMENS_path_user <- paste(user_path,"/LUMENS/LUMENS.log", sep="")
log.file<-read.table(LUMENS_path_user, header=FALSE, sep=",")
proj.file<-paste(log.file[1,1], "/", log.file[1,2],"/",log.file[1,2], ".lpj", sep="")
tempEnv<-new.env()
load(proj.file, tempEnv)

#Set PUR directory
working_directory<-paste(log.file[1,1], "/", log.file[1,2],"/PUR/PUR_analysis_",tempEnv$PUR.index, sep="")
setwd(working_directory)

time_start<-paste(eval(parse(text=(paste("Sys.time ()")))), sep="")

wd_usertemp<-paste(working_directory,"/temp", sep="")
#setwd(working_directory)
#st_area_file<- substr(basename(recon_file), 1, nchar(basename(recon_file)) - 4)
#sa<-readOGR(dsn=working_directory,st_area_file)
sa<-recon_file
attribute<-paste(working_directory, "/PUR_attribute.csv", sep="")
unresolved<-paste(working_directory, "/PUR_unresolved_case.csv", sep="")

cek_unresolved<-as.numeric(file.exists(unresolved))

if (cek_unresolved==1) {
  attribute<- read.table(attribute, header=TRUE, sep=",",)
  unresolved<- read.table(unresolved, header=TRUE, sep=",",)
  unresolved$X<-NULL
  unresolved$resolved<-"NULL"
  unresolved_edit<-edit(unresolved)
  unresolved_edit.c1<-as.data.frame(unresolved_edit$ID)
  unresolved_edit.c2<-as.data.frame(unresolved_edit$resolved)
  colnames(unresolved_edit.c1)[1]<-"ID"
  colnames(unresolved_edit.c2)[1]<-"resolved"
  unresolved_edit.join<-cbind(unresolved_edit.c1,unresolved_edit.c2)
  attribute.edit<-merge(attribute,unresolved_edit.join, by="ID", all=TRUE)
  
  test<-as.data.frame(unique(unresolved_edit$resolved))
  test2<-as.data.frame(unique(attribute$Rec_phase1b))
  colnames(test)[1]<-"add"
  colnames(test2)[1]<-"add"
  test3<-rbind(test,test2)
  levels(attribute.edit$resolved)<-levels(test3$add)
  colnames(attribute.edit)[1]<-"PU_name"
  
  len<-nrow(attribute.edit)
  for(s in 1:len){
    if (is.na(attribute.edit$resolved[s])==TRUE) {
      attribute.edit$resolved[s]<-attribute.edit$Rec_phase1b[s]
      attribute.edit$res_id[s]<-attribute.edit$PU_name[s]
    }
  }
  
  unique_class<-as.data.frame(unique(attribute.edit$resolved))
  colnames(unique_class)[1]<-"resolved"
  countrow<-nrow(unique_class)
  unique_class$PU_ID<-seq(countrow)
  attribute.edit<-merge(attribute.edit, unique_class, by="resolved")
  
  sa<-merge(sa,attribute.edit, by="PU_name", all=TRUE)
  writeOGR(sa, dsn=working_directory, "PUR_final_reconciliation", driver="ESRI Shapefile", overwrite_layer=TRUE, delete_dsn=TRUE)
  
  shp_dir<-paste(working_directory,"/", "PUR_final_reconciliation", ".shp", sep="")
  file_out<-paste(working_directory,"/", "PUR_final_reconciliation.tif", sep="")
  kolom_data<-paste('PU_ID')
  opsi<-1
  res<-100
  if (file.exists("C:/Program Files (x86)/LUMENS/bin/gdal_rasterize.exe")){
    gdalraster = "C:/Progra~2/LUMENS/bin/gdal_rasterize.exe "
  } else{
    gdalraster = "C:/Progra~1/LUMENS/bin/gdal_rasterize.exe "
  }
  osgeo_comm<-paste(gdalraster,shp_dir, file_out,"-a",kolom_data, "-tr", res, res, "-a_nodata 255 -ot Byte", sep=" ")
  system(osgeo_comm)
  
  test4<-raster(file_out)
  test4 <- ratify(test4, filename='PUR.grd', count=TRUE, overwrite=TRUE)
  summary_PUR<-as.data.frame(levels(test4))
  colnames(summary_PUR)[1]<-"PU_ID"
  summary_PUR<-merge(summary_PUR,unique_class, by="PU_ID")
  
  #FUNCTION FOR PLOTTING
  #Create Map for report
  myColors1 <- brewer.pal(9,"Set1")
  myColors2 <- brewer.pal(8,"Accent")
  myColors3 <- brewer.pal(12,"Paired")
  myColors4 <- brewer.pal(9, "Pastel1")
  myColors5 <- brewer.pal(8, "Set2")
  myColors6 <- brewer.pal(8, "Dark2")
  myColors7 <- brewer.pal(11, "Spectral")
  myColors  <-c(myColors1,myColors7, myColors2, myColors3, myColors4, myColors5, myColors6)
  rm(myColors1,myColors7, myColors2, myColors3, myColors4, myColors5, myColors6)
  
  
  #Plot 6 (Peta hasil rekonsiliasi)
  PUR.Rec.lab<-unique_class
  #PUR.Rec.lab$COUNT<-NULL
  myColors.PUR.Rec <- myColors[1:length(unique(PUR.Rec.lab$PU_ID))]
  ColScale.PUR.Rec<-scale_fill_manual(name="Planning Unit",breaks=PUR.Rec.lab$PU_ID, labels=PUR.Rec.lab$resolved, values = myColors.PUR.Rec )
  plot.PUR.Rec  <- gplot(test4, maxpixels=100000) + geom_raster(aes(fill=as.factor(value))) +
    ColScale.PUR.Rec + coord_equal() + ggtitle(paste("Final Reconciliation Map")) +
    theme(plot.title = element_text(lineheight= 5, face="bold")) +
    theme( axis.title.x=element_blank(),axis.title.y=element_blank(),
           panel.grid.major=element_blank(), panel.grid.minor=element_blank(),
           legend.title = element_text(size=8),
           legend.text = element_text(size = 6),
           legend.key.height = unit(0.25, "cm"),
           legend.key.width = unit(0.25, "cm"))
  
  #barplot(reconciliation phase 1 summary)
  area_rec1<-summary_PUR
  myColors.RPB <- myColors[1:length(unique(area_rec1$resolved))]
  names(myColors.RPB) <- unique(area_rec1$resolved)
  ColScale.RPB<-scale_fill_manual(values = myColors.RPB)
  Rec.phs.bar<-ggplot(data=area_rec1, aes(x=resolved, y=COUNT, fill=resolved)) + geom_bar(stat="identity") +coord_flip() + ColScale.RPB +
    geom_text(data=area_rec1, aes(x=resolved, y=COUNT, label=round(COUNT, 1)),size=3, vjust=0.1) +
    ggtitle(paste("Hasil akhir rekonsiliasi" )) + guides(fill=FALSE) + ylab("Luas (ha)") +
    theme(plot.title = element_text(lineheight= 5, face="bold")) + scale_y_continuous() +
    theme(axis.title.x=element_blank(), axis.text.x = element_text(size=8),
          panel.grid.major=element_blank(), panel.grid.minor=element_blank())
  
  
  #rtf report file
  title<-"\\b\\fs40 LUMENS-PUR Project Report\\b0\\fs20"
  sub_title<-"\\b\\fs32 REKONSILIASI UNIT PERENCANAAN MENGGUNAKAN DATA ACUAN\\b0\\fs20"
  date<-paste("Date : ", date(), sep="")
  time_start<-paste("Processing started : ", time_start, sep="")
  time_end<-paste("Processing ended : ", eval(parse(text=(paste("Sys.time ()")))), sep="")
  area_name_rep<-paste("\\b", "\\fs20", tempEnv$location, "\\b0","\\fs20")
  line<-paste("------------------------------------------------------------------------------------------------------------------------------------------------")
  rtffile <- RTF("LUMENS_PUR_FINAL_report.lpr", font.size=9)
  addParagraph(rtffile, title)
  addParagraph(rtffile, sub_title)
  addNewLine(rtffile)
  addParagraph(rtffile, line)
  addParagraph(rtffile, date)
  addParagraph(rtffile, time_start)
  addParagraph(rtffile, time_end)
  addParagraph(rtffile, line)
  addNewLine(rtffile)
  addParagraph(rtffile, "Rekonsiliasi unit perencanaan adalah proses untuk menyelesaikan tumpang-tindih ijin dengan merujuk pada peta acuan/referensi fungsi. Rekonsiliasi dilakukan dengan menganalisa kesesuaian fungsi antara data-data ijin dengan data referensi. Data ijin yang dimaksud datapat berupa data konsesi pengelolaan hutan, ijin perkebunan, ijin tambang dan lain sebagainya, Sedangkan data referensi yang digunakan dapat berupa data rencana tata ruang atau penunjukan kawasan. ")
  addNewLine(rtffile)
  
  addParagraph(rtffile, "\\b \\fs32 HASIL REKONSILIASI \\b0 \\fs20")
  addParagraph(rtffile, line)
  addParagraph(rtffile, "Pada bagian ini ditunjukkan hasil proses rekonsiliasi dengan menggunakan peta referensi ")
  addNewLine(rtffile)
  addNewLine(rtffile)
  addNewLine(rtffile)
  addPlot.RTF(rtffile, plot.fun=plot, width=6.7, height=4.73, res=150, plot.PUR.Rec )
  addNewLine(rtffile)
  addTable(rtffile, summary_PUR)
  addNewLine(rtffile)
  addPlot.RTF(rtffile, plot.fun=plot, width=6.7, height=4.73, res=150, Rec.phs.bar )
  addNewLine(rtffile)
  
  done(rtffile)
  
  summary_PUR$COUNT<-NULL
  write.csv(summary_PUR, "PUR_final_lookup_table.csv", row.names=FALSE)
  
  #PUR.index = PUR.index + 1
  #resave(PUR.index, file=proj.file)
  
  command<-paste("start ", "winword ", working_directory, "/LUMENS_PUR_FINAL_report.lpr", sep="" )
  shell(command)
}
