##[PUR]=group
##PUR_rec1=output raster

library(grid)
library(gridExtra)
library(rasterVis)
library(ggplot2)
library(RColorBrewer)
library(rtf)
library(foreign)
library(spatial.tools)

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
setwd(wd_usertemp)
datalist<-as.data.frame(list.files(path=wd_usertemp, full.names=TRUE, pattern="\\.tif$"))
datalist2<-as.data.frame(list.files(path=working_directory, pattern="PURREF", full.names=TRUE))
#datalist3<-as.data.frame(list.files(path=wd_usertemp, pattern="-ADD.tif", full.names=TRUE))

#====PREPARE REFERENCE DATA====
ref<-as.character(datalist2[2,])
ref.table<-as.character(datalist2[1,])
ref <- raster(ref)
ref.name<-names(ref)
lookup_ref<- read.table(ref.table, header=TRUE, sep=",",)
n<-ncol(lookup_ref)
n1<-n-1
n2<-n-2
lookup_ref<-lookup_ref[,c(n2,n1,n)]
colnames(lookup_ref)[1]<-"REFERENCE"

#====PREPARE PLANNING UNIT FILE====
file<-datalist
file.number <- nrow(datalist)
command1 <- paste()
command2 <- paste()
command3 <- paste()
central_attr<-NULL
for(i in 1:file.number) {
  input <- as.character(file[i,])
  eval(parse(text=(paste("pu", i, ' <- ', 'raster("', input, '")', sep="")))) #pu1 <- rasterize(pu_v1, r)
  eval(parse(text=(paste("pu", i, '.name<-names(pu', i, ')', sep=""))))
  eval(parse(text=(paste("pu", i, ".name<-substr(pu", i, ".name, 1, nchar(pu", i, ".name) - 3)", sep=""))))
  eval(parse(text=(paste( "central_attr<-append(central_attr, pu", i, ".name)", sep=""     ))))

  eval(parse(text=(paste("pu", i, "<-spatial_sync_raster(pu", i, ',ref, method = "ngb")', sep=""))))
  eval(parse(text=(paste("pu", i, "[is.na(pu", i, ")]<-0", sep="")))) #pu1[is.na(pu1)]<-0
  
  if (i != file.number) {
    command1<-paste(command1, "pu", i, ",", sep="")
    command2<-paste(command2, "pu", i, "[]", ",", sep="")
    command3<-paste(command3, "Var",i, ",", sep="")
  } else {
    command1<-paste(command1, "pu", i, sep="")
    command2<-paste(command2, "pu", i, "[]", sep="")
    command3<-paste(command3, "Var", i, sep="")
  }
}

#====COMBINE PLANNING UNIT FILES AND REFERENCE FILE====
ref.number <- file.number+1 #numOfReference
PUR <- ref 
command1 <- paste(command1, ",ref", sep="") 
command2 <- paste(command2, ",ref[]", sep="") 
command3 <- paste(command3, ",Var", as.character(ref.number), sep="")
eval(parse(text=(paste("PUR[] <- as.integer(interaction(", command2, "))", sep="")))) 
PUR <- ratify(PUR, filename='PUR.grd', count=TRUE, overwrite=TRUE) 
eval(parse(text=(paste("PUR_stack <- stack(", command1, ")", sep="")))) 
PUR_db <- crosstab(PUR_stack)
for (h in 1:(file.number+1)) {
  eval(parse(tex=(paste("PUR_db$Var", h, "[is.na(PUR_db$Var", h, ")]<-0", sep=""))))
}

eval(parse(text=(paste("PUR_db <- transform(PUR_db, unique_id=as.integer(interaction(", command3, ", drop=TRUE)))", sep="")))); 
PUR_db <- PUR_db[ which(PUR_db$Freq > 0),] 
eval(parse(text=(paste("PUR_db <- PUR_db[ which(PUR_db$Var", ref.number, '!="NA"),]', sep=""))))

#==CONDUCT RECONCILIATION==#
for(i in 1:(file.number)) {
  eval(parse(text=(paste("colnames(PUR_db)[",i,"]<-pu", i, ".name", sep=""))))
}
colnames(PUR_db)[file.number+1]<-ref.name
colnames(lookup_ref)[2]<-ref.name
PUR_dbmod<-merge(PUR_db,lookup_ref, by=ref.name)
for(j in 1:(file.number)) {
  eval(parse(text=(paste("name<-pu", j, ".name", sep=""))))
  eval(parse(text=(paste("PUR_dbmod<-within(PUR_dbmod,{cek", j, "<-as.numeric(", name, "==IDS)})",sep=""))))
}

command4<-paste()
for (p in 1:file.number) {
  if (p!=file.number) {
    eval(parse(text=(paste("command4<-paste(command4,", '"cek', p, '+', '")', sep=""))))
  } else {
    eval(parse(text=(paste("command4<-paste(command4,", '"cek', p, '")', sep=""))))
  }
}
PUR_dbmod<-within(PUR_dbmod, {reconcile1<-eval(parse(text=(command4)))})
PUR_dbmod<-within(PUR_dbmod, {reconcile_attr<-ifelse(reconcile1==0,as.character(REFERENCE), "unresolved")})

command5<-paste()
for (r in 1:file.number) {
  if (r!=file.number) {
    eval(parse(text=(paste("command5<-paste(command5, ", '"(cek",', r,',"*",' , r, ', ")+", sep="")', sep="" ))))
  }
  else {
    eval(parse(text=(paste("command5<-paste(command5, ", '"(cek",', r,',"*",' , r, ', ")", sep="")', sep="" ))))
  }
}
PUR_dbmod<-within(PUR_dbmod, {reconcile_attr2<-ifelse(reconcile1==1, reconcile_attr2<-eval(parse(text=(command5))),100)})

central_attr<-as.data.frame(central_attr)
numb_ca<-nrow(central_attr)
numb_ca<-as.data.frame(seq(numb_ca))
central_attr<-cbind(numb_ca,central_attr)
central_attrmod<-central_attr
colnames(central_attrmod)[2]="Rec_phase1"
colnames(central_attrmod)[1]="reconcile_attr2"
add5<- c("none")
add6<- c(100)
add_22<- data.frame(add5,add6)
colnames(add_22)[1]="Rec_phase1"
colnames(add_22)[2]="reconcile_attr2"
central_attrmod<-rbind(central_attrmod, add_22)

PUR_dbfinal<-merge(PUR_dbmod,central_attrmod, by='reconcile_attr2')
PUR_dbfinal<-within(PUR_dbfinal, {
  Rec_phase1<-ifelse(Rec_phase1=="none", as.character(reconcile_attr), as.character(Rec_phase1))})
len <- nrow(PUR_dbfinal)
angka = 1
PUR_dbfinal$Rec_phase1b<-PUR_dbfinal$Rec_phase1
for(s in 1:len){
  if(as.character(PUR_dbfinal$Rec_phase1[s])=="unresolved"){
    eval(parse(text=(paste("PUR_dbfinal$Rec_phase1b[", s, "]<-'unresolved_case", angka, "'", sep='')))) #PUR_dbfinal$Rec_phase1[50]<-"unresolved"
    angka = angka + 1
  }
}

PUR_dbfinal2<-PUR_dbfinal[,c('unique_id','Rec_phase1b')]
colnames(PUR_dbfinal2)[1]= "ID"
test1<-unique(PUR_dbfinal2)[1]
test2<-unique(PUR_dbfinal2)[2]
test3<-cbind(test1,test2)
levels(PUR)<-merge((levels(PUR)),test3,by="ID"); #output shapefile
PUR_rec1 <- deratify(PUR,'Rec_phase1b')
PUR_rec2<-ratify(PUR_rec1, filename='PUR_rec1.grd',count=TRUE,overwrite=TRUE) #cuma untuk di merge aja
levels(PUR_rec1)<-merge((levels(PUR_rec1)),levels(PUR_rec2),by="ID")
PUR_rec3<-stack(PUR, PUR_rec1)
setwd(working_directory)
writeRaster(PUR_rec1, filename="PUR_reconciliation_result", format="GTiff", overwrite=TRUE)

#DATABASE HANDLING
database_final<-PUR_dbfinal
database_unresolved<-subset(PUR_dbfinal, Rec_phase1 == "unresolved")
test_unresolve<-nrow(database_unresolved)
database_final<-as.data.frame(levels(PUR_rec1))
data_attribute<-database_final[,c(1,2)]
setwd(working_directory)
write.csv(data_attribute, "PUR_attribute.csv", row.names=FALSE)
write.dbf(data_attribute, "PUR_attribute.dbf")

if (test_unresolve!=0) {
  len <- nrow(database_unresolved)
  for(r in 1:file.number){
    eval(parse(text=(paste("database_unresolved$PU_", r, '<-"NULL"', sep=""))))
    word1<-paste("cek", r, sep="")
    word2<-paste("PU_", r, sep="")
    for(s in 1:len){
      eval(parse(text=(paste("if((database_unresolved$", word1, "[", s, "])>0){database_unresolved$", word2, "[", s, "]<-pu", r, ".name} else {database_unresolved$", word2, "[", s, ']<-"-"}', sep=""))))
    }
  }
  
  numberx<-ncol(database_unresolved)
  numbery<-numberx-(file.number)
  database_unresolved_out<-database_unresolved[,c(numbery:numberx)]
  dat1<-as.data.frame(database_unresolved$unique_id)
  dat2<-as.data.frame(database_unresolved$Freq)
  dat3<-as.data.frame(database_unresolved$REFERENCE)
  colnames(dat1)[1]<-"ID"
  colnames(dat2)[1]<-"COUNT"
  colnames(dat3)[1]<-"REFERENCE"
  database_unresolved_out<-cbind(database_unresolved_out, dat3, dat2)
  database_unresolved_out<-merge(data_attribute, database_unresolved_out, by="Rec_phase1b")
  write.csv(database_unresolved_out, "PUR_unresolved_case.csv")
} else {
  database_unresolved_out<-as.data.frame("There are no unresolved area in this analysis session")
  colnames(database_unresolved_out)[1]<-"Reconciliation result"
  #PUR.index = PUR.index + 1
  #resave(PUR.index, file=proj.file)
}

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
PUR.Rec.lab<-database_final
PUR.Rec.lab$COUNT<-NULL
myColors.PUR.Rec <- myColors[1:length(unique(PUR.Rec.lab$ID))]
ColScale.PUR.Rec<-scale_fill_manual(name="Planning Unit",breaks=PUR.Rec.lab$ID, labels=PUR.Rec.lab$Rec_phase1, values = myColors.PUR.Rec )
plot.PUR.Rec  <- gplot(PUR_rec2, maxpixels=100000) + geom_raster(aes(fill=as.factor(value))) +
  ColScale.PUR.Rec + coord_equal() + ggtitle(paste("Reconciliation Map")) +
  theme(plot.title = element_text(lineheight= 5, face="bold")) +
  theme( axis.title.x=element_blank(),axis.title.y=element_blank(),
         panel.grid.major=element_blank(), panel.grid.minor=element_blank(),
         legend.title = element_text(size=8),
         legend.text = element_text(size = 6),
         legend.key.height = unit(0.25, "cm"),
         legend.key.width = unit(0.25, "cm"))

#barplot(reconciliation phase 1 summary)
area_rec1<-database_final
myColors.RPB <- myColors[1:length(unique(area_rec1$Rec_phase1b))]
names(myColors.RPB) <- unique(area_rec1$Rec_phase1b)
ColScale.RPB<-scale_fill_manual(values = myColors.RPB)
Rec.phs.bar<-ggplot(data=area_rec1, aes(x=Rec_phase1b, y=COUNT, fill=Rec_phase1b)) + geom_bar(stat="identity") +coord_flip() + ColScale.RPB +
  geom_text(data=area_rec1, aes(x=Rec_phase1b, y=COUNT, label=round(COUNT, 1)),size=3, vjust=0.1) +
  ggtitle(paste("Unit Perencanaan Fase 1" )) + guides(fill=FALSE) + ylab("Luas (ha)") +
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
rtffile <- RTF("LUMENS_PUR_report_reconcile.lpr", font.size=9)
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
addParagraph(rtffile, "\\b \\fs32 DATA YANG DIGUNAKAN \\b0 \\fs20")
addParagraph(rtffile, line)
addNewLine(rtffile)
addParagraph(rtffile, "\\b Data acuan \\b0")
addNewLine(rtffile)
addParagraph(rtffile, "Data acuan adalah data yang digunakan sebagai referensi dalam melakukan pengecekan kesesuaian fungsi peta-peta unit perencanaan dengan fungsi referensi. Peta ini dapat berupa peta acuan penunjukan kawasan atau peta tata ruang. Pada prinsipnya, data referensi adalah data dengan tingkat kepastian hukum tertinggi atau data yang apling dipercaya sebagai acuan fungsi unit perencanaan di sebuah daerah")
addNewLine(rtffile)

#datalist2[1]<-NULL
addTable(rtffile, datalist2)
#datalist[1]<-NULL
addNewLine(rtffile)
addParagraph(rtffile, "\\b Data ijin \\b0")
addNewLine(rtffile)
addParagraph(rtffile, "Data ijin adalah data-data unit perencanaan yang akan digunakan untuk menunjukkan konfigurasi perencanaan penggunaan lahan di sebuah daerah. Data-data dalam bentuk peta ini menggambarkan arahan pengelolaan atau perubahan penggunaan lahan pada sebuah bagian bentang lahan")
addNewLine(rtffile)
addTable(rtffile, datalist)
addNewLine(rtffile)
addPlot.RTF(rtffile, plot.fun=print, width=6.7, height=3.73, res=150, plot(PUR_stack))
addNewLine(rtffile)

addParagraph(rtffile, "\\b \\fs32 HASIL REKONSILIASI \\b0 \\fs20")
addParagraph(rtffile, line)
addParagraph(rtffile, "Pada bagian ini ditunjukkan hasil proses rekonsiliasi dengan menggunakan peta referensi ")
addNewLine(rtffile)
addNewLine(rtffile)
addNewLine(rtffile)
addPlot.RTF(rtffile, plot.fun=plot, width=6.7, height=3.73, res=150, plot.PUR.Rec )
addNewLine(rtffile)
addTable(rtffile, database_final)
addNewLine(rtffile)
addPlot.RTF(rtffile, plot.fun=plot, width=6.7, height=3.73, res=150, Rec.phs.bar )
addNewLine(rtffile)

addParagraph(rtffile, "\\b \\fs32 DATA IJIN YANG TIDAK TERREKONSILIASI \\b0 \\fs20")
addParagraph(rtffile, line)
addNewLine(rtffile)
addParagraph(rtffile, "Data ijin yang tidak dapat terekonsiliasi akan tercantum pada bagian ini. Rekonsiliasi berbasis acuan fungsi, tidak dapat dilakukan jika ditemukan dua atau lebih unit perencanaan yang memiliki kesuaian fungsi dengan data acuan/referensi. Jika hal ini terjadi maka proses rekonsiliasi harus dilanjutkan melalui diskusi dengan semau pemangku kepentingan yang terkait ")
addNewLine(rtffile)
addTable(rtffile, database_unresolved_out, font.size=7)
addNewLine(rtffile)

done(rtffile)

command<-paste("start ", "winword ", working_directory, "/LUMENS_PUR_report_reconcile.lpr", sep="" )
shell(command)
