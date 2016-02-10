# Multi-level Biodiversity Compostion and Importance Value Index (IVI) Calculator
# (adapted from .....)
# @author Dony Indiarto, Aditio Ramadhian, Mega Senoputri, Alfa Nugraha, Sidiq Pambudi, Dienda Citasyari, Degi Harja

wdir<-"D:/LUMENS/Ques B Intro"
input_file<-"D:/LUMENS/Ques B Intro/Data-Sumsel.csv"
province<-"Sumatra Selatan"
location<- "Banyuasin"
province<- "Sumatra Selatan"
country<-"Indonesia"
T1<-2015


time_start<-paste(eval(parse(text=(paste("Sys.time ()")))), sep="")
library(reshape2)
library(reshape)
library(plyr)
library(ggplot2)
#library(labdsv)
library(vegan)
library(rtf)

#===check metadata, create folder===
setwd(wdir)
if (file.exists("qb_metadata.csv")){
  cmeta<-read.table("qb_metadata.csv", header=T, sep=",")
  cmeta[1,2]<-as.numeric(cmeta[1,2])+1
  subDir<-paste("QuES-B_Tree_diversity_analysis_",cmeta[1,2],"_",cmeta[1,1])
  out.dir<-file.path(wdir,subDir)
  dir.create(file.path(wdir,subDir))
  write.csv(cmeta, "qb_metadata.csv", row.names=F)
} else{
  cmeta<-data.frame(date=Sys.Date(), id=1)
  subDir<-paste("QuES-B_Tree_diversity_analysis_",cmeta[1,2],"_",cmeta[1,1])
  dir.create(file.path(wdir,subDir))
  out.dir<-file.path(wdir,subDir)
  dir.create(file.path(wdir,subDir))
  write.csv(cmeta, "qb_metadata.csv", row.names=F)
}


#====open file====
file_raw_data <- read.csv(input_file, sep=",")

colnames(file_raw_data) <- c("District", "Sub.District", "Village",
                             "Site", "Date", "Kode.Plot", "System",
                             "replication","Age" ,"Latitude", "Longitude", "Sub.plot.size",
                             "Area", "Sp.code", "local.name", "sp.name", "plot.unit", "girth","diameter","height","density","indv")

file_raw_data$basal_area<-((22/7)*(file_raw_data$diameter/2)^2)
file_raw_data$basal_area<- round(file_raw_data$basal_area, 3)

#====reshape for quick stat at stats data====
setwd(file.path(wdir,subDir))
#melt.cast function for numeric data type
melt.cast.persistem<-function(data,kolom.a, kolom.b, fungsi) {
  x.melt <- melt(data = data, id.vars=c(paste(kolom.a)), measure.vars=c(paste(kolom.b)))
  eval(parse(text=( paste('x.melt.cast <- dcast(data = x.melt, formula =',kolom.a,' ~ variable, fun.aggregate =',fungsi,')', sep=''))))
  return(x.melt.cast)
}

#melt.cast function for character data type
melt.cast.persistem2<-function(data,kolom.a, kolom.b, fungsi) {
  x.melt <- melt(data = data, id.vars=c(paste(kolom.a)), measure.vars=c(paste(kolom.b)))
  x.melt.unique<-unique(x.melt)
  x.melt.unique$indvu<-1
  eval(parse(text=( paste('x.melt.cast <- dcast(data = x.melt.unique, formula =',kolom.a,' ~ indvu, fun.aggregate =',fungsi,')', sep=''))))
  return(x.melt.cast)
}

#LU type - total individu
mc.sum<-melt.cast.persistem(file_raw_data,"System", "indv", 'sum')
#LU type - total plot unit
mc.plot_unit<-melt.cast.persistem(file_raw_data,"System", "plot.unit", 'max')
#LU type - maximum diameter
mc.max<-melt.cast.persistem(file_raw_data,"System", "basal_area", 'max')
#LU type - minimum diameter
mc.min<-melt.cast.persistem(file_raw_data,"System", "basal_area", 'min')
#LU type - mean diameter
mc.mean<-melt.cast.persistem(file_raw_data,"System", "basal_area", 'mean')
mc.mean$basal_area <- round(mc.mean$basal_area, 3)
#LU type - median diameter
mc.med<-melt.cast.persistem(file_raw_data,"System", "basal_area", 'median')
#LU type - sd diameter
mc.sd<-melt.cast.persistem(file_raw_data,"System", "basal_area", 'sd')
mc.sd$basal_area <- round(mc.sd$basal_area, 3)

#LU type - species richness
mc.sp_rich<-melt.cast.persistem2(file_raw_data,"System", "sp.name", 'length')
#LU type - total plot code
mc.sp_total_plot_code<-melt.cast.persistem2(file_raw_data,"System", "Kode.Plot", 'length')

#====quick statssummary table of main tree data====
quick.stats<-cbind(mc.sum,mc.sp_rich[2], mc.sp_total_plot_code[2],mc.plot_unit[2], mc.max[2], mc.min[2], mc.mean[2], mc.med[2], mc.sd[2])
colnames(quick.stats)<-c('Land_use_system','Total_Indv','Sp_Richness', 'total_plot', 'total_sub_plot', 'max_basal', 'min_basal', 'mean_basal', 'median_basal', 'sd_basal')


#==== IVI function ====
ivi.function<-function(raw.data){
  #IVI and H'
  #jumlah individu
  raw.data.agg <- aggregate(raw.data$indv, by=list(raw.data$sp.name), sum)
  names(raw.data.agg)[1] <- "Nama.Species"
  names(raw.data.agg)[2] <- "Jumlah.indv"
  
  #frekuensi
  raw.data.freq <- raw.data[,c("sp.name", "plot.unit")]
  raw.data.freq$sp.name <- sort(raw.data.freq$sp.name)
  raw.data.freq$count <- 1
  raw.data.freq <- cast(raw.data.freq, sp.name~plot.unit)
  raw.data.freq[raw.data.freq == 0] <- NA
  raw.data.freq <- melt(raw.data.freq, id=c("sp.name, Plot.unit"))
  raw.data.freq.fil <- raw.data.freq[complete.cases(raw.data.freq),] #removing NAs!!!!!!!!!!
  raw.data.freq.fil <- count(raw.data.freq.fil, 'sp.name')
  names(raw.data.freq.fil)[1] <- "Nama.Species"
  
  
  #BA
  raw.data.ba <- aggregate(raw.data$basal_area, by=list(raw.data$sp.name), sum)
  names(raw.data.ba)[1] <- "Nama.Species"
  names(raw.data.ba)[2] <- "BA.kumulatif"
  
  #density
  raw.data.dens <- raw.data.agg
  names(raw.data.dens)[2] <- "Kerapatan"
  raw.data.dens$Kerapatan <- raw.data.dens$Kerapatan/400 #luas area 400 m2
  
  #accumulation & INP
  raw.data.acc <- join_all(list(raw.data.agg, raw.data.freq.fil, raw.data.ba, raw.data.dens))
  as.vector(raw.data.acc$freq)
  raw.data.acc$freq.r <- raw.data.acc$freq/sum(raw.data.acc$freq)
  raw.data.acc$BA.r <- raw.data.acc$BA.kumulatif/sum(raw.data.acc$BA.kumulatif)
  raw.data.acc$kr.r <- raw.data.acc$Kerapatan/sum(raw.data.acc$Kerapatan)
  raw.data.acc$INP <- raw.data.acc$freq.r+raw.data.acc$BA.r+raw.data.acc$kr.r
  
  #H'
  raw.data.acc$Shannon <- -(sum(raw.data.acc$Jumlah.indv/sum(raw.data.acc$Jumlah.indv)*log(raw.data.acc$Jumlah.indv/sum(raw.data.acc$Jumlah.indv))))
  for(i in 6:length(raw.data.acc)){
  raw.data.acc[i] <- round(raw.data.acc[i],2)
}
  return(raw.data.acc)
}
#=====calculating dissimilarity indices==============

#defining function
horn.disty <- function(dataset = file_raw_data){
  
  #producing table with cnames: "System"  "sp.name" "variable" "value"
  m.bas.ar <- melt(dataset, id.vars = c("System","sp.name"), measure.vars = "basal_area")
  
  #producing table which is ready to be calculated with reshapeGUI()
  c.bas.ar <- dcast(data = m.bas.ar, formula = System ~ sp.name, fun.aggregate = sum)
  spec.bas.syst <- c.bas.ar[,2:ncol(c.bas.ar)]
  row.names(spec.bas.syst) <- c.bas.ar$System
  
  #3
  #calculating Morisita-Horn similarity indices among different land use syst.
  horn.ind <- vegdist(spec.bas.syst, method = "horn")
  
  #4
  #convert "dist" matrix to dataframe
  dis.indc.df <- as.data.frame(as.matrix(horn.ind))
  
  #5
  #create cont_table
  
  #buat dua baris sisipan di atas data index
  LU_names <- colnames(dis.indc.df)
  #baris 0
  b0_ctr_tab <- vector(mode = "character", length = ncol(dis.indc.df))
  for(i in 1:length(b0_ctr_tab)){
    if(i == 1) b0_ctr_tab[[i]] <- paste("FSQ_TABLE")
    else b0_ctr_tab[[i]] <- NA_character_
  }
  
  #baris 1
  b1_ctr_tab <- vector(mode = "character", length = ncol(dis.indc.df))
  for(i in 1:length(b1_ctr_tab)){
    if(i == 1) b1_ctr_tab[[i]] <- paste("#CLASS_LIST_LITERAL(",LU_names[i],sep="")
    else b1_ctr_tab[[i]] <- LU_names[i]
  }
  
  #baris 2
  b2_ctr_tab <- vector(mode = "character", length = ncol(dis.indc.df))
  for(i in 1:length(b2_ctr_tab)){
    if(i == 1) b2_ctr_tab[[i]] <- paste("CLASS_LIST_NUMERIC(1")
    else b2_ctr_tab[[i]] <- i
  }
  
  #rbind b0+b1+b2+dis.ind
  gabungan <- as.data.frame(rbind(b0_ctr_tab, b1_ctr_tab, b2_ctr_tab, dis.indc.df))
  
  write.table(gabungan, file = paste("ctr_tab_LU-sys",location,".csv", sep = ""), sep = ",", row.names = FALSE, col.names = FALSE, na = "")
  
  round(dis.indc.df,3)
  
}

dis.indc.df <-  horn.disty()


#=== plot level IVI ===
#plot level Importance value calculation
lookup_unique_plot<-data.matrix(sort(unique(file_raw_data$Kode.Plot)))

for (i in 1:nrow(lookup_unique_plot)){
  eval(parse(text=( paste('plot.x<-lookup_unique_plot[',i,']', sep=''))))
  eval(parse(text=( paste('plot.lvl <- file_raw_data[grep(plot.x, file_raw_data$Kode.Plot, ignore.case =T),]', sep=''))))
  eval(parse(text=( paste('plot.lvl.summary<- ivi.function(plot.lvl)', sep=''))))
  eval(parse(text=( paste('plot.lvl.summary$plot <- plot.x', sep=''))))
  eval(parse(text=( paste('plot.lvl.summary_',i,' <-plot.lvl.summary', sep='')))) ; #plot level summary
  eval(parse(text=( paste('plot.lvl.summary_',i,' <-plot.lvl.summary', sep=''))))
  eval(parse(text=( paste('write.csv(plot.lvl.summary, "plot.lvl.summary_',i,'.csv", row.names=TRUE)', sep=''))))
}


#=== land use system level IVI ===
#system level Importance value calculation
lookup_unique_system<-data.matrix(sort(unique(file_raw_data$System)))

for (i in 1:nrow(lookup_unique_system)){
  eval(parse(text=( paste('system.x<-lookup_unique_system[',i,']', sep=''))))
  eval(parse(text=( paste('system.lvl <- file_raw_data[grep(system.x, file_raw_data$System, ignore.case =T),]', sep=''))))
  eval(parse(text=( paste('system.lvl.summary<- ivi.function(system.lvl)', sep=''))))
  eval(parse(text=( paste('system.lvl.summary$system <- system.x', sep=''))))
  eval(parse(text=( paste('system.lvl.summary_',i,' <-system.lvl.summary', sep='')))) ; #system level summary
  eval(parse(text=( paste('system.lvl.top10_',i,' <-system.lvl.summary[order(-system.lvl.summary$INP),][1:10,]', sep=''))))
  eval(parse(text=( paste('write.csv(system.lvl.summary, "system.lvl.summary_',i,'.csv", row.names=TRUE)', sep=''))))
}


#===data preparation for species are curve at system level and diversity index at plot level===
system_lookup<-unique(file_raw_data$System)
file_raw_data.melt <- melt(data = file_raw_data, id.vars=c('System','Kode.Plot','sp.name'), measure.vars=c('indv'))


#===species diversity index at plot level grouped by land use system===

for (i in 1:length(system_lookup)){
  plot.y<-system_lookup[i]
  file_raw_data.melt2 <- file_raw_data.melt[grep(plot.y, file_raw_data.melt$System, ignore.case =T),]
  file_raw_data.melt.cast <- dcast(data = file_raw_data.melt2, formula = Kode.Plot ~ sp.name, fun.aggregate = sum)
  ncol.sp<-ncol(file_raw_data.melt.cast)
  eval(parse(text=( paste('shannon<-diversity(file_raw_data.melt.cast[2:ncol.sp], index = "shannon")', sep=''))))
  eval(parse(text=( paste('simpson<-diversity(file_raw_data.melt.cast[2:ncol.sp], index = "simpson")', sep=''))))
  eval(parse(text=( paste('fisher<-fisher.alpha(file_raw_data.melt.cast[2:ncol.sp], MARGIN=1)', sep=''))))
  eval(parse(text=( paste('index.summary_',i,'<-cbind(file_raw_data.melt.cast[1],shannon, simpson, fisher)', sep=''))));#diversity index summary at plot level
  eval(parse(text=( paste('index.summary_',i,'$shannon <- round(index.summary_',i,'$shannon, 3)', sep = ''))))
  eval(parse(text=( paste('index.summary_',i,'$simpson <- round(index.summary_',i,'$simpson, 3)', sep = ''))))
  eval(parse(text=( paste('index.summary_',i,'$fisher <- round(index.summary_',i,'$fisher, 3)', sep = ''))))
}


#===species diversity index at Land use system  level===
file_raw_data.melt.cast <- dcast(data = file_raw_data.melt, formula = System ~ sp.name, fun.aggregate = sum)
ncol.sp<-ncol(file_raw_data.melt.cast)
shannon<-diversity(file_raw_data.melt.cast[2:ncol.sp], index = "shannon")
shannon<- round(shannon, 3)
simpson<-diversity(file_raw_data.melt.cast[2:ncol.sp], index = "simpson")
simpson<- round(simpson, 3)
fisher<-fisher.alpha(file_raw_data.melt.cast[2:ncol.sp], MARGIN=1)
fisher<- round(fisher, 3)
index.summary.all<-cbind(file_raw_data.melt.cast[1],shannon, simpson, fisher);#diversity index summary at plot level

#===species area curve at land use system level (eg. LOF, UF, AF) ===
for (i in 1:length(system_lookup)){
  plot.y<-system_lookup[i]
  file_raw_data.melt2 <- file_raw_data.melt[grep(plot.y, file_raw_data.melt$System, ignore.case =T),]
  file_raw_data.melt.cast <- dcast(data = file_raw_data.melt2, formula = Kode.Plot ~ sp.name, fun.aggregate = sum)
  ncol.sp<-ncol(file_raw_data.melt.cast)
  eval(parse(text=( paste('sp.curve_a_',i,'<- specaccum(file_raw_data.melt.cast[2:ncol.sp])', sep=''))));#species area cummulative at system level
  eval(parse(text=( paste('sp.curve_b_',i,' <- specaccum(file_raw_data.melt.cast[2:ncol.sp], "random")', sep=''))))
  print(plot.y)
  print(eval(parse(text=( paste('summary(sp.curve_b_',i,')', sep='')))))
}
#plot(sp.curve_a_1, ci.type="poly", col="blue", lwd=2, ci.lty=0, ci.col="lightblue")
#boxplot(sp2, col="yellow", add=F, pch="+")


#=== species richness plot====
richness.melt.cast <- dcast(data = file_raw_data.melt, formula = Kode.Plot + System ~ value, fun.aggregate = sum)
colnames(richness.melt.cast)<-c("Kode.Plot", "System", "null", "Richness")
sp.rich.plot<- ggplot(richness.melt.cast, aes(x = factor(System), fill = factor(System), y = Richness)) +
  geom_dotplot(binaxis = "y", stackdir = "center", position = "dodge")

#=== DBH plot each land use system
#DBH
fungsi.dbh<-function(raw.data){
  raw.data$kelas.DBH <- 0
  raw.data$kelas.DBH <- ifelse((10<raw.data$diameter) & (20>=raw.data$diameter), "1",
                               ifelse((20<raw.data$diameter) & (30>=raw.data$diameter), "2",
                                      ifelse((30<raw.data$diameter) & (40>=raw.data$diameter), "3",
                                             ifelse((40<raw.data$diameter) & (50>=raw.data$diameter), "4",
                                                    ifelse((50<raw.data$diameter) & (60>=raw.data$diameter), "5", "6")))))
  as.numeric(raw.data$kelas.DBH)
  raw.data.DBH <- aggregate(raw.data$indv, by=list(raw.data$kelas.DBH), sum)
  colnames(raw.data.DBH)<-c("ID", "Total")
  kelas.dbh<-data.frame(ID=c(1,2,3,4,5,6), class=c("10-20 cm", "20-30 cm", "30-40 cm","40-50 cm", "50-60 cm", "60++ cm"))
  summary_DBH<-merge(kelas.dbh, raw.data.DBH, by="ID", all=T)
  return(summary_DBH)
}
lookup_unique_system<-data.matrix(sort(unique(file_raw_data$System)))
i=2
for (i in 1:nrow(lookup_unique_system)){
  eval(parse(text=( paste('system.x<-lookup_unique_system[',i,']', sep=''))))
  eval(parse(text=( paste('system.lvl <- file_raw_data[grep(system.x, file_raw_data$System, ignore.case =T),]', sep=''))))
  eval(parse(text=( paste('system.lvl.dbh<- fungsi.dbh(system.lvl)', sep=''))))
  eval(parse(text=( paste('system.lvl.dbh_',i,' <-system.lvl.dbh', sep='')))) ; #system level summary
  eval(parse(text=( paste('write.csv(system.lvl.dbh, "system.lvl.dbh_',i,'.csv", row.names=TRUE)', sep=''))))
}

#plotting<- beri judul
#summary_DBH_plot<-ggplot(summary_DBH, aes(x = factor(class), y = Total)) + geom_bar(stat = "identity")


#====Write Report====
rtffile <- RTF("LUMENS_QuES-B_Tree_diversity_analysis_.lpr", font.size=9)
title1<-"{\\colortbl;\\red0\\green0\\blue0;\\red255\\reen0\\blue0;\\red146\\green208\\blue80;\\red0\\green176\\blue240;\\red140\\green175\\blue71;\\red0\\green112\\blue192;\\red79\\green98\\blue40;} \\pard\\qr\\b\\fs70\\cf2 L\\cf3U\\cf4M\\cf5E\\cf6N\\cf7S \\cf1REPORT \\par\\b0\\fs20\\ql\\cf1"
title2<-paste("\\pard\\qr\\b\\fs40\\cf1 QuES-B Tree diversity analysis ", "for ", location, ", ", province, ", ", country, "\\par\\b0\\fs20\\ql\\cf1", sep="")
sub_title<-"\\cf2\\b\\fs32 ANALISIS KEANEKARAGAMAN HAYATI PADA LEVEL PLOT DAN SISTEM PENGGUNAAN LAHAN\\cf1\\b0\\fs20"
#date<-paste("Date : ", date, sep="")
time_start<-paste("Proses dimulai : ", time_start, sep="")
time_end<-paste("Proses selesai : ", eval(parse(text=(paste("Sys.time ()")))), sep="")
line<-paste("------------------------------------------------------------------------------------------------------------------------------------------------")
area_name_rep<-paste("\\b", "\\fs20", location, "\\b0","\\fs20")
I_O_period_1_rep<-paste("\\b","\\fs20", T1)
addNewLine(rtffile)
addNewLine(rtffile)
addNewLine(rtffile)
addNewLine(rtffile)
addNewLine(rtffile)
addNewLine(rtffile)
addNewLine(rtffile)
addNewLine(rtffile)
addNewLine(rtffile)
addNewLine(rtffile)
addNewLine(rtffile)
addNewLine(rtffile)
addNewLine(rtffile)
addNewLine(rtffile)
addNewLine(rtffile)
addNewLine(rtffile)
addNewLine(rtffile)
addNewLine(rtffile)
addNewLine(rtffile)
addNewLine(rtffile)
addParagraph(rtffile, title1)
addParagraph(rtffile, title2)
addNewLine(rtffile)
addParagraph(rtffile, line)
addParagraph(rtffile, time_start)
addParagraph(rtffile, time_end)
addParagraph(rtffile, line)
addNewLine(rtffile)
width<-as.vector(c(1.34,3.1))
addPageBreak(rtffile)
addParagraph(rtffile, sub_title)
addNewLine(rtffile)
addParagraph(rtffile, line)

#addParagraph(rtffile, date)
addNewLine(rtffile)
addParagraph(rtffile, "Analisis keanekaragaman hayati pohon dilakukan untuk mengevaluasi kualitas data, tingkat keterwakilan data, dan juga mendeskripsikan kondisi keanekaragaman hayati. Proses ini dilakukan pula sebagai masukan data untuk analisis keanekaragaman hayati di tingkat bentang lahan")
addNewLine(rtffile)
addParagraph(rtffile, paste("\\b \\fs24 INTISARI DATA KEANEKARAGAMAN HAYATI\\b0 \\fs24", sep=""))
addNewLine(rtffile)
addParagraph(rtffile, line)
addNewLine(rtffile)
addNewLine(rtffile)
addNewLine(rtffile)
addParagraph(rtffile, paste("\\b \\fs20 Tabel 1. Tabel ringkasan data keanekaragaman hayati pada tingkat sistem penggunaan lahan di \\b0 \\fs20 ", location, "\\b \\fs20 Tahun \\b0 \\fs20",  I_O_period_1_rep,sep=" "))
addTable(rtffile,quick.stats,font.size=8)
addNewLine(rtffile)
addNewLine(rtffile)
addPlot(rtffile,plot.fun=print, width=6.7,height=4,res=300, sp.rich.plot)
addParagraph(rtffile, paste("\\b \\fs20 Gambar 1. Distribusi kekayaan spesies di tiap plot pada masing-masing sistem penggunaan lahan di \\b0 \\fs20 ", location, "\\b \\fs20 Tahun \\b0 \\fs20", I_O_period_1_rep, sep=" "))
addNewLine(rtffile)
addNewLine(rtffile)
addParagraph(rtffile, paste("\\b \\fs20 Tabel 2. Tabel indeks keanekaragaman hayati pada tingkat sistem penggunaan lahan di \\b0 \\fs20 ", location, "\\b \\fs20 Tahun \\b0 \\fs20",  I_O_period_1_rep,sep=" "))
addTable(rtffile,index.summary.all,font.size=8)
addNewLine(rtffile)
addNewLine(rtffile)
addNewLine(rtffile)
addParagraph(rtffile, paste("\\b \\fs20 Tabel 3. Tabel indeks ketidak samaan pada tingkat sistem penggunaan lahan di \\b0 \\fs20 ", location, "\\b \\fs20 Tahun \\b0 \\fs20",  I_O_period_1_rep,sep=" "))
addTable(rtffile,dis.indc.df,font.size=8, row.names = TRUE)
addNewLine(rtffile)
addNewLine(rtffile)
addNewLine(rtffile)
addParagraph(rtffile, paste("\\b \\fs24 KEANEKARAGAMAN HAYATI PADA TIAP SISTEM PENGGUNAAN LAHAN\\b0 \\fs24", sep=""))
addNewLine(rtffile)
addParagraph(rtffile, line)
addNewLine(rtffile)
addNewLine(rtffile)

for (i in 1:nrow(lookup_unique_system)){
  lokasi.plot<-lookup_unique_system[i]
  addParagraph(rtffile, paste('\\b \\fs20 Tabel ',i,'. 10 Spesies penting pada sistem penggunaan lahan ',lokasi.plot,'di \\b0 \\fs20 ', location, '\\b \\fs20 Tahun \\b0 \\fs20',  I_O_period_1_rep,sep=" "))
  eval(parse(text=( paste('addTable(rtffile,system.lvl.top10_',i,',font.size=8)', sep=''))))
  addNewLine(rtffile)
}
addNewLine(rtffile)
addNewLine(rtffile)

for (i in 1:nrow(lookup_unique_system)){
  lokasi.plot<-lookup_unique_system[i]
  addParagraph(rtffile, paste('\\b \\fs20 Tabel ',i,'. Intisari indeks keanekaragaman hayati pada  sistem penggunaan lahan',lokasi.plot,' di \\b0 \\fs20 ', location, '\\b \\fs20 Tahun \\b0 \\fs20',  I_O_period_1_rep,sep=" "))
  eval(parse(text=( paste('addTable(rtffile,index.summary_',i,',font.size=8)', sep=''))))
  addNewLine(rtffile)
}

addNewLine(rtffile)

for (i in 1:nrow(lookup_unique_system)){
  lokasi.plot<-lookup_unique_system[i]
  eval(parse(text=( paste('summary_DBH_plot<-ggplot(system.lvl.dbh_',i,', aes(x = factor(class), y = Total)) + geom_bar(stat = "identity")', sep=''))))
  addPlot(rtffile,plot.fun=print, width=6.7,height=4,res=300, summary_DBH_plot)
  addParagraph(rtffile, paste('\\b \\fs20 Gambar ',i,'. Kelas DBH pada  sistem penggunaan lahan',lokasi.plot,' di \\b0 \\fs20 ', location, '\\b \\fs20 Tahun \\b0 \\fs20',  I_O_period_1_rep,sep=" "))
  addNewLine(rtffile)
}

for (i in 1:nrow(lookup_unique_system)){
  lokasi.plot<-lookup_unique_system[i]
  eval(parse(text=( paste('sp.curve.final<-sp.curve_a_',i,sep=''))))
  sp.curve.mod<-rbind(sp.curve.final[3]$sites,sp.curve.final[4]$richness, sp.curve.final[5]$sd)
  rownames(sp.curve.mod)<-c("sites","richness", "sd")
  sp.curve.mod<-as.data.frame(t(sp.curve.mod))
  plot.sac<-ggplot(sp.curve.mod, aes(x =sites, y =richness)) + geom_line(position='')+ geom_point()
  addPlot(rtffile,plot.fun=print, width=6.7,height=4,res=300, plot.sac)
  
  addParagraph(rtffile, paste('\\b \\fs20 Gambar ',i,'. Kurva penambahan spesies pada',lokasi.plot,' di \\b0 \\fs20 ', location, '\\b \\fs20 Tahun \\b0 \\fs20',  I_O_period_1_rep,sep=" "))
  addNewLine(rtffile)
}

done(rtffile)