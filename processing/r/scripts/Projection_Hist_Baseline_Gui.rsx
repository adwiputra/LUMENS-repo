##[SCIENDO]=group
##iteration=number 5

#====LOAD LIBRARY====
library(plyr)
library(reshape2)
library(ggplot2)
library(foreign)
library(rtf)
library(tcltk)

time_start<-paste(eval(parse(text=(paste("Sys.time ()")))), sep="")

#====READ LUMENS LOG FILE====
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
load(proj.file)

#====READ QUES-C DB FROM LUMENS DB====
quesc_list<-as.data.frame(ls(pattern="QUESC_database_"))
n<-nrow(quesc_list)
if(n==0){
msgBox <- tkmessageBox(title = "SCIENDO",
message = "No QUES-C database found",
icon = "info",
type = "ok")
quit()
}
data.y<-NULL
data.w<-NULL
for (q in 1:n) {
n_dta<-nchar(as.character(factor(quesc_list[q,1])))
data.x<-substr(as.character(factor(quesc_list[q,1])), (n_dta-8), (n_dta-5))
data.z<-substr(as.character(factor(quesc_list[q,1])), (n_dta-3), n_dta)
if(data.z > data.x){
data.y<-c(data.y,data.x)
data.w<-c(data.w,data.z)
} else {
data.y<-c(data.y,data.z)
data.w<-c(data.w,data.x)
}
}
qdata<-as.data.frame(cbind(data.y,data.w))

#====SELECT QUES-C DATABASE TO BE ANALYZED====
quesc_list$usage<-0
colnames(quesc_list)[1]="database"
repeat{
quesc_list<-edit(quesc_list)
if(sum(quesc_list$usage)==1){
break
} else {
msgBox <- tkmessageBox(title = "Based on period",
message = "Choose at least one QUES-C database. Retry?",
icon = "question",
type = "retrycancel", default="retry")
if(as.character(msgBox)=="cancel"){
quit()
}
}
}
qdata<-cbind(quesc_list,qdata)
qdata2<-qdata[which(qdata$usage==1),]
qdata2$usage<-NULL
quesc_db<-as.character(qdata2[1,1])
T1<-as.numeric(as.character(qdata2[1,2]))
T2<-as.numeric(as.character(qdata2[1,3]))
pu_name<-substr(as.character(factor(quesc_list[q,1])), 16:(n_dta-10), (n_dta-10))

#====CREATE FOLDER AND WORKING DIRECTORY====
SCIENDO1.index=SCIENDO1.index+1
hist_folder<-paste("Historical_", pu_name,"_", T1,"_",T2,"_",SCIENDO1.index,sep="")
result_dir<-paste(dirname(proj.file),"/SCIENDO/", sep="")
setwd(result_dir)
dir.create(hist_folder)

result_dir<-paste(result_dir, hist_folder, sep='')
setwd(result_dir)

data<-eval(parse(text=(paste(quesc_db))))
data2<-eval(parse(text=(paste(quesc_db))))
period <- T2-T1
#CALCULATE TRANSITION PROBABILITY MATRIX
n.zone<-nrow(as.data.frame(unique(data2$ZONE)))
data2.melt <- melt(data = data2, id.vars=c('ID_LC2','ZONE'), measure.vars=c('COUNT'))
lu.count.zone.t2<- dcast(data = data2.melt, formula = ID_LC2 + ZONE ~ ., fun.aggregate = sum)
data2.melt <- melt(data = data2, id.vars=c('ID_LC1','ZONE'), measure.vars=c('COUNT'))
lu.count.zone.t1<- dcast(data = data2.melt, formula = ID_LC1 + ZONE ~ ., fun.aggregate = sum)
colnames(lu.count.zone.t1)[3]<-"COUNT.LU.ZONE.t1"
colnames(lu.count.zone.t2)[3]<-"COUNT.LU.ZONE.t2"
data2<-merge(data2,lu.count.zone.t1, by=c("ID_LC1", "ZONE"))
data2<-merge(data2,lu.count.zone.t2, by.x=c("ID_LC1", "ZONE"), by.y=c("ID_LC2", "ZONE"))
data2$TPM1<-data2$COUNT/data2$COUNT.LU.ZONE.t1

#HANDLING NEW EMERGING LAND USE TYPE IN TPM
data2 <- replace(data2, is.na(data2), 0)
data2.cek<- melt(data = data2, id.vars=c('ID_LC1','ZONE'), measure.vars=c('TPM1'))
data2.cek<- dcast(data = data2.cek, formula = ID_LC1 + ZONE ~ ., fun.aggregate = sum)
colnames(data2.cek)[3]<-"CEK"
data2.cek1<-subset(data2.cek, CEK==0)
if(nrow(data2.cek1)!=0){
data2.cek1$ACT<-"Fix"
}
data2.cek2<-subset(data2.cek, CEK>0)
if(nrow(data2.cek2)!=0){
data2.cek2$ACT<-"Ignore"
}
data2.cek<-rbind(data2.cek1,data2.cek2)
data2.cek$CEK<-NULL
data3<-merge(data2,data2.cek, by=c("ID_LC1", "ZONE"))
data3.cek1<-subset(data3, ACT=="Fix")
data3.cek2<-subset(data3, ACT=="Ignore")
if(nrow(data3.cek1)){
data3.cek1a<-subset(data3.cek1, ID_LC1==ID_LC2)
data3.cek1b<-subset(data3.cek1, ID_LC1!=ID_LC2)
data3.cek1a$TPM1<-1
data4<-rbind(data3.cek1a,data3.cek1b,data3.cek2)
} else {
data4<-data3.cek2
}

#CALCULATE PREDICTED AREA AT ITERATION 1
data4$COUNT.it0<-data4$COUNT
data4$COUNT.it1<-data4$TPM1*data4$COUNT.LU.ZONE.t2

#CALCULATE PREDICTED AREA FOR NEXT N ITERATION
#ORIGINAL CODE
#data4.melt <- melt(data = data4, id.vars=c('ID_LC2','ZONE'), measure.vars=c('COUNT.it1'))
#lu.count.zone.t3<- dcast(data = data4.melt, formula = ID_LC2 + ZONE ~ ., fun.aggregate = sum)
#colnames(lu.count.zone.t3)[3]<-"COUNT.LU.ZONE.t3"
#data4<-merge(data4,lu.count.zone.t3, by.x=c("ID_LC1", "ZONE"), by.y=c("ID_LC2", "ZONE"))
#data4$COUNT.it2<-data4$TPM1*data4$COUNT.LU.ZONE.t3
for (w in 2:iteration) {
eval(parse(text=(paste("data4.melt <- melt(data = data4, id.vars=c('ID_LC2','ZONE'), measure.vars=c('COUNT.it",w-1,"'))", sep=""))))
eval(parse(text=(paste("lu.count.zone.t", w+1, "<- dcast(data = data4.melt, formula = ID_LC2 + ZONE ~ ., fun.aggregate = sum)", sep=""))))
eval(parse(text=(paste("colnames(lu.count.zone.t", w+1,')[3]<-"COUNT.LU.ZONE.t', w+1, '"', sep=""))))
eval(parse(text=(paste('data4<-merge(data4,lu.count.zone.t', w+1, ', by.x=c("ID_LC1", "ZONE"), by.y=c("ID_LC2", "ZONE"))', sep=""))))
eval(parse(text=(paste("data4$COUNT.it", w, "<-data4$TPM1*data4$COUNT.LU.ZONE.t", w+1, sep=""))))
}
LUTMDatabase<-data4
total<-sum(LUTMDatabase$COUNT)

#TRANSITION MATRIX & TPM FOR EACH ZONE (Optional)
#data4zone1 <- subset(data4, ZONE==1)
#data4zone1tpm <- melt(data=data4zone1, id.vars=c('LC_t1','LC_t2'), measure.vars=c('TPM1'))
#data4zone1count <- melt(data=data4zone1, id.vars=c('LC_t1','LC_t2'), measure.vars=c('COUNT'))
#data4zone1tpmcast <- dcast(data = data4zone1tpm, formula = LC_t1 ~ LC_t2, fun.aggregate = sum)
#data4zone1countcast <- dcast(data = data4zone1count, formula = LC_t1 ~ LC_t2, fun.aggregate = sum)
#write.dbf(data4zone1countcast, 'data4zone1')

#CALCULATE EMISSION/SEQUESTRATION
for(i in 0:iteration){
eval(parse(text=(paste( "LUTMDatabase$EM", i, " <- (LUTMDatabase$CARBON_t1 - LUTMDatabase$CARBON_t2) * LUTMDatabase$ck_em * LUTMDatabase$COUNT.it", i, " * 3.67", sep="" ))))
eval(parse(text=(paste( "LUTMDatabase$SQ", i, " <- (LUTMDatabase$CARBON_t2 - LUTMDatabase$CARBON_t1) * LUTMDatabase$ck_sq * LUTMDatabase$COUNT.it", i, " * 3.67", sep="" ))))
}

#SUMMARY
#Parameters <- c("Total emission (CO2 eq)", "Total sequestration (CO2 eq)", "Net emission (CO2 eq)", "Emission rate (CO2 eq/(ha.yr))", "Cumulative emission (CO2 eq/(ha.yr))")
Parameters <- c("Emisi Per-Ha Area (ton CO2-eq/ha.tahun)", "Sequestrasi Per-Ha Area (ton CO2-eq/ha.tahun)", "Emisi Total (ton CO2-eq/tahun)", "Sequestrasi Total (ton CO2-eq/tahun)", "Emisi Bersih Per-Ha Area (ton CO2-eq/ha.tahun)", "Emisi Bersih (ton CO2-eq/tahun)", "Cumulative emission (ton CO2-eq/ha.year)")

# sum_em0 <- sum(LUTMDatabase$EM0)
# sum_sq0 <- sum(LUTMDatabase$SQ0)
# net_em0 <- sum(sum_em0-sum_sq0)
# rate_em0 <- net_em0/(total*period)
# cum0 <- 0
# Base <- c(sum_em0,sum_sq0,net_em0,rate_em0,cum0)

sum_em0 <- sum(LUTMDatabase$EM0)
sum_sq0 <- sum(LUTMDatabase$SQ0)
net_em0 <- sum(sum_em0-sum_sq0)
em_tot0 <- sum_em0 / period
sq_tot0 <- sum_sq0 / period
net_y0 <- net_em0 / period
em_ha0 <- sum_em0 / (total*period)
sq_ha0 <- sum_sq0 / (total*period)
net_ha0 <- net_em0 / (total*period)
cum0 <- em_ha0
Base <- c(em_ha0,sq_ha0,em_tot0,sq_tot0,net_ha0,net_y0,cum0)

for(i in 1:iteration){
eval(parse(text=(paste( "sum_em", i, " <- sum(LUTMDatabase$EM", i, ")", sep="" ))))
eval(parse(text=(paste( "sum_sq", i, " <- sum(LUTMDatabase$SQ", i, ")", sep="" ))))
eval(parse(text=(paste( "net_em", i, " <- sum(sum_em", i, " - sum_sq", i, ")", sep="" ))))
eval(parse(text=(paste( "em_tot", i, " <- sum_em", i, " / period", sep="" ))))
eval(parse(text=(paste( "sq_tot", i, " <- sum_sq", i, " / period", sep="" ))))
eval(parse(text=(paste( "net_y", i, " <- net_em", i, " / period", sep="" ))))
eval(parse(text=(paste( "em_ha", i, " <- sum_em", i, " / (total*period)", sep="" ))))
eval(parse(text=(paste( "sq_ha", i, " <- sum_sq", i, " / (total*period)", sep="" ))))
eval(parse(text=(paste( "net_ha", i, " <- net_em", i, " / (total*period)", sep="" ))))
if(i==1){
eval(parse(text=(paste( "cum1 <- em_ha0 + em_ha1", sep="" ))))
} else {
eval(parse(text=(paste( "cum", i, " <- cum", i-1, " + em_ha", i, sep="" ))))
}
eval(parse(text=(paste( "Iteration", i, " <- c(em_ha", i, ", sq_ha", i, ", em_tot", i, ", sq_tot", i, ", net_ha", i, ", net_y", i, ", cum", i, ")", sep="" ))))
if(i==1){
eval(parse(text=(paste( "summary_SCIENDO_iteration1 <- data.frame(Parameters, Base, Iteration1)", sep="" ))))
} else {
eval(parse(text=(paste( "summary_SCIENDO_iteration", i, " <- data.frame(summary_SCIENDO_iteration", i-1, ", Iteration", i, ")", sep="" ))))
}
}

#SAVE SCIENDO-LUWES Database
SCIENDO_LUWES <- LUTMDatabase

#SAVE SCIENDO-LUWES Summary
eval(parse(text=(paste( "SCIENDO_LUWES_summary <- summary_SCIENDO_iteration", iteration, sep="" ))))

#CONDUCT ANALYSIS ON THE DATASET
SCIENDO_LUWES_summary[,2:ncol(SCIENDO_LUWES_summary)]<-round(SCIENDO_LUWES_summary[,2:ncol(SCIENDO_LUWES_summary)],digits=2)
SL_overall<-SCIENDO_LUWES_summary
SL_analysis<-SCIENDO_LUWES
SL_overall.melt <- melt(data = SL_overall)
SL_overall.melt.cast <- dcast(data = SL_overall.melt, formula = Parameters ~ variable, fun.aggregate = sum, subset = .(Parameters=="Cumulative emission (ton CO2-eq/ha.year)"))
SL_overall_data<- melt(data = SL_overall.melt.cast)
#SL_overall_data<-SL_overall_data[-c(1),]

#t_1<-T2
#t_2<-t_1+period
#Period.db<-as.data.frame(NULL)
# for ( i in 1:nrow(SL_overall_data)){
#   period.int<-paste(t_1,"-",t_2, sep="")
#   Period.db<-c(Period.db,period.int)
#   t_1<-t_1+period
#   t_2<-t_1+period
# }
# Period.db<-as.character(Period.db)
t_1<-T1
t_2<-t_1+period
Periode<-as.data.frame(NULL)
for ( i in 1:nrow(SL_overall_data)){
period.int<-paste(t_1,"-",t_2, sep="")
Periode<-c(Periode,period.int)
t_1<-t_1+period
t_2<-t_1+period
}
Periode<-as.character(Periode)

m.var<-'EM0'
for(i in 1:iteration){
var<-paste('EM',i,sep="")
m.var<-c(m.var,var)
}

#write output to file
#write.dbf(SCIENDO_LUWES,"SCIENDO-LUWES_database.dbf")

row.number<-nrow(SL_overall_data)
#SL_overall_data[(row.number+1),]<-SL_overall_data[1,]
#NewData<-Summary[1,2:(iteration_number+2)]
#NewData<-cumsum(t(NewData))
#SL_overall_data[3]<-NewData
NewID<-seq(1,(iteration+1))
SL_overall_data[2]<-NewID

plot1<-ggplot(SL_overall_data,aes(variable,value,group=1,fill=Parameters))+ geom_line(colour="red")+
geom_point(colour="red", size=4, shape=21, fill="white") +
geom_text(data=SL_overall_data, aes(x=variable, y=value, label=round(value, 1)),size=3, hjust=1.5,vjust=-0.5) +
scale_x_discrete(breaks=SL_overall_data$variable ,labels=Periode) +
xlab('Year') +  ylab('Cum.CO2-eq/ha.yr') +
theme( legend.title = element_text(size=8),legend.text = element_text(size = 8))

SL_overall_data$value<-round(SL_overall_data$value,digits=2)
Cum.em<-as.data.frame(cbind(Periode,SL_overall_data$value))
colnames(Cum.em)[1]<-"Periode"
colnames(Cum.em)[2]<-"Cumulative emission rate (CO2eq/ha.yr)"

SL_netem.melt.cast <- dcast(data = SL_overall.melt, formula = Parameters ~ variable, fun.aggregate = sum, subset = .(Parameters=="Emisi Bersih (ton CO2-eq/tahun)"))
SL_netem_data<- melt(data = SL_netem.melt.cast)
#SL_netem_data<-SL_netem_data[-c(1),]
SL_netem_data[2]<-NewID
SL_netem_data$value<-cumsum(t(SL_netem_data$value))

Net.em<-as.data.frame(cbind(Periode,SL_netem_data$value))
Net.em.plot<-ggplot(data=Net.em, aes(x=Periode, y=V2)) + xlab('Periode') + ylab('Emisi Bersih Kumulatif (ton CO2-eq/tahun)') +  geom_bar(stat='identity') + coord_flip()
colnames(Net.em)[1]<-"Periode"
colnames(Net.em)[2]<-"Emisi Bersih Kumulatif (ton CO2-eq/tahun)"

SL_netem_ha.melt.cast <- dcast(data = SL_overall.melt, formula = Parameters ~ variable, fun.aggregate = sum, subset = .(Parameters=="Emisi Bersih Per-Ha Area (ton CO2-eq/ha.tahun)"))
SL_netem_ha_data<- melt(data = SL_netem_ha.melt.cast)
SL_netem_ha_data[2]<-NewID
SL_netem_ha_data$value<-cumsum(t(SL_netem_ha_data$value))

Net_ha.em<-as.data.frame(cbind(Periode,SL_netem_ha_data$value))
Net_ha.em.plot<-ggplot(data=Net_ha.em, aes(x=Periode, y=V2)) + xlab('Periode') + ylab('Emisi Bersih Kumulatif Per-Ha Area (ton CO2-eq/ha.tahun)') +  geom_bar(stat='identity') + coord_flip()
colnames(Net_ha.em)[1]<-"Periode"
colnames(Net_ha.em)[2]<-"Emisi Bersih Kumulatif Per-Ha Area (ton CO2-eq/ha.tahun)"

plot2 <- ggplot(SL_netem_data,aes(variable,value,group=1,fill=Parameters))+ geom_line(colour="red")+
geom_point(colour="red", size=4, shape=21, fill="white") +
geom_text(data=SL_netem_data, aes(x=variable, y=value, label=round(value, 1)),size=3, hjust=1.5,vjust=-0.5) +
scale_x_discrete(breaks=SL_netem_data$variable ,labels=Periode) +
xlab('Periode') +  ylab('Emisi Bersih (ton CO2-eq/tahun)') +
theme( legend.title = element_text(size=8),legend.text = element_text(size = 8))

TableSum<-SCIENDO_LUWES_summary[1:6,]
#NewYear<-paste(as.character(T1),as.character(T2),sep="-")
#Period.db<-append(NewYear,Period.db)
CName<-append("Parameters", Periode)
colnames(TableSum) <- c(CName)

pu_em0 <- melt(data = LUTMDatabase, id.vars=c('ZONE','Z_NAME'), measure.vars=c('EM0'))
pu_em0 <- dcast(data = pu_em0, formula = Z_NAME + ZONE ~ variable, fun.aggregate = sum )
pu_sq0 <- melt(data = LUTMDatabase, id.vars=c('ZONE','Z_NAME'), measure.vars=c('SQ0'))
pu_sq0 <- dcast(data = pu_sq0, formula = Z_NAME + ZONE ~ variable, fun.aggregate = sum )

for(i in 1:iteration){
eval(parse(text=(paste("pu_em", i, " <- melt(data = LUTMDatabase, id.vars=c('ZONE','Z_NAME'), measure.vars=c('EM", i, "'))", sep=""))))
eval(parse(text=(paste("pu_em", i, " <- dcast(data = pu_em", i, ", formula = Z_NAME + ZONE ~ variable, fun.aggregate = sum )", sep=""))))
eval(parse(text=(paste("pu_sq", i, " <- melt(data = LUTMDatabase, id.vars=c('ZONE','Z_NAME'), measure.vars=c('SQ", i, "'))", sep=""))))
eval(parse(text=(paste("pu_sq", i, " <- dcast(data = pu_sq", i, ", formula = Z_NAME + ZONE ~ variable, fun.aggregate = sum )", sep=""))))

eval(parse(text=(paste("pu_em0<-merge(pu_em0, pu_em", i, ", by=c('ZONE', 'Z_NAME'))", sep=""))))
eval(parse(text=(paste("pu_sq0<-merge(pu_sq0, pu_sq", i, ", by=c('ZONE', 'Z_NAME'))", sep=""))))
}

pu_em0$ZONE<-NULL
pu_em0_round<-round((pu_em0[,(2:(iteration+2))]),digits=2)
pu_em0<-cbind((pu_em0[1]),pu_em0_round)
colnames(pu_em0) <- c(CName)

pu_sq0$ZONE<-NULL
pu_sq0_round<-round((pu_sq0[,(2:(iteration+2))]),digits=2)
pu_sq0<-cbind((pu_sq0[1]),pu_sq0_round)
colnames(pu_sq0) <- c(CName)

pu_em0$Total<-rowSums(pu_em0[,2:(iteration+2)])
pu_sq0$Total<-rowSums(pu_sq0[,2:(iteration+2)])
pu_em0_total<-sum(pu_em0$Total)
pu_sq0_total<-sum(pu_sq0$Total)
pu_em0$Percentage<-round(((pu_em0$Total/pu_em0_total)*100),digits=2)
pu_sq0$Percentage<-round(((pu_sq0$Total/pu_sq0_total)*100),digits=2)

pu_em0 <- pu_em0[order(-pu_em0$Percentage),]
pu_sq0 <- pu_sq0[order(-pu_sq0$Percentage),]

#====WRITE REPORT====
title<-"\\b\\fs32 LUMENS-SCIENDO - PROYEKSI BASELINE EMISI HISTORIS\\b0\\fs20"
#sub_title<-"\\b\\fs28 RAD GRK - 4.1. Skenario Baseline (HISTORIS) \\b0\\fs20"
date<-paste("Date : ", as.character(Sys.Date()), sep="")
time_start<-paste("Processing started : ", time_start, sep="")
time_end<-paste("Processing ended : ", eval(parse(text=(paste("Sys.time ()")))), sep="")
line<-paste("------------------------------------------------------------------------------------------------------------------------------------------------")
I_O_period_1_rep<-paste("\\b","\\fs20", T1)
I_O_period_2_rep<-paste("\\b","\\fs20", T2)
rtffile <- RTF("LUMENS_SCIENDO-PHB_report.lpr", font.size=9)
addParagraph(rtffile, "\\b\\fs32 Hasil Analisis\\b0\\fs20")
addNewLine(rtffile)
addNewLine(rtffile)
addParagraph(rtffile, title)
addNewLine(rtffile)
addNewLine(rtffile)
#addParagraph(rtffile, sub_title)
addNewLine(rtffile)
addParagraph(rtffile, line)
addParagraph(rtffile, date)
addParagraph(rtffile, time_start)
addParagraph(rtffile, time_end)
addParagraph(rtffile, line)
addNewLine(rtffile)
addParagraph(rtffile, paste("\\b \\fs28 Prediksi Emisi dan Proyeksi Emisi Historis\\b0 \\fs20 "))
addParagraph(rtffile, paste("\\b \\fs20 Emisi Tahunan - Intisari Perhitungan dan Prediksi \\b0 \\fs20"))
addTable(rtffile,TableSum, font.size=8) #SCIENDO_LUWES_summary
addParagraph(rtffile, "Keterangan:")
addParagraph(rtffile, "Tabel ini menunjukan emisi historis dan prediksi perhitungan emisi berdasarkan laju perubahan penggunaan lahan pada tiap periode. Perhitungan disajikan dalam bentuk per-hektar dan total seluruh area.")
addNewLine(rtffile)
addParagraph(rtffile, paste("\\b \\fs20 Emisi Bersih Kumulatif Per-hektar \\b0 \\fs20"))
addTable(rtffile,Net_ha.em)
addParagraph(rtffile, "Keterangan:")
addParagraph(rtffile, "Tabel ini menunjukan emisi bersih kumulatif per-hektar area")
addNewLine(rtffile)
addParagraph(rtffile, paste("\\b \\fs20 Emisi Bersih Kumulatif (Total) \\b0 \\fs20"))
addTable(rtffile,Net.em)
addParagraph(rtffile, "Keterangan:")
addParagraph(rtffile, "Tabel ini menunjukan emisi bersih kumulatif total")
addNewLine(rtffile)
addNewLine(rtffile)
addParagraph(rtffile, paste("\\b \\fs20 Grafik Emisi Bersih Tahunan (Per-hektar) \\b0 \\fs20"))
addParagraph(rtffile, "Grafik ini menunjukan nilai emisi bersih tahunan per-hektar.")
addPlot(rtffile,plot.fun=print, width=6.7,height=3,res=300, Net_ha.em.plot)
addNewLine(rtffile)
addParagraph(rtffile, paste("\\b \\fs20 Grafik Emisi Bersih Tahunan (Total) \\b0 \\fs20"))
addParagraph(rtffile, "Grafik ini menunjukan nilai emisi CO2-eq tiap tahun, biasanya digunakan untuk menyajikan baseline emisi di suatu wilayah.")
addPlot(rtffile,plot.fun=print, width=6.7,height=3,res=300, Net.em.plot)
addNewLine(rtffile)
addNewLine(rtffile)
addParagraph(rtffile, paste("\\b \\fs20 Grafik Emisi Bersih Kumulatif (Total) \\b0 \\fs20"))
addParagraph(rtffile, "Grafik ini menunjukan nilai emisi CO2-eq kumulatif, biasanya digunakan untuk menyajikan baseline emisi di suatu wilayah.")
addNewLine(rtffile)
addPlot(rtffile,plot.fun=print, width=6.7,height=3,res=300, plot2)
addNewLine(rtffile)
addParagraph(rtffile, paste("\\b \\fs20 Prediksi emisi pada unit perencanaan (ton CO2eq/yr) \\b0 \\fs20"))
addTable(rtffile,pu_em0, font.size=8)
addNewLine(rtffile)
addParagraph(rtffile, paste("\\b \\fs20 Prediksi sequestrasi pada unit perencanaan (ton CO2eq/yr) \\b0 \\fs20"))
addTable(rtffile,pu_sq0, font.size=8)
addNewLine(rtffile)

done(rtffile)

command<-paste("start ", "winword ", result_dir, "/LUMENS_SCIENDO-PHB_report.lpr", sep="" )
shell(command)

#====CREATE .CAR FILE FOR NEW ABACUS====
pu <- melt(data = data2, id.vars=c('ZONE','Z_NAME'), measure.vars=c('COUNT'))
pu <- dcast(data = pu, formula = Z_NAME + ZONE ~ variable, fun.aggregate = sum )

d1<-melt(data=data2, id.vars=c('ID_LC1','LC_t1'))
d1$variable<-d1$value<-NULL
d1<-unique(d1)
d2<-melt(data=data2, id.vars=c('ID_LC2','LC_t2'))
d2$variable<-d2$value<-NULL
d2<-unique(d2)
lu1.lost<-unique(data2$ID_LC2)[is.na(match(unique(data2$ID_LC2),unique(data2$ID_LC1)))]
lu2.lost<-unique(data2$ID_LC1)[is.na(match(unique(data2$ID_LC1),unique(data2$ID_LC2)))]
lu.lost<-c(as.integer(as.matrix(lu1.lost)),as.integer(as.matrix(lu2.lost)))
while(length(lu1.lost)!=0 || length(lu2.lost)!=0){
if(length(lu1.lost)!=0){
new.lu<-d2[d2$ID_LC2 %in% lu1.lost, 1:2]
colnames(new.lu)[1]<-'ID_LC1'
colnames(new.lu)[2]<-'LC_t1'
d1<-rbind(d1,new.lu)
lu1.lost<-unique(d2$ID_LC2)[is.na(match(unique(d2$ID_LC2),unique(d1$ID_LC1)))]
} else if(length(lu2.lost)!=0){
new.lu<-d1[d1$ID_LC1 %in% lu2.lost, 1:2]
colnames(new.lu)[1]<-'ID_LC2'
colnames(new.lu)[2]<-'LC_t2'
d2<-rbind(d2,new.lu)
lu2.lost<-unique(d1$ID_LC1)[is.na(match(unique(d1$ID_LC1),unique(d2$ID_LC2)))]
}
}
colnames(d2)<-c("ID","CLASS")

name.matrix<-d2
name.matrix$order<-name.matrix$ID
name.matrix$order<-as.numeric(levels(name.matrix$order))[name.matrix$order]
name.matrix<- as.data.frame(name.matrix[order(name.matrix$order, decreasing=FALSE),])
name.matrix$order<-NULL

options(scipen=999)
Scenario_name<-gsub(" ","","Historical baseline")

#CREATING ABACUS PROJECT FILE
#General and Project information
Gnrl.info.1<-c("file_version")
Gnrl.info.2<-c("1.2.0")
Gnrl.info<-paste(Gnrl.info.1,Gnrl.info.2,sep="=")
#fileConn<-file(paste(result_dir,"/",Scenario_name,".txt",sep=""))
text0<-"#GENERAL"
write(text0, paste(result_dir, "/",Scenario_name,".car",sep=""),append=TRUE, sep="\t")
write.table(Gnrl.info, paste(result_dir,"/",Scenario_name,".car",sep=""),append=TRUE,quote=FALSE,col.names=FALSE,row.names=FALSE,sep="\t")

Project.info.1<-c("title","description", "baseyear0", "baseyear1", "n_iteration")
Project.info.2<-c("SCIENDO", "Project description", T1, T2, iteration)
Project.info<-paste(Project.info.1,Project.info.2,sep="=")
text<-"\n#PROJECT"
write(text, paste(result_dir, "/",Scenario_name,".car",sep=""),append=TRUE, sep="\t")
write.table(Project.info, paste(result_dir,"/",Scenario_name,".car",sep=""),append=TRUE,quote=FALSE,col.names=FALSE,row.names=FALSE,sep="\t")

#Landcover information
text<-"\n#LANDCOVER"
write(text, paste(result_dir, "/",Scenario_name,".car",sep=""),append=TRUE, sep="\t")
name.matrix$lc_id<-0:(nrow(name.matrix)-1)
name.lc<-as.data.frame(name.matrix$lc_id)
name.lc$label<-name.matrix$CLASS
name.lc$description<-''
colnames(name.lc)[1]='//lc_id'
name.lc.temp<-name.lc
write.table(name.lc, paste(result_dir, "/",Scenario_name,".car",sep=""),append=TRUE,quote=FALSE,col.names=TRUE,row.names=FALSE,sep="\t")

#Zone information
text<-"\n#ZONE"
write(text, paste(result_dir, "/",Scenario_name,".car",sep=""),append=TRUE, sep="\t")
pu$order<-pu$ZONE
pu$order<-as.numeric(levels(pu$order))[pu$order]
pu<-as.data.frame(pu[order(pu$order, decreasing=FALSE),])
pu$zone_id<-0:(nrow(pu)-1)
name.pu<-as.data.frame(pu$zone_id)
name.pu$label<-pu$Z_NAME
name.pu$description<-''
colnames(name.pu)[1]='//zone_id'
name.pu.temp<-name.pu
colnames(name.pu.temp)[2]='Z_NAME'
write.table(name.pu, paste(result_dir, "/",Scenario_name,".car",sep=""),append=TRUE,quote=FALSE,col.names=TRUE,row.names=FALSE,sep="\t")

#Landcover change
text<-"\n#LANDCOVER_CHANGE"
write(text, paste(result_dir, "/",Scenario_name,".car",sep=""),append=TRUE, sep="\t")
name.lcc<-data
name.lcc$iteration_id<-name.lcc$'//scenario_id'<-0
name.lcc<-merge(name.lcc, name.pu.temp, by="Z_NAME")
colnames(name.lc.temp)[2]='LC_t1'
name.lcc<-merge(name.lcc, name.lc.temp, by="LC_t1")
name.lcc$lc1_id<-name.lcc$'//lc_id'
name.lcc$'//lc_id'<-NULL
colnames(name.lc.temp)[2]='LC_t2'
name.lcc<-merge(name.lcc, name.lc.temp, by="LC_t2")
name.lcc$lc2_id<-name.lcc$'//lc_id'
name.lcc<-name.lcc[c('//scenario_id','iteration_id','//zone_id','lc1_id','lc2_id','COUNT')]
colnames(name.lcc)[3]='zone_id'
colnames(name.lcc)[4]='lc1_id'
colnames(name.lcc)[5]='lc2_id'
colnames(name.lcc)[6]='area'
name.lcc<-name.lcc[which(name.lcc$area != 0),]
write.table(name.lcc, paste(result_dir, "/",Scenario_name,".car",sep=""),append=TRUE,quote=FALSE,col.names=TRUE,row.names=FALSE,sep="\t")

#Carbon Stock
text<-"\n#CARBONSTOCK"
write(text, paste(result_dir, "/",Scenario_name,".car",sep=""),append=TRUE, sep="\t")
colnames(name.lc.temp)[2]<-"LC"
colnames(lut.c)[2]<-"LC"
colnames(lut.c)[3]<-"CARBON"
name.carbon.temp<-merge(name.lc.temp, lut.c, by="LC")
name.carbon.temp<-name.carbon.temp[c('//lc_id','CARBON')]
name.carbon<-data.frame()
for(i in 0:(nrow(name.pu)-1)){
for(j in 1:nrow(name.lc)){
name.carbon<-rbind(name.carbon, c(0, 0, i, name.carbon.temp$'//lc_id'[j], name.carbon.temp$CARBON[j]))
}
}
colnames(name.carbon)=c('//scenario_id','iteration_id','zone_id','lc_id','area')
write.table(name.carbon, paste(result_dir, "/",Scenario_name,".car",sep=""),append=TRUE,quote=FALSE, col.names=TRUE,row.names=FALSE, sep="\t")

Abacus_Project_File = paste(result_dir, "/",Scenario_name,".car",sep="")

#====DATABASE EXPORT
#eval(parse(text=(paste("Historical_data_", T1, "_", T2, "_", SCIENDO1.index, "<-LUTMDatabase", sep=""))))
#historical<-paste("Historical_data_", T1, "_", T2, "_", SCIENDO1.index, sep="")
#eval(parse(text=(paste("resave(SCIENDO1.index, ", historical, ", file='",proj.file,"')", sep=""))))
resave(SCIENDO1.index, file=proj.file)
