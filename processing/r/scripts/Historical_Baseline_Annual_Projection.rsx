##[SCIENDO]=group
##iteration=number 10

library(ggplot2)
library(foreign)
library(rtf)
library(sp)
library(raster)
library(reshape2)
library(tcltk)
#include_peat=1

time_start<-paste(eval(parse(text=(paste("Sys.time ()")))), sep="")

#READ LUMENS LOG FILE
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


substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}

# SELECTING AVAILABLE QUES-C ANALYSIS
QUESC_list<-as.data.frame(ls(pattern="QUESC_database"))
colnames (QUESC_list) [1]<-"Data"
QUESC_list$Usage<-0

QUESC_list_n<-nrow(QUESC_list)
if(QUESC_list_n<2){
  msgBox <- tkmessageBox(title = "QUES",
                         message = "Minimum two QUES-C Analysis Result required",
                         icon = "info",
                         type = "ok")
  quit()
}

repeat{
  QUESC_list<-edit(QUESC_list)
  if(sum(QUESC_list$Usage)>2){
    break
  } else {
    msgBox <- tkmessageBox(title = "Annual projection",
                           message = "Choose at least three QUES-C database. Retry?",
                           icon = "question", 
                           type = "retrycancel", default="retry")
    if(as.character(msgBox)=="cancel"){
      quit()
    }
  }
}

QUESC_list <- QUESC_list[which(QUESC_list$Usage==1),]
QUESC_list$Usage<-NULL
QUESC_list_n<-nrow(QUESC_list)
dbase_all<-NULL

data<-as.character(QUESC_list [QUESC_list_n,1])
n_dta<-nchar(data)
t1<-as.integer(substr(data, (n_dta-8):(n_dta-5), (n_dta-5)))
t2<-as.integer(substr(data, (n_dta-3):n_dta, n_dta))
pu_name<-substr(data, 16:(n_dta-10), (n_dta-10))
pu_list<-as.data.frame(ls(pattern="pu_pu"))
colnames(pu_list)[1]<-"PU"
pu_list$PU<-as.character(pu_list$PU)
pu_list$Name<-"name"
pu_list$LUT<-"LUT"
for(i in 1:nrow(pu_list)){
  eval(parse(text=(paste("pu_list[",i,",2]<-names(",pu_list$PU[i],")", sep=""))))
  eval(parse(text=(paste("pu_list[",i,",3]<-'lut.pu",i,"'",sep=""))))
}
pu_list<-rbind(pu_list, c("ref", names(ref), "p.admin.df"))
pu_selected<-pu_list[which(pu_list$Name==pu_name),]
eval(parse(text=(paste("lut.pu<-", pu_selected$LUT ,sep=""))))

dirAnnual<-paste(dirname(proj.file), "/SCIENDO/Annual_", pu_name, sep="")
dir.create(dirAnnual, mode="0777")
setwd(dirAnnual)
workingDirectory<-dirAnnual

eval(parse(text=(paste("central_data<-", data, sep=""))))
central_data$key<- do.call(paste, c(central_data[c("LU_CHG", "Z_NAME")], sep = " in "))

for(i in 1:QUESC_list_n) {
  data<-as.character(QUESC_list [i,1])
  n_dta<-nchar(data)
  t1<-as.integer(substr(data, (n_dta-8):(n_dta-5), (n_dta-5)))
  t2<-as.integer(substr(data, (n_dta-3):n_dta, n_dta))
  eval(parse(text=(paste("dbase<-", data, sep=""))))
  dbase$Start_year<-t1
  dbase$End_year<-t2
  dbase$nYear<-dbase$End_year-dbase$Start_year
  data2<-dbase
  
  data2$ID_LC1<-as.character(data2$ID_LC1)
  data2$ID_LC2<-as.character(data2$ID_LC2)
  
  data2.1<-subset(data2, ID_LC1==ID_LC2)
  data2.2<-subset(data2, ID_LC1!=ID_LC2)
  
  data2.1$ID_LC1<-as.factor(data2.1$ID_LC1)
  data2.1$ID_LC2<-as.factor(data2.1$ID_LC2) 
  data2.2$ID_LC1<-as.factor(data2.2$ID_LC1)
  data2.2$ID_LC2<-as.factor(data2.2$ID_LC2)  
  
  data2.2$COUNTx<-data2.2$COUNT/data2.2$nYear
  data2.2$COUNTy<-data2.2$COUNT-data2.2$COUNTx
  data2.2$COUNT<-data2.2$COUNTx
  data2.2$COUNTx<-NULL
  data2.2melt <- melt(data = data2.2, id.vars=c('LC_t1','Z_NAME'), measure.vars=c('COUNTy'))
  data2.2cast<- dcast(data = data2.2melt, formula = LC_t1 + Z_NAME ~ ., fun.aggregate = sum,fill = 0, drop = FALSE)
  data2.2cast$key<- do.call(paste, c(data2.2cast[c("LC_t1", "Z_NAME")], sep = " in "))
  data2.2cast$LC_t1<-NULL
  data2.2cast$Z_NAME<-NULL
  data2.1$key<- do.call(paste, c(data2.1[c("LC_t1", "Z_NAME")], sep = " in "))
  data2.1<-merge(data2.1, data2.2cast, by="key", all=TRUE)
  data2.1<-replace(data2.1, is.na(data2.1), 0)
  data2.1$COUNT<-data2.1$COUNT+data2.1$.
  data2.1$key<-NULL
  data2.1$.<-NULL
  data2.2$COUNTy<-NULL
  data2ann<-rbind(data2.1,data2.2)
  
  #CALCULATE TRANSITION PROBABILITY MATRIX
  n.zone<-nrow(as.data.frame(unique(data2ann$Z_NAME)))
  data2.melt <- melt(data = data2ann, id.vars=c('LC_t2','Z_NAME'), measure.vars=c('COUNT'))
  lu.count.zone.t2<- dcast(data = data2.melt, formula = LC_t2 + Z_NAME ~ ., fun.aggregate = sum,fill = 0, drop = FALSE)
  data2.melt <- melt(data = data2ann, id.vars=c('LC_t1','Z_NAME'), measure.vars=c('COUNT'))
  lu.count.zone.t1<- dcast(data = data2.melt, formula = LC_t1 + Z_NAME ~ ., fun.aggregate = sum, fill = 0, drop = FALSE)
  colnames(lu.count.zone.t1)[3]<-"COUNT.LU.ZONE.t1"
  colnames(lu.count.zone.t2)[3]<-"COUNT.LU.ZONE.t2"
  data2ann<-merge(data2ann,lu.count.zone.t1, by=c("LC_t1", "Z_NAME"), all=TRUE)
  data2ann<-replace(data2ann, is.na(data2ann), 0)
  data2ann<-merge(data2ann,lu.count.zone.t2, by.x=c("LC_t1", "Z_NAME"), by.y=c("LC_t2", "Z_NAME"), all=TRUE)
  data2ann<-replace(data2ann, is.na(data2ann), 0)
  eval(parse(text=(paste("data2ann$TPM", i, "<-data2ann$COUNT/data2ann$COUNT.LU.ZONE.t1", sep=""))))
  
  #HANDLING NEW EMERGING LAND USE TYPE IN TPM
  data2ann <- replace(data2ann, is.na(data2ann), 0)
  data2.cek<- melt(data = data2ann, id.vars=c('ID_LC1','ZONE'), measure.vars=c(paste("TPM", i, sep="")))
  data2.cek<- dcast(data = data2.cek, formula = ID_LC1 + ZONE ~ ., fun.aggregate = sum, fill = 0, drop = FALSE)
  colnames(data2.cek)[3]<-"CEK"
  data2.cek1<-subset(data2.cek, CEK==0)
  data2.cek1$ACT<-"Fix"
  data2.cek2<-subset(data2.cek, CEK>0)
  data2.cek2$ACT<-"Ignore"
  data2.cek<-rbind(data2.cek1,data2.cek2)
  data2.cek$CEK<-NULL
  data3<-merge(data2ann,data2.cek, by=c("ID_LC1", "ZONE"), all=TRUE)
  data3<-replace(data3, is.na(data3), 0)
  
  data3.cek1<-subset(data3, ACT=="Fix")
  data3.cek1$ID_LC1<-as.character(data3.cek1$ID_LC1)
  data3.cek1$ID_LC2<-as.character(data3.cek1$ID_LC2)
  data3.cek1a<-subset(data3.cek1, ID_LC1==ID_LC2) #check if null
  data3.cek1b<-subset(data3.cek1, ID_LC1!=ID_LC2) #check if null
  data3.cek1a$ID_LC1<-as.factor(data3.cek1a$ID_LC1)
  data3.cek1a$ID_LC2<-as.factor(data3.cek1a$ID_LC2) 
  data3.cek1b$ID_LC1<-as.factor(data3.cek1b$ID_LC1)
  data3.cek1b$ID_LC2<-as.factor(data3.cek1b$ID_LC2)  
  if(nrow(data3.cek1a)!=0){
    eval(parse(text=(paste("data3.cek1a$TPM", i, "<-1", sep="")))) 
  }
  
  data3.cek2<-subset(data3, ACT=="Ignore")
  data3.cek2<-data3.cek2[which(data3.cek2$LU_CHG!=0),]
  
  data4<-rbind(data3.cek1a,data3.cek1b,data3.cek2)
  data4$key<- do.call(paste, c(data4[c("LU_CHG", "Z_NAME")], sep = " in "))
  eval(parse(text=(paste("data4<-subset(data4,select=c(key, TPM", i, "))", sep=""))))
  central_data<-merge(central_data, data4, by="key", all=TRUE)
}
data2ori<-data2
data2<-replace(central_data, is.na(central_data), 0)

command<-NULL
for(j in 1:QUESC_list_n){
  if (j!=QUESC_list_n) {
    command<-paste(command,"data2$TPM", j, "+", sep="")
  } else {
    command<-paste(command,"data2$TPM", j, sep="")
  }
}

eval(parse(text=(paste("data2$TPM1 <-(", command, ")/QUESC_list_n", sep="" ))))

n.zone<-nrow(as.data.frame(unique(data2$Z_NAME)))
data2.melt <- melt(data = data2, id.vars=c('LC_t2','Z_NAME'), measure.vars=c('COUNT'))
lu.count.zone.t2<- dcast(data = data2.melt, formula = LC_t2 + Z_NAME ~ ., fun.aggregate = sum, fill = 0, drop = FALSE)
data2.melt <- melt(data = data2, id.vars=c('LC_t1','Z_NAME'), measure.vars=c('COUNT'))
lu.count.zone.t1<- dcast(data = data2.melt, formula = LC_t1 + Z_NAME ~ ., fun.aggregate = sum, fill = 0, drop = FALSE)
colnames(lu.count.zone.t1)[3]<-"COUNT.LU.ZONE.t1"
colnames(lu.count.zone.t2)[3]<-"COUNT.LU.ZONE.t2"
data2<-merge(data2,lu.count.zone.t1, by=c("LC_t1", "Z_NAME"), all=TRUE)
data2<-merge(data2,lu.count.zone.t2, by.x=c("LC_t1", "Z_NAME"), by.y=c("LC_t2", "Z_NAME"), all=TRUE)
data2<-replace(data2, is.na(data2), 0)

#CALCULATE PREDICTED AREA AT ITERATION 1
data4<-data2
data4$COUNT.it0<-data4$COUNT
data4$COUNT.it1<-data4$TPM1*data4$COUNT.LU.ZONE.t2

#CALCULATE PREDICTED AREA FOR NEXT N ITERATION
for (w in 2:iteration) {
  eval(parse(text=(paste("data4.melt <- melt(data = data4, id.vars=c('LC_t2','ZONE'), measure.vars=c('COUNT.it",w-1,"'))", sep=""))))
  eval(parse(text=(paste("lu.count.zone.t", w+1, "<- dcast(data = data4.melt, formula = LC_t2 + ZONE ~ ., fun.aggregate = sum, fill = 0, drop = FALSE)", sep=""))))
  eval(parse(text=(paste("colnames(lu.count.zone.t", w+1,')[3]<-"COUNT.LU.ZONE.t', w+1, '"', sep=""))))
  eval(parse(text=(paste('data4<-merge(data4,lu.count.zone.t', w+1, ', by.x=c("LC_t1", "ZONE"), by.y=c("LC_t2", "ZONE"), all=TRUE)', sep=""))))
  data4<-replace(data4, is.na(data4), 0)
  eval(parse(text=(paste("data4$COUNT.it", w, "<-data4$TPM1*data4$COUNT.LU.ZONE.t", w+1, sep=""))))
}

tryCatch({
  write.dbf(data4,"SCIENDO-LUWES_annual_database.dbf")
},error=function(e){cat("dbf file can't be written", "\n")})

annual_emission<-NULL
for (y in 0:iteration) {
  eval(parse(text=(paste("data4$em_t", y,"<-data4$COUNT.it", y, "*(data4$CARBON_t1-data4$CARBON_t2)*data4$ck_em*3.67", sep=""))))
  eval(parse(text=(paste("emtot<-sum(data4$em_t", y,")", sep=""))))
  annual_emission<-c(annual_emission, emtot)
}

annual_sequestration<-NULL
for (x in 0:iteration) {
  eval(parse(text=(paste("data4$sq_t", x,"<-data4$COUNT.it", x, "*(data4$CARBON_t2-data4$CARBON_t1)*data4$ck_sq*3.67", sep=""))))
  eval(parse(text=(paste("sqtot<-sum(data4$sq_t", x,")", sep=""))))
  annual_sequestration<-c(annual_sequestration, sqtot)
}

cum_em<-cumsum(annual_emission)
cum_sq<-cumsum(annual_sequestration)
em<-as.data.frame(cbind(annual_emission, cum_em, annual_sequestration, cum_sq))
em$netem<-em$annual_emission-em$annual_sequestration
em$cum_netem<-cumsum(em$netem)

year<-as.character(QUESC_list [QUESC_list_n,1])
t0<-t2
yearsim<-c(t0:(t0+iteration))

em<-as.data.frame(cbind(yearsim,em))
em$yearsim<-factor(em$yearsim)
plot1<-ggplot(em,aes(yearsim,cum_em,group=1))+ geom_line(colour="red")+geom_point(colour="red", size=4, shape=21, fill="white")
plot2<-ggplot(em,aes(yearsim,annual_emission,group=1))+ geom_line(colour="red")+geom_point(colour="red", size=4, shape=21, fill="white")
plot3<-ggplot(em,aes(yearsim,annual_sequestration,group=1))+ geom_line(colour="red")+geom_point(colour="red", size=4, shape=21, fill="white")
plot4<-ggplot(em,aes(yearsim,cum_netem,group=1))+ geom_line(colour="red")+geom_point(colour="red", size=4, shape=21, fill="white")

#====CREATE .CAR FILE FOR NEW ABACUS====
# pu <- melt(data = data2, id.vars=c('ZONE','Z_NAME'), measure.vars=c('COUNT'))
# pu <- dcast(data = pu, formula = Z_NAME + ZONE ~ variable, fun.aggregate = sum )
# pu_zname <- lut.pu
# colnames(pu_zname)[1] <- "ZONE"
# colnames(pu_zname)[2] <- "Z_NAME"
# pu<-pu[which(pu$Z_NAME != 0),]
# pu <- merge(pu, pu_zname, by="Z_NAME")
# pu<-na.omit(pu)
# pu$Penunjukkan<-pu$ZONE.y<-pu$COUNT<-NULL
# rownames(pu)<-NULL
# colnames(pu)[2]="ZONE"

pu <- lut.pu
colnames(pu)[1] <- "ZONE"
colnames(pu)[2] <- "Z_NAME"

d1<-melt(data=data2, id.vars=c('ID_LC1','LC_t1'))
d1$variable<-d1$value<-NULL
d1<-unique(d1)
d1<-na.omit(d1)
d2<-melt(data=data2, id.vars=c('ID_LC2','LC_t2'))
d2$variable<-d2$value<-NULL
d2<-unique(d2)
d2<-na.omit(d2)
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
Scenario_name<-gsub(" ","","Annual projection")

#CREATING ABACUS PROJECT FILE
#General and Project information
Gnrl.info.1<-c("file_version")
Gnrl.info.2<-c("1.2.0")
Gnrl.info<-paste(Gnrl.info.1,Gnrl.info.2,sep="=")
#fileConn<-file(paste(result_dir,"/",Scenario_name,".txt",sep=""))
text0<-"#GENERAL"
write(text0, paste(dirAnnual, "/",Scenario_name,".car",sep=""),append=TRUE, sep="\t")
write.table(Gnrl.info, paste(dirAnnual,"/",Scenario_name,".car",sep=""),append=TRUE,quote=FALSE,col.names=FALSE,row.names=FALSE,sep="\t")

Project.info.1<-c("title","description", "baseyear0", "baseyear1", "n_iteration")
Project.info.2<-c("SCIENDO", "Project description", t2-1, t2, iteration)
Project.info<-paste(Project.info.1,Project.info.2,sep="=")
text<-"\n#PROJECT"
write(text, paste(dirAnnual, "/",Scenario_name,".car",sep=""),append=TRUE, sep="\t")
write.table(Project.info, paste(dirAnnual,"/",Scenario_name,".car",sep=""),append=TRUE,quote=FALSE,col.names=FALSE,row.names=FALSE,sep="\t")

#Landcover information
text<-"\n#LANDCOVER"
write(text, paste(dirAnnual, "/",Scenario_name,".car",sep=""),append=TRUE, sep="\t")
name.matrix$lc_id<-0:(nrow(name.matrix)-1)
name.lc<-as.data.frame(name.matrix$lc_id)
name.lc$label<-name.matrix$CLASS
name.lc$description<-''
colnames(name.lc)[1]='//lc_id'
name.lc.temp<-name.lc
write.table(name.lc, paste(dirAnnual, "/",Scenario_name,".car",sep=""),append=TRUE,quote=FALSE,col.names=TRUE,row.names=FALSE,sep="\t")

#Zone information
text<-"\n#ZONE"
write(text, paste(dirAnnual, "/",Scenario_name,".car",sep=""),append=TRUE, sep="\t")
pu$order<-pu$ZONE
#pu$order<-as.numeric(levels(pu$order))[pu$order]
pu<-as.data.frame(pu[order(pu$order, decreasing=FALSE),])
pu$zone_id<-0:(nrow(pu)-1)
name.pu<-as.data.frame(pu$zone_id)
name.pu$label<-pu$Z_NAME
name.pu$description<-''
colnames(name.pu)[1]='//zone_id'
name.pu.temp<-name.pu
colnames(name.pu.temp)[2]='Z_NAME'
write.table(name.pu, paste(dirAnnual, "/",Scenario_name,".car",sep=""),append=TRUE,quote=FALSE,col.names=TRUE,row.names=FALSE,sep="\t")

#Landcover change
text<-"\n#LANDCOVER_CHANGE"
write(text, paste(dirAnnual, "/",Scenario_name,".car",sep=""),append=TRUE, sep="\t")
eval(parse(text=(paste("name.lcc<-",data))))
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
write.table(name.lcc, paste(dirAnnual, "/",Scenario_name,".car",sep=""),append=TRUE,quote=FALSE,col.names=TRUE,row.names=FALSE,sep="\t")

#Carbon Stock
text<-"\n#CARBONSTOCK"
write(text, paste(dirAnnual, "/",Scenario_name,".car",sep=""),append=TRUE, sep="\t")
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
write.table(name.carbon, paste(dirAnnual, "/",Scenario_name,".car",sep=""),append=TRUE,quote=FALSE, col.names=TRUE,row.names=FALSE, sep="\t")

#Cost Benefit Unit (if exist)
#text<-"\n#COSTBENEFIT_UNIT\nlabel=Private\ndescription=Net return received by the land-use operator, farmers\n*TABLE"
#write(text, paste(result_dir, "/",Scenario_name,".car",sep=""),append=TRUE, sep="\t")

Abacus_Project_File = paste(dirAnnual, "/",Scenario_name,".car",sep="") #work with car file and also supported text file with abacus project format
#Original_Project_File = paste(workingDirectory, "/","Original_data.car",sep="")
#file.copy(Abacus_Project_File,Original_Project_File)


if (file.exists("C:/Program Files (x86)/LUMENS/Abacus2")){
  abacusExecutable = "C:/Progra~2/LUMENS/Abacus2/abacus2 "
} else{
  abacusExecutable = "C:/Progra~1/LUMENS/Abacus2/abacus2 "
}
systemCommand <- paste(abacusExecutable, Abacus_Project_File, "-ref LUMENS -wd", dirAnnual)
system(systemCommand)

#====WRITE REPORT====
# title<-"\\b\\fs32 LUMENS-SCIENDO - HISTORICAL BASELINE ANNUAL PROJECTION \\b0\\fs20"
# date<-paste("Date : ", as.character(Sys.Date()), sep="")
# time_start<-paste("Processing started : ", time_start, sep="")
# time_end<-paste("Processing ended : ", eval(parse(text=(paste("Sys.time ()")))), sep="")
# line<-paste("------------------------------------------------------------------------------------------------------------------------------------------------")
# I_O_period_1_rep<-paste("\\b","\\fs20", period1)
# I_O_period_2_rep<-paste("\\b","\\fs20", period2)
# rtffile <- RTF("LUMENS_SCIENDO-Annual_Projection_report.lpr", font.size=9)
# addParagraph(rtffile, "\\b\\fs32 Hasil Analisis\\b0\\fs20")
# addNewLine(rtffile)
# addNewLine(rtffile)
# addParagraph(rtffile, title)
# addNewLine(rtffile)
# addNewLine(rtffile)
# addParagraph(rtffile, line)
# addParagraph(rtffile, date)
# addParagraph(rtffile, time_start)
# addParagraph(rtffile, time_end)
# addParagraph(rtffile, line)
# addNewLine(rtffile)
# addTable(rtffile,em, font.size=8) 
# addNewLine(rtffile)
# addPlot(rtffile,plot.fun=print, width=6.7,height=3,res=300, plot1)
# addNewLine(rtffile)
# addPlot(rtffile,plot.fun=print, width=6.7,height=3,res=300, plot2)
# done(rtffile)
# 
# command<-paste("start ", "winword ", dirAnnual, "/LUMENS_SCIENDO-Annual_Projection_report.lpr", sep="" )
# shell(command)


