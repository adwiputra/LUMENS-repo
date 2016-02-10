##[QUES]=group
##Land_cover_lookup_table=file
##Analysis_option=selection All analysis; Perubahan dominan di tiap zona; Dinamika perubahan di tiap zona (Alpha-Beta); Analisis alur perubahan (Pre-QUES)
##Land_cover_no_data_value=number 0

library(rtf)
library(rgdal)
library(spatial.tools)
library(ggplot2)
library(plyr)
library(grid)
library(tiff)
library(RColorBrewer)
library(rasterVis)
library(reshape2)
library(foreign)
library(tcltk)

#INPUTS
#data - contains landcover 1 and 2
#pu - contains planning unit data
#T1
#T2
#command1

Look_up_table<-Land_cover_lookup_table
analysis.option<-Analysis_option
raster.nodata<-Land_cover_no_data_value

time_start<-paste(eval(parse(text=(paste("Sys.time ()")))), sep="")
lut.lc<-read.table(Look_up_table, header=TRUE, sep=",")

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

#====READ LANDUSE DATA FROM LUMENS DATABASE====
per<-as.data.frame(ls(pattern="freq"))
n<-nrow(per)
if(n==0){
  msgBox <- tkmessageBox(title = "Pre-QUES",
                         message = "No Land Use/Cover found",
                         icon = "info",
                         type = "ok")
  quit()
}
data<-per
data.y<-NULL
for (q in 1:n) {
  data.x<-substr(as.character(factor(data[q,1])), 5, 14)
  data.y<-c(data.y,data.x)
  
}
data<-as.data.frame(data.y)

n<-nrow(data)
command1<-NULL
command2<-NULL
for(i in 1:n) {
  if (i!=n){
    command1<-paste(command1,"period", i, ",", sep="")
    command2<-paste(command2,"landuse_t", i, ",", sep="")
  } else {
    command1<-paste(command1,"period", i, sep="")
    command2<-paste(command2,"landuse_t", i, sep="")
  }
}

#===Check LUMENS Pre-QUES log file====
if (file.exists(paste(user_path,"/LUMENS/LUMENS_pre_ques.log", sep=""))) {
  log.preques<-read.table(paste(user_path,"/LUMENS/LUMENS_pre_ques.log", sep=""), sep=",", header=T, row.names=1)
  print("LUMENS Pre-QuES log file is available")
} else {
  log.preques<-data.frame(IDX=NA, 
                          MODULE=NA, 
                          DATE=NA,
                          TIME=NA,
                          LU1=NA,
                          LU2=NA,
                          PU=NA,
                          T1=NA,
                          T2=NA,
                          LOOKUP_LC=NA,
                          LOOKUP_ZONE=NA,
                          NODATA=NA,
                          ANALYSIS_OPTION=NA,
                          OUTPUT_FOLDER=NA, row.names=NULL)
}

#if pu is not exist, use p.admin.df as planning unit reference
data2<-as.data.frame(as.character(ls(pattern="pu_pu")))
n_pu<-nrow(data2)
if (n_pu==0) {
  msgBox <- tkmessageBox(title = "Pre-QUES",
                         message = "No planning unit found. Do you want to use administrative boundary as planning unit?",
                         icon = "question", 
                         type = "yesno", default="yes")
  if(as.character(msgBox)=="no"){
    quit()
  }
  ref[ref==0]<-NA
  lut.pu<-p.admin.df[2]
  lut.pu[2]<-p.admin.df[1]
  pu<-"ref"
} else {
  command3<-NULL
  for(i in 1:n_pu) {
    if (i!=n_pu){
      command3a<-eval(parse(text=(paste( "names(pu_pu", i, ")", sep=""))))
      command3<-c(command3,command3a)
    } else {
      command3a<-eval(parse(text=(paste( "names(pu_pu", i, ")", sep=""))))
      command3<-c(command3,command3a)
    }
  }
}

rr<-nrow(per)
command4<-NULL
for(i in 1:rr) {
  if (i!=rr){
    command4<-paste(command4,"freqLanduse_", i, ",", sep="")
  } else {
    command4<-paste(command4,"freqLanduse_", i, sep="")
  }
}
#end create command

#====SELECT DATA TO BE ANALYZED====
eval(parse(text=(paste("year<-c(", command1, ")", sep=""))))
data<-as.data.frame(cbind(data,year))
data$t1<-0
data$t2<-0
colnames(data)[1]<-"data"
data$data<-as.character(data$data)
data3<-data
a<-nrow(data3)
repeat{
  data_temp<-edit(data)
  if(sum(data_temp$t1)==1 & sum(data_temp$t2)==1){
    data_temp$sum<-data_temp$t1+data_temp$t2
    data_temp <- data_temp[which(data_temp$sum==1),]
    n_temp<-nrow(data_temp)
    if(n_temp!=0) {
      data<-data_temp
      break  
    }
  } else {
    msgBox <- tkmessageBox(title = "Pre-QUES",
                           message = "Choose data to be analyzed. Retry?",
                           icon = "question", 
                           type = "retrycancel", default="retry")
    if(as.character(msgBox)=="cancel"){
      quit()
    }
  }
}

data$t1<-NULL
data$t2<-NULL
data$sum<-NULL

n<-nrow(data)
command1<-NULL
T1<-data[1,2]
T2<-data[2,2]

#====SELECT PLANNING UNIT TO BE ANALYZED====
if(n_pu!=0){
  data2<-as.data.frame(cbind(data2,command3))
  data2$usage<-0
  colnames(data2)[1]<-"data"
  colnames(data2)[2]<-"sources"
  data2$data<-as.character(data2$data)
  data2$sources<-as.character(data2$sources)
  data2<-rbind(data2, c("ref", "Administrative", 0))
  data2$usage<-as.integer(data2$usage)  
  repeat{
    data2<-edit(data2)
    if(sum(data2$usage)==1){
      break
    } else {
      msgBox <- tkmessageBox(title = "Pre-QUES",
                             message = "Choose one data as a planning unit. Retry?",
                             icon = "question", 
                             type = "retrycancel", default="retry")
      if(as.character(msgBox)=="cancel"){
        quit()
      }
    }
  } 
  data2 <- data2[which(data2$usage==1),]
  data2$usage<-NULL
  pu<-as.character(data2[1,1])
  if(pu=="ref"){
    ref[ref==0]<-NA
    lut.pu<-p.admin.df[2]
    lut.pu[2]<-p.admin.df[1]
  } else {
    pu_selected<-substr(pu, 6, 7)
    eval(parse(text=(paste("lut.pu<-lut.pu", pu_selected, sep=""))))
  }
}

#====PROJECTION HANDLING====
for(j in 1:n) {
  input <- as.character(data[j,1])
  eval(parse(text=(paste(input,"[",input, "==", raster.nodata, "]<-NA", sep=""))))
  command1<-paste(command1,input, ",", sep="")
}

#====projection handling====
if (grepl("+units=m", as.character(ref@crs))){
  print("Raster maps have projection in meter unit")
  Spat_res<-res(ref)[1]*res(ref)[2]/10000
  paste("Raster maps have ", Spat_res, " Ha spatial resolution, QuES-C will automatically generate data in Ha unit")
} else if (grepl("+proj=longlat", as.character(ref@crs))){
  print("Raster maps have projection in degree unit")
  Spat_res<-res(ref)[1]*res(ref)[2]*(111319.9^2)/10000
  paste("Raster maps have ", Spat_res, " Ha spatial resolution, QuES-C will automatically generate data in Ha unit")
} else{
  msgBox <- tkmessageBox(title = "QUES",
                         message = "Raster map projection is unknown",
                         icon = "info",
                         type = "ok")
  quit()
}

#===CHECK EXISTING RASTER BRICK OR CROSSTAB====
setwd(paste(dirname(proj.file),"/QUES/", sep=""))
command1<-paste(command1,pu, sep="")
eval(parse(text=(paste("pu_name<-names(",pu[1],")", sep=''))))
check_lucdb<-FALSE
eval(parse(text=(paste("check_crosstab<-file.exists('lu.db_", pu_name ,"_", T1, "_", T2, ".dbf')", sep="")))) 
eval(parse(text=(paste("check_rbrick<-file.exists('r.brick_", pu_name ,"_", T1, "_", T2, ".grd')", sep=""))))  
if(check_crosstab){
  eval(parse(text=(paste("data_merge<-read.dbf('lu.db_", pu_name ,"_", T1, "_", T2, ".dbf')", sep="")))) 
  #eval(parse(text=(paste("lu.db<-read.dbf('lu.db_", pu_name ,"_", T1, "_", T2, ".dbf')", sep=""))))
} else if(check_rbrick){
  eval(parse(text=(paste("r.brick<-brick('r.brick_", pu_name ,"_", T1, "_", T2, ".grd')", sep=""))))
  lu.db<-crosstab(r.brick,long=TRUE,useNA=FALSE,progress='-')
  check_lucdb<-TRUE
} else {
  eval(parse(text=(paste("r.brick<-brick(stack(", command1, "), filename='r.brick_",pu_name,"_",T1, "_", T2, "')", sep=""))))
  lu.db<-crosstab(r.brick,long=TRUE,useNA=FALSE,progress='-')
  check_lucdb<-TRUE
}

#====CREATE OVERALL SUMMARY OF LAND USE CHANGE====
colnames(lut.lc)[1]<-"value"
colnames(lut.lc)[2]<-"class"

eval(parse(text=(paste("lu.summary<-na.omit(merge(lut.lc, freq", data3[1,1], ', by="value"))', sep=""))))
colnames(lu.summary)[3]<-paste(data3[1,2], "_ha", sep="")

for (q in 2:a) {
  eval(parse(text=(paste("lu.summary<-na.omit(merge(lu.summary, freq", data3[q,1], ', by="value"))', sep=""))))
  colnames(lu.summary)[q+2]<-paste(data3[q,2], "_ha", sep="")
}

#====load look up table=====
lookup_lc2<-lut.lc
lookup_l<- lut.lc
lookup_z <- lut.pu
lookup_lc<- lut.lc
colnames(lookup_l)<-c("ID", "CLASS")
colnames(lookup_lc)<-c("ID", "CLASS")
colnames(lookup_z)<-c("ID", "ZONE")

#====CREATE INDIVIDUAL TABLE FOR EACH LANDUSE MAP====
if(check_lucdb){
  eval(parse(text=(paste("area_lc1<-freq", data[1,1], sep=""))))
  eval(parse(text=(paste("area_lc2<-freq", data[2,1], sep=""))))
  colnames(area_lc1)[1] = "ID"
  colnames(area_lc1)[2] = "COUNT_LC1"
  colnames(area_lc2)[1] = "ID"
  colnames(area_lc2)[2] = "COUNT_LC2"
  area_lc1<-merge(area_lc1,lookup_l,by="ID")
  area_lc2<-merge(area_lc2,lookup_l,by="ID")
  colnames(area_lc1)[3] = "CLASS_LC1"
  colnames(area_lc2)[3] = "CLASS_LC2"
  
  sub1.lu.db<-lu.db[n-1]
  sub2.lu.db<-lu.db[n]
  sub3.lu.db<-lu.db[n+1]
  sub4.lu.db<-lu.db[n+2]
  cross<-cbind(sub1.lu.db,sub2.lu.db,sub3.lu.db,sub4.lu.db)
  cross$Freq<-cross$Freq*Spat_res
  
  #Cross
  colnames(cross)[1] ="ID_LC1"
  colnames(cross)[2] = "ID_LC2"
  colnames(cross)[3] = "ZONE"
  colnames(cross)[4] = "COUNT"
  colnames(lookup_l)[1]="ID_LC1"
  colnames(lookup_l)[2]="LC_t1"
  data_merge <- merge(cross,lookup_l,by="ID_LC1")
  colnames(lookup_l)[1]="ID_LC2"
  colnames(lookup_l)[2]="LC_t2"
  data_merge <- as.data.frame(merge(data_merge,lookup_l,by="ID_LC2"))
  colnames(lookup_z)[1]="ZONE"
  colnames(lookup_z)[2]="Z_NAME"
  data_merge <- as.data.frame(merge(data_merge,lookup_z,by="ZONE"))
  
  #eval(parse(text=(paste("write.dbf(data_merge,'lu.db_", pu_name ,"_", T1, "_", T2, ".dbf')", sep=""))))
} else {
  data_merge$COUNT <- data_merge$COUNT*Spat_res
}
colnames(lookup_l)<-c("ID", "CLASS")
colnames(lookup_z)<-c("ID", "ZONE")
eval(parse(text=(paste("area_zone<-as.data.frame(freq(", pu,"))", sep=""))))
colnames(area_zone)[1] = "ID"
colnames(area_zone)[2] = "COUNT_ZONE"
area_zone<-merge(area_zone,lookup_z,by="ID")

#area<-min(sum(area_zone[,2]), sum(area_lc1[,2]), sum(area_lc2[,2]))

data_merge_sel <- data_merge[ which(data_merge$COUNT > 0),]
data_merge_sel$LU_CHG <- do.call(paste, c(data_merge_sel[c("LC_t1", "LC_t2")], sep = " to "))

#====Create land use change map====
if(check_lucdb){
  luchg<-data_merge_sel
  luchg$ID_LC1<-as.numeric(as.character((luchg$ID_LC1)))
  luchg$ID_LC2<-as.numeric(as.character((luchg$ID_LC2)))
  luchg$ID<-luchg$ID_LC1+(luchg$ID_LC2*100)
  eval(parse(text=(paste("luchg_map<-overlay(", data[1,1], ",", data[2,1], ",fun=function(x,y){return(x+(y*100))})", sep=""))))
  luchg_att<-as.data.frame(freq(luchg_map))
  #luchg_att$count<-luchg_att$count*Spat_res
  colnames(luchg_att)<-c("ID","AREA")
  luchg_att<-merge(luchg_att,luchg,by="ID")
  eval(parse(text=(paste("writeRaster(luchg_map, filename='luchgmap_", pu_name ,"_", T1, "_", T2, ".tif', format='GTiff', overwrite=TRUE)", sep=""))))
  eval(parse(text=(paste("write.dbf(luchg_att, 'lu.db_", pu_name ,"_", T1, "_", T2, ".dbf')", sep="")))) 
}

lg_chg <- data_merge_sel
lg_chg$ID1<-as.numeric(as.character((lg_chg$ID_LC1)))
lg_chg$ID2<-as.numeric(as.character((lg_chg$ID_LC2)))
lg_chg$IDC<-lg_chg$ID1-lg_chg$ID2
lg_chg<-lg_chg[ which(lg_chg$IDC!=0),]
lg_chg <- as.data.frame(lg_chg[order(-lg_chg$COUNT),])
lg_chg$CHG_CODE<-as.factor(toupper(abbreviate(lg_chg$LU_CHG, minlength=5, strict=FALSE, method="both")))
lg_chg$ID1<-lg_chg$ID2<-lg_chg$IDC<-NULL
lg_chg_top<-head(lg_chg, n=10)
lg_chg_top$LC_t1<-lg_chg_top$LC_t2<-NULL

#====Landuse Dominant Change====
chg_only<-aggregate(COUNT~LU_CHG,data=lg_chg,FUN=sum)
chg_only$CHG_CODE<-as.factor(toupper(abbreviate(chg_only$LU_CHG, minlength=5, strict=FALSE, method="both")))
chg_only<-chg_only[order(-chg_only$COUNT),]
chg_only<-chg_only[c(1,3,2)]
chg_only_top<-head(chg_only, n=10)

#====Zonal Dominant Change====
lg_chg_zonal<-as.data.frame(NULL)
for (l in 1:length(area_zone$ID)){
  tryCatch({
    a<-(area_zone$ID)[l]
    lg_chg_z<-lg_chg
    lg_chg_z<-as.data.frame(lg_chg_z[which(lg_chg_z$ZONE == a),])
    lg_chg_z<-aggregate(COUNT~ZONE+LU_CHG,data=lg_chg_z,FUN=sum)
    lg_chg_z$CHG_CODE<-as.factor(toupper(abbreviate(lg_chg_z$LU_CHG, minlength=5, strict=FALSE, method="both")))
    lg_chg_z<-lg_chg_z[order(-lg_chg_z$COUNT),]
    lg_chg_z<-lg_chg_z[c(1,2,4,3)]
    lg_chg_z_10<-head(lg_chg_z,n=10)
    lg_chg_zonal<-rbind(lg_chg_zonal,lg_chg_z_10)
  },error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
}

#====calculate basic statistic====
for (z in 3:ncol(lu.summary)){
  lu.summary[z]<-lu.summary[z]*Spat_res
}
Ov_chg<-lu.summary
colnames(Ov_chg)[2]="Land_use_type"
Ov_chg$LU_CODE<-as.factor(toupper(abbreviate(Ov_chg$Land_use_type, minlength=4, strict=FALSE, method="both")))

c.name<-NULL
a<-nrow(data3)

for(m in 2:a){
  if (m!=a) {
    eval(parse(text=(paste("Period<-period", m,"-period", m-1, sep=""))))
    sub<-eval(parse(text=(paste("period", m, sep=""))))
    sub<-paste(sub,"_ha", sep="")
    eval(parse(text=(paste("colnames(Ov_chg)[", m+2, ']="', sub, '"', sep=""))))
    c.name<-c(c.name,sub)
  }
}

Ov_chg.ha<-Ov_chg[1:2]
Ov_chg.rate<-Ov_chg.ha
count.period<-a-1
p.name<-NULL
p.name2<-NULL

for(o in 1:count.period){
  name<-paste((eval(parse(text=(paste("period",o,sep=""))))), "-", eval(parse(text=(paste("period",o+1,sep="")))),"_ha", sep="")
  name2<-paste((eval(parse(text=(paste("period",o,sep=""))))), "-", eval(parse(text=(paste("period",o+1,sep="")))),"_%/yrs", sep="")
  eval(parse(text=(paste("p.chg", "<-Ov_chg[", o, "+2+1]-Ov_chg[", o, "+2]", sep=""))))
  p.rate<-round(((p.chg/(Ov_chg[3]*Period))*100), 2)
  colnames(p.chg)[1]<-name
  colnames(p.rate)[1]<-name2
  Ov_chg.ha<-cbind(Ov_chg.ha,p.chg)
  Ov_chg.rate<-cbind(Ov_chg.rate,p.rate)
  p.name<-c(p.name,name)
  p.name2<-c(p.name2,name2)
}

#WARNING: OUTPUT FILE UP To THIS POINT ARE Ov_chg, Ov_chg.ha, Ov_chg.rate, lu.db!!!

#====CREATE OVERALL CHANGE TABLE AND GRAPH====
Ov_chg.melt <- melt(data = Ov_chg, id.vars=c('Land_use_type','LU_CODE'))
Ov_chg.melt<-Ov_chg.melt[which(Ov_chg.melt$variable!="value"),]
colnames(Ov_chg.melt)<-c("Land_use_type","LU_CODE", "Year", "Area")

ov.change.plot.2<-ggplot(Ov_chg.melt,aes(x=reorder(LU_CODE, -Area),y=Area,fill=Year))+geom_bar(stat="identity",position="dodge")+
  theme(axis.text.x= element_text(angle=45,hjust=1))+ ylab("Area (Ha)") + xlab("Land use type") +
  theme( legend.title = element_text(size=10),legend.text = element_text(size = 10), axis.text.x = element_text(size = 10),
         axis.title.x=element_blank())

#====CREATE OVERALL CHANGE TABLE AND GRAPH IN RATE====
#Ov_chg.merge<-cbind(Ov_chg.ha,(Ov_chg.rate[3:4]),(Ov_chg[6])) SOURCE OF ERROR STILL IN HARDCODE
eval(parse(text=(paste("Ov_chg.merge<-cbind(Ov_chg.ha,(Ov_chg.rate[3:",2+count.period,"]),(Ov_chg[", 2+a+1, "]))", sep=""))))

Ov_chg.melt2 <- melt(data = Ov_chg.merge, id.vars=c('Land_use_type','LU_CODE'), measure.vars=p.name)
colnames(Ov_chg.melt2)<-c("Land_use_type","LU_CODE", "Year", "Area")
ov.change.plot.3<-ggplot(Ov_chg.melt2,aes(x=reorder(LU_CODE, -Area),y=Area,fill=Year))+geom_bar(stat="identity",position="dodge")+
  theme(axis.text.x= element_text(angle=45,hjust=1))+ ylab("Area (Ha)") + xlab("Land use type") +
  theme( legend.title = element_text(size=8),legend.text = element_text(size = 8), axis.text.x = element_text(size = 10),
         axis.title.x=element_blank())+coord_flip()

Ov_chg.melt3 <- melt(data = Ov_chg.merge, id.vars=c('Land_use_type','LU_CODE'), measure.vars=p.name2)
colnames(Ov_chg.melt3)<-c("Land_use_type","LU_CODE", "Year", "Area")
ov.change.plot.4<-ggplot(Ov_chg.melt3,aes(x=reorder(LU_CODE, -Area),y=Area,fill=Year))+geom_bar(stat="identity",position="dodge")+
  theme(axis.text.x= element_text(angle=45,hjust=1))+ ylab("Area (Ha)") + xlab("Land use type") +
  theme( legend.title = element_text(size=8),legend.text = element_text(size = 8), axis.text.x = element_text(size = 10),
         axis.title.x=element_blank())+coord_flip()+ylim (c(-100, 100))

#Create Database
PreQUES.index=PreQUES.index+1
preques_folder<-paste("PreQUES_analysis_",pu_name,"_" ,T1,"_",T2,"_",PreQUES.index,sep="")
result_dir<-paste(dirname(proj.file),"/QUES/PreQUES/", sep="")
setwd(result_dir)
dir.create(preques_folder)

result_dir<-paste(result_dir,preques_folder, sep='')
setwd(result_dir)
write.dbf(Ov_chg, "Overall_change.dbf")
write.dbf(data_merge, "Changes_database.dbf")
write.dbf(Ov_chg.ha, "Overall_change_in_hectares.dbf")
write.dbf(Ov_chg.rate, "Overall_change_in_rates.dbf")

#====ALPHA BETA TABLE AND CHART====
alphabeta<-function(cross_temp_all, lookup_cover, t1, t2, area.analysis) {
  
  #select LU changes for creating blank diagonal values matrix
  cross_temp.blank_diag<-cross_temp_all
  cross_temp.blank_diag$check<-as.numeric(as.character(unlist((cross_temp.blank_diag$ID_LC2))))-as.numeric(as.character(unlist((cross_temp.blank_diag$ID_LC1))))
  #cross_temp.blank_diag<-filter(cross_temp.blank_diag, check!=0)
  cross_temp.blank_diag<-cross_temp.blank_diag[which(cross_temp.blank_diag$check!=0),]
  
  #create land use transition matrix
  cross_temp.blank_diag$ID_LC1<-as.numeric(as.character(unlist((cross_temp.blank_diag$ID_LC1))))
  colnames(lookup_cover)[1] = "ID_LC1"
  colnames(lookup_cover)[2] = "LC1"
  cross_temp.blank_diag<-merge(cross_temp.blank_diag,lookup_cover[],by='ID_LC1', all=T)
  
  cross_temp.blank_diag$ID_LC2<-as.numeric(as.character(unlist((cross_temp.blank_diag$ID_LC2))))
  colnames(lookup_cover)[1] = "ID_LC2"
  colnames(lookup_cover)[2] = "LC2"
  cross_temp.blank_diag<-merge(cross_temp.blank_diag,lookup_cover[],by='ID_LC2', all=T)
  
  cross_temp.melt <- melt(data = cross_temp.blank_diag, id.vars=c('ID_LC1','ID_LC2'), measure.vars=c('COUNT'))
  cross_temp.melt.cast <- dcast(data = cross_temp.melt, formula = ID_LC1 ~ ID_LC2, fun.aggregate = sum)
  cross_temp.melt.cast[1]<-NULL
  cross_temp.melt.cast<-cross_temp.melt.cast[-nrow(cross_temp.melt.cast),]; #remove last row
  cross_temp.melt.cast<-cross_temp.melt.cast[,-ncol(cross_temp.melt.cast)]; #remove last column
  
  a.table<-cross_temp.melt.cast
  tot.col<-colSums(cross_temp.melt.cast)
  
  
  for(x in 1:ncol(a.table)){
    eval(parse(text=(paste("a.table[", x, "]<-a.table[",x,"]/tot.col[",x,"]", sep=""))))
  }
  a.table[is.na(a.table)] <- 0
  a.table$"NA"<-NULL ;#remove NA column
  #a.table<-a.table[-nrow(a.table),];#remove last NA row
  a.table<-do.call(data.frame,lapply(a.table, function(x) replace(x, is.infinite(x),0)));#replace Inf
  
  a.val<-rowSums(a.table)
  
  #calculate beta
  b.table<-cross_temp.melt.cast
  tot.row<-rowSums(cross_temp.melt.cast)
  
  for(y in 1:ncol(b.table)){
    eval(parse(text=(paste("b.table[", y, ",]<-b.table[",y,",]/tot.row[",y,"]", sep=""))))
  }
  b.table[is.na(b.table)] <- 0
  b.table<-do.call(data.frame,lapply(b.table, function(x) replace(x, is.infinite(x),0)));#replace Inf
  b.table$"NA"<-NULL
  #b.table <- b.table[-nrow(b.table),];#remove last NA row
  b.val<-colSums(b.table)
  
  
  #Bind alpha and beta
  eval(parse(text=( paste("id<-na.omit(as.integer(colnames(cross_temp.melt.cast)))",sep=''))))
  fromto.table<-cbind(na.omit(id),a.val,b.val)
  
  colnames(lookup_cover)[1] = "ID"
  colnames(lookup_cover)[2] = "LC"
  colnames(fromto.table)<-c("ID","a.val","b.val")
  fromto.table<-merge(lookup_cover,fromto.table, by='ID', all=T); #<- ALPHA beta TABLE
  
  #============================
  #LULC summary
  
  area.lu1<-ddply(cross_temp_all, c("ID_LC1"), summarise,LU1=sum(COUNT))
  colnames(area.lu1)[1]<-"ID"
  area.lu2<-ddply(cross_temp_all, c("ID_LC2"), summarise,LU2=sum(COUNT))
  colnames(area.lu2)[1]<-"ID"
  LULC_summary<-merge(area.lu1, area.lu2, by="ID", ALL=T)
  #LULC_summary$ID<-as.character(LULC_summary$ID)
  #LULC_summary[is.na(LULC_summary)] <- 0
  LULC_summary <- LULC_summary[-nrow(LULC_summary),]
  
  
  LULC_summary$Change<-as.numeric(LULC_summary$LU2)-as.numeric(LULC_summary$LU1)
  LULC_summary$Total_change<-abs(LULC_summary$Change)
  fromto.table_LULC<-na.omit(merge(fromto.table, LULC_summary, by='ID', all=T))
  value<-data.frame(stringsAsFactors=FALSE)
  
  #NEgative Change or positive change
  for(d in 1:nrow(fromto.table_LULC)){
    if(fromto.table_LULC[d,7]>0){
      nval<-1
      value<-rbind(value,nval)
    } else {
      nval<-0
      value<-rbind(value,nval)
    }}
  
  value[value==0]<-"decreasing"
  value[value==1]<-"increasing"
  colnames(value)<-"Trend"
  fromto.table_LULC<-cbind(fromto.table_LULC,value)
  fromto.table_LULC<-arrange(fromto.table_LULC, -Total_change)
  
  #Rate of Change
  fromto.table_LULC$rate<-(abs(fromto.table_LULC$Total_change/fromto.table_LULC$LU1)*100)
  fromto.table_LULC$rate[is.na(fromto.table_LULC$rate)] <- 0
  
  fromto.table_LULC$area<-area.analysis
  fromto.table_LULC$lc_abr<-toupper(abbreviate(fromto.table_LULC$LC))
  
  #print(fromto.table_LULC)
  return(fromto.table_LULC)
}

alphabeta.plot<-function(alphabeta_table, t1, t2, area.analysis){
  #IGNORED TRANSTITION LESS THAN 1
  #alphabeta_table[alphabeta_table$a.val<0.5&alphabeta_table$b.val<0.5, 1:10]<-NA 
  alphabeta_table<-alphabeta_table[-which(alphabeta_table$a.val < 0.5 & alphabeta_table$b.val < 0.5),]
  if(nrow(alphabeta_table)!=0){
    ab.plot<-ggplot(alphabeta_table, aes(x=a.val, y=b.val,xend=6, yend=6)) +
      geom_point(data=alphabeta_table,aes(x=a.val, y=b.val,size =Total_change, colour =Trend), alpha=.5)+
      geom_text(size=3, aes(label=lc_abr),hjust=0.5,vjust=-1, angle =0)+
      scale_size(range = c(1,50)) + labs(x = paste("Alpha", t1), y= paste("Beta", t2))+ ggtitle(paste(area.analysis, t1, '-', t2))
    return(ab.plot)
    #+theme_bw()
  }
  
}

#ALPHA BETA AT LANDSCAPE LEVEL
landscape.alphabeta<-alphabeta(data_merge, lookup_lc, T1, T2, paste("Keseluruhan", location))
landscape.alphabeta.plot<-alphabeta.plot(landscape.alphabeta, T1, T2,paste("Keseluruhan", location))

if(analysis.option==2 | analysis.option==0){
  #ALPHA BETA AT PLANNING UNIT LEVEL
  alpha_beta_database<-data.frame()
  for(i in 1:nrow(lookup_z)){
    if (i==1){
      tryCatch({
        zone_id<-lookup_z$ID[i]
        #print(zone_id)
        eval(parse(text=( paste("cross_temp_zone<-na.omit(data_merge[ which(data_merge$ZONE==",zone_id,"),])", sep=''))))
        zona<-paste(lookup_z$ZONE[i])
        eval(parse(text=( paste('alphabeta_zone_', zone_id,"<-alphabeta(cross_temp_zone, lookup_l, T1,T2, zona)", sep=''))))
        eval(parse(text=( paste('alphabeta_plot_zone_', zone_id,"<-alphabeta.plot(alphabeta_zone_", zone_id,", T1, T2, zona)", sep=''))))
        #plot.name<-paste("alpha_beta_",location,"_",T1,"_",T2,"_zona_", zona,".png", sep='')
        #eval(parse(text=( paste('ggsave(alphabeta_plot_zone_', zone_id,",file= plot.name,width=20,height=20, units='cm')", sep=''))))
        alpha_beta_database<-rbind(landscape.alphabeta,alphabeta_zone_1)
      }, error=function(e){cat("No Alpha-beta analysis", "\n")})
    } else {
      tryCatch({
        zone_id<-lookup_z$ID[i]
        #print(zone_id)
        eval(parse(text=( paste("cross_temp_zone<-na.omit(data_merge[ which(data_merge$ZONE==",zone_id,"),])", sep=''))))
        zona<-paste(lookup_z$ZONE[i])
        eval(parse(text=( paste('alphabeta_zone_', zone_id,"<-alphabeta(cross_temp_zone, lookup_l, T1,T2, zona)", sep=''))))
        eval(parse(text=( paste('alphabeta_plot_zone_', zone_id,"<-alphabeta.plot(alphabeta_zone_", zone_id,", T1, T2, zona)", sep=''))))
        eval(parse(text=( paste("alpha_beta_database<-rbind(alpha_beta_database, alphabeta_zone_", zone_id,")", sep=''))))
        #eval(parse(text=( paste('alphabeta_plot_zone_', zone_id, sep=""))))
      },error=function(e){cat("Nice try pal! ~ please re-check your input data :",conditionMessage(e), "\n")})
    }
  }
  
  write.dbf(alpha_beta_database, "Pre_QUES_IO_table.dbf")
} else {print("Alpha-beta analysis is skipped")}


#====Create Map for report====
myColors1 <- brewer.pal(9,"Set1")
myColors2 <- brewer.pal(8,"Accent")
myColors3 <- brewer.pal(12,"Paired")
myColors4 <- brewer.pal(9, "Pastel1")
myColors5 <- brewer.pal(8, "Set2")
myColors6 <- brewer.pal(8, "Dark2")
myColors7 <- rev(brewer.pal(11, "RdYlGn"))
myColors8 <- "#000000"
myColors9 <- brewer.pal(12, "Set3")

if (0 %in% area_zone$ID){
  myColors  <-c(myColors8, myColors5,myColors1, myColors2, myColors7, myColors4, myColors5, myColors6)
} else {
  myColors  <-c(myColors5,myColors1, myColors2, myColors3, myColors4, myColors7, myColors6)
}

#zone map
eval(parse(text=(paste("zone<-", pu, sep=""))))
myColors.Z <- myColors[1:length(unique(area_zone$ID))]
ColScale.Z<-scale_fill_manual(name="Unit perencanaan", breaks=area_zone$ID, labels=area_zone$ZONE, values=myColors.Z)
plot.Z<-gplot(zone, maxpixels=100000) + geom_raster(aes(fill=as.factor(value))) +
  coord_equal() + ColScale.Z +
  theme(plot.title = element_text(lineheight= 5, face="bold")) +
  theme( axis.title.x=element_blank(),axis.title.y=element_blank(),
         panel.grid.major=element_blank(), panel.grid.minor=element_blank(),
         legend.title = element_text(size=8),
         legend.text = element_text(size=6),
         legend.key.height = unit(0.25, "cm"),
         legend.key.width = unit(0.25, "cm"))

#rm(myColors8, myColors7,myColors1, myColors2, myColors3, myColors4, myColors5, myColors6, myColors9)

#Largest Source of Change in Landuse
colnames(chg_only_top)[3]<-"COUNT"
Largest.chg<- ggplot(data=chg_only_top, aes(x=reorder(CHG_CODE, -COUNT),y=COUNT, fill=CHG_CODE))+geom_bar(stat='identity',position='dodge')+
  geom_text(data=chg_only_top, aes(x=CHG_CODE, y=COUNT, label=round(COUNT, 1)),size=3, vjust=0.1) +
  ggtitle(paste("10 Perubahan Tutupan Lahan Dominan di", location, T1,"-",T2 )) +
  labs(x = 'Jenis perubahan penutupan lahan', y='Luas area (Ha)') + guides(fill=FALSE)+
  theme(plot.title = element_text(lineheight= 5, face="bold")) + scale_y_continuous() +
  theme(axis.title.x=element_blank(), axis.text.x = element_text(size=8),
        panel.grid.major=element_blank(), panel.grid.minor=element_blank())

setwd(result_dir)
#====WRITE REPORT====
title1<-"{\\colortbl;\\red0\\green0\\blue0;\\red255\\green0\\blue0;\\red146\\green208\\blue80;\\red0\\green176\\blue240;\\red140\\green175\\blue71;\\red0\\green112\\blue192;\\red79\\green98\\blue40;} \\pard\\qr\\b\\fs70\\cf2 L\\cf3U\\cf4M\\cf5E\\cf6N\\cf7S \\cf1REPORT \\par\\b0\\fs20\\ql\\cf1"
title2<-paste("\\pard\\qr\\b\\fs40\\cf1 PreQUES-Land Use Change Analysis ", "for ", location, ", ", province, ", ", country, "\\par\\b0\\fs20\\ql\\cf1", sep="")
sub_title<-"\\cf2\\b\\fs32 ANALISA PERUBAHAN PENGGUNAAN LAHAN\\cf1\\b0\\fs20"
#date<-paste("Date : ", date, sep="")
time_start<-paste("Proses dimulai : ", time_start, sep="")
time_end<-paste("Proses selesai : ", eval(parse(text=(paste("Sys.time ()")))), sep="")
line<-paste("------------------------------------------------------------------------------------------------------------------------------------------------")
area_name_rep<-paste("\\b", "\\fs20", location, "\\b0","\\fs20")
I_O_period_1_rep<-paste("\\b","\\fs20", T1)
I_O_period_2_rep<-paste("\\b","\\fs20", T2)
chapter1<-"\\cf2\\b\\fs28 DATA YANG DIGUNAKAN \\cf1\\b0\\fs20"
chapter2<-"\\cf2\\b\\fs28 HASIL ANALISA PADA TINGKAT BENTANG LAHAN \\cf1\\b0\\fs20"
chapter3<-"\\cf2\\b\\fs28 HASIL ANALISA PADA TINGKAT UNIT PERENCANAAN \\cf1\\b0\\fs20"
rtffile <- RTF("LUMENS_Pre-QUES_change_report.lpr", font.size=9)
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
addTable(rtffile,proj_descr,font.size=8,col.widths=width)
addPageBreak(rtffile)
addParagraph(rtffile, sub_title)
addNewLine(rtffile)
addParagraph(rtffile, line)
#addParagraph(rtffile, date)
addParagraph(rtffile, time_start)
addParagraph(rtffile, time_end)
addParagraph(rtffile, line)
addNewLine(rtffile)
addParagraph(rtffile, "Analisa perubahan tutupan lahan dilakukan untuk mengetahui kecenderungan perubahan tutupan lahan di suatu daerah pada satu kurun waktu. Analisa ini dilakukan dengan menggunakan data peta tutupan lahan pada dua periode waktu yang berbeda. Selain itu, dengan memasukkan data unit perencanaan kedalam proses analisa, dapat diketahui kecenderungan perubahan tutupan lahan pada masing-masing kelas unit perencanaan yang ada. Informasi yang dihasilkan melalui analisa ini dapat digunakan dalam proses perencanaan untuk berbagai hal. Diantaranya adalah: menentukan prioritas pembangunan, mengetahui faktor pemicu perubahan penggunaan lahan, merencanakan skenario pembangunan di masa yang akan datang, dan lain sebagainya.")
addNewLine(rtffile)
addParagraph(rtffile, chapter1)
addParagraph(rtffile, line)
addNewLine(rtffile)
addParagraph(rtffile, "Data yang digunakan dalam analisa ini adalah data peta penggunaan lahan dan data peta unit perencanaan daerah. Data pendukung yang digunakan adalah peta acuan tipe penggunaan lahan dan data acuan kelas unit perencanaan")
addNewLine(rtffile)
#addTable(rtffile,test3,font.size=8)
#addNewLine(rtffile)
for(q in 1:n){
  tryCatch({
    eval(parse(text=(paste("landusemap<-Landuse_", q, sep=""))))
    eval(parse(text=(paste("area_lcmap<-as.data.frame(freq(Landuse_", q,"))", sep=""))))
    colnames(area_lcmap)[1] = "ID"
    colnames(area_lcmap)[2] = "COUNT_LC1"
    colnames(lookup_l)[1]<-"ID"
    area_lcmap<-merge(area_lcmap,lookup_l,by="ID")
    colnames(area_lcmap)[3] = "CLASS_LC1"
    if (0 %in% area_lcmap$ID){
      myColors  <-c(myColors8, myColors7,myColors1, myColors2, myColors3, myColors4, myColors5, myColors6)
    } else {
      myColors  <-c(myColors7,myColors1, myColors2, myColors3, myColors4, myColors5, myColors6)
    }
    myColors.lu <- myColors[1:length(unique(area_lcmap$ID))]
    periodmap<-eval(parse(text=(paste("T",q, sep="" ))))
    addParagraph(rtffile, paste("\\b \\fs20 Peta Tutupan Lahan ", location, " tahun ", periodmap," \\b0 \\fs20", sep=""))
    addNewLine(rtffile, n=1)
    #Land cover map
    ColScale.lu<-scale_fill_manual(name="Jenis tutupan lahan", breaks=area_lcmap$ID, labels=area_lcmap$CLASS_LC1, values=myColors.lu)
    plot.LU<-gplot(landusemap, maxpixels=100000) + geom_raster(aes(fill=as.factor(value))) +
      coord_equal() + ColScale.lu +
      theme(plot.title = element_text(lineheight= 5, face="bold")) +
      theme( axis.title.x=element_blank(),axis.title.y=element_blank(),
             panel.grid.major=element_blank(), panel.grid.minor=element_blank(),
             legend.title = element_text(size=8),
             legend.text = element_text(size = 6),
             legend.key.height = unit(0.25, "cm"),
             legend.key.width = unit(0.25, "cm"))
    
    addPlot.RTF(rtffile, plot.fun=plot, width=6.7, height=3, res=150, plot.LU )
    addNewLine(rtffile, n=1)
  },error=function(e){cat("Nice try pal! ~ please re-check your input data :",conditionMessage(e), "\n"); addParagraph(rtffile, "no data");addNewLine(rtffile)})
}
addNewLine(rtffile)
addParagraph(rtffile, paste("\\b \\fs20 Peta Unit Perencanaan "," \\b0 \\fs20", sep=""))                                                                                                                                                                                                                                                                                                                                                                                                                           
addNewLine(rtffile)
addPlot(rtffile,plot.fun=print, width=6.7,height=3,res=150, plot.Z)
addNewLine(rtffile)
addParagraph(rtffile, chapter2)
addParagraph(rtffile, line)
addNewLine(rtffile)
addParagraph(rtffile, "Pada bagian ini disajikan hasil analisa perubahan penggunaan lahan untuk keseluruhan bentang lahan yang dianalisa. Beberapa bentuk analisa yang dilakukan antara lain: perbandingan luasan tutupan lahan pada periode analisa dan tipe perubahan lahan dominan pada bentang lahan yang dianalisa")
addNewLine(rtffile)
addParagraph(rtffile, "Tabel Intisari Perubahan Tutupan Lahan menyajikan perbandingan luasan tipe-tipe tutupan lahan di sebuah bentnag lahan pada kurun waktu tertentu. Kolom Overall Change menunjukkan perubahan luasan dalam satuan hektar. Notasi negatif pada kolom ini menunjukkan pengurangan luasan sebaliknya notasi positif menunjukkan penambahan luasan. Kolom Rate menunjukkan laju perubahan luasan tutupan lahan dalam satuan %/tahun. Kolom ini dihitung dengan mengurangi luasan pada t2-t1 kemudian dibagi dengan perkalian luasan pada t1 dan kurun waktu analisa")
addNewLine(rtffile)
addParagraph(rtffile, paste("\\b \\fs20 Intisari Perubahan Tutupan Lahan di", location, "\\b \\fs20", sep=" "))
#width<-as.vector(c(0.44,2,0.69,0.69,0.69,0.69))
addTable(rtffile,Ov_chg,font.size=8)
addNewLine(rtffile)
addPlot(rtffile,plot.fun=print, width=6.7,height=4,res=150,  ov.change.plot.2)
addNewLine(rtffile)
addParagraph(rtffile, paste("\\b \\fs20 Luasan Perubahan Tutupan Lahan (ha) di", location, "\\b \\fs20", sep=" "))
#width<-as.vector(c(0.44,2,0.69,0.69))
addTable(rtffile,Ov_chg.ha,font.size=8)
addNewLine(rtffile)
addPlot(rtffile,plot.fun=print, width=6.7,height=4,res=150,  ov.change.plot.3)

addNewLine(rtffile)
addParagraph(rtffile, paste("\\b \\fs20 Rerata Luasan Perubahan Tutupan Lahan (%/tahun) di", location, "\\b \\fs20", sep=" "))
#width<-as.vector(c(0.44,2,1,1))
addTable(rtffile,Ov_chg.rate,font.size=8)
addNewLine(rtffile)
addPlot(rtffile,plot.fun=print, width=6.7,height=4,res=150,  ov.change.plot.4)
addNewLine(rtffile)
addParagraph(rtffile, paste("\\b \\fs20 Sepuluh Perubahan Lahan Dominan di",location, "\\b0 \\fs20", sep=" "))
addNewLine(rtffile, n=1)
colnames(chg_only_top)[3]<-"Luas(ha)"
addTable(rtffile, chg_only_top)
addNewLine(rtffile, n=1)
addPlot(rtffile,plot.fun=print, width=6.7,height=3,res=150,Largest.chg)

addNewLine(rtffile)
addNewLine(rtffile)
addParagraph(rtffile, paste("\\b \\fs20 Grafik IO: dinamika perubahan lahan di\\b0 \\fs20 ", area_name_rep, I_O_period_1_rep, "\\b \\fs20 - \\b0 \\fs20", I_O_period_2_rep, sep=" "))
addPlot(rtffile,plot.fun=print, width=6.7,height=5,res=150,  landscape.alphabeta.plot)
addNewLine(rtffile)
addNewLine(rtffile)

if(analysis.option==1 | analysis.option==0){
  addParagraph(rtffile, chapter3)
  addParagraph(rtffile, line)
  addNewLine(rtffile)
  addParagraph(rtffile, "Pada bagian ini disajikan hasil analisa perubahan penggunaan lahan untuk masing-masing kelas unit perencanaan yang dianalisa. Beberapa bentuk analisa yang dilakukan antara lain: perbandingan luasan tutupan lahan pada periode analisa dan tipe perubahan lahan dominan pada unit perencanaan yang dianalisa")
  addNewLine(rtffile)
  for(i in 1:length(area_zone$ID)){
    tryCatch({
      zonal.db<-area_zone
      zonal.db$Z_CODE<-toupper(abbreviate(zonal.db$ZONE))
      zona<-paste("\\b", "\\fs20", i, "\\b0","\\fs20")
      zona_nm<-paste("\\b", "\\fs20", zonal.db$ZONE[i], "\\b0","\\fs20")
      zona_ab<-paste("\\b", "\\fs20", zonal.db$Z_CODE[i], "\\b0","\\fs20")
      addParagraph(rtffile, "\\b \\fs20 Sepuluh Tipe Perubahan Tutupan Lahan Dominan di Unit Perencanaan \\b0 \\fs20", zona,"\\b \\fs20 - \\b0 \\fs20", zona_nm, "\\b \\fs20 (\\b0 \\fs20", zona_ab, "\\b \\fs20)\\b0 \\fs20" )
      addNewLine(rtffile, n=1)
      lg_chg_zon<-lg_chg_zonal[which(lg_chg_zonal$ZONE == i),]
      lg_chg_zon$ZONE<-NULL
      lg_chg_plot<-lg_chg_zon
      colnames(lg_chg_zon)[3]<-"Luas (ha)"
      addTable(rtffile, lg_chg_zon)
      addNewLine(rtffile, n=1)
      #Largest Source of Change in Planning Unit Level
      Largest.chg.z<- ggplot(data=lg_chg_plot, aes(x=reorder(CHG_CODE, -COUNT),y=COUNT, fill=CHG_CODE))+geom_bar(stat='identity',position='dodge')+
        geom_text(data=lg_chg_plot, aes(x=CHG_CODE, y=COUNT, label=round(COUNT, 1)),size=3, vjust=0.1) +
        ggtitle(paste("10 Perubahan Dominan pada Unit Perencanaan",i, "-", zonal.db$Z_CODE[i] )) + guides(fill=FALSE) +
        labs(x = 'Jenis perubahan penutupan lahan', y='Luas area (Ha)') + guides(fill=FALSE)+
        theme(plot.title = element_text(lineheight= 5, face="bold")) + scale_y_continuous() +
        theme(axis.title.x=element_blank(), axis.text.x = element_text(size=8),
              panel.grid.major=element_blank(), panel.grid.minor=element_blank())
      
      addPlot.RTF(rtffile, plot.fun=plot, width=6.7, height=3, res=150, Largest.chg.z )
      addNewLine(rtffile, n=1)
    },error=function(e){cat("Nice try pal! ~ please re-check your input data :",conditionMessage(e), "\n"); addParagraph(rtffile, "no data");addNewLine(rtffile)})
  }
}

if(analysis.option==2 | analysis.option==0){
  for(i in 1:nrow(lookup_z)){
    checkAlphabetaPlot<-eval(parse(text=( paste("exists('alphabeta_plot_zone_", i, "')" , sep=""))))
    if(checkAlphabetaPlot){
      tryCatch({
        eval(parse(text=( paste("checkNULL<-is.null(alphabeta_plot_zone_", i, ")", sep=""))))
        if(!checkNULL){
          zona<-paste("\\b", "\\fs20", i, "\\b0","\\fs20")
          zona_nm<-paste("\\b", "\\fs20", lookup_z$ZONE[i], "\\b0","\\fs20")
          addParagraph(rtffile, "\\b \\fs20 Grafik IO: dinamika perubahan lahan di \\b0 \\fs20", zona,"\\b \\fs20 - \\b0 \\fs20", zona_nm )
          
          addNewLine(rtffile)
          eval(parse(text=( paste("addPlot(rtffile,plot.fun=print, width=6.7,height=5,res=150, alphabeta_plot_zone_",i,")", sep=''))))
          
          addNewLine(rtffile)
        }
      },error=function(e){cat("please re-check your input data :",conditionMessage(e), "\n"); addParagraph(rtffile, "no data");addNewLine(rtffile)})
    }
  }
}

addNewLine(rtffile)
done(rtffile)

#====Land use change database export====
#eval(parse(text=(paste("PreQUES_data_", data[1,2], "_", data[2,2], "<-data", sep=""   ))))
#newPre<-paste("PreQUES_data_", data[1,2], "_", data[2,2], sep="")

#command<-paste("resave(PreQUES.index,Ov_chg,Ov_chg.ha,lut.lc,Ov_chg.rate,", newPre, ",",  sep="")
command<-paste("resave(PreQUES.index,lut.lc,",  sep="")

command<-paste(command,"file='",proj.file,"')", sep="")
eval(parse(text=(command)))

#====Pre-QuES Land Use Change Trajectories====
if(analysis.option==3 | analysis.option==0){
  #PREQUES
  #substitute lookup table internal
  trj<-c(11:17,22:27,32:37,42:44,46:47,52:57,62:67,77,88)
  lookup_traj<-as.data.frame(trj)
  remove(trj)
  lookup_traj$Traj<-c("Stable natural forest","Loss to logged-over forest","Recovery to tree cropping","Recovery to agroforest","Loss to cropland","Loss to bare land and abandoned","Loss to infrastructure","Loss to logged-over forest","Recovery to tree cropping","Recovery to agroforest","Loss to cropland","Loss to bare land and abandoned","Loss to infrastructure","Recovery to forest","Recovery to tree cropping","Recovery to agroforest","Loss to cropland","Loss to bare land and abandoned","Loss to infrastructure","Recovery to forest","Recovery to tree cropping","Recovery to agroforest","Loss to bare land and abandoned","Loss to infrastructure","Recovery to forest","Recovery to tree cropping","Recovery to agroforest","Loss to cropland","Loss to bare land and abandoned","Loss to infrastructure","Recovery to forest","Recovery to tree cropping","Recovery to agroforest","Loss to cropland","Loss to bare land and abandoned","Loss to infrastructure","Loss to infrastructure","Other")
  lookup_traj$Def<-c("Stable forest", "Forest degradation", "Deforestation","Deforestation","Deforestation","Deforestation","Deforestation","Stable forest", "Deforestation","Deforestation","Deforestation","Deforestation","Deforestation", "Reforestation", "Other","Other","Other","Other","Other", "Other","Other","Other","Other","Other","Other","Other","Other","Other","Other","Other", "Other","Other","Other","Other","Other","Other","Other", "Others")
  name_traj<-lookup_traj
  name_traj$ID_trf<-c(5,3,7,6,1,4,2,3,7,6,1,4,2,8,7,6,1,4,2,8,7,6,4,2,8,7,6,1,4,2,8,7,6,1,4,2,2,9)
  ID_T<-c(1:9)
  leg_traj<-as.data.frame(ID_T)
  remove(ID_T)
  leg_traj$Trajectories<-c("Loss to cropland","Loss to infrastructure","Loss to logged-over forest","Loss to bare land and abandoned","Stable natural forest","Recovery to agroforest","Recovery to tree cropping","Recovery to forest","Other")
  
  lu_class<-c(1,2,3,4,5,6,7,8)
  lu_class<-as.data.frame(lu_class)
  lu_class$Rec_LU<-c("Hutan primer", "Hutan sekunder", "Tanaman pohon monokultur", "Tanaman pohon campuran", "Tanaman pertanian semusim", "Semak, rumput, dan lahan terbuka", "Pemukiman", "Lain-lain")
  
  #load look up table (user input)
  lookup_l<- lut.lc
  lookup_z <- lut.pu
  #lookup_lr<-read.table(lu_reclass, header=TRUE, sep=",")
  
  lookup_l2<-lookup_l
  lookup_l2$ID_L<-0
  countrow<-nrow(lookup_l2)
  x<-rep("---", times=countrow)
  xx<-x
  lookup_lr_edit<-cbind(lookup_l2,x,xx)
  xxx<-rep("---", times=countrow-(nrow(lu_class)))
  xxxx<-xxx
  V<-cbind(xxx,xxxx)
  colnames(V)[1]<-"lu_class"
  colnames(V)[2]<-"Rec_LU"
  lookup_lr_edit2<-rbind(lu_class,V)
  #lookup_lr_edit3<-cbind(lookup_lr_edit2, x,xx)
  lookup_lr<-cbind(lookup_lr_edit, lookup_lr_edit2)
  lookup_lr<-edit(lookup_lr)
  lookup_lr<-subset(lookup_lr, select=c(1, 2, 3))
  
  #create trajectories database
  colnames(lookup_lr)[1]="ID_LC1"
  colnames(lookup_lr)[2]="CLASS1"
  colnames(lookup_lr)[3]="ID_L1"
  data_merge_tr<-as.data.frame(merge(data_merge,lookup_lr, by="ID_LC1"))
  colnames(lookup_lr)[1]="ID_LC2"
  colnames(lookup_lr)[2]="CLASS2"
  colnames(lookup_lr)[3]="ID_L2"
  data_merge_tr<-as.data.frame(merge(data_merge_tr,lookup_lr, by="ID_LC2"))
  data_merge_tr$CLASS1<-data_merge_tr$CLASS2<-NULL
  data_merge_tr$T1<-data_merge_tr$ID_L1*10
  data_merge_tr$T2<-data_merge_tr$ID_L2
  data_merge_tr$TR<-data_merge_tr$T1+data_merge_tr$T2
  colnames(lookup_traj)[1]="TR"
  PreQUES_traj_database<-as.data.frame(merge(data_merge_tr,lookup_traj, by="TR"))
  PreQUES_traj_database$Traj_Code<-toupper(abbreviate(PreQUES_traj_database$Traj))
  colnames(lookup_lr)[1]="ID"
  
  lookup_l<-lookup_lr; #lu table
  lookup_l[3]<-NULL
  
  #cross_temp<-data.frame(Var1=PreQUES_traj_database$ID_LC1, Var2=PreQUES_traj_database$ID_LC2, Var3=PreQUES_traj_database$ZONE, Freq=PreQUES_traj_database$COUNT)
  
  #LAND COVER CHAGE TRAJECTORY INDEXES
  #Calculate deforestation, degradation, and reforestation rate
  #PREQUES_filter_2<-function(idlc1, idlc2, filter1, filter2) {
  #  eval(parse(text=( paste("PreQUES_traj_database.filtered<-filter(PreQUES_traj_database,ID_L1",filter1,as.characteridlc1,',','ID_L2',filter2,idlc2,')', sep=''))))
  #  #PreQUES_traj_database.filtered<-filter(PreQUES_traj_database,ID_L1 == idlc1, ID_L2 == idlc2)
  #  PreQUES_traj_database.filtered<-filter(PreQUES_traj_database.filtered, COUNT!=0)
  #  PreQUES_traj_database.filtered <- aggregate(COUNT ~  Z_NAME, data=PreQUES_traj_database.filtered, FUN=sum)
  #  return(PreQUES_traj_database.filtered)
  #}
  
  #1 Deforestation
  #index.deforest<-PREQUES_filter_2(c(1,2),c(1,2),'==','!=')
  tryCatch({
    #index.deforest<-filter(PreQUES_traj_database, ID_L1==c(1,2), ID_L2!=c(1,2)) 
    index.deforest1<-subset(PreQUES_traj_database, ID_L1==1)
    index.deforest2<-subset(PreQUES_traj_database, ID_L1==2)
    index.deforest<-rbind(index.deforest1, index.deforest2)
    index.deforest<-subset(index.deforest, ID_L2!=1)
    index.deforest<-subset(index.deforest, ID_L2!=2)
    index.deforest<-subset(index.deforest, COUNT!=0)
    index.deforest <- aggregate(COUNT ~  Z_NAME, data=index.deforest, FUN=sum)
    colnames(index.deforest)<-c('ZONE', 'Deforestasi')
    total.deforest<-data.frame(ZONE="TOTAL",Deforestasi=sum(index.deforest[,2]))
    index.deforest<-rbind(index.deforest,total.deforest)
  },error=function(e){cat("No deforestation found", "\n")})
  
  #2 Degradasi Hutan
  tryCatch({
    #index.forest.degrad<-filter(PreQUES_traj_database,ID_L1==1,ID_L2==c(2))
    index.forest.degrad<-subset(PreQUES_traj_database, ID_L1==1)
    index.forest.degrad<-subset(index.forest.degrad, ID_L2==2)
    index.forest.degrad<-subset(index.forest.degrad, COUNT!=0)
    index.forest.degrad <- aggregate(COUNT ~  Z_NAME, data=index.forest.degrad, FUN=sum)
    colnames(index.forest.degrad)<-c('ZONE', 'Degradasi_Hutan')
    total.forest.degrad<-data.frame(ZONE="TOTAL",Degradasi_Hutan=sum(index.forest.degrad[,2]))
    index.forest.degrad<-rbind(index.forest.degrad,total.forest.degrad)
  },error=function(e){cat("No degradation found", "\n")})
  
  #3 Reforestasi
  tryCatch({
    #index.reforest<-filter(PreQUES_traj_database,ID_L1==c(3,4,5,6,7,8),ID_L2==c(1,2))
    index.reforest<-subset(PreQUES_traj_database,ID_L1!=1)
    index.reforest<-subset(index.reforest,ID_L1!=2)
    index.reforest1<-subset(index.reforest,ID_L2==1)
    index.reforest2<-subset(index.reforest,ID_L2==2)
    index.reforest<-rbind(index.reforest1, index.reforest2)
    index.reforest<-subset(index.reforest, COUNT!=0)
    index.reforest <- aggregate(COUNT ~  Z_NAME, data=index.reforest, FUN=sum)
    colnames(index.reforest)<-c('ZONE', 'Reforestasi')
    total.reforest<-data.frame(ZONE="TOTAL",Reforestasi=sum(index.reforest[,2]))
    index.reforest<-rbind(index.reforest,total.reforest)
  },error=function(e){cat("No reforestation found", "\n")})
  
  #4 Stable Forest
  #index.stable.forest<-PREQUES_filter_2(1,1,'==','==')
  #colnames(index.stable.forest)<-c('ZONE', 'Tetap_Hutan')
  #total.stable.forest<-data.frame(ZONE="TOTAL",Tetap_Hutan=sum(index.stable.forest[,2]))
  #index.stable.forest<-rbind(index.stable.forest,total.stable.forest)
  
  #5 Initial Forest Cover
  tryCatch({
    index.init.forest<-subset(PreQUES_traj_database,ID_L1==1)
    index.init.forest<-subset(index.init.forest, COUNT!=0)
    index.init.forest <- aggregate(COUNT ~  Z_NAME, data=index.init.forest, FUN=sum)
    colnames(index.init.forest)<-c('ZONE', 'Forest_T1')
    total.init.forest<-data.frame(ZONE="TOTAL",Forest_T1=sum(index.init.forest[,2]))
    index.init.forest<-rbind(index.init.forest,total.init.forest)
  },error=function(e){cat("No initial forest cover found", "\n")})
  
  #6 Total forest cover
  tryCatch({
    #index.init.forestandlogged<-filter(PreQUES_traj_database,ID_L1==c(1,2))
    index.init.forestandlogged1<-subset(PreQUES_traj_database,ID_L1==1)
    index.init.forestandlogged2<-subset(PreQUES_traj_database,ID_L1==2)
    index.init.forestandlogged<-rbind(index.init.forestandlogged1, index.init.forestandlogged2)
    index.init.forestandlogged<-subset(index.init.forestandlogged, COUNT!=0)
    index.init.forestandlogged <- aggregate(COUNT ~  Z_NAME, data=index.init.forestandlogged, FUN=sum)
    colnames(index.init.forestandlogged)<-c('ZONE', 'Forest_T1')
    total.init.forestandlogged<-data.frame(ZONE="TOTAL",Forest_T1=sum(index.init.forestandlogged[,2]))
    index.init.forestandlogged<-rbind(index.init.forestandlogged,total.init.forestandlogged)
  },error=function(e){cat("No total forest cover found", "\n")})
  
  #7 Initial non forest
  tryCatch({
    #index.init.nonforest<-filter(PreQUES_traj_database,ID_L1!=c(1,2))
    index.init.nonforest<-subset(PreQUES_traj_database,ID_L1!=1)
    index.init.nonforest<-subset(index.init.nonforest,ID_L1!=2)
    index.init.nonforest<-subset(index.init.nonforest, COUNT!=0)
    index.init.nonforest <- aggregate(COUNT ~  Z_NAME, data=index.init.nonforest, FUN=sum)
    colnames(index.init.nonforest)<-c('ZONE', 'Forest_T1')
    total.init.nonforest<-data.frame(ZONE="TOTAL",Forest_T1=sum(index.init.nonforest[,2]))
    index.init.nonforest<-rbind(index.init.nonforest,total.init.nonforest)
  },error=function(e){cat("No initial forest cover found", "\n")})
  
  tryCatch({
    #Degradation rate *********
    degrad.rate<-merge(index.forest.degrad, index.init.forest, by='ZONE', all=T)
    degrad.rate$Degradation_Rate<-(degrad.rate[2]/degrad.rate[3]*100)[,1]
    degrad.rate<-cbind(degrad.rate[1], round(degrad.rate[4],2))
  },error=function(e){cat("No degradation found",conditionMessage(e), "\n")})
  
  tryCatch({
    #Deforestation rate*********
    deforest.rate<-merge(index.deforest, index.init.forestandlogged, by='ZONE', all=T)
    deforest.rate$Deforestation_Rate<-(deforest.rate[2]/deforest.rate[3]*100)[,1]
    deforest.rate<-cbind(deforest.rate[1], round(deforest.rate[4],2))
  },error=function(e){cat("No deforestation found",conditionMessage(e), "\n")})
  
  tryCatch({
    #Reforestation rate***********************
    reforest.rate<-merge(index.reforest, index.init.nonforest, by='ZONE', all=T)
    reforest.rate$Reforestation_Rate<-(reforest.rate[2]/reforest.rate[3]*100)[,1]
    reforest.rate<-cbind(reforest.rate[1], round(reforest.rate[4],2))
  },error=function(e){cat("No reforestation found",conditionMessage(e), "\n")})
  
  #SUMMARY FOREST CHANGE IN %
  forest.change.rate<-merge(deforest.rate, degrad.rate,by='ZONE',all=T)
  if(ncol(reforest.rate)==2){
    forest.change.rate<-merge(forest.change.rate, reforest.rate,by='ZONE',all=T)
  } else {print('no reforestation found')}
  
  
  #create trajectories map
  eval(parse(text=paste("landuse_tr1<-",data[1,1],sep="")))
  eval(parse(text=paste("landuse_tr2<-",data[2,1],sep="")))
  landuse_tr1<- reclassify(landuse_tr1, cbind(128,NA))
  landuse_tr2<- reclassify(landuse_tr2, cbind(128,NA))
  landuse_tr1<-ratify(landuse_tr1, filename='lu1.grd',count=TRUE,overwrite=TRUE)
  landuse_tr2<-ratify(landuse_tr2, filename='lu2.grd',count=TRUE,overwrite=TRUE)
  #lookup_lr<-read.table(lu_reclass, header=TRUE, sep=",")
  colnames(lookup_lr)[3]="ID_L1"
  levels(landuse_tr1)<-merge((levels(landuse_tr1)),lookup_lr, by="ID")
  colnames(lookup_lr)[3]="ID_L2"
  levels(landuse_tr2)<-merge((levels(landuse_tr2)),lookup_lr, by="ID")
  landuse_tr1<-deratify(landuse_tr1,'ID_L1')
  landuse_tr2<-deratify(landuse_tr2,'ID_L2')
  lu_trajectories<-overlay(landuse_tr1,landuse_tr2,fun=function(x,y){return((x*10)+y)})
  lu_trajectories<-ratify(lu_trajectories, filename='lu_trajectories.grd',count=TRUE,overwrite=TRUE)
  colnames(name_traj)[1]="ID"
  levels(lu_trajectories)<-merge((levels(lu_trajectories)),name_traj,by='ID')
  lu_trajectories_final<-deratify(lu_trajectories,'ID_trf')
  lu_trajectories_final<-ratify(lu_trajectories_final, filename='lu_trajectories_final.grd',count=TRUE,overwrite=TRUE)
  colnames(leg_traj)[1]="ID"
  levels(lu_trajectories_final)<-merge((levels(lu_trajectories_final)),leg_traj,by='ID')
  
  #====calculate summary statistics by zone and overall====
  PreQUES_traj_database.melt <- melt(data = PreQUES_traj_database, id.vars=c('Z_NAME','Traj', 'Traj_Code'), measure.vars=c('COUNT'))
  PreQUES_traj_database.zone <- dcast(data = PreQUES_traj_database.melt, formula = Z_NAME ~ Traj_Code, fun.aggregate = sum)
  PreQUES_traj_database.overal <- dcast(data = PreQUES_traj_database.melt, formula = Traj ~ ., fun.aggregate = sum)
  
  PreQUES_traj_forest.melt <- melt(data = PreQUES_traj_database, id.vars=c('Z_NAME','Def'), measure.vars=c('COUNT'))
  PreQUES_traj_forest.zone <- dcast(data = PreQUES_traj_forest.melt, formula = Z_NAME ~ Def, fun.aggregate = sum)
  PreQUES_traj_forest.zone$Other<-NULL
  PreQUES_traj_forest.zone$Others<-NULL
  
  PreQUES_traj_forest.overal <- dcast(data = PreQUES_traj_forest.melt, formula = Def ~ ., fun.aggregate = sum)
  
  PreQUES_traj_drive.melt <- melt(data = PreQUES_traj_database, id.vars=c('Traj','Def'), measure.vars=c('COUNT'))
  PreQUES_traj_drive.zone <- dcast(data = PreQUES_traj_drive.melt, formula = Traj ~ Def, fun.aggregate = sum)
  PreQUES_traj_drive.zone$Other<-NULL
  PreQUES_traj_drive.zone$Others<-NULL
  
  #====plot trajectories map====
  myColors1 <- brewer.pal(9,"Set1")
  myColors2 <- brewer.pal(8,"Accent")
  myColors3 <- brewer.pal(12,"Paired")
  myColors4 <- brewer.pal(9, "Pastel1")
  myColors5 <- brewer.pal(8, "Set2")
  myColors6 <- brewer.pal(8, "Dark2")
  myColors7 <- brewer.pal(11, "Spectral")
  myColors  <-c(myColors7,myColors1, myColors2, myColors3, myColors4, myColors5, myColors6)
  
  if (0 %in% levels(lu_trajectories_final)[[1]][1]){
    myColors  <-c(myColors8, myColors7,myColors1, myColors2, myColors3, myColors4, myColors5, myColors6)
  } else {
    myColors  <-c(myColors7,myColors1, myColors2, myColors3, myColors4, myColors5, myColors6)
  }
  myColors.lu <- myColors[1:nrow(unique(levels(lu_trajectories_final)[[1]][1]))]
  
  ColScale.lu<-scale_fill_manual(name="Jenis alur perubahan", breaks=as.vector(unlist(levels(lu_trajectories_final)[[1]][1])), labels=as.vector(unlist(levels(lu_trajectories_final)[[1]][3])), values=myColors.lu)
  
  plot.LU1<-gplot(lu_trajectories_final, maxpixels=100000) + geom_raster(aes(fill=as.factor(value))) +
    coord_equal() + ColScale.lu +
    theme(plot.title = element_text(lineheight= 5, face="bold")) +
    theme( axis.title.x=element_blank(),axis.title.y=element_blank(),
           panel.grid.major=element_blank(), panel.grid.minor=element_blank(),
           legend.title = element_text(size=9),
           legend.text = element_text(size = 8),
           legend.key.height = unit(0.25, "cm"),
           legend.key.width = unit(0.25, "cm"))
  
  colnames(PreQUES_traj_database.melt)<-c("Zone", "Trajectories","Abbrev", "variable", "Area"); #rename column names
  #graph needs to be checked, sometimes plot with melt data type isn't correctly displaying   
  plot_traj<-ggplot(data=PreQUES_traj_database.melt,aes(factor(Zone),Area,fill=factor(Trajectories)))+geom_bar(stat="identity",position="dodge")+ coord_equal() +
    theme(axis.text.x= element_text(angle=90,hjust=1))+ labs(x = 'Zona', y='Luas area (Ha)')+
    theme(axis.text.x= element_text(angle=360, hjust=1))+ labs(x = 'Jenis perubahan penutupan lahan', y='Luas area (Ha)')+coord_flip()+
    theme( legend.title = element_text(size=8),legend.text = element_text(size = 6))
  
  plot_traj_group<-ggplot(data=PreQUES_traj_database.overal,aes(Traj,.,fill=Traj))+geom_bar(stat="identity",position="dodge")+ coord_equal() +
    theme(axis.text.x= element_text(angle=90,hjust=1))+ labs(x = 'Zona', y='Luas area (Ha)')+
    theme(axis.text.x= element_text(angle=360, hjust=1))+ labs(x = 'Jenis perubahan tutupan lahan', y='Luas area (Ha)')+coord_flip()+
    theme( legend.title = element_text(size=8),legend.text = element_text(size = 6))
  
  colnames(PreQUES_traj_forest.melt)<-c("Zone", "Forest_Change","variable", "Area"); #rename column names
  #graph needs to be checked, sometimes plot with melt data type isn't correctly displaying   
  plot_def<-ggplot(data=PreQUES_traj_forest.melt,aes(factor(Zone),Area,fill=factor(Forest_Change)))+geom_bar(stat="identity",position="dodge")+ coord_equal() +
    theme(axis.text.x= element_text(angle=90,hjust=1))+ labs(x = 'Zona', y='Luas area (Ha)')+
    theme(axis.text.x= element_text(angle=360, hjust=1))+ labs(x = 'Jenis perubahan tutupan hutan', y='Luas area (Ha)')+coord_flip()+
    theme( legend.title = element_text(size=8),legend.text = element_text(size = 6))
  
  colnames(PreQUES_traj_drive.melt)<-c("Trajectories", "Forest_Change","variable", "Area"); #rename column names
  #graph needs to be checked, sometimes plot with melt data type isn't correctly displaying   
  plot_drive<-ggplot(data=PreQUES_traj_drive.melt,aes(factor(Trajectories),Area,fill=factor(Forest_Change)))+geom_bar(stat="identity",position="dodge")+ coord_equal() +
    theme(axis.text.x= element_text(angle=90,hjust=1))+ labs(x = 'Zona', y='Luas area (Ha)')+
    theme(axis.text.x= element_text(angle=360, hjust=1))+ labs(x = 'Jenis perubahan tutupan hutan', y='Luas area (Ha)')+coord_flip()+
    theme( legend.title = element_text(size=8),legend.text = element_text(size = 6))
  
  colnames(PreQUES_traj_database.overal)<-c("Trajectories", "Area (Ha)")
  colnames(PreQUES_traj_database.zone)[1]<-c("Trajectories")
  colnames(PreQUES_traj_forest.overal)<-c("Forest cover changes", "Area (Ha)")
  
  #===Trajectories Data Export===
  PreQUES.index.traj=paste( "_",pu_name,"_",data[1,2], "_", data[2,2], sep="")
  eval(parse(text=(paste("PreQUES_traj_database", PreQUES.index.traj, "<-PreQUES_traj_database", sep=""   ))))
  newTraj<-paste("PreQUES_traj_database", PreQUES.index.traj, sep="")
  
  #eval(parse(text=(paste("PreQUES_traj_summary", PreQUES.index.traj, "<-PreQUES_traj_database.overal", sep=""   ))))
  #newTrajsum<-paste("PreQUES_traj_summary", PreQUES.index.traj, sep="")
  
  #eval(parse(text=(paste("PreQUES_traj_zone", PreQUES.index.traj, "<-PreQUES_traj_database.zone", sep=""   ))))
  #newTrajz<-paste("PreQUES_traj_zone", PreQUES.index.traj, sep="")
  
  #eval(parse(text=(paste("PreQUES_traj_map", PreQUES.index.traj, "<-lu_trajectories_final", sep=""   ))))
  #newTrajmap<-paste("PreQUES_traj_map", PreQUES.index.traj, sep="")
  
  #command<-paste("resave(", newTraj, ",", newTrajsum, ",",newTrajz,",",newTrajmap, ",", sep="")
  command<-paste("resave(", newTraj,",", sep="")
  command<-paste(command,"file='",proj.file,"')", sep="")
  
  #list.landuse<-ls(pattern="landuse_t")
  #command<-paste("resave(PreQUES.index,r.brick, Ov_chg,Ov_chg.ha,Ov_chg.rate,", newPre, ",", newludb, ",",  sep="")
  
  eval(parse(text=(command)))
  
  #====Write Report====
  rtffile <- RTF("LUMENS_Pre-QUES_Trajectory_report.lpr", font.size=9)
  title1<-"{\\colortbl;\\red0\\green0\\blue0;\\red255\\green0\\blue0;\\red146\\green208\\blue80;\\red0\\green176\\blue240;\\red140\\green175\\blue71;\\red0\\green112\\blue192;\\red79\\green98\\blue40;} \\pard\\qr\\b\\fs70\\cf2 L\\cf3U\\cf4M\\cf5E\\cf6N\\cf7S \\cf1REPORT \\par\\b0\\fs20\\ql\\cf1"
  title2<-paste("\\pard\\qr\\b\\fs40\\cf1 PreQUES-Land Use Trajectory Analysis ", "for ", location, ", ", province, ", ", country, "\\par\\b0\\fs20\\ql\\cf1", sep="")
  sub_title<-"\\cf2\\b\\fs32 ANALISA ALUR PENGGUNAAN LAHAN\\cf1\\b0\\fs20"
  #date<-paste("Date : ", date, sep="")
  time_start<-paste("Proses dimulai : ", time_start, sep="")
  time_end<-paste("Proses selesai : ", eval(parse(text=(paste("Sys.time ()")))), sep="")
  line<-paste("------------------------------------------------------------------------------------------------------------------------------------------------")
  area_name_rep<-paste("\\b", "\\fs20", location, "\\b0","\\fs20")
  I_O_period_1_rep<-paste("\\b","\\fs20", T1)
  I_O_period_2_rep<-paste("\\b","\\fs20", T2)
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
  addTable(rtffile,proj_descr,font.size=8,col.widths=width)
  addPageBreak(rtffile)
  addParagraph(rtffile, sub_title)
  addNewLine(rtffile)
  addParagraph(rtffile, line)
  
  #addParagraph(rtffile, date)
  addNewLine(rtffile)
  addParagraph(rtffile, "Analisa perubahan tutupan lahan dilakukan untuk mengetahui kecenderungan perubahan tutupan lahan di suatu daerah pada satu kurun waktu. Analisa ini dilakukan dengan menggunakan data peta tutupan lahan pada dua periode waktu yang berbeda. Selain itu, dengan memasukkan data unit perencanaan kedalam proses analisa, dapat diketahui kecenderungan perubahan tutupan lahan pada masing-masing kelas unit perencanaan yang ada. Informasi yang dihasilkan melalui analisa ini dapat digunakan dalam proses perencanaan untuk berbagai hal. Diantaranya adalah: menentukan prioritas pembangunan, mengetahui faktor pemicu perubahan penggunaan lahan, merencanakan skenario pembangunan di masa yang akan datang, dan lain sebagainya.")
  addNewLine(rtffile)
  addParagraph(rtffile, paste("\\b \\fs24 ALUR PERUBAHAN PENGGUNAAN LAHAN\\b0 \\fs24", sep=""))
  addNewLine(rtffile)
  addParagraph(rtffile, line)
  addNewLine(rtffile)
  addParagraph(rtffile, "Alur perubahan penggunaan lahan merupakan ringkasan keseluruhan tipe rangkaian perubahan penggunaan lahan yang mungkin terjadi di sebuah daerah. Kategori besar dari alur perubahan lahan dibagi menjadi dua jenis yaitu Loss of tree cover dan recovery of tree cover")
  addNewLine(rtffile)
  
  addPlot(rtffile,plot.fun=print, width=6.7,height=4,res=300, plot.LU1)
  addParagraph(rtffile, paste("\\b \\fs20 Gambar 1. Peta Kelompok Perubahan Penutupan Lahan\\b0 \\fs20 ", location, "\\b \\fs20 Tahun \\b0 \\fs20", I_O_period_1_rep,"-", I_O_period_2_rep, sep=" "))
  addNewLine(rtffile)
  addPlot(rtffile,plot.fun=print, width=6.7,height=4,res=300, plot_traj_group)
  addParagraph(rtffile, paste("\\b \\fs20 Gambar 2. Grafik Kelompok Perubahan Penutupan Lahan\\b0 \\fs20 ", location, "\\b \\fs20 Tahun \\b0 \\fs20",I_O_period_1_rep,"-", I_O_period_2_rep, sep=" "))
  addNewLine(rtffile)
  addPlot(rtffile,plot.fun=print, width=6.7,height=4,res=300, plot_traj)
  addParagraph(rtffile, paste("\\b \\fs20 Gambar 3. Grafik Kelompok Perubahan Penutupan Lahan\\b0 \\fs20 ", location, "\\b \\fs20 Tahun \\b0 \\fs20",  I_O_period_1_rep,"-", I_O_period_2_rep,"\\b \\fs20 berdasarkan Unit Perencanaan \\b0 \\fs20", sep=" "))
  addNewLine(rtffile)
  addNewLine(rtffile)
  addParagraph(rtffile, paste("\\b \\fs20 Tabel 1. Luas Area Kelompok Perubahan Penutupan Lahan \\b0 \\fs20 ", location, "\\b \\fs20 Tahun \\b0 \\fs20",  I_O_period_1_rep,"-", I_O_period_2_rep, sep=" "))
  addTable(rtffile,PreQUES_traj_database.overal,font.size=8)
  addNewLine(rtffile)
  addParagraph(rtffile, paste("\\b \\fs20 Tabel 2. Luas Area Kelompok Perubahan Lahan di \\b0 \\fs20 ", location, "\\b \\fs20 Tahun \\b0 \\fs20", I_O_period_1_rep, "\\b \\fs20 - \\b0 \\fs20", I_O_period_2_rep,"\\b \\fs20 Tiap Unit Perencanaan \\b0 \\fs20", sep=" "))
  addTable(rtffile,PreQUES_traj_database.zone,font.size=8)
  
  #PLOT: TOTAL CHANGES PER TRAJECTORY
  for(s in 2:(ncol(PreQUES_traj_database.zone))){
    print(s)
    c<-s-1
    PreQUES_traj_database.zone.melt_pertrajek<- melt(data = PreQUES_traj_database.zone, id.vars=c('Trajectories'), measure.vars=c(colnames(PreQUES_traj_database.zone)[s]))
    plot_per_trajek<-ggplot(data=PreQUES_traj_database.zone.melt_pertrajek,aes(factor(Trajectories),value,fill=factor(Trajectories)))+geom_bar(stat="identity",position="dodge")+ coord_equal() +
      theme(axis.text.x= element_text(angle=90,hjust=1))+ labs(x = 'Zona', y='Luas area (Ha)')+
      theme(axis.text.x= element_text(angle=360, hjust=1))+ labs(x = 'Jenis perubahan tutupan lahan', y='Luas area (Ha)')+coord_flip()+
      theme( legend.title = element_text(size=8),legend.text = element_text(size = 6))
    #eval(parse(text=( paste("plot.per.trajek_",s,'_',colnames(PreQUES_traj_database.zone)[s],'<-plot_per_trajek', sep=''))));#save plots
    addNewLine(rtffile)
    addPlot(rtffile,plot.fun=print, width=6.7,height=4,res=300, plot_per_trajek)
    addNewLine(rtffile)
    addParagraph(rtffile, paste("\\b \\fs20 Sub Gambar ",c,". Grafik Perubahan Lahan di Berbagai Zona Perencanaan untuk jenis ",colnames(PreQUES_traj_database.zone)[s], "\\b0 \\fs20 di ", location, "\\b \\fs20 Tahun \\b0 \\fs20",I_O_period_1_rep,"-", I_O_period_2_rep, sep=" "))
    addNewLine(rtffile)
  }
  
  addNewLine(rtffile)
  addParagraph(rtffile, paste("\\b \\fs24 PERUBAHAN TUTUPAN HUTAN\\b0 \\fs24", sep=""))
  addNewLine(rtffile)
  addParagraph(rtffile, line)
  addNewLine(rtffile)
  addParagraph(rtffile, "Salah satu bentuk alur perubahan penggunaan lahan yang paling banyak mendapatkan perhatian adalah alur perubahan hutan alam menjadi tipe tutupan lahan lainnya (deforestasi) dan perubahan hutan alam primer menjadi hutan alam sekunder (degradasi). Bagian ini memperlihatkan hasil analisa LUMENS terhadap perubahan tutupan hutan di sebuah daerah")
  addNewLine(rtffile)
  addParagraph(rtffile, paste("\\b \\fs20 Gambar 4. Grafik Perubahan Tutupan Hutan di Berbagai Zona Perencanaan\\b0 \\fs20 ", location, "\\b \\fs20 Tahun \\b0 \\fs20", I_O_period_1_rep,"-", I_O_period_2_rep,"\\b \\fs20 berdasarkan Unit Perencanaan \\b0 \\fs20", sep=" "))
  addNewLine(rtffile)
  addPlot(rtffile,plot.fun=print, width=6.7,height=4,res=300, plot_def)
  addNewLine(rtffile)
  addParagraph(rtffile, paste("\\b \\fs20 Tabel 3. Luas deforestasi \\b0 \\fs20 ", location, "\\b \\fs20 Tahun \\b0 \\fs20",I_O_period_1_rep,"-", I_O_period_2_rep, sep=" "))
  addTable(rtffile,PreQUES_traj_forest.overal,font.size=8)
  addNewLine(rtffile)
  addParagraph(rtffile, paste("\\b \\fs20 Tabel 4. Luas deforestasi berdasarkan zonasi \\b0 \\fs20 ", location, "\\b \\fs20 Tahun \\b0 \\fs20",I_O_period_1_rep,"-", I_O_period_2_rep, sep=" "))
  addTable(rtffile,PreQUES_traj_forest.zone,font.size=8)
  addNewLine(rtffile)
  addParagraph(rtffile, paste("\\b \\fs20 Tabel 5. Laju deforestasi berdasarkan zonasi \\b0 \\fs20 ", location, "\\b \\fs20 Tahun \\b0 \\fs20", I_O_period_1_rep,"-", I_O_period_2_rep, sep=" "))
  addTable(rtffile,forest.change.rate,font.size=8)
  addNewLine(rtffile)
  
  #PLOT: TOTAL CHANGES PER TRAJECTORY
  for(s in 2:(ncol(PreQUES_traj_forest.zone))){
    print(s)
    c<-s-1
    PreQUES_traj_database.zone.melt_pertrajek<- melt(data = PreQUES_traj_forest.zone, id.vars=c('Z_NAME'), measure.vars=c(colnames(PreQUES_traj_forest.zone)[s]))
    colnames(PreQUES_traj_database.zone.melt_pertrajek)[1]<-'ZONE'
    plot_per_trajek<-ggplot(data=PreQUES_traj_database.zone.melt_pertrajek,aes(factor(ZONE),value,fill=factor(ZONE)))+geom_bar(stat="identity",position="dodge")+ coord_equal() +
      theme(axis.text.x= element_text(angle=90,hjust=1))+ labs(x = 'Zona', y='Luas area (Ha)')+
      theme(axis.text.x= element_text(angle=360, hjust=1))+ labs(x = 'Jenis perubahan tutupan hutan', y='Luas area (Ha)')+coord_flip()+
      theme( legend.title = element_text(size=8),legend.text = element_text(size = 6))
    #eval(parse(text=( paste("plot.per.trajek_",s,'_',colnames(PreQUES_traj_database.zone)[s],'<-plot_per_trajek', sep=''))));#save plots
    addNewLine(rtffile)
    addPlot(rtffile,plot.fun=print, width=6.7,height=4,res=300, plot_per_trajek)
    addNewLine(rtffile)
    addParagraph(rtffile, paste("\\b \\fs20 Sub Gambar ",c,". Grafik Perubahan Hutan di Berbagai Zona Perencanaan untuk ",colnames(PreQUES_traj_forest.zone)[s], "\b  di \\b0 \\fs20", location, "\\b \\fs20 Tahun \\b0 \\fs20", I_O_period_1_rep,"-", I_O_period_2_rep, sep=" "))
    addNewLine(rtffile)
  }
  
  addPlot(rtffile,plot.fun=print, width=6.7,height=4,res=300, plot_drive)
  addNewLine(rtffile)
  addParagraph(rtffile, paste("\\b \\fs20 Gambar 5. Grafik Perubahan Tutupan Hutan dan alur perubahan yang menyebabkannya\\b0 \\fs20 ", location, "\\b \\fs20 Tahun \\b0 \\fs20", I_O_period_1_rep,"-", I_O_period_2_rep,"\\b \\fs20 berdasarkan Unit Perencanaan \\b0 \\fs20", sep=" "))
  addNewLine(rtffile)
  
  addParagraph(rtffile, paste("\\b \\fs20 Tabel 5. Luas deforestasi berdasarkan alur perubahan \\b0 \\fs20 ", location, "\\b \\fs20 Tahun \\b0 \\fs20", I_O_period_1_rep,"-", I_O_period_2_rep, sep=" "))
  addTable(rtffile,PreQUES_traj_drive.zone,font.size=8)
  addNewLine(rtffile)
  done(rtffile)
}

command2<-paste("start ", "winword ", result_dir, "/LUMENS_Pre-QUES_change_report.lpr", sep="" )
shell(command2)

#====write LUMENS log file====
add.log<-data.frame(IDX=(PreQUES.index), 
                    MODULE="Pre-QuES", 
                    DATE=format(Sys.time(), "%d-%m%-%Y"),
                    TIME=format(Sys.time(), "%X"),
                    LU1=data[1,1],
                    LU2=data[2,1],
                    PU=pu[1],
                    T1=T1,
                    T2=T2,
                    LOOKUP_LC=Look_up_table,
                    LOOKUP_ZONE="From DB",
                    NODATA=raster.nodata,
                    ANALYSIS_OPTION=analysis.option,
                    OUTPUT_FOLDER=result_dir, row.names=NULL)
log.preques<-na.omit(rbind(log.preques,add.log))
write.csv(log.preques, paste(user_path,"/LUMENS/LUMENS_pre_ques.log", sep=""))

#CLEAN ENVIRONMENT
#rm(list=ls(all.names=TRUE))

gc()
