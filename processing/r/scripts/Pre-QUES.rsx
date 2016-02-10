##[QUES]=group
##lookup_lc=file
##raster.nodata=number 0

library(foreign)
library(raster)
library(rasterVis)
library(reshape2)
library(plyr)
library(lattice)
library(latticeExtra)
library(RColorBrewer)
library(hexbin)
library(grid)
library(ggplot2)
library(spatial.tools)
library(rtf)
library(tcltk)

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

#====READ LANDUSE DATA FROM LUMENS DATABASE====
per<-as.data.frame(ls(pattern="freq"))
n<-nrow(per)
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

#if pu is not exist, use p.admin.df as planning unit reference
data2<-as.data.frame(as.character(ls(pattern="pu_pu")))
if (nrow(data2)==0) {
  pu_pu1<-ref
  pu_pu1[pu_pu1==0]<-NA
  lut.pu<-p.admin.df[2]
  lut.pu[2]<-p.admin.df[1]
}
data2<-as.data.frame(as.character(ls(pattern="pu_pu")))

n<-nrow(data2)
command3<-NULL
for(i in 1:n) {
  if (i!=n){
    command3a<-eval(parse(text=(paste( "names(pu_pu", i, ")", sep=""))))
    command3<-c(command3,command3a)
  } else {
    command3a<-eval(parse(text=(paste( "names(pu_pu", i, ")", sep=""))))
    command3<-c(command3,command3a)
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
  data<-edit(data)
  if(sum(data$t1)==1 & sum(data$t2)==1){
    break
  }
}
data$sum<-data$t1+data$t2
data <- data[which(data$sum==1),]

data$t1<-NULL
data$t2<-NULL
data$sum<-NULL

n<-nrow(data)
command1<-NULL
T1<-data[1,2]
T2<-data[2,2]

#====SELECT PLANNING UNIT TO BE ANALYZED====
data2<-as.data.frame(cbind(data2,command3))
data2$usage<-0
colnames(data2)[1]<-"data"
colnames(data2)[2]<-"sources"
data2$data<-as.character(data2$data)

repeat{
  data2<-edit(data2)
  if(sum(data2$usage)==1){
    break
  }
}

data2 <- data2[which(data2$usage==1),]
data2$usage<-NULL
pu<-as.character(data2[1,1])

#wd
preques_folder<-paste("PreQUES_analysis_", T1,"_",T2,"_",PreQUES.index,sep="")
result_dir<-paste(dirname(proj.file),"/QUES/PreQUES/", sep="")
setwd(result_dir)
dir.create(preques_folder)

result_dir<-paste(result_dir,preques_folder, sep='')
setwd(result_dir)

#set project properties
Area_name=location
tab_title<-as.data.frame(Area_name)
Year_T1=T1
Year_T2=T2
Period=Year_T1-Year_T2
proj_prop<-as.data.frame(Area_name)
proj_prop$Year_T1<-Year_T1
proj_prop$Year_T2<-Year_T2
proj_prop$period <- do.call(paste, c(proj_prop[c("Year_T1", "Year_T2")], sep = " - "))
proj_period<-do.call(paste, c(proj_prop[c("Year_T1", "Year_T2")], sep = " - "))

#load datasets
landuse1 <- eval(parse(text=(paste(data[1,1], sep=""))))
landuse2 <- eval(parse(text=(paste(data[2,1], sep=""))))
zone <- eval(parse(text=(paste(pu, sep=""))))

NAvalue(landuse1)<-raster.nodata
NAvalue(landuse2)<-raster.nodata
NAvalue(zone)<-raster.nodata

#projection handling
if (grepl("+units=m", as.character(landuse1@crs))){
  print("Raster maps have projection in meter unit")
  Spat_res<-res(landuse1)[1]*res(landuse1)[2]/10000
  paste("Raster maps have ", Spat_res, " Ha spatial resolution, Pre-QUES will automatically generate data in Ha unit")
} else if (grepl("+proj=longlat", as.character(landuse1@crs))){
  print("Raster maps have projection in degree unit")
  Spat_res<-res(landuse1)[1]*res(landuse1)[2]*(111319.9^2)/10000
  paste("Raster maps have ", Spat_res, " Ha spatial resolution, Pre-QUES will automatically generate data in Ha unit")
} else{
  msgBox <- tkmessageBox(title = "QUES",
                         message = "Raster map projection is unknown",
                         icon = "info",
                         type = "ok")
  quit()
}

#Extent handling and raster resolution land-cover maps
if (as.character(landuse1@crs)==as.character(landuse2@crs)){
  print("Raster map time series 1 and 2 have the same projection")
  if (res(landuse1)[1]==res(landuse2)[1]){
    print("Raster map time series 1 and 2 have the same extent")
  } else{
    print("Raster map time series 1 and 2 don't have the same extent, synchronising land-cover map...")
    landuse2<-spatial_sync_raster(landuse2, landuse1, method = "ngb")
  }
} else{
  print("Raster map time series 1 and 2 don't have the same projection, synchronising land-cover map...")
  landuse2<-spatial_sync_raster(landuse2, landuse1, method = "ngb")
}

# Extent handling and raster resolution zone map
if(is.na(zone@crs)){
  zone@crs<-landuse1@crs
}
if (as.character(landuse1@crs)==as.character(zone@crs)){
  print("Raster map time series 1 and 2 have the same projection")
  if (res(landuse1)[1]==res(zone)[1]){
    print("Raster map time series 1 and 2 have the same resolution")
    if (landuse1@extent==zone@extent){
      print("Raster map time series 1 and 2 have the same extent")
    } else {
      print("Raster map time series 1 and 2 don't have the same extent, synchronising land-cover map...")
      zone<-spatial_sync_raster(zone, landuse1, method = "ngb")
    }
  } else{
    print("Raster map time series 1 and 2 don't have the same resolution, synchronising land-cover map...")
    zone<-spatial_sync_raster(zone, landuse1, method = "ngb")
  }
} else{
  print("Raster map time series 1 and 2 don't have the same projection, synchronising land-cover map...")
  zone<-spatial_sync_raster(zone, landuse1, method = "ngb")
}

#load look up table
lookup_lc2<-lookup_lc
lookup_l<- read.table(lookup_lc, header=TRUE, sep=",",)
lookup_z <- lut.pu
lookup_lc<- read.table(lookup_lc, header=TRUE, sep=",",)
colnames(lookup_l)<-c("ID", "CLASS")
colnames(lookup_lc)<-c("ID", "CLASS")
colnames(lookup_z)<-c("ID", "ZONE")

#set raster attribute table (RAT)
#landuse1<-ratify(landuse1, filename='landuse1.grd',count=TRUE,overwrite=TRUE)
#landuse2<-ratify(landuse2, filename='landuse2.grd',count=TRUE,overwrite=TRUE)
#zone<-ratify(zone, filename='ratify.grd',count=TRUE,overwrite=TRUE)

#create land use change database
#area_lc1<-as.data.frame(levels(landuse1))
#area_lc2<-as.data.frame(levels(landuse2))
#area_zone<-as.data.frame(levels(zone))
eval(parse(text=(paste("area_lc1<-freq", data[1,1], sep=""))))
eval(parse(text=(paste("area_lc2<-freq", data[2,1], sep=""))))
eval(parse(text=(paste("area_zone<-as.data.frame(freq(", pu,"))", sep=""))))
area_lc1$count<-area_lc1$count*Spat_res
area_lc2$count<-area_lc2$count*Spat_res
area_zone$count<-area_zone$count*Spat_res
area<-min(sum(area_zone[,2]), sum(area_lc1[,2]), sum(area_lc2[,2]))
colnames(area_lc1)[1] = "ID"
colnames(area_lc1)[2] = "COUNT_LC1"
colnames(area_lc2)[1] = "ID"
colnames(area_lc2)[2] = "COUNT_LC2"
colnames(area_zone)[1] = "ID"
colnames(area_zone)[2] = "COUNT_ZONE"

area_lc1<-merge(area_lc1,lookup_l,by="ID")
area_lc2<-merge(area_lc2,lookup_l,by="ID")
area_zone<-merge(area_zone,lookup_z,by="ID")
#area_lc1<-as.data.frame(levels(landuse1))
#area_lc2<-as.data.frame(levels(landuse2))
#area_zone<-as.data.frame(levels(zone))
#colnames(area_lc1)[2] = "COUNT_LC1"
colnames(area_lc1)[3] = "CLASS_LC1"
#colnames(area_lc2)[2] = "COUNT_LC2"
colnames(area_lc2)[3] = "CLASS_LC2"
cross <- as.data.frame(crosstab((stack(landuse1,landuse2,zone))))
colnames(cross)[1] ="ID_LC1"
colnames(cross)[2] = "ID_LC2"
colnames(cross)[3] = "ZONE"
colnames(cross)[4] = "COUNT"
cross$COUNT<-cross$COUNT*Spat_res
colnames(lookup_l)[1]="ID_LC1"
colnames(lookup_l)[2]="LC_t1"
data_merge <- merge(cross,lookup_l,by="ID_LC1")
colnames(lookup_l)[1]="ID_LC2"
colnames(lookup_l)[2]="LC_t2"
data_merge <- as.data.frame(merge(data_merge,lookup_l,by="ID_LC2"))
colnames(lookup_z)[1]="ZONE"
colnames(lookup_z)[2]="Z_NAME"
data_merge <- as.data.frame(merge(data_merge,lookup_z,by="ZONE"))
data_merge_sel <- data_merge[ which(data_merge$COUNT > 0),]
data_merge_sel$LU_CHG <- do.call(paste, c(data_merge_sel[c("LC_t1", "LC_t2")], sep = " to "))
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

#Landuse Dominant Change
chg_only<-aggregate(COUNT~LU_CHG,data=lg_chg,FUN=sum)
chg_only$CHG_CODE<-as.factor(toupper(abbreviate(chg_only$LU_CHG, minlength=5, strict=FALSE, method="both")))
chg_only<-chg_only[order(-chg_only$COUNT),]
chg_only<-chg_only[c(1,3,2)]
chg_only_top<-head(chg_only, n=10)

#Zonal Dominant Change
lg_chg_zonal<-as.data.frame(NULL)
for (i in 1:length(area_zone$ID)){
  tryCatch({
    a<-(area_zone$ID)[i]
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

# calculate basic statistic
#different landuse number handling
lu1.lost<-unique(data_merge_sel$ID_LC2)[is.na(match(unique(data_merge_sel$ID_LC2),unique(data_merge_sel$ID_LC1)))]
lu2.lost<-unique(data_merge_sel$ID_LC1)[is.na(match(unique(data_merge_sel$ID_LC1),unique(data_merge_sel$ID_LC2)))]
if(length(lu1.lost)!=0){
  new.lu<-area_lc2[area_lc2$ID %in% lu1.lost, 1:3]
  new.lu$COUNT_LC2<-0
  colnames(new.lu)[2]<-"COUNT_LC1"
  colnames(new.lu)[3]<-"CLASS_LC1"
  area_lc1<-rbind(area_lc1,new.lu)
}
if(length(lu2.lost)!=0){
  new.lu<-area_lc1[area_lc1$ID %in% lu2.lost, 1:3]
  new.lu$COUNT_LC1<-0
  colnames(new.lu)[2]<-"COUNT_LC2"
  colnames(new.lu)[3]<-"CLASS_LC2"
  area_lc2<-rbind(area_lc2,new.lu)
}
area_summary <- merge(area_lc1,area_lc2,by="ID")
Ov_chg<-as.data.frame(area_summary$CLASS_LC1)
colnames(Ov_chg)[1]="Land_use_type"
Ov_chg$LU_CODE<-as.factor(toupper(abbreviate(Ov_chg$Land_use_type, minlength=4, strict=FALSE, method="both")))
Ov_chg$area_t1<-area_summary$COUNT_LC1
Ov_chg$area_t2<-area_summary$COUNT_LC2
colnames(Ov_chg)[3]="t1_hectares"
colnames(Ov_chg)[4]="t2_hectares"
Ov_chg$Overall_change<-Ov_chg$t2_hectares-Ov_chg$t1_hectares
Ov_chg$Rate<-as.numeric(format(round((Ov_chg$Overall_change/(Ov_chg$t1_hectares*abs(Period)) * 100),2), nsmall=2))
colnames(Ov_chg)<-c("Land_use_type","LU_CODE",paste(T1,"(ha)",sep=""), paste(T2, "(ha)",sep=""), "Overall_change(ha)", "Rate(%)")

Ov_chg.melt <- melt(data = Ov_chg, id.vars=c('Land_use_type',"LU_CODE"), measure.vars=c(as.character(paste(T1,"(ha)",sep="")), as.character(paste(T2, "(ha)",sep=""))))
colnames(Ov_chg.melt)<-c("Land_use_type","LU_CODE", "Year", "Area")

#create land use change map
cross_temp<-as.data.frame(crosstab(landuse1,landuse2))
colnames(cross_temp)[1] = "Var1"
colnames(cross_temp)[2] = "Var2"
cross_temp$Freq<-cross_temp$Freq*Spat_res
cross_temp$chkVar1<-as.numeric(is.na(cross_temp$Var1))
cross_temp$chkVar2<-as.numeric(is.na(cross_temp$Var2))
cross_temp$chkNull<-cross_temp$chkVar1+cross_temp$chkVar2
cross_temp <- cross_temp[ which(cross_temp$chkNull < 1),]
cross_temp$Var1r<-as.numeric(cross_temp$Var1)
cross_temp$Var2r<-as.numeric(cross_temp$Var2)
cross_temp$ID<-as.factor(cross_temp$Var1r+(cross_temp$Var2r*100))
colnames(cross_temp)[1] = "ID_LC1"
colnames(cross_temp)[2] = "ID_LC2"
colnames(cross_temp)[3] = "COUNT"
colnames(lookup_l)[1]="ID_LC1"
colnames(lookup_l)[2]="LC_t1"
cross_temp <- merge(cross_temp,lookup_l,by="ID_LC1")
colnames(lookup_l)[1]="ID_LC2"
colnames(lookup_l)[2]="LC_t2"
cross_temp <- as.data.frame(merge(cross_temp,lookup_l,by="ID_LC2"))
cross_temp$LU_CHG <- do.call(paste, c(cross_temp[c("LC_t1", "LC_t2")], sep = " to "))
luchg<-overlay(landuse1,landuse2,fun=function(x,y){return(x+(y*100))})
luchg_att<-as.data.frame(freq(luchg))
luchg_att$count<-luchg_att$count*Spat_res
colnames(luchg_att)<-c("ID","AREA")
luchg_att<-merge(luchg_att,cross_temp,by="ID")

#create land use transition matrix
cross_temp.melt <- melt(data = cross_temp, id.vars=c('LC_t1','LC_t2'), measure.vars=c('COUNT'))
cross_temp.melt.cast <- dcast(data = cross_temp.melt, formula = LC_t1 ~ LC_t2, fun.aggregate = sum)
cross_temp.melt.dbf<-cross_temp.melt.cast

#CHANGE MAP
#cmap<-ratify(luchg)
#cmaptab<-merge((levels(luchg)),cross_temp,by="ID")
cmap_length<-NROW(luchg_att)
c.legend<-1:cmap_length
cmapleg<-as.data.frame(cbind(luchg_att[1],as.data.frame(c.legend),luchg_att[13]))
#levels(cmap)<-merge((levels(cmap)),cmapleg,by="ID")
cmapleg$ID<-NULL
colnames(cmapleg)<-c('ID', 'Jenis perubahan penutupan lahan')

#PLOT
# ov.change.plot<-ggplot(Ov_chg.melt,aes(x=reorder(Land_use_type, -Area),y=Area,fill=Year))+geom_bar(stat="identity",position="dodge")+
#   theme(axis.text.x= element_text(angle=45,hjust=1))+
#   theme(text = element_text(size=30))
ov.change.plot.2<-ggplot(Ov_chg.melt,aes(x=reorder(LU_CODE, -Area),y=Area,fill=Year))+geom_bar(stat="identity",position="dodge")+
  theme(axis.text.x= element_text(angle=45,hjust=1))+ ylab("Area (Ha)") +
  theme( legend.title = element_text(size=8),legend.text = element_text(size = 6), axis.text.x = element_text(size = 8),
         axis.title.x=element_blank())

#Create Database
write.dbf(proj_prop, "Pre_QUES_Project_properties.dbf")
write.dbf(data_merge_sel, "land_use_change_database.dbf")
write.dbf(Ov_chg, "Overall_change.dbf")
colnames(cross_temp.melt.dbf)<-gsub(" ","_", colnames(cross_temp.melt.dbf) , fixed=TRUE)
write.dbf(cross_temp.melt.dbf, "LU_transition_matrix.dbf")
writeRaster(luchg, filename="lulcc_map.tif", format="GTiff", overwrite=TRUE)
write.dbf(luchg_att, "lulcc_map.dbf")

#====database export====
PreQUES.index=PreQUES.index+1
resave(PreQUES.index, file=proj.file)

#Create Map for report
myColors1 <- brewer.pal(9,"Set1")
myColors2 <- brewer.pal(8,"Accent")
myColors3 <- brewer.pal(12,"Paired")
myColors4 <- brewer.pal(9, "Pastel1")
myColors5 <- brewer.pal(8, "Set2")
myColors6 <- brewer.pal(8, "Dark2")
myColors7 <- rev(brewer.pal(11, "RdYlGn"))
myColors8 <- "#000000"
myColors9 <- brewer.pal(12, "Set3")

if (0 %in% area_lc1$ID){
  myColors  <-c(myColors8, myColors7,myColors1, myColors2, myColors3, myColors4, myColors5, myColors6)
} else {
  myColors  <-c(myColors7,myColors1, myColors2, myColors3, myColors4, myColors5, myColors6)
}


#Landuse 1 map
myColors.lu <- myColors[1:length(unique(area_lc1$ID))]
ColScale.lu<-scale_fill_manual(name="Tipe tutupan lahan tahun 1", breaks=area_lc1$ID, labels=area_lc1$CLASS_LC1, values=myColors.lu)
plot.LU1<-gplot(landuse1, maxpixels=100000) + geom_raster(aes(fill=as.factor(value))) +
  coord_equal() + ColScale.lu +
  theme(plot.title = element_text(lineheight= 5, face="bold")) +
  theme( axis.title.x=element_blank(),axis.title.y=element_blank(),
         panel.grid.major=element_blank(), panel.grid.minor=element_blank(),
         legend.title = element_text(size=8),
         legend.text = element_text(size = 6),
         legend.key.height = unit(0.25, "cm"),
         legend.key.width = unit(0.25, "cm"))

#Landuse 2 map
ColScale.lu<-scale_fill_manual(name="Tipe tutupan lahan tahun 2", breaks=area_lc2$ID, labels=area_lc2$CLASS_LC2, values=myColors.lu)
plot.LU2<-gplot(landuse2, maxpixels=100000) + geom_raster(aes(fill=as.factor(value))) +
  coord_equal() + ColScale.lu +
  theme(plot.title = element_text(lineheight= 5, face="bold")) +
  theme( axis.title.x=element_blank(),axis.title.y=element_blank(),
         panel.grid.major=element_blank(), panel.grid.minor=element_blank(),
         legend.title = element_text(size=8),
         legend.text = element_text(size = 6),
         legend.key.height = unit(0.25, "cm"),
         legend.key.width = unit(0.25, "cm"))

if (0 %in% area_zone$ID){
  myColors  <-c(myColors8, myColors5,myColors1, myColors2, myColors7, myColors4, myColors5, myColors6)
} else {
  myColors  <-c(myColors5,myColors1, myColors2, myColors3, myColors4, myColors7, myColors6)
}

#zone map
myColors.Z <- myColors[1:length(unique(area_zone$ID))]
ColScale.Z<-scale_fill_manual(name="Unit perencanaan", breaks=area_zone$ID, labels=area_zone$ZONE, values=myColors.Z)
plot.Z<-gplot(zone, maxpixels=100000) + geom_raster(aes(fill=as.factor(value))) +
  coord_equal() + ColScale.Z +
  theme(plot.title = element_text(lineheight= 5, face="bold")) +
  theme( axis.title.x=element_blank(),axis.title.y=element_blank(),
         panel.grid.major=element_blank(), panel.grid.minor=element_blank(),
         legend.title = element_text(size=8),
         legend.text = element_text(size = 6),
         legend.key.height = unit(0.25, "cm"),
         legend.key.width = unit(0.25, "cm"))

rm(myColors8, myColors7,myColors1, myColors2, myColors3, myColors4, myColors5, myColors6, myColors9)

#Largest Source of Change in Landuse
Largest.chg<- ggplot(data=chg_only_top, aes(x=reorder(CHG_CODE, -COUNT),y=COUNT, fill=CHG_CODE))+geom_bar(stat='identity',position='dodge')+
  geom_text(data=chg_only_top, aes(x=CHG_CODE, y=COUNT, label=round(COUNT, 1)),size=3, vjust=0.1) +
  ggtitle(paste("10 Perubahan Tutupan Lahan Dominan di", location, T1,"-",T2 )) +
  labs(x = 'Jenis perubahan penutupan lahan', y='Luas area (Ha)') + guides(fill=FALSE)+
  theme(axis.text.x= element_text(angle=45,hjust=1))+
  theme(plot.title = element_text(lineheight= 5, face="bold")) + scale_y_continuous() +
  theme(axis.title.x=element_blank(), axis.text.x = element_text(size=8),
        panel.grid.major=element_blank(), panel.grid.minor=element_blank())
  
#   Fig_7<-ggplot(data=lg_chg_top,aes(x=CHG_CODE,y=COUNT,fill=CHG_CODE))+geom_bar(stat='identity',position='dodge')+ guides(fill=FALSE)+
#     theme(axis.text.x= element_text(angle=360, hjust=1))+ labs(x = 'Jenis perubahan penutupan lahan', y='Luas area (Ha)')+
#     theme( legend.title = element_text(size=8),legend.text = element_text(size = 6))

printArea <- function(x){
  format(x, digits=15, big.mark=",")
}

printRate <- function(x){
  format(x, digits=15, nsmall=2, decimal.mark=".", big.mark=",")
}

tabel_ket<-proj_descr
row.names(tabel_ket)<-NULL
tabel_ket$Type<-as.character(tabel_ket$Type)
colnames(tabel_ket)<-c("Tipe", "Keterangan")
tabel_ket[1,1]<-"Proyek"
tabel_ket[2,1]<-"Deskripsi"
tabel_ket[3,1]<-"Direktori"
tabel_ket[4,1]<-"Wilayah Analisis"
tabel_ket[5,1]<-"Provinsi"
tabel_ket[6,1]<-"Negara"

#WRITE REPORT
title1<-"{\\colortbl;\\red0\\green0\\blue0;\\red255\\green0\\blue0;\\red146\\green208\\blue80;\\red0\\green176\\blue240;\\red140\\green175\\blue71;\\red0\\green112\\blue192;\\red79\\green98\\blue40;} \\pard\\qr\\b\\fs70\\cf2 L\\cf3U\\cf4M\\cf5E\\cf6N\\cf7S \\cf1HASIL ANALISIS \\par\\b0\\fs20\\ql\\cf1"
title2<-paste("\\pard\\qr\\b\\fs40\\cf1 Modul PreQUES - Analisis Perubahan Tutupan/Penggunaan Lahan\\par\\b0\\fs20\\ql\\cf1", sep="")
sub_title<-"\\cf2\\b\\fs32 ANALISIS PERUBAHAN PENGGUNAAN LAHAN\\cf1\\b0\\fs20"
rad_grk<-"\\pard\\qr\\b\\fs40\\cf1 Dokumen RAD GRK - Bab 2.1. Profil dan Karakteristik Daerah \\par\\b0\\fs20\\ql\\cf1"
#date<-paste("Date : ", date, sep="")
time_start<-paste("Proses dimulai : ", time_start, sep="")
time_end<-paste("Proses selesai : ", eval(parse(text=(paste("Sys.time ()")))), sep="")
line<-paste("------------------------------------------------------------------------------------------------------------------------------------------------")
area_name_rep<-paste("\\b", "\\fs20", location, "\\b0","\\fs20")
I_O_period_1_rep<-paste("\\b","\\fs20", T1)
I_O_period_2_rep<-paste("\\b","\\fs20", T2)
chapter1<-"\\cf2\\b\\fs28 DATA YANG DIGUNAKAN \\cf1\\b0\\fs20"
chapter2<-"\\cf2\\b\\fs28 HASIL ANALISIS PADA TINGKAT BENTANG LAHAN \\cf1\\b0\\fs20"
chapter3<-"\\cf2\\b\\fs28 HASIL ANALISIS PADA TINGKAT UNIT PERENCANAAN \\cf1\\b0\\fs20"
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
addParagraph(rtffile, rad_grk)
addNewLine(rtffile)
addParagraph(rtffile, line)
addParagraph(rtffile, time_start)
addParagraph(rtffile, time_end)
addParagraph(rtffile, line)
addNewLine(rtffile)
width<-as.vector(c(1.34,3.1))
addTable(rtffile,tabel_ket,font.size=8,col.widths=width)
addPageBreak(rtffile)
addParagraph(rtffile, sub_title)
addNewLine(rtffile)
addParagraph(rtffile, line)
#addParagraph(rtffile, date)
addParagraph(rtffile, time_start)
addParagraph(rtffile, time_end)
addParagraph(rtffile, line)
addNewLine(rtffile)
addParagraph(rtffile, "Analisis perubahan tutupan lahan dilakukan untuk mengetahui kecenderungan perubahan tutupan lahan di suatu daerah pada satu kurun waktu. Analisa ini dilakukan dengan menggunakan data peta tutupan lahan pada dua periode waktu yang berbeda. Selain itu, dengan memasukkan data unit perencanaan kedalam proses analisis, dapat diketahui kecenderungan perubahan tutupan lahan pada masing-masing kelas unit perencanaan yang ada. Informasi yang dihasilkan melalui analisis ini dapat digunakan dalam proses perencanaan untuk berbagai hal, diantaranya menentukan prioritas pembangunan, mengetahui faktor pemicu perubahan penggunaan lahan, dan merencanakan skenario pembangunan di masa yang akan datang.")
addNewLine(rtffile)
addParagraph(rtffile, chapter1)
addParagraph(rtffile, line)
addNewLine(rtffile)
addParagraph(rtffile, "Data yang digunakan dalam analisis ini adalah data peta tutupan/penggunaan lahan dan data peta unit perencanaan yang bersumber dari peta penunjukan kawasan atau peta pola ruang RTRW. Data pendukung yang digunakan adalah peta acuan tipe penggunaan lahan dan data acuan kelas unit perencanaan.")
addNewLine(rtffile)
addParagraph(rtffile, paste("\\b \\fs20 Peta Tutupan Lahan Tahun ", T1," \\b0 \\fs20", sep=""))
addNewLine(rtffile)
addPlot(rtffile,plot.fun=print, width=6.7,height=4,res=150, plot.LU1)
addNewLine(rtffile)
addParagraph(rtffile, paste("\\b \\fs20 Peta Tutupan Lahan Tahun ", T2," \\b0 \\fs20", sep=""))
addNewLine(rtffile)
addPlot(rtffile,plot.fun=print, width=6.7,height=4,res=150, plot.LU2)
addNewLine(rtffile)
addParagraph(rtffile, paste("\\b \\fs20 Peta Unit Perencanaan\\b0 \\fs20", sep=""))
addNewLine(rtffile)
addPlot(rtffile,plot.fun=print, width=6.7,height=4,res=150, plot.Z)
addNewLine(rtffile)
addNewLine(rtffile)
addNewLine(rtffile)
addNewLine(rtffile)
addNewLine(rtffile)
addParagraph(rtffile, chapter2)
addParagraph(rtffile, line)
addNewLine(rtffile)
addParagraph(rtffile, "Pada bagian ini disajikan hasil analisis perubahan penggunaan lahan untuk keseluruhan bentang lahan yang dianalisis. Beberapa bentuk analisis yang dilakukan antara lain: perbandingan luasan tutupan lahan pada periode analisis dan tipe perubahan lahan dominan pada bentang lahan yang dianalisis")
addNewLine(rtffile)
addParagraph(rtffile, "Tabel Intisari Perubahan Tutupan Lahan menyajikan perbandingan luasan tipe-tipe tutupan lahan di sebuah bentang lahan pada kurun waktu tertentu. Kolom Overall Change menunjukkan perubahan luasan dalam satuan hektar. Notasi negatif pada kolom ini menunjukkan pengurangan luasan sebaliknya notasi positif menunjukkan penambahan luasan. Kolom Rate menunjukkan laju perubahan luasan tutupan lahan dalam satuan %/tahun. Kolom ini dihitung dengan mengurangi luasan pada t2-t1 kemudian dibagi dengan perkalian luasan pada t1 dan kurun waktu analisis")
addNewLine(rtffile)
addParagraph(rtffile, paste("\\b \\fs20 Intisari Perubahan Tutupan Lahan di\\b0 \\fs20 ", area_name_rep, I_O_period_1_rep, "\\b \\fs20 - \\b0 \\fs20", I_O_period_2_rep, sep=" "))
Ov_chg[3]<-printArea(Ov_chg[3])
Ov_chg[4]<-printArea(Ov_chg[4])
Ov_chg[5]<-printArea(Ov_chg[5])
addTable(rtffile,Ov_chg,font.size=8)
addNewLine(rtffile)
addPlot(rtffile,plot.fun=print, width=6.7,height=3,res=150,  ov.change.plot.2)
addNewLine(rtffile)
addParagraph(rtffile, paste("\\b \\fs20 Sepuluh Perubahan Penggunaan Lahan Dominan di\\b0 \\fs20 ", area_name_rep, I_O_period_1_rep, "\\b \\fs20 - \\b0 \\fs20", I_O_period_2_rep, sep=" "))
addNewLine(rtffile, n=1)
chg_only_top[3]<-printArea(chg_only_top[3])
addTable(rtffile, chg_only_top)
addNewLine(rtffile, n=1)
addPlot(rtffile,plot.fun=print, width=6.7,height=3,res=150,Largest.chg)
addNewLine(rtffile)
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
    lg_chg_zon<-lg_chg_zonal[which(lg_chg_zonal$ZONE == zonal.db$ID[i]),]
    lg_chg_zon$ZONE<-NULL
    lg_chg_plot<-lg_chg_zon
    colnames(lg_chg_zon)[3]<-"Luas (ha)"
    lg_chg_zon[3]<-printArea(lg_chg_zon[3])
    addTable(rtffile, lg_chg_zon)
    addNewLine(rtffile, n=1)
    #Largest Source of Change in Planning Unit Level
    Largest.chg.z<- ggplot(data=lg_chg_plot, aes(x=reorder(CHG_CODE, -COUNT),y=COUNT, fill=CHG_CODE))+geom_bar(stat='identity',position='dodge')+
      geom_text(data=lg_chg_plot, aes(x=CHG_CODE, y=COUNT, label=round(COUNT, 1)),size=3, vjust=0.1) +
      ggtitle(paste("10 Perubahan Dominan pada Unit Perencanaan",i, "-", zonal.db$Z_CODE[i] )) + guides(fill=FALSE) +
      labs(x = 'Jenis perubahan penutupan lahan', y='Luas area (Ha)') + guides(fill=FALSE)+
      theme(axis.text.x= element_text(angle=45,hjust=1))+
      theme(plot.title = element_text(lineheight= 5, face="bold")) + scale_y_continuous() +
      theme(axis.title.x=element_blank(), axis.text.x = element_text(size=8),
            panel.grid.major=element_blank(), panel.grid.minor=element_blank())
    
    addPlot.RTF(rtffile, plot.fun=plot, width=6.7, height=3, res=150, Largest.chg.z )
    addNewLine(rtffile, n=1)
  },error=function(e){cat("Nice try pal! ~ please re-check your input data :",conditionMessage(e), "\n"); addParagraph(rtffile, "no data");addNewLine(rtffile)})
}
addNewLine(rtffile)
done(rtffile)

command<-paste("start ", "winword ", result_dir, "/LUMENS_Pre-QUES_change_report.lpr", sep="" )
shell(command)
