##[PUR]=group
##Select_planning_unit_file=vector
##Select_planning_unit_attribute_field=field Select_planning_unit_file
##Planning_unit_title=string
##Planning_unit_type=selection reconciliation; additional

library(foreign)

pu_data<-Select_planning_unit_file
Field<-Select_planning_unit_attribute_field
data_name<-Planning_unit_title
Type<-Planning_unit_type

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
tempEnv<-new.env()
load(proj.file, tempEnv)
wd_user<-paste(log.file[1,1], "/", log.file[1,2],"/PUR/PUR_analysis_",tempEnv$PUR.index, sep="")
rm(tempEnv)

if (Type==0) {
Type<-"PU"
} else {
Type<-"ADD"
}
#wd_ref<-dirname(as.character(pu_data))
#setwd(wd_ref)
#st_area_file<- substr(basename(pu_data), 1, nchar(basename(pu_data)) - 4)
#sa<-readOGR(dsn=wd_ref,st_area_file)
sa<-pu_data
wd_usertemp<-paste(wd_user,"/temp", sep="")
writeOGR(sa, dsn=wd_usertemp, data_name, driver="ESRI Shapefile")

setwd(wd_user)
#acuan_kelas<-c("Conservation", "Production", "Other")
#acuan_kode<-c(10,20,30)
#tabel_acuan<-cbind(acuan_kelas,acuan_kode)
#tabel_acuan<-edit(tabel_acuan)
tabel_acuan<-paste(wd_user,"/hirarki_rekonsiliasi.csv", sep="")
tabel_acuan<-read.csv(tabel_acuan, header = TRUE, sep = ",")
tabel_acuan[1]<-NULL
#pu_data_attr<-substr(pu_data,1,nchar(pu_data)-3)
pu_data_attr<-paste(wd_usertemp,"/",data_name,".dbf", sep="")
pu_data_attr<-read.dbf(pu_data_attr)
pu_data_attr_unique<-paste("unique(pu_data_attr$",Field, ")", sep="")
pu_data_attr_unique<-eval(parse(text=pu_data_attr_unique))
pu_data_attr_unique<-as.data.frame(pu_data_attr_unique)
countrow<-nrow(pu_data_attr_unique)
pu_data_attr_unique$IDS<-seq(countrow)
colnames(pu_data_attr_unique)[1]<-Field

if (Type=="PU") {
countrow_ref<-nrow(tabel_acuan)
if (countrow<countrow_ref) {
x_pre<-c(rep("---", times=(countrow_ref-countrow)))
x_pre2<-x_pre
x_pre3<-cbind(x_pre, x_pre2)
colnames(x_pre3)[1]<-Field
colnames(x_pre3)[2]<-"IDS"
pu_data_attr_unique_rev<-rbind(pu_data_attr_unique,x_pre3)
x_pre4<-rep("---", times=countrow_ref)
x_pre5<-x_pre4
x_pre6<-cbind(x_pre4, x_pre5)
colnames(x_pre6)[1]<-"--"
colnames(x_pre6)[2]<-"---"
tabel_acuan2<-tabel_acuan
tabel_acuan2<-cbind(tabel_acuan2, x_pre6)
pu_data_attr_unique_rev<-cbind(pu_data_attr_unique_rev,x_pre6)
pu_data_attr_unique_rev<-cbind(pu_data_attr_unique_rev,tabel_acuan2)
pu_data_attr_unique_rev<-edit(pu_data_attr_unique_rev)
} else {
x_pre<-rep("---", times=countrow-(nrow(tabel_acuan)))
x_pre2<-x_pre
x_pre3<-cbind(x_pre,x_pre2)
x_pre4<-rep("---", times=1)
colnames(x_pre3)[1]<-"acuan_kelas"
colnames(x_pre3)[2]<-"acuan_kode"
tabel_acuan2<-rbind(tabel_acuan,x_pre3)
tabel_acuan2<-cbind(x_pre4, x_pre4, tabel_acuan2)
colnames(tabel_acuan2)[1]<-"---"
colnames(tabel_acuan2)[2]<-"--"
pu_data_attr_unique<-cbind(pu_data_attr_unique,tabel_acuan2)
pu_data_attr_unique_rev<-edit(pu_data_attr_unique)
}
test3<-merge(pu_data_attr,pu_data_attr_unique_rev, by=Field)
test3<-test3[,c(Field,"IDS")]
target<-paste(wd_usertemp,"/",data_name,".dbf", sep="")
write.dbf(test3,target)
csv_pu<-paste(wd_usertemp,"/",data_name, "_PUR.csv", sep="")
write.csv(test3, csv_pu)
} else {
test3<-pu_data_attr_unique
target<-paste(wd_usertemp,"/",data_name,".dbf", sep="")
write.dbf(test3,target)
csv_pu<-paste(wd_usertemp,"/",data_name, "_PURADD.csv", sep="")
write.csv(test3, csv_pu)
}

shp_dir<-paste(wd_usertemp,"/", data_name, ".shp", sep="")
file_out<-paste(wd_usertemp,"/",data_name, "-", Type,  ".tif", sep="")
kolom_data<-paste('IDS')
opsi<-1
res<-100
if (file.exists("C:/Program Files (x86)/LUMENS/bin/gdal_rasterize.exe")){
gdalraster = "C:/Progra~2/LUMENS/bin/gdal_rasterize.exe "
} else{
gdalraster = "C:/Progra~1/LUMENS/bin/gdal_rasterize.exe "
}
osgeo_comm<-paste(gdalraster,shp_dir, file_out,"-a",kolom_data, "-tr", res, res, "-a_nodata 255 -ot Byte", sep=" ")
system(osgeo_comm)
