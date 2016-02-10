##[PUR]=group
##ref_data=vector
##Field=field ref_data
##data_name=string

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
PUR.index=tempEnv$PUR.index+1
tempEnv$resave(PUR.index, file=proj.file)
wd_user<-paste(log.file[1,1], "/", log.file[1,2],"/PUR/PUR_analysis_",PUR.index, sep="")
rm(tempEnv)

#wd_ref<-dirname(as.character(ref_data))
#setwd(wd_ref)
#st_area_file<- substr(basename(ref_data), 1, nchar(basename(ref_data)) - 4)
#sa<-readOGR(dsn=wd_ref,st_area_file)
sa<-ref_data
sa<-subset(sa, select=Field)

dir.create(wd_user, mode="0777")
setwd(wd_user)
acuan_kelas<-c("Conservation", "Production", "Other")
acuan_kode<-c(10,20,30)
tabel_acuan<-cbind(acuan_kelas,acuan_kode)
tabel_acuan<-edit(tabel_acuan)
#ref_data_attr<-substr(ref_data,1,nchar(ref_data)-3)
#ref_data_attr<-paste(ref_data_attr,"dbf", sep="")
ref_data_attr<-as.data.frame(sa)
ref_data_attr_unique<-paste("unique(ref_data_attr$",Field, ")", sep="")
ref_data_attr_unique<-eval(parse(text=ref_data_attr_unique))
ref_data_attr_unique<-as.data.frame(ref_data_attr_unique)
countrow<-nrow(ref_data_attr_unique)
ref_data_attr_unique.x<-ref_data_attr_unique
ref_data_attr_unique.x$IDO<-seq(countrow)
ref_data_attr_unique$IDS<-seq(countrow)
colnames(ref_data_attr_unique)[1]<-Field
colnames(ref_data_attr_unique.x)[1]<-Field
x<-rep("---", times=countrow)
xx<-x
ref_data_attr_unique<-cbind(ref_data_attr_unique,x,xx)
xxx<-rep("---", times=countrow-(nrow(tabel_acuan)))
xxxx<-xxx
V<-cbind(xxx,xxxx)
tabel_acuan2<-rbind(tabel_acuan,V)
tabel_acuan2<-cbind(tabel_acuan2, x,xx)
ref_data_attr_unique<-cbind(ref_data_attr_unique,tabel_acuan2)
ref_data_attr_unique_rev<-edit(ref_data_attr_unique)
ref_data_attr_unique_rev<-ref_data_attr_unique_rev[,c(Field,"IDS")]
ref_data_attr_unique_rev<-merge(ref_data_attr_unique.x, ref_data_attr_unique_rev, by=Field)

test3<-merge(sa,ref_data_attr_unique_rev, by=Field)
#test4<-
#target<-paste(wd_usertemp,"/",data_name,".dbf", sep="")
target2<-paste(wd_user,"/PURREF_",data_name, ".csv", sep="")
#write.dbf(test3,target)
#test3<-test3[,c(Field,"IDO","IDS")]
write.csv(test3, target2)
write.csv(tabel_acuan, "hirarki_rekonsiliasi.csv")
wd_usertemp<-paste(wd_user,"/temp", sep="")
writeOGR(test3, dsn=wd_usertemp, data_name, driver="ESRI Shapefile")

shp_dir<-paste(wd_usertemp,"/", data_name, ".shp", sep="")
file_out<-paste(wd_user,"/", "PURREF_" ,data_name, ".tif", sep="")
kolom_data<-paste('IDO')
opsi<-1
res<-100
if (file.exists("C:/Program Files (x86)/LUMENS/bin/gdal_rasterize.exe")){
gdalraster = "C:/Progra~2/LUMENS/bin/gdal_rasterize.exe "
} else{
gdalraster = "C:/Progra~1/LUMENS/bin/gdal_rasterize.exe "
}
# "gdalraster shp_dir file_out -a kolom_data -tr 100 100 -a_nodata 255 -ot Byte" (-a_nodata 65535 <- default to 64Float)
osgeo_comm<-paste(gdalraster, shp_dir, file_out, "-a", kolom_data, "-tr", res, res, "-a_nodata 255 -ot Byte", sep=" ")
system(osgeo_comm)
