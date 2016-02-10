##[LUMENS]=group
##data=raster
##Description=string
##passfilenames

#data="C:/RAD-GRK_2015/Data_Pack/Kalimantan_Tengah/Data_Spasial/Raster/Peta_Gambut_Kalteng.tif"
Peat_value=1

library(spatial.tools)

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
lumens_database<-proj.file

#IMPORTING DATA INTO LUMENS DATABASE
command="raster"
peat.index<-1
peat.value<-Peat_value
data_name<-paste("Peat",sep="")
eval(parse(text=(paste(data_name,"_", peat.index, "<-", command,'("', data, '")', sep=""))))
eval(parse(text=(paste(data_name,"_", peat.index, "<-spatial_sync_raster(",data_name,"_", peat.index, ',', 'ref, method = "ngb")', sep=""))))
eval(parse(text=(paste(data_name,"_", peat.index, "<-", data_name,"_", peat.index, "*1",  sep=""))))
eval(parse(text=(paste("names(",data_name,"_", peat.index, ")<-Description", sep=""))))
eval(parse(text=(paste("resave(", data_name,"_", peat.index, ",peat.index, file=lumens_database)", sep=""))))

gc()