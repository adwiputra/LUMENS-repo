##[LUMENS]=group
##passfilenames
##data=file
##description=string

library(tiff)
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
lumens_database<-paste(log.file[1,2], ".lpj", sep="")

#READ PROJECT DESCRIPTION FILE AND IDENTIFY RDATA
working_directory<-paste(proj_descr[3,2], "/", proj_descr[1,2], sep="")
setwd(working_directory)

#IMPORTING DATA INTO LUMENS DATABASE
SCIENDO1.index<-SCIENDO1.index+1
command="raster"
data_name<-"factor"
eval(parse(text=(paste(data_name,"_", SCIENDO1.index, "<-", command,'("', data, '")', sep=""))))
eval(parse(text=(paste(data_name,"_", SCIENDO1.index, "<-spatial_sync_raster(",data_name,"_", SCIENDO1.index, ',', 'ref, method = "ngb")', sep=""))))
eval(parse(text=(paste(data_name,"_", SCIENDO1.index, "<-", data_name,"_", SCIENDO1.index, "*1",  sep=""))))
eval(parse(text=(paste("factor_desc", SCIENDO1.index, "<-description", sep=""))))
data_name<-paste(data_name,"_",SCIENDO1.index, sep="")
data_description_name<-paste("factor_desc",SCIENDO1.index, sep="" )
eval(parse(text=(paste("resave(", data_name, ",", data_description_name, ",SCIENDO1.index, file=lumens_database)", sep=""))))


#CLEAN ENVIRONMENT
rm(list=ls(all.names=TRUE))

gc()