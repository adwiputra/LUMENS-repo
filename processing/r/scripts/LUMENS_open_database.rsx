##[LUMENS]=group
##project_file=file
##overview=output raster
##passfilenames

load(project_file)

#Reopen LUMENS.log
LUMENS_log <- as.data.frame(Sys.info())
OS <- substr(as.character(LUMENS_log[2,1]), 1, 2)
username <- as.character(LUMENS_log[6,1])
if(OS == "XP") {
  user_path<-paste("C:/Documents and Settings/All Users", sep="")
} else {
  user_path<-paste("C:/Users/Public", sep="")
}
LUMENS_path_user <- paste(user_path,"/LUMENS", sep="")
sink(paste(LUMENS_path_user, "/LUMENS.log", sep=""))
working_directory<-paste(proj_descr[3,2], sep="")
project<-as.character(proj_descr[1,2])
time_start<-paste(eval(parse(text=(paste("Sys.time ()")))), sep="")
cat(working_directory, project, time_start, sep=",")
sink()
overview<-ref