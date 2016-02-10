##[LUMENS]=group
##new_working_directory=folder
##LUMENS_project_file=file

load(LUMENS_project_file)

#READ LUMENS LOG FILE
LUMENS_log <- as.data.frame(Sys.info())
OS <- substr(as.character(LUMENS_log[2,1]), 1, 2)
username <- as.character(LUMENS_log[6,1])
if(OS == "XP") {
  user_path<-paste("C:/Documents and Settings/All Users", sep="")
} else {
  user_path<-paste("C:/Users/Public", sep="")
}
LUMENS_path_user <- paste(user_path,"/LUMENS", sep="")
dir.create(LUMENS_path_user, mode="0777")

#MODIFY LOG FILE
proj_descr$Description<-as.character(proj_descr$Description)
sink(paste(LUMENS_path_user, "/LUMENS.log", sep=""))
time_start<-paste(eval(parse(text=(paste("Sys.time ()")))), sep="")
cat(new_working_directory, proj_descr[1,2], time_start, sep=",")
sink()

#MODIFY VALUE
proj_descr[3,2] <- new_working_directory

#CREATE DIRECTORY IN NEW WORKING DIRECTORY
setwd(new_working_directory)
LUMENS_path <- paste(new_working_directory, "/", proj_descr[1,2], sep="")
PUR_path <- paste(LUMENS_path, "/PUR", sep="")
QUES_path <- paste(LUMENS_path, "/QUES", sep="")
PreQUES_path <- paste(QUES_path, "/PreQUES", sep="")
QUESC_path <- paste(QUES_path, "/QUES-C", sep="")
QUESB_path <- paste(QUES_path, "/QUES-B", sep="")
QUESH_path <- paste(QUES_path, "/QUES-H", sep="")
TA_path <- paste(LUMENS_path, "/TA", sep="")
SCIENDO_path  <- paste(LUMENS_path, "/SCIENDO", sep="")

dir.create(LUMENS_path, mode="0777")
dir.create(PUR_path, mode="0777")
dir.create(QUES_path, mode="0777")
dir.create(PreQUES_path, mode="0777")
dir.create(QUESC_path, mode="0777")
dir.create(QUESB_path, mode="0777")
dir.create(QUESH_path, mode="0777")
dir.create(TA_path, mode="0777")
dir.create(SCIENDO_path, mode="0777")

#SAVE LPJ TO NEW WORKING DIRECTORY
setwd(LUMENS_path)
remove(username, user_path, time_start, new_working_directory, OS, LUMENS_path, LUMENS_project_file, LUMENS_log)
remove(PUR_path, QUES_path, PreQUES_path, QUESC_path, QUESB_path, QUESH_path, TA_path, SCIENDO_path)
save.image(file=paste(proj_descr[1,2],".lpj", sep=""))

