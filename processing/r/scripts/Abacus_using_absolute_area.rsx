##[SCIENDO]=group
##Historical_Baseline_Car = file

LUMENS_log <- as.data.frame(Sys.info())
OS <- substr(as.character(LUMENS_log[2,1]), 1, 2)
username <- as.character(LUMENS_log[6,1])
if(OS == "XP") {
  user_path<-paste("C:/Documents and Settings/All Users", sep="")
} else {
  user_path<-paste("C:/Users/Public", sep="")
}
LUMENS_log_dir <- paste(user_path,"/LUMENS", sep="")

if (file.exists("C:/Program Files (x86)/LUMENS/Abacus2")){
abacusExecutable = "C:/Progra~2/LUMENS/Abacus2/abacus2 "
} else{
abacusExecutable = "C:/Progra~1/LUMENS/Abacus2/abacus2 "
}

systemCommand <- paste(abacusExecutable, Historical_Baseline_Car, "-ref LUMENS -wd", LUMENS_log_dir)

system(systemCommand)
