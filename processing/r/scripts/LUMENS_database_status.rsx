##[LUMENS]=group
##database_status=output table

library(tcltk)

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
tryCatch({
log.file<-read.table(LUMENS_path_user, header=FALSE, sep=",")
},error=function(e){
msgBox <- tkmessageBox(title = "Database",
message = "No active project in LUMENS. Create new project first",
icon = "info",
type = "ok")
quit()
})

#FIND EXISTING .lpj FILE
tryCatch({
proj.file<-paste(log.file[1,1], "/", log.file[1,2],"/",log.file[1,2], ".lpj", sep="")
},error=function(e){
msgBox <- tkmessageBox(title = "Database",
message = "No LUMENS project file found. Create new project first",
icon = "info",
type = "ok")
quit()
})

#LOAD .lpj FILE
tryCatch({
load(proj.file)
},error=function(e){
msgBox <- tkmessageBox(title = "Database",
message = "Status LUMENS database can't be loaded. Please open/import .lpj file",
icon = "info",
type = "ok")
quit()
})

numberOfObject<-length(ls(all.names=T))

#CREATE FUNCTION
.ls.objects <- function (pos = 1, pattern, order.by,
decreasing=FALSE, head=FALSE, n=5) {
napply <- function(names, fn) sapply(names, function(x)
fn(get(x, pos = pos)))
names <- ls(pos = pos, pattern = pattern)
obj.class <- napply(names, function(x) as.character(class(x))[1])
obj.mode <- napply(names, mode)
obj.type <- ifelse(is.na(obj.class), obj.mode, obj.class)
obj.size <- napply(names, object.size)
obj.dim <- t(napply(names, function(x)
as.numeric(dim(x))[1:2]))
vec <- is.na(obj.dim)[, 1] & (obj.type != "function")
obj.dim[vec, 1] <- napply(names, length)[vec]
out <- data.frame(obj.type, obj.size, obj.dim)
names(out) <- c("Type", "Size", "Rows", "Columns")
if (!missing(order.by))
out <- out[order(out[[order.by]], decreasing=decreasing), ]
if (head)
out <- head(out, n)
out
}
lsos <- function(..., n=10) {
.ls.objects(..., order.by="Size", decreasing=TRUE, head=TRUE, n=n)
}

listOfData<-lsos(pos = environment(), n=numberOfObject)
listOfData$var_name<-row.names(listOfData)
row.names(listOfData)<-NULL
setwd(paste(log.file[1,1], "/", log.file[1,2],sep=""))


#description
status_country<-c("country", eval(parse(text=(paste("country")))) )
status_province<-c("province", eval(parse(text=(paste("province")))) )
status_location<-c("location", eval(parse(text=(paste("location")))) )
database_status<-as.data.frame(rbind(status_country, status_location, status_province))
database_status$V1<-as.character(factor(database_status$V1))
database_status$V2<-as.character(factor(database_status$V2))

#all maps
#check Landuse_ var
row.names(database_status)<-NULL
status_landuse<-as.data.frame(ls(pattern="freq"))
n<-nrow(status_landuse)
if(n!=0){
data.y<-NULL
for (q in 1:n) {
data.x<-substr(as.character(factor(status_landuse[q,1])), 5, 14)
data.z<-names(eval(parse(text=(paste(data.x, sep="")))))
if(q==1){
data.y<-data.frame(data.x,data.z)
} else {
data_temp<-data.frame(data.x,data.z)
data.y<-rbind(data.y,data_temp)
}
}
colnames(data.y)[1]="V1"
colnames(data.y)[2]="V2"
status_landuse<-as.data.frame(data.y)
database_status<-rbind(database_status, status_landuse)
}

#check Peat_ var
status_peat<-as.data.frame(ls(pattern="Peat"))
n<-nrow(status_peat)
if(n!=0){
  data.y<-NULL
  for (q in 1:n) {
    data.x<-as.character(factor(status_peat[q,1]))
    data.z<-names(eval(parse(text=(paste(data.x, sep="")))))
    if(q==1){
      data.y<-data.frame(data.x,data.z)
    } else {
      data_temp<-data.frame(data.x,data.z)
      data.y<-rbind(data.y,data_temp)
    }
  }
  colnames(data.y)[1]="V1"
  colnames(data.y)[2]="V2"
  status_peat<-as.data.frame(data.y)
  database_status<-rbind(database_status, status_peat)
}

#check pu_ var
status_pu<-as.data.frame(as.character(ls(pattern="pu_pu")))
n<-nrow(status_pu)
if (n!=0) {
data.y<-NULL
for (q in 1:n) {
data.x<-as.character(factor(status_pu[q,]))
data.z<-names(eval(parse(text=(paste(data.x, sep="")))))
if(q==1){
data.y<-data.frame(data.x,data.z)
} else {
data_temp<-data.frame(data.x,data.z)
data.y<-rbind(data.y,data_temp)
}
}
colnames(data.y)[1]="V1"
colnames(data.y)[2]="V2"
status_pu<-as.data.frame(data.y)
database_status<-rbind(database_status, status_pu)
} else {
status_pu<-c("ref", eval(parse(text=(paste("names(ref)")))) )
database_status<-rbind(database_status, status_pu)
}

#index
status_PUR.index<-c("PUR.index", eval(parse(text=(paste("PUR.index")))) )
status_PreQUES.index<-c("PreQUES.index", eval(parse(text=(paste("PreQUES.index")))) )
status_QUESB.index<-c("QUESB.index", eval(parse(text=(paste("QUESB.index")))) )
status_QUESC.index<-c("QUESC.index", eval(parse(text=(paste("QUESC.index")))) )
status_QUESH.index<-c("QUESH.index", eval(parse(text=(paste("QUESH.index")))) )
status_SCIENDO1.index<-c("SCIENDO1.index", eval(parse(text=(paste("SCIENDO1.index")))) )
status_SCIENDO2.index<-c("SCIENDO2.index", eval(parse(text=(paste("SCIENDO2.index")))) )
status_TA1.index<-c("TA1.index", eval(parse(text=(paste("TA1.index")))) )
status_TA2.index<-c("TA2.index", eval(parse(text=(paste("TA2.index")))) )
status_admin.index<-c("admin.index", eval(parse(text=(paste("admin.index")))) )
status_landuse.index<-c("landuse.index", eval(parse(text=(paste("landuse.index")))) )
status_lut_carbon.index<-c("lut_carbon.index", eval(parse(text=(paste("lut_carbon.index")))) )
status_lut_landuse.index<-c("lut_landuse.index", eval(parse(text=(paste("lut_landuse.index")))) )
status_lut_zone.index<-c("lut_zone.index", eval(parse(text=(paste("lut_zone.index")))) )
database_status<-rbind(database_status, status_PUR.index, status_PreQUES.index,
status_QUESB.index, status_QUESC.index, status_QUESH.index,
status_SCIENDO1.index, status_SCIENDO2.index, status_TA1.index,
status_TA2.index, status_admin.index, status_landuse.index,
status_lut_carbon.index, status_lut_landuse.index, status_lut_zone.index)

#check period
status_period<-as.data.frame(as.character(ls(pattern="period")))
n<-nrow(status_period)
if (n!=0) {
data.y<-NULL
for (q in 1:n) {
data.x<-as.character(factor(status_period[q,]))
data.z<-eval(parse(text=(paste(data.x, sep=""))))
if(q==1){
data.y<-data.frame(data.x,data.z)
} else {
data_temp<-data.frame(data.x,data.z)
data.y<-rbind(data.y,data_temp)
}
}
colnames(data.y)[1]="V1"
colnames(data.y)[2]="V2"
status_period<-as.data.frame(data.y)
database_status<-rbind(database_status, status_period)
}
row.names(database_status)<-NULL
colnames(database_status)[1]="Data"
colnames(database_status)[2]="Value"

gc()