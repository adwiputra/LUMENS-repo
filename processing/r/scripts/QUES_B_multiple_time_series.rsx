##[QUES]=group
##Land_cover_lookup_table=file
##gridres=number 10000
##windowsize=number 1000
##window.shape= number 0
##raster.nodata= number 0
##classdesc=file
##edgecon=file
##zone_lookup=file
##ref.map.id= number 1
##mwfile.init=output raster
##mwfile.final=output raster
##habitat.loss.NA=output raster
##habitat.degradation=output raster
##habitat.gain.NA=output raster
##habitat.recovery=output raster
##passfilenames


#==== packages ====
library(DBI)
library(raster)
library(RSQLite)
library(SDMTools)
library(sp)
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
library(dplyr)
library(tcltk)
library(gridExtra)
library(pracma)
library(rgeos)

#====A1 READ LUMENS LOG FILE====
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

result_dir<-paste(dirname(proj.file),"/QUES/QUES-B/", sep="")
setwd(result_dir)

Look_up_table<-Land_cover_lookup_table
raster.nodata

time_start<-paste(eval(parse(text=(paste("Sys.time ()")))), sep="")
lut.lc<-read.table(Look_up_table, header=TRUE, sep=",")

#===A2 Check LUMENS QuES-B log file====
if (file.exists(paste(user_path,"/LUMENS/LUMENS_quesb.log", sep=""))) {
  log.quesb<-read.table(paste(user_path,"/LUMENS/LUMENS_quesb.log", sep=""), sep=",", header=T, row.names=1)
  print("LUMENS Pre-QuES log file is available")
} else {
  log.quesb<-data.frame(IDX=NA, 
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
                        GRIDRES=NA,
                        WINDOWSIZE=NA,
                        WINDOWSHAPE=NA,
                        CLASSDESC=NA,
                        EDGECON=NA,
                        OUTPUT_FOLDER=NA, row.names=NULL)
}

#====B READ LANDUSE DATA FROM LUMENS DATABASE====
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
    command2<-paste(command2,"landuse_tt", i, ",", sep="")
  } else {
    command1<-paste(command1,"period", i, sep="")
    command2<-paste(command2,"landuse_tt", i, sep="")
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
    command4<-paste(command4,"freqlanduse_t", i, ",", sep="")
  } else {
    command4<-paste(command4,"freqlanduse_t", i, sep="")
  }
}
#command 2 & command 4 buat apa ya?
#end create command

#====C SELECT DATA TO BE ANALYZED====
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

#====D SELECT PLANNING UNIT TO BE ANALYZED====
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

#====E Calculate landscape area====
eval(parse(text=(paste("totarea<-max(sum((freq",data[1,1],")[2]),sum((freq",data[2,1],")[2]))",sep=""))))

#====F Projection Handling====
for(j in 1:n) {
  input <- as.character(data[j,1])
  eval(parse(text=(paste(input,"[",input, "==", raster.nodata, "]<-NA", sep=""))))
  command1<-paste(command1,input, ",", sep="")
}

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

#====G Raster Nodata Handling====
#Raster nodata check 
nodata.cek<-0
for (i in 1: nrow(data3)){
  raster.cek<-paste(getwd(),"/landuse_tNA_", data3[i,2],".tif", sep='')
  eval(parse(text=(paste("if( file.exists('",raster.cek ,"')){print('landuse_tNA_", data3[i,2],".tif is available'); nodata.cek<-nodata.cek+1}", sep=""))))
}
#RUN NODATA SYNC FOR ALL RASTER FILES IF NOT AVAILABLE YET IN QUES-B DIRECTORY
if (nodata.cek<nrow(data3)){
  #Create temporary boolean raster value
  for(i in 1: nrow(data3)){
    eval(parse(text=(paste("landuse_t",i,"<- reclassify(",data3[i,1],",cbind(raster.nodata,NA))", sep=''))))
    eval(parse(text=(paste("Landuse_t",i,"_temp<-landuse_t",i,">=0", sep=''))))
    
  }
  #No data check raster
  lu.nodata.check<-Landuse_t1_temp
  for(i in 2:nrow(data3)){
    eval(parse(text=(paste("lu.nodata.check<-lu.nodata.check*Landuse_t",i,"_temp", sep=''))))
  }
  
  #syncronized nodata raster map
  for(i in 1:nrow(data3)){
    eval(parse(text=(paste("landuse_t",i,"<-lu.nodata.check*landuse_t",i, sep=''))))
    lu_path<-paste(getwd(),"/landuse_tNA_", data3[i,2],".tif", sep="")
    eval(parse(text=(paste("writeRaster(landuse_t",i,",  filename=",'basename(lu_path)',", format='GTiff', overwrite=TRUE, NAflag=255)", sep=''))))
  }
} else {print ("landuse_tNA files are ready")
  for(i in 1: nrow(data3)){
    eval(parse(text=(paste("landuse_t",i,"<- reclassify(",data3[i,1],",cbind(raster.nodata,NA))", sep=''))))
    eval(parse(text=(paste("Landuse_t",i,"_temp<-landuse_t",i,">=0", sep=''))))
    
  }
  #No data check raster
  lu.nodata.check<-Landuse_t1_temp
  for(i in 2:nrow(data3)){
    eval(parse(text=(paste("lu.nodata.check<-lu.nodata.check*Landuse_t",i,"_temp", sep=''))))
  }
}

#====H Define WD, Output folder, raster lu1, lu2, pu====

command1<-paste(command1,pu, sep="")
eval(parse(text=(paste("pu_name<-names(",pu[1],")", sep='')))) ; #selected planning unit given name

QUESB.index<-QUESB.index+1
quesb_folder<-paste("QuES-B_analysis_",pu_name,"_" ,T1,"_",T2,"_",QUESB.index,sep="")
dir.create(quesb_folder)

result_dir2<-paste(result_dir, quesb_folder, sep='')
result_dir3<-paste(dirname(proj.file),"/QUES/", sep="")

#landuse path
lu1_path<-paste(getwd(),"/landuse_tNA_", data[1,2],".tif", sep="")
lu2_path<-paste(getwd(),"/landuse_tNA_", data[2,2],".tif", sep="")
lu1<-eval(parse(text=(paste(data[1,1], sep=''))))
lu2<-eval(parse(text=(paste(data[2,1], sep=''))))
zone<-eval(parse(text=(paste(pu[1], sep=''))))

#====I Sampling grid raster preparation====
generate_sampling_grid<-function(landuse, gridresolution){
  xl1<-xmin(landuse)
  yl1<-ymin(landuse)
  xu1<-xmax(landuse)
  yu1<-ymax(landuse)
  pjg<-xu1-xl1
  lbr<-yu1-yl1
  ncellx<-pjg/gridres
  ncelly<-lbr/gridres
  ncellx<-ceiling(ncellx)
  ncelly<-ceiling(ncelly)
  
  newproj<-proj4string(landuse)
  raster.grid<-raster(xmn=xl1, xmx=xu1, ymn=yl1, ymx=yu1, ncol=ncellx, nrow=ncelly, crs=newproj)
  res(raster.grid)<-gridres
  vals <- 1:ncell(raster.grid)
  raster.grid<-setValues(raster.grid,vals)
  return (raster.grid)
}
#Initial raster sampling grid
sampling.grid.rast<-generate_sampling_grid(lu1, gridres)

#Resample sampling grid with landuse raster specification
sampling.grid.rast.resampled<-resample(sampling.grid.rast,lu1, method="ngb"); #sampling grid raster file

#Generate Polygon sampling
polygrid<-rasterToPolygons(sampling.grid.rast, n=4, na.rm=FALSE, digits=12, dissolve=TRUE)

#Generate Centroid
centro<- gCentroid(polygrid,byid=TRUE)

#====J DEFINING FOCAL AREA====
#modify biodiversity lookup table
lookup_bh<- read.table(classdesc, header=TRUE, sep=",", stringsAsFactors=FALSE)
lookup_z<- read.table(zone_lookup, header=TRUE, sep=",")
colnames(lookup_z)[1]<-c("ZONE")
lookup_bh$Enabled[lookup_bh$Enabled==TRUE]<-1
lookup_bh$Enabled[lookup_bh$Enabled==FALSE]<-0
lookup_bh[4]<-NULL
colnames(lookup_bh)<-c("ID", "Name", "BIODIV")


#Focal Area Function
focal.area<-function(landuse.character){
  eval(parse(text=paste("colnames(freq",landuse.character,")[1]<-'ID'", sep='')))
  eval(parse(text=paste("foc.area.reclass<-merge(freq",landuse.character,",lookup_bh,by='ID')", sep='')))
  foc.area.reclass$count<-NULL
  foc.area.reclass$Name<-NULL
  eval(parse(text=paste("foc.area<-reclassify(",landuse.character, ",foc.area.reclass)", sep='')))
  return (foc.area)
}

#Initial focal area
foc.area.init<-focal.area(data[1,1])
#Final focal area
foc.area.final<-focal.area(data[2,1])


#Total Focal area fraction function
foc.area.grid.sampled<-function(foc.area, sampling.grid){
  tothab<-zonal(foc.area, sampling.grid, 'sum')
  tothab<-as.data.frame(tothab)
  colnames(tothab) <- c("ID", "sum")
  tothab$sum<-((tothab$sum/totarea)*100)
  return(tothab)}

#Initial focal area fraction inside sampling grid
tothab.init<-foc.area.grid.sampled(foc.area.init, sampling.grid.rast.resampled)

#Final focal area fraction inside sampling grid
tothab.final<-foc.area.grid.sampled(foc.area.final, sampling.grid.rast.resampled)

#====K Fragstats .fca Preparation and Execution====
#Prepare fca file for processing tif 
teci.analysis<-function(landuse, lu_path){
  modid=1
  internal<-paste('')
  cpf<-paste('')
  io<-paste('[BAND:1]')
  desc<-paste('')
  drlib<-paste('GDAL')
  drname<-paste('GeoTIFF grid (.tif)')
  drid<-paste('63B45E15-C8E5-44f6-A9AB-60E1852CDB5D')
  
  #extent input of file 1
  xl1<-xmin(landuse)
  yl1<-ymin(landuse)
  xu1<-xmax(landuse)
  yu1<-ymax(landuse)
  
  #cell size input of file 1
  csize1<-xres(landuse)
  #row and column size input of file 1
  rowc1<-nrow(landuse)
  colc1<-ncol(landuse)
  
  
  aczero="1"
  #no data value input
  nodata=255
  bvalue=999
  
  #common tables input
  contab<-read.table(file=edgecon, header=TRUE, sep=',', skip=1)
  contab2<-round(contab, digits=2)
  
  #raster file directory for landuse
  #dirname_raster<-dirname(paste(getwd(),"/landuse_tNA_", data[1,2],".tif", sep=''))
  #setwd(dirname_raster)
  
  #Clean previous teci process
  for (i in 1:3){
    mwout<-paste(lu_path,'_mw',i, sep='')
    teci.dir<-paste(mwout,"/",list.files(mwout), sep='')
    if (file.exists(teci.dir)==TRUE){
      mwout2<-paste(lu_path,'_mw',i, sep='')
      unlink(mwout2, recursive=TRUE)
      print(paste(i,"deleting previous raster file found, algorithm continue..."))
    }else{
      print(paste(i,"no previous raster file found, algorithm continue..."))
    }
  }
  
  #Check .FCA model
  LUMENS_path_user <- paste(user_path,"/LUMENS/", sep="")
  if (file.exists(paste(LUMENS_path_user,'/teciuf.fca',sep=''))){
    fca<-paste(LUMENS_path_user,'/teciuf.fca',sep='')
    print("Fragstats' model found!")
  } else { stop("Fragstats model file is not found, please make sure the file is located in your LUMENS folder in Program files")
  }
  
  #=Connect to fragstats' .fca file=
  SQLite(max.con = 200, fetch.default.rec = 500, force.reload = FALSE, shared.cache=FALSE)
  drv <- dbDriver("SQLite")
  con <- dbConnect(drv, dbname=as.character(fca))
  
  #delete all record from frg_landscape layer
  del<-paste("DELETE FROM frg_landscape_layers")
  ll <- dbSendQuery(con, del)
  
  input_desc<-paste("UPDATE frg_table_strings SET value='",classdesc,"' WHERE rec_id=5;",sep="")
  input_edge<-paste("UPDATE frg_table_strings SET value='",edgecon,"' WHERE rec_id=2;",sep="")
  input_out<-paste("UPDATE frg_table_strings SET value='",result_dir,"' WHERE rec_id=6;",sep="")
  input_window_size_sqr<-paste("UPDATE frg_table_numerics SET value=",windowsize,"WHERE rec_id=18;"); #change square window radius
  input_window_size_circ<-paste("UPDATE frg_table_numerics SET value=",windowsize,"WHERE rec_id=19;"); #change circle window radius
  input_window_type<-paste("UPDATE frg_table_numerics SET value=",window.shape,"WHERE rec_id=13;")
  ll <- dbSendQuery(con, input_desc)
  ll <- dbSendQuery(con, input_edge)
  ll <- dbSendQuery(con, input_out)
  ll <- dbSendQuery(con, input_window_size_sqr)
  ll <- dbSendQuery(con, input_window_size_circ)
  ll <- dbSendQuery(con, input_window_type)
  
  landlayer1<-paste("INSERT INTO frg_landscape_layers(model_id, name_internal, name_external, io_info, driver_lib, driver_name, driver_id, xll, yll, xur, yur, cell_size, row_count, col_count, allow_class_zero, no_data_value, background_value) VALUES ('",modid,"','",internal,"','",lu_path,"','",io,"','",drlib,"','",drname,"','",drid,"','",xl1,"','",yl1,"','",xu1,"','",yu1,"','",csize1,"','",rowc1,"','",colc1,"','",aczero,"','",nodata,"','",bvalue,"');",sep="")
  
  ll <- dbSendQuery(con, landlayer1)
  
  #Fragstats directory
  if (file.exists("C:/Program Files (x86)/Fragstats 4")){
    setwd("C:/Program Files (x86)/Fragstats 4/")
  } else{
    setwd("C:/Program Files/Fragstats 4/")
  }
  
  #= Execute fragstats, generate TECI=
  sysout<-paste(result_dir, "/fragout", sep="")
  f <- paste('frg -m',shQuote(fca),' -o',sysout)
  system(f)
  
  #delete all record from frg_landscape layer
  del<-paste("DELETE FROM frg_landscape_layers")
  ll <- dbSendQuery(con, del)
  
  setwd(result_dir)
  mwout<-paste(lu_path,'_mw1', sep='')
  teci.dir<-paste(mwout,"/",list.files(mwout)[1], sep='')
  
  #convert -999 value to NA
  mwfile<-raster(teci.dir)
  NAvalue(mwfile)<-(999*-1)
  
  return(mwfile)
}

#Execute TECI T1
mwfile.init<-teci.analysis(lu1, lu1_path)
#Execute TECI T2
mwfile.final<-teci.analysis(lu2, lu2_path)

#dbGetStatement(ll)
#dbHasCompleted(ll)

#End of Fragstats TECI moving window analysis

#====L Write Raster TECI & Focal area File==== 
setwd(result_dir2)
#save TECI file function
saveTECI<-function(mwfile, location, period){
  file.teci<-paste('TECI_',location,'_',period,'_NA',sep='')
  writeRaster(mwfile, filename=file.teci, format="GTiff", overwrite=TRUE)
  return(print(paste(file.teci, "has been written")))
}

#save TECI file
saveTECI(mwfile.init, location, T1)
saveTECI(mwfile.final, location, T2)

saveFocal<-function(foc.area, location, period){
  file.habitat.name<-paste('focal_area_',location,'_',period, sep='')
  writeRaster(foc.area, filename=file.habitat.name, format="GTiff", overwrite=TRUE)
  return(print(paste(file.habitat.name, "has been written")))
}
saveFocal(foc.area.init, location, T1)
saveFocal(foc.area.final, location, T2)

#====M Generate DIFA Table and Chart + AUC ===== 
#DIFA Table Function
generateDIFAtable<-function(mwfile, tothab, location, period){
  
  #extract value from MW TECI with points
  tecival<-extract(mwfile, centro, method='simple', na.rm=T, df=T)
  
  poly.data<-as.data.frame(sampling.grid.rast,xy=TRUE) #NEW
  colnames(poly.data)<-c("x","y","ID.grid") #ID = id grid
  
  #combine dataframe of teci and habitat
  colnames(tecival)<-c("ID.grid","teci")
  colnames(tothab)<-c("ID.grid","sum")
  ctab<-merge(tothab,poly.data,by="ID.grid")
  ctab<-merge(ctab,tecival,by="ID.grid")
  sort.ctab <- ctab[order(ctab$teci, decreasing=F, na.last=TRUE), ]
  sort.ctab <- sort.ctab[!(sort.ctab$sum==0),]
  sumtab1<-cbind(sort.ctab, Cum.Sum=cumsum(sort.ctab$sum))
  cumax<-max(sumtab1$Cum.Sum, na.rm=TRUE)
  row.names(sumtab1)<-1:nrow(sumtab1)
  sumtab1[nrow(sumtab1)+1, ] <- c(sumtab1$ID.centro[nrow(sumtab1)], sumtab1$ID.grid[nrow(sumtab1)],100,sumtab1$x[nrow(sumtab1)],sumtab1$y[nrow(sumtab1)],100,cumax)
  
  sumtab2.init<-round(sumtab1,digits=2)
  colnames(sumtab2.init)<-c("ID.grid","Habitat Area (Ha)","X.cor","Y.cor","TECI(%)", "Cumulative Habitat(%)")
  nama.tabel.teci<-paste("QUES-B_DIFA_Table_", location,"_", period, ".csv", sep='')
  write.table(sumtab2.init, nama.tabel.teci, row.names = FALSE, sep=",")
  
  return (sumtab1)
}
difa.table.init<-generateDIFAtable(mwfile.init, tothab.init, location, T1)
difa.table.final<-generateDIFAtable(mwfile.final, tothab.final, location, T2)

#DIFA Chart Initial
difa.init<-ggplot(difa.table.init, aes(x =difa.table.init$teci, y =difa.table.init$Cum.Sum, xend=100, yend=100)) +
  geom_area(position='') + ggtitle(paste(location, T1)) +
  labs(x = "Sorted TECI value (%)", y='Cumulative Proportion of Focal Areas (%)')

#DIFA Chart Final
difa.final<-ggplot(difa.table.final, aes(x =difa.table.final$teci, y =difa.table.final$Cum.Sum, xend=100, yend=100)) +
  geom_area(position='') + ggtitle(paste(location, T2)) +
  labs(x = "Sorted TECI value (%)", y='Cumulative Proportion of Focal Areas (%)')


#Calculate area under the curve
AUC.init = round((trapz(na.omit(difa.table.init$teci),difa.table.init$Cum.Sum))/100,digits=2)
AUC.final = round((trapz(na.omit(difa.table.final$teci),difa.table.final$Cum.Sum))/100,digits=2)

#====N Zonal statistics on QUES-B====
zstat.init<-ZonalStat(mwfile.init, zone, FUN = "all")
zstat.init[3]<-NULL
zstat.init[3]<-NULL
zstat.init[3]<-NULL

zstat.final<-ZonalStat(mwfile.final, zone, FUN = "all")
zstat.final[3]<-NULL
zstat.final[3]<-NULL
zstat.final[3]<-NULL
#rcl.mean.final<-cbind(zstat.final$zone,zstat.final$mean)
#teci_zstat_mean.final<-reclassify(zone, rcl.mean.final);# PU teci value mean

#====O SDM Tools Landscape metrics =====
#SDM Tools fragstats; mean patch area calculation; patch number calculation
foc.area.stats.init<- ClassStat(foc.area.init,bkgd=0, cellsize=(res(foc.area.init)[1]/100))
foc.area.stats.init<-t(as.data.frame(foc.area.stats.init))
foc.area.stats.init<-round(foc.area.stats.init[,1], digits=2)

foc.area.stats.final<- ClassStat(foc.area.final,bkgd=0, cellsize=(res(foc.area.final)[1]/100))
foc.area.stats.final<-t(as.data.frame(foc.area.stats.final))
foc.area.stats.final<-round(foc.area.stats.final[,1], digits=2)

#Combine class STATS
foc.area.stats<-cbind(foc.area.stats.init,foc.area.stats.final)
foc.area.stats.temp1<-foc.area.stats[2:4,c('foc.area.stats.init','foc.area.stats.final')]
total.edge<-(foc.area.stats[6:6,c('foc.area.stats.init','foc.area.stats.final')]*100)
foc.area.stats.temp3<-(foc.area.stats[10:13,c('foc.area.stats.init','foc.area.stats.final')])
foc.area.stats<-rbind(foc.area.stats.temp1,total.edge,foc.area.stats.temp3)
rm(foc.area.stats.temp1,total.edge,foc.area.stats.temp3)



col.init<-paste('class.stats.',T1, sep='')
colnames(foc.area.stats)<-c(paste('class.stats.',T1, sep=''),paste('class.stats.',T2, sep=''))
foc.area.stats.filename<-paste("Focal_area_class_metrics",location,'_',T1,'_',T2,'.csv', sep='')
write.csv(foc.area.stats, foc.area.stats.filename, row.names=TRUE)

#combine teci_zstat with planning unit name

#QUES-B database
#dbase.quesb.name<-paste("QuESB_database_", location,'_',T1,'_',T2,'.ldbase', sep='')
#save(lu1_path,lu1,T1,lu2_path,lu2,T2,zone,zone_lookup,location,totarea,lookup_bh,polygrid,
#     difa.table.init,difa.init,AUC.init,foc.area.init,mwfile.init,zstat.init,foc.area.stats.init,
#     difa.table.final,difa.final,AUC.final,mwfile.final,zstat.final,foc.area.stats.final, 
#    file=dbase.quesb.name)
#load(dbase.quesb.name)

#====P MULTI-TEMPORAL ANALYSIS====
#important variables above:
#habitat.recovery
#habitat.degradation
#habitat.gain.NA
#habitat.loss.NA

#Focal area decrement and increment
chk_loss<-foc.area.init>foc.area.final
chk_gain<-foc.area.init<foc.area.final
foc.area.loss<-(foc.area.init-foc.area.final)*chk_loss;#integration increase
foc.area.gain<-(foc.area.final-foc.area.init)*chk_gain;#integration decrease

if (maxValue(foc.area.gain)==0 & minValue(foc.area.gain)==0){print(paste("NO FOCAL AREA RECOVERED"))} else {foc.area.gain}


#Habitat loss (TECI increment) and Habitat recovery (decrement) except nodata
mwfile.init.chk<-mwfile.init
mwfile.init.chk0<-reclassify(mwfile.init.chk, cbind(NA,0))
mwfile.final.chk<-mwfile.final
mwfile.final.chk0<-reclassify(mwfile.final.chk, cbind(NA,0))

chk_teci_decrement<-mwfile.init.chk>mwfile.final.chk
chk_teci_decrement <- reclassify(chk_teci_decrement, cbind(NA,0))
chk_teci_increment<-mwfile.final.chk>mwfile.init.chk
chk_teci_increment <- reclassify(chk_teci_increment, cbind(NA,0))
habitat.recovery<-(mwfile.init.chk0-mwfile.final.chk0)*chk_teci_decrement*(foc.area.init==0);#TECI value decrement
#habitat.recovery1<-(mwfile.init.chk0-mwfile.final.chk0)*chk_teci_decrement*(foc.area.init);#TECI value decrement
habitat.degradation<-(mwfile.final.chk0-mwfile.init.chk0)*chk_teci_increment*(foc.area.final>=0);#TECI value increment


#TECI loss and gain in NA data
mwfile.init.NA <- reclassify(mwfile.init.chk, cbind(NA, 999))
mwfile.init.NA<-((mwfile.init.NA/999)==1)

mwfile.final.NA <- reclassify(mwfile.final.chk, cbind(NA, 999))
mwfile.final.NA<-((mwfile.final.NA/999)==1)


#Habitat gain and recovery
habitat.gain.NA<-mwfile.final.chk0*mwfile.init.NA;#TECI gain in NA area
#habitat.gain.NA<- reclassify(habitat.gain.NA, cbind(0, NA))
habitat.gain.NA<-habitat.gain.NA*(foc.area.final>=0)
#habitat.gain.recovery<-mosaic(habitat.recovery, habitat.gain.NA, fun="max")

#Habitat loss and degradation
habitat.loss.NA<-mwfile.init.chk0*mwfile.final.NA;#TECI loss in NA area
#habitat.loss.NA<- reclassify(habitat.loss.NA, cbind(0, NA))
habitat.loss.NA<- habitat.loss.NA*(foc.area.final>=0)
#habitat.loss.degradation<-mosaic(habitat.degradation, habitat.loss.NA, fun="max")

#====Q Focal Area Loss Evaluation ====

#important variables below:
#foc.area.gain.att
#foc.area.loss.att
#zstat.foc.area

#focal area loss evaluation: generate focal area loss map contained with final land-cover types
if (maxValue(chk_loss)>0){
  foc.area.loss<-chk_loss*lu2
  foc.area.loss <- reclassify(foc.area.loss, cbind(0, NA))
  foc.area.loss.att<-na.omit(as.data.frame(freq(foc.area.loss)))
  foc.area.loss.att$prop<-(foc.area.loss.att$count/sum(foc.area.loss.att$count))*100
  
  colnames(foc.area.loss.att)[1]<-c("ID")
  foc.area.loss.att<-merge(lookup_bh, foc.area.loss.att, by="ID")
  foc.area.loss.att$BIODIV<-NULL
  colnames(foc.area.loss.att)<-c("ID", "LULC", "Area", "Proportion_of_loss")
  foc.area.loss.att$Area<-foc.area.loss.att$Area*(res(foc.area.init)[1]*res(foc.area.init)[2]/10000)
  foc.area.loss.att<-arrange(foc.area.loss.att, -Proportion_of_loss)
  foc.area.loss.att$Proportion_of_loss<-round(foc.area.loss.att$Proportion_of_loss, digits=2)
  foc.area.loss.att.filename<-paste("Focal_area_loss_source_",location,'_',T1,'_',T2,'.dbf', sep='')
  write.dbf(foc.area.loss.att, foc.area.loss.att.filename)
} else { print("No focal area loss found")}

if (maxValue(chk_gain)>0) {
  foc.area.gain<-chk_gain*lu2
  foc.area.gain <- reclassify(foc.area.gain, cbind(0, NA))
  foc.area.gain.att<-na.omit(as.data.frame(freq(foc.area.gain)))
  foc.area.gain.att$prop<-(foc.area.gain.att$count/sum(foc.area.gain.att$count))*100
  
  colnames(foc.area.gain.att)[1]<-c("ID")
  foc.area.gain.att<-merge(lookup_bh, foc.area.gain.att, by="ID")
  foc.area.gain.att$BIODIV<-NULL
  colnames(foc.area.gain.att)<-c("ID", "LULC", "Area", "Proportion_of_gain")
  foc.area.gain.att$Area<-foc.area.gain.att$Area*(res(foc.area.init)[1]*res(foc.area.init)[2]/10000)
  foc.area.gain.att<-arrange(foc.area.gain.att, -Proportion_of_gain)
  foc.area.gain.att$Proportion_of_gain<-round(foc.area.gain.att$Proportion_of_gain, digits=2)
  foc.area.gain.att.filename<-paste("Focal_area_gain_source",location,'_',T1,'_',T2,'.dbf', sep='')
  write.dbf(foc.area.gain.att, foc.area.gain.att.filename)
} else { print("No focal area gain found")}


#zonal stat for focal area gain/loss
lookup_z.area<-as.data.frame(na.omit(freq(zone)))
lookup_z.area[2]<-lookup_z.area[2]*Spat_res
colnames(lookup_z.area)<-c('ZONE','zone.area')
#lookup_z.area$zone.area<-lookup_z.area$zone.area*Spat_res
#foc.area.change.map<-reclassify((foc.area.final-foc.area.init),cbind(0,NA))
zstat.foc.area.basic<-as.data.frame(zonal((foc.area.final-foc.area.init), zone, fun='sum'))
colnames(zstat.foc.area.basic) =c("ZONE","foc.area.change")
zstat.foc.area<-zstat.foc.area.basic

zstat.foc.area$foc.area.change<-zstat.foc.area$foc.area.change*Spat_res
zstat.foc.area<-merge(lookup_z,zstat.foc.area,by='ZONE')
zstat.foc.area<-merge(zstat.foc.area,lookup_z.area,by='ZONE')
zstat.foc.area$change.proportion<-round((zstat.foc.area$foc.area.change/zstat.foc.area$zone.area)*100, digits=2)
zstat.foc.area<-arrange(zstat.foc.area, foc.area.change)

#====R zonal stat for habitat recovery and degradation ====
#important variables below:
#foc.area.gain.att
#foc.area.loss.att
#zstat.foc.area
#zstat.habitat.degradation
#zstat.habitat.recovery

tryCatch({
  habitat.recovery.0<-reclassify(habitat.recovery, cbind(NA, 0))
  zstat.habitat.recovery<-ZonalStat(habitat.recovery.0, zone, FUN = "all")
  colnames(zstat.habitat.recovery)[1] ="ZONE"
  zstat.habitat.recovery<-merge(lookup_z, zstat.habitat.recovery, by="ZONE")
  zstat.habitat.recovery[4]<-NULL
  zstat.habitat.recovery[4]<-NULL
  zstat.habitat.recovery[4]<-NULL
  zstat.habitat.recovery[7]<-NULL
  zstat.habitat.recovery$max<-round(zstat.habitat.recovery$max, digits=2)
  zstat.habitat.recovery$min<-round(zstat.habitat.recovery$min, digits=2)
  zstat.habitat.recovery$mean<-round(zstat.habitat.recovery$mean, digits=2)
  zstat.habitat.recovery$sd<-round(zstat.habitat.recovery$sd, digits=2)
  zstat.habitat.recovery<-merge(zstat.habitat.recovery, zstat.foc.area.basic, by="ZONE")
  zstat.habitat.recovery$norm.mean<-zstat.habitat.recovery$mean/abs(zstat.habitat.recovery$foc.area)
  zstat.habitat.recovery$norm.mean<-round(zstat.habitat.recovery$norm.mean, digits=3)
  zstat.habitat.recovery<-arrange(zstat.habitat.recovery, -norm.mean)
},error=function(e){cat("Skipping zonal stats on habitat recovery:",conditionMessage(e), "\n")})

tryCatch({
  habitat.degradation.0<-reclassify(habitat.degradation, cbind(NA, 0))
  zstat.habitat.degradation<-ZonalStat(habitat.degradation.0, zone, FUN = "all")
  colnames(zstat.habitat.degradation)[1] ="ZONE"
  zstat.habitat.degradation<-merge(lookup_z, zstat.habitat.degradation, by="ZONE")
  zstat.habitat.degradation[4]<-NULL
  zstat.habitat.degradation[4]<-NULL
  zstat.habitat.degradation[4]<-NULL
  zstat.habitat.degradation[7]<-NULL
  zstat.habitat.degradation$max<-round(zstat.habitat.degradation$max, digits=2)
  zstat.habitat.degradation$min<-round(zstat.habitat.degradation$min, digits=2)
  zstat.habitat.degradation$mean<-round(zstat.habitat.degradation$mean, digits=2)
  zstat.habitat.degradation$sd<-round(zstat.habitat.degradation$sd, digits=2)
  zstat.habitat.degradation<-merge(zstat.habitat.degradation, zstat.foc.area.basic, by="ZONE")
  zstat.habitat.degradation$norm.mean<-zstat.habitat.degradation$mean/abs(zstat.habitat.degradation$foc.area)
  zstat.habitat.degradation$norm.mean<-round(zstat.habitat.degradation$norm.mean, digits=3)
  zstat.habitat.degradation<-arrange(zstat.habitat.degradation, -norm.mean)
},error=function(e){cat("Skipping zonal stats on habitat gain:",conditionMessage(e), "\n")})

#write zonal stats table
tryCatch({
  zstat.gain.recover.filename<-paste("Habitat_recovery_zonal_stat_",location,'_',T1,'_',T2,'.dbf', sep='')
  write.dbf(zstat.habitat.recovery, zstat.gain.recover.filename)
},error=function(e){cat("Skipping zonal stats table export process:",conditionMessage(e), "\n")})

tryCatch({
  zstat.loss.degradation.filename<-paste("Habitat_degradation_zonal_stat_",location,'_',T1,'_',T2,'.dbf', sep='')
  write.dbf(zstat.habitat.degradation, zstat.loss.degradation.filename)
},error=function(e){cat("Skipping zonal stats table export process:",conditionMessage(e), "\n")})






#backround map
background<-lu1/lu1
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

myColors  <-c(myColors7,myColors1, myColors2, myColors3, myColors4, myColors5, myColors6)

#Landuse 1 map
area_lc1<-as.data.frame(freq(lu1))
colnames(area_lc1)[1]<-'ID'
area_lc1<-merge(area_lc1, lookup_bh, by='ID')
colnames(area_lc1)[3]<-'CLASS_LC1'
area_lc1[4]<-NULL
id_length<-max(length(unique(area_lc1$ID)), length(unique(area_lc2$ID)))
myColors.lu <- myColors[1:id_length]
ColScale.lu<-scale_fill_manual(name="Land Use Class", breaks=area_lc1$ID, labels=area_lc1$CLASS_LC1, values=myColors.lu)
plot.LU1<-gplot(lu1, maxpixels=100000) + geom_raster(aes(fill=as.factor(value))) +
  coord_equal() + ColScale.lu +
  theme(plot.title = element_text(lineheight= 5, face="bold")) +
  theme( axis.title.x=element_blank(),axis.title.y=element_blank(),
         panel.grid.major=element_blank(), panel.grid.minor=element_blank(),
         legend.title = element_text(size=8),
         legend.text = element_text(size = 6),
         legend.key.height = unit(0.25, "cm"),
         legend.key.width = unit(0.25, "cm"))

#Landuse 2 map
area_lc2<-as.data.frame(freq(lu2))
colnames(area_lc2)[1]<-'ID'
area_lc2<-merge(area_lc2, lookup_bh, by='ID')
colnames(area_lc2)[3]<-'CLASS_LC2'
area_lc2[4]<-NULL
ColScale.lu<-scale_fill_manual(name="Land Use Class", breaks=area_lc2$ID, labels=area_lc2$CLASS_LC2, values=myColors.lu)
plot.LU2<-gplot(lu2, maxpixels=100000) + geom_raster(aes(fill=as.factor(value))) +
  coord_equal() + ColScale.lu +
  theme(plot.title = element_text(lineheight= 5, face="bold")) +
  theme( axis.title.x=element_blank(),axis.title.y=element_blank(),
         panel.grid.major=element_blank(), panel.grid.minor=element_blank(),
         legend.title = element_text(size=8),
         legend.text = element_text(size = 6),
         legend.key.height = unit(0.25, "cm"),
         legend.key.width = unit(0.25, "cm"))

myColors  <-c(myColors5,myColors1, myColors2, myColors3, myColors4, myColors7, myColors6)

#zone map
area_zone<-lookup_z
colnames(area_zone)[1]<-'ID'
colnames(area_zone)[2]<-'ZONE'
myColors.Z <- myColors[1:length(unique(area_zone$ID))]
ColScale.Z<-scale_fill_manual(name="Zone Class", breaks=area_zone$ID, labels=area_zone$ZONE, values=myColors.Z)
plot.Z<-gplot(zone, maxpixels=100000) + geom_raster(aes(fill=as.factor(value))) +
  coord_equal() + ColScale.Z +
  theme(plot.title = element_text(lineheight= 5, face="bold")) +
  theme( axis.title.x=element_blank(),axis.title.y=element_blank(),
         panel.grid.major=element_blank(), panel.grid.minor=element_blank(),
         legend.title = element_text(size=8),
         legend.text = element_text(size = 6),
         legend.key.height = unit(0.25, "cm"),
         legend.key.width = unit(0.25, "cm"))

#Focal Area Change: plot.FAC.loss and plot.FAC.gain
lookup_change<-as.data.frame(cbind(0,NA))
lookup_change<-rbind(lookup_change, cbind(1,2))

if(maxValue(chk_loss)>0)
{
  foc.area.loss.reclass<- reclassify(chk_loss, lookup_change)
  foc.area.loss<-mosaic(foc.area.init, foc.area.loss.reclass, fun="max")
  ID<-as.data.frame(levels(ratify(foc.area.loss)));# or as.data.frame(na.omit(freq(foc.area.loss)))
  Label<-c("Non Focal Area", "Stable Focal Area", "Focal Area loss")
  FAC<-as.data.frame(cbind(ID, Label))
  myColors.FAC <- c("#FFCC66", "#003300","#FF0000")
  ColScale.FAC<-scale_fill_manual(name="Area Class", breaks=FAC$ID, labels=FAC$Label, values=myColors.FAC)
  plot.FAC.loss<-gplot(foc.area.loss, maxpixels=100000) + geom_raster(aes(fill=as.factor(value))) +
    coord_equal() + ColScale.FAC +
    theme(plot.title = element_text(lineheight= 5, face="bold")) +
    theme( axis.title.x=element_blank(),axis.title.y=element_blank(),
           panel.grid.major=element_blank(), panel.grid.minor=element_blank(),
           legend.title = element_text(size=8),
           legend.text = element_text(size = 6),
           legend.key.height = unit(0.25, "cm"),
           legend.key.width = unit(0.25, "cm"))
} else
{
  print("No Habitat loss found")
}

if(maxValue(chk_gain)>0)
{
  foc.area.gain.reclass<- reclassify(chk_gain, lookup_change)
  foc.area.gain<-mosaic(foc.area.init, foc.area.gain.reclass, fun="max")
  ID<-as.data.frame(levels(ratify(foc.area.gain)));# or as.data.frame(na.omit(freq(foc.area.gain)))
  Label<-c("Non Focal Area", "Stable Focal Area", "Focal Area gain")
  FAC<-as.data.frame(cbind(ID, Label))
  myColors.FAC <- c("#FFCC66", "#003300","#0276FD")
  ColScale.FAC<-scale_fill_manual(name="Area Class", breaks=FAC$ID, labels=FAC$Label, values=myColors.FAC)
  plot.FAC.gain<-gplot(foc.area.gain, maxpixels=100000) + geom_raster(aes(fill=as.factor(value))) +
    coord_equal() + ColScale.FAC +
    theme(plot.title = element_text(lineheight= 5, face="bold")) +
    theme( axis.title.x=element_blank(),axis.title.y=element_blank(),
           panel.grid.major=element_blank(), panel.grid.minor=element_blank(),
           legend.title = element_text(size=8),
           legend.text = element_text(size = 6),
           legend.key.height = unit(0.25, "cm"),
           legend.key.width = unit(0.25, "cm"))
} else
{
  print("No Habitat gain found")
}

#====S Subsequent Land Use: Habitat Degradation, Recovey, Loss, Gain=====
setwd(result_dir3)
#Identify Changemap
eval(parse(text=(paste("check_changemap<-file.exists('luchgmap_", pu_name ,"_", T1, "_", T2, ".tif')", sep="")))) 
eval(parse(text=(paste("check_ludb<-file.exists('lu.db_", pu_name ,"_", T1, "_", T2, ".dbf')", sep=""))))

#habitat changes function
subsequent.changes<-function(habitat.analysis,lu_db, analysis, location, T1, T2){
  habitat.analysis.bol<-reclassify(habitat.analysis/habitat.analysis, cbind(0,NA))
  
  if (as.character(ref@crs)==as.character(habitat.analysis.bol@crs)){
    print("Final land use/cover map has the same projection")
    if (res(ref)[1]==res(habitat.analysis.bol)[1]){
      print("change map has the same extent with the habitat analysis map")
    } else{
      print("change map doesn't have the same extent with the habitat analysis map, synchronising habitat analysis map...")
      habitat.analysis.bol<-spatial_sync_raster(habitat.analysis.bol, ref, method = "ngb")
    }
  } else{
    print("change map doesn't have the same projection with the habitat analysis map, synchronising habitat analysis map...")
    habitat.analysis.bol<-spatial_sync_raster(habitat.analysis.bol, ref, method = "ngb")
  }
  
  #identify contributing land use change to focal area analysis and loss
  luchg.analysis<-lu_chg*habitat.analysis.bol; #focal area analysis LUC
  luchg.analysis.att<-na.omit(as.data.frame(freq(luchg.analysis)))
  
  colnames(luchg.analysis.att)<-c("ID","AREA")
  luchg.analysis.att<-merge(luchg.analysis.att,lu_db,by='ID')
  
  luchg.analysis.att<-as.data.frame(cbind(luchg.analysis.att[1],luchg.analysis.att[2],luchg.analysis.att[8],luchg.analysis.att[9]), stringsAsFactors=T)
  luchg.analysis.att<-unique(luchg.analysis.att[,1:4])
  luchg.analysis.att<-luchg.analysis.att[ order(-luchg.analysis.att[,2]), ]
  luchg.analysis.att<-transform(luchg.analysis.att, LUCHG=paste0(LC_t1," to " ,LC_t2))
  luchg.analysis.att$LUCHG<-as.character(luchg.analysis.att$LUCHG)
  
  for (i in 1:nrow(luchg.analysis.att)){
    if(as.character(luchg.analysis.att[i,4])==as.character(luchg.analysis.att[i,3])){
      luchg.analysis.att[i,5]<-paste("Persistent",luchg.analysis.att[i,3])
    }}
  luchg.analysis.att<-cbind(luchg.analysis.att[1], luchg.analysis.att[2], luchg.analysis.att[5])
  
  setwd(result_dir2)
  tryCatch({
    luchg.db.filename<-paste("LUCHG_",analysis,"_database",location,'_',T1,'_',T2,'.dbf', sep='')
    write.dbf(luchg.analysis.att, luchg.db.filename)
  },error=function(e){cat("Skipping database export process :",conditionMessage(e), "\n")})
  return(luchg.analysis.att)
}

if (check_changemap && check_ludb){
  
  eval(parse(text=(paste("lu_chg<-raster('luchgmap_", pu_name ,"_", T1, "_", T2, ".tif')", sep=""))))
  eval(parse(text=(paste("lu_db<-read.dbf('lu.db_", pu_name ,"_", T1, "_", T2, ".dbf')", sep=""))))
  
  
  #Landscape level Habitat Degradation
  luchg.db.degrad<-subsequent.changes(habitat.degradation,lu_db, "Degradation", location, T1,T2)
  #Landscape level Habitat Recovery
  luchg.db.recovery<-subsequent.changes(habitat.recovery,lu_db, "Recovery", location, T1,T2)
  
  #top 10 habitat degradation
  luchg.degradation.10<-luchg.db.degrad[1:10,]
  
  #Landscape level Habitat Loss
  luchg.db.loss<-subsequent.changes(habitat.loss.NA,lu_db, "Loss", location, T1,T2)
  #zonal stat for habitat loss
  zonal_stat<-function(habitat.change, zone, lookup_z){
    habitat.change.0<-reclassify(habitat.change>0, cbind(NA, 0))
    zstat.habitat.change<-ZonalStat(habitat.change.0, zone, FUN = "sum")
    colnames(zstat.habitat.change)[1] ="ZONE"
    zstat.habitat.change<-merge(lookup_z, zstat.habitat.change, by="ZONE")
    zstat.habitat.change$Proportion<-((zstat.habitat.change$sum)/(sum(zstat.habitat.change$sum)))*Spat_res*100
    zstat.habitat.change.prop<-as.data.frame(cbind(' ','TOTAL',(sum(zstat.habitat.change$sum)), (sum(zstat.habitat.change$Proportion))))
    zstat.habitat.change$Proportion<-round(zstat.habitat.change$Proportion,digits=2)
    colnames(zstat.habitat.change.prop)<-c('ZONE','Z_NAME','sum','Proportion')
    zstat.habitat.change<-zstat.habitat.change[ order(as.numeric(zstat.habitat.change$sum), decreasing=TRUE), ]
    #zstat.habitat.change<-rbind(zstat.habitat.change,zstat.habitat.change.prop)
    colnames(zstat.habitat.change)<-c('ID','ZONE','total.area','Proportion')
    return(zstat.habitat.change)}
  
  #Zonal Stat Habitat LOSS
  luchg.db.loss.zstat<-zonal_stat(habitat.loss.NA, zone, lookup_z)
  
  
  tryCatch({
    luchg.db.gain<-subsequent.changes(habitat.gain.NA,lu_db, "Gain", location, T1,T2)
  },error=function(e){cat("Skipping habitat gain analysis:",conditionMessage(e), "\n")})
  
  
} else {
  print (paste("Pre-QUES Land Use Change Database and Changemap for ",T1,"-",T2, " is not found", sep=""))
}

#====Plot Habitat extent Map t1====
plot.mw.init<-gplot(mwfile.init.chk*lu.nodata.check, maxpixels=100000) + geom_raster(aes(fill=value)) +
  coord_equal() + scale_fill_gradient(low = "#FFCC66", high="#003300", guide="colourbar") +
  theme(plot.title = element_text(lineheight= 5, face="bold")) +
  theme( axis.title.x=element_blank(),axis.title.y=element_blank(),
         panel.grid.major=element_blank(), panel.grid.minor=element_blank(),
         legend.title = element_text(size=8),
         legend.text = element_text(size = 8),
         legend.key.height = unit(0.375, "cm"),
         legend.key.width = unit(0.375, "cm"))

#====Plot Habitat extent Map t2====
plot.mw.fin<-gplot(mwfile.final, maxpixels=100000) + geom_raster(aes(fill=value)) +
  coord_equal() + scale_fill_gradient(low = "#FFCC66", high="#003300", guide="colourbar") +
  theme(plot.title = element_text(lineheight= 5, face="bold")) +
  theme( axis.title.x=element_blank(),axis.title.y=element_blank(),
         panel.grid.major=element_blank(), panel.grid.minor=element_blank(),
         legend.title = element_text(size=8),
         legend.text = element_text(size = 8),
         legend.key.height = unit(0.375, "cm"),
         legend.key.width = unit(0.375, "cm"))

#====Plot Habitat loss and degradation====
#plot.background<-gplot(background, maxpixels=100000) + geom_raster(aes(fill=as.factor(value)))
tryCatch({
  maxval<-ceiling(maxValue(habitat.degradation)/10)
  maxval<-maxval*10
  background[background==1]<-(-maxval)
  plot.hbt.loss<-merge(habitat.degradation, background, overlap=TRUE)
  plot.HD<-gplot(plot.hbt.loss, maxpixels=100000) + geom_raster(aes(fill=value)) +
    coord_equal() + scale_fill_gradient2(low="#999999",mid = "#FFCC66", high="#003300",limits=c(0,maxval), guide="colourbar") +
    theme(plot.title = element_text(lineheight= 5, face="bold")) +
    theme( axis.title.x=element_blank(),axis.title.y=element_blank(),
           panel.grid.major=element_blank(), panel.grid.minor=element_blank(),
           legend.title = element_text(size=8),
           legend.text = element_text(size = 8),
           legend.key.height = unit(0.375, "cm"),
           legend.key.width = unit(0.375, "cm"))
  background[background==-maxval]<-1
},error=function(e){cat("skipping habitat degradation plot :",conditionMessage(e), "\n")})

tryCatch({
  maxval<-ceiling(maxValue(habitat.loss.NA>0)/10)
  #maxval<-maxval*10
  background[background==1]<-(-maxval)
  plot.hbt.loss<-merge(habitat.loss.NA>0, background, overlap=TRUE)
  plot.HL<-gplot(habitat.loss.NA>0, maxpixels=100000) + geom_raster(aes(fill=value)) +
    coord_equal() + scale_fill_gradient2(low="#999999",mid = "#FFCC66", high="#003300",limits=c(0,maxval), guide="colourbar") +
    theme(plot.title = element_text(lineheight= 5, face="bold")) +
    theme( axis.title.x=element_blank(),axis.title.y=element_blank(),
           panel.grid.major=element_blank(), panel.grid.minor=element_blank(),
           legend.title = element_text(size=8),
           legend.text = element_text(size = 8),
           legend.key.height = unit(0.375, "cm"),
           legend.key.width = unit(0.375, "cm"))
  background[background==-maxval]<-1
},error=function(e){cat("skipping habitat loss plot :",conditionMessage(e), "\n")})

#====Plot Habitat gain and recovery ====
#plot.background<-gplot(background, maxpixels=100000) + geom_raster(aes(fill=as.factor(value)))
tryCatch({
  maxval<-ceiling(maxValue(habitat.recovery)/10)
  maxval<-maxval*10
  background[background==1]<-(-maxval)
  plot.hbt.gain<-merge(habitat.recovery>0, background, overlap=TRUE)
  plot.HR<-gplot(plot.hbt.gain, maxpixels=100000) + geom_raster(aes(fill=value)) +
    coord_equal() + scale_fill_gradient2(low="#999999",mid = "#FFCC66", high="#003300",limits=c(0,maxval), guide="colourbar") +
    theme(plot.title = element_text(lineheight= 5, face="bold")) +
    theme( axis.title.x=element_blank(),axis.title.y=element_blank(),
           panel.grid.major=element_blank(), panel.grid.minor=element_blank(),
           legend.title = element_text(size=8),
           legend.text = element_text(size = 8),
           legend.key.height = unit(0.375, "cm"),
           legend.key.width = unit(0.375, "cm"))
  background[background==-maxval]<-1
},error=function(e){cat("skipping habitat recovery plot :",conditionMessage(e), "\n")})

tryCatch({
  #plot.background<-gplot(background, maxpixels=100000) + geom_raster(aes(fill=as.factor(value)))
  maxval<-ceiling(maxValue(habitat.gain.NA)/10)
  #maxval<-maxval*10
  background[background==1]<-(-maxval)
  plot.hbt.gain<-merge(habitat.gain.NA, background, overlap=TRUE)
  plot.HG<-gplot(plot.hbt.gain, maxpixels=100000) + geom_raster(aes(fill=value)) +
    coord_equal() + scale_fill_gradient2(low="#999999",mid = "#FFCC66", high="#003300",limits=c(0,maxval), guide="colourbar") +
    theme(plot.title = element_text(lineheight= 5, face="bold")) +
    theme( axis.title.x=element_blank(),axis.title.y=element_blank(),
           panel.grid.major=element_blank(), panel.grid.minor=element_blank(),
           legend.title = element_text(size=8),
           legend.text = element_text(size = 8),
           legend.key.height = unit(0.375, "cm"),
           legend.key.width = unit(0.375, "cm"))
  background[background==-maxval]<-1
},error=function(e){cat("skipping habitat gain plot :",conditionMessage(e), "\n")})


#====Create RTF Report File====
setwd(result_dir2)
title<-"\\b\\fs32 LUMENS-QUES Project Report\\b0\\fs20"
sub_title<-"\\b\\fs28 Sub-modules: Biodiversity Analysis\\b0\\fs20"
test<-as.character(Sys.Date())
date<-paste("Date : ", test, sep="")
time_start<-paste("Processing started : ", time_start, sep="")
time_end<-paste("Processing ended : ", eval(parse(text=(paste("Sys.time ()")))), sep="")
line<-paste("------------------------------------------------------------------------------------------------------------------------------------------------")
area_name_rep<-paste("\\b", "\\fs20", location, "\\b0","\\fs20")
I_O_period_1_rep<-paste("\\b","\\fs20", T1)
I_O_period_2_rep<-paste("\\b","\\fs20", T2)
chapter1<-"\\b\\fs24 I. DATA INPUT \\b0\\fs20"
chapter2<-"\\b\\fs24 II. FOCAL AREA CHANGES \\b0\\fs20"
chapter3<-"\\b\\fs24 III. MAP OF DISSIMILARITIES FROM FOCAL AREAS RELATIVE TO ZONE/PLANNING UNIT \\b0\\fs20"
chapter4<-"\\b\\fs24 IV. DEGREE OF INTEGRATION OF FOCAL AREA (DIFA) \\b0\\fs20"
chapter5<-"\\b\\fs24 V. HABITAT CHANGE ANALYSIS \\b0\\fs20"
chapter6<-"\\b\\fs24 VI. HABITAT QUALITY COMPARISON \\b0\\fs20"
chapter7<-"\\b\\fs24 VII. TECI ZONAL STATISTICS \\b0\\fs20"

#==== Report I. DATA INPUT ====
rtffile <- RTF("LUMENS_QUES-B_report.lpr", font.size=9)
addParagraph(rtffile, title)
addParagraph(rtffile, sub_title)
addNewLine(rtffile)
addParagraph(rtffile, line)
addParagraph(rtffile, date)
addParagraph(rtffile, time_start)
addParagraph(rtffile, time_end)
addParagraph(rtffile, time_end)
addParagraph(rtffile, line)
addNewLine(rtffile)
addParagraph(rtffile, chapter1)
addNewLine(rtffile)

text <- paste("\\b \\fs20 Land Use Map of\\b0 \\fs20 ", area_name_rep, I_O_period_1_rep, sep=" ")
addParagraph(rtffile, text)
addNewLine(rtffile, n=1)
addPlot.RTF(rtffile, plot.fun=plot, width=6.4, height=4, res=150, plot.LU1 )
#rm(plot.LU1)
text <- paste("\\b \\fs20 Land Use Map of\\b0 \\fs20 ", area_name_rep, I_O_period_2_rep, sep=" ")
addParagraph(rtffile, text)
addNewLine(rtffile, n=1)
addPlot.RTF(rtffile, plot.fun=plot, width=6.7, height=4, res=150, plot.LU2 )
#rm(plot.LU2)
text <- paste("\\b \\fs20 Zone Map of\\b0 \\fs20 ", area_name_rep, sep=" ")
addParagraph(rtffile, text)
addNewLine(rtffile, n=1)
addPlot.RTF(rtffile, plot.fun=plot, width=6.7, height=4, res=150, plot.Z )
#rm(plot.Z)
addNewLine(rtffile, n=1)
addNewLine(rtffile, n=1)
addNewLine(rtffile, n=1)
addNewLine(rtffile, n=1)

#==== Report II. Focal Area Changes ====
addParagraph(rtffile, chapter2)

if(maxValue(chk_loss)>0)
{
  addNewLine(rtffile)
  text <- paste("\\b \\fs20 Focal Area Change Map of \\b0 \\fs20 ", area_name_rep, I_O_period_1_rep, "\\b \\fs20 - \\b0 \\fs20 ", I_O_period_2_rep,  sep="")
  addParagraph(rtffile, text)
  addPlot.RTF(rtffile, plot.fun=plot, width=6.7, height=4, res=150, plot.FAC.loss )
  addNewLine(rtffile, n=1)
  text <- paste("\\b \\fs20 Subsequent land uses following focal area loss in \\b0 \\fs20 ",area_name_rep, I_O_period_1_rep,I_O_period_2_rep,  sep=" ")
  addParagraph(rtffile, text)
  addTable(rtffile, foc.area.loss.att)
  addParagraph(rtffile, "\\b \\fs20 *Area in Hectares Unit ; Proportion in Percentage (%) \\b0 \\fs20 ")
  addNewLine(rtffile, n=1)
} else {print("No habitat loss found")}

if(maxValue(chk_gain)>0)
{
  addNewLine(rtffile)
  text <- paste("\\b \\fs20 Focal Area Change Map of \\b0 \\fs20 ", area_name_rep, I_O_period_1_rep, "\\b \\fs20 - \\b0 \\fs20 ", I_O_period_2_rep,  sep="")
  addParagraph(rtffile, text)
  addPlot.RTF(rtffile, plot.fun=plot, width=6.7, height=4, res=150, plot.FAC.gain )
  addNewLine(rtffile, n=1)
  text <- paste("\\b \\fs20 Subsequent land uses following focal area gain in \\b0 \\fs20 ",area_name_rep, I_O_period_1_rep,'\\b \\fs20 - \\b0 \\fs20',I_O_period_2_rep,  sep=" ")
  addParagraph(rtffile, text)
  addTable(rtffile, foc.area.gain.att)
  addParagraph(rtffile, "\\b \\fs20 *Area in Hectares Unit ; Proportion in Percentage (%) \\b0 \\fs20 ")
  addNewLine(rtffile, n=1)
} else {print("No habitat gain found")}

text <- paste("\\b \\fs20 Focal Area Change by Zone/Planning Unit  in \\b0 \\fs20 ",area_name_rep, I_O_period_1_rep,'\\b \\fs20 - \\b0 \\fs20',I_O_period_2_rep,  sep=" ")
addParagraph(rtffile, text)
addTable(rtffile, zstat.foc.area)
addParagraph(rtffile, "\\b \\fs20 *Area in Hectares Unit ; Proportion in Percentage (%) \\b0 \\fs20 ")
addNewLine(rtffile, n=1)
text <- paste("\\b \\fs20 Focal area class metrics \\b0 \\fs20 ")
addParagraph(rtffile, text)
addTable(rtffile, foc.area.stats, row.names=TRUE)
addNewLine(rtffile, n=1)

#==== Report III.  Map of dissimilarities from focal area====
addParagraph(rtffile, chapter3)
addNewLine(rtffile)
text <- paste("\\b \\fs20 Map of dissimilarities from focal areas in \\b0 \\fs20 ",area_name_rep, I_O_period_1_rep,'relative to Protected Areas', sep="")
addParagraph(rtffile, text)
addPlot.RTF(rtffile, plot.fun=plot, width=6.7, height=4, res=150, plot.mw.init )
addNewLine(rtffile, n=1)
text <- paste("\\b \\fs20 Map of dissimilarities from focal areas in \\b0 \\fs20 ",area_name_rep, I_O_period_2_rep,'relative to Protected Areas',sep="")
addParagraph(rtffile, text)
addPlot.RTF(rtffile, plot.fun=plot, width=6.7, height=4, res=150, plot.mw.fin )
addNewLine(rtffile, n=1)

#==== Report IV.  DIFA Chart ====
addParagraph(rtffile, chapter4)
addNewLine(rtffile)
text <- paste("\\b \\fs20 Degree of Integration of Focal Area (DIFA) \\b0 \\fs20 ")
addParagraph(rtffile, text)
addPlot.RTF(rtffile, plot.fun=print, width=6.7, height=3, res=150, grid.arrange(difa.init, difa.final, ncol=2))
addNewLine(rtffile, n=1)
text <- paste("\\b \\fs20 Area Under Curve: \\b0 \\fs20 ")
addParagraph(rtffile, text)
text <- paste(I_O_period_1_rep, " : ", AUC.init, "%", "          ;    " ,I_O_period_2_rep, " : ", AUC.final, "%", sep=" ")
addParagraph(rtffile, text)
addNewLine(rtffile, n=1)

#==== Report V.  Habitat Change Analysis ====
addParagraph(rtffile, chapter5)
addNewLine(rtffile)
tryCatch({
  text <- paste("\\b \\fs20 1.Habitat degradation \\b0 \\fs20 ",area_name_rep, I_O_period_1_rep, "\\b \\fs20 - \\b0 \\fs20 ", I_O_period_2_rep,  sep="")
  addParagraph(rtffile, text)
  addPlot.RTF(rtffile, plot.fun=print, width=6.7, height=3, res=150, plot.HD)
  addNewLine(rtffile, n=1)
  text <- paste("\\b \\fs20 Habitat degradation due to LULCC \\b0 \\fs20 ", sep="")
  addParagraph(rtffile, text)
  addTable(rtffile, luchg.db.degrad[1:10,])
  #addNewLine(rtffile, n=1)
  #text <- paste("\\b \\fs20 Habitat degradation due to neighboring focal area change \\b0 \\fs20 ", sep="")
  #addParagraph(rtffile, text)
  # addTable(rtffile, luchg.degradation.10.no.change)
  #addNewLine(rtffile, n=1)
  #text <- paste("\\b \\fs20 Habitat degradation of focal area due to neighboring changes \\b0 \\fs20 ", sep="")
  #addParagraph(rtffile, text)
  #addTable(rtffile, luchg.degradation.focal.area)
  addNewLine(rtffile, n=1)
},error=function(e){cat("skipping habitat degradation plot :",conditionMessage(e), "\n")})
addNewLine(rtffile, n=1)

tryCatch({
  if(maxValue(chk_loss)>0)
  {
    text <- paste("\\b \\fs20 2.Habitat loss \\b0 \\fs20 ",area_name_rep, I_O_period_1_rep, "\\b \\fs20 - \\b0 \\fs20 ", I_O_period_2_rep,  sep="")
    addParagraph(rtffile, text)
    addPlot.RTF(rtffile, plot.fun=print, width=6.7, height=3, res=150, plot.HL)
    addNewLine(rtffile, n=1)
    text <- paste("\\b \\fs20 Top 10 habitat loss due to focal area loss \\b0 \\fs20 ", sep="")
    addParagraph(rtffile, text)
    addTable(rtffile, luchg.db.loss[1:10,])
    addNewLine(rtffile, n=1)
    #text <- paste("\\b \\fs20 Habitat loss statistics by zone/planning unit \\b0 \\fs20 ", sep="")
    #addParagraph(rtffile, text)
    #addTable(rtffile, zstat.habitat.loss.NA)
    #addNewLine(rtffile, n=1)
  } else {print("No habitat loss found")}
},error=function(e){cat("skipping habitat loss plot :",conditionMessage(e), "\n")})
addNewLine(rtffile, n=1)

tryCatch({
  text <- paste("\\b \\fs20 3.Habitat recovery \\b0 \\fs20 ",area_name_rep, I_O_period_1_rep, "\\b \\fs20 - \\b0 \\fs20 ", I_O_period_2_rep,  sep="")
  addParagraph(rtffile, text)
  addPlot.RTF(rtffile, plot.fun=print, width=6.7, height=3, res=150, plot.HR)
  addNewLine(rtffile, n=1)
  text <- paste("\\b \\fs20 Habitat recovery due to LULCC  \\b0 \\fs20 ", sep="")
  addParagraph(rtffile, text)
  addTable(rtffile, luchg.db.recovery[1:10,])
  addNewLine(rtffile, n=1)
  #text <- paste("\\b \\fs20 Habitat recovery due to neighboring focal area change \\b0 \\fs20 ", sep="")
  #addParagraph(rtffile, text)
  #addTable(rtffile, luchg.recovery.10.no.change)
  #addNewLine(rtffile, n=1)
  #text <- paste("\\b \\fs20 Habitat recovery of focal area due to neighboring changes \\b0 \\fs20 ", sep="")
  #addParagraph(rtffile, text)
  #addTable(rtffile, luchg.recovery.focal.area)
  #addNewLine(rtffile, n=1)
},error=function(e){cat("skipping habitat recovery plot :",conditionMessage(e), "\n")})
addNewLine(rtffile, n=1)

tryCatch({
  if(maxValue(chk_gain)>0)
  {
    text <- paste("\\b \\fs20 4.Habitat gain \\b0 \\fs20 ",area_name_rep, I_O_period_1_rep, "\\b \\fs20 - \\b0 \\fs20 ", I_O_period_2_rep,  sep="")
    addParagraph(rtffile, text)
    if (maxValue(chk_gain)>0) {
      addPlot.RTF(rtffile, plot.fun=print, width=6.7, height=3, res=150, plot.HG)
    } else {print('skipping habitat gain plot ')}
    addNewLine(rtffile, n=1)
    text <- paste("\\b \\fs20 Top 10 habitat gain due adjacent focal area loss \\b0 \\fs20 ", sep="")
    addParagraph(rtffile, text)
    addTable(rtffile, lluchg.db.gain[1:10,])
    #addNewLine(rtffile, n=1)
    #text <- paste("\\b \\fs20 Habitat gain statistics by zone/planning unit \\b0 \\fs20 ", sep="")
    #addParagraph(rtffile, text)
    #addTable(rtffile, zstat.habitat.gain.NA)
    #addNewLine(rtffile, n=1)
  } else {print("No habitat gain found")}
},error=function(e){cat("skipping habitat gain plot :",conditionMessage(e), "\n")})

#==== XXX Report VI.  Habitat Quality Comparison XXX ====
#addParagraph(rtffile, chapter6)
addNewLine(rtffile)
#tryCatch({
#text <- paste("\\b \\fs20 Habitat Quality Comparison in \\b0 \\fs20 ",area_name_rep, I_O_period_1_rep, "\\b \\fs20 - \\b0 \\fs20 ", I_O_period_2_rep,  sep="")
#addParagraph(rtffile, text)
#addPlot.RTF(rtffile, plot.fun=print, width=6.7, height=4, res=150, grid.arrange(plot.hb.chg.init, plot.hb.chg.final, ncol=2) )
#addNewLine(rtffile, n=1)
#addTable(rtffile, habitat.change)
#addNewLine(rtffile, n=1)
#},error=function(e){cat("skipping Habitat Quality Comparison analysis:",conditionMessage(e), "\n")})
#addNewLine(rtffile, n=1)

#==== Report VI.  TECI Zonal Statistics ====

addParagraph(rtffile, chapter7)
addNewLine(rtffile)
tryCatch({
  text <- paste("\\b \\fs20 Habitat degradation zonal statistics \\b0 \\fs20 ",area_name_rep, I_O_period_1_rep, "\\b \\fs20 - \\b0 \\fs20 ", I_O_period_2_rep,  sep="")
  addParagraph(rtffile, text)
  addTable(rtffile, zstat.habitat.degradation)
  addParagraph(rtffile, "\\b \\fs20 *max, min, mean, and sd are total edge contrast index value representing habitat loss and degradation degree \\b0 \\fs20 ")
  addParagraph(rtffile, "\\b \\fs20 *foc.area or total focal area in is Hectare unit \\b0 \\fs20 ")
  addNewLine(rtffile, n=1)
  addNewLine(rtffile, n=1)
  
  text <- paste("\\b \\fs20 Habitat recovery zonal statistics \\b0 \\fs20 ",area_name_rep, I_O_period_1_rep, "\\b \\fs20 - \\b0 \\fs20 ", I_O_period_2_rep,  sep="")
  addParagraph(rtffile, text)
  addTable(rtffile, zstat.habitat.recovery)
  addParagraph(rtffile, "\\b \\fs20 *max, min, mean, and sd are total edge contrast index value representing habitat gain and recovery degree \\b0 \\fs20 ")
  addParagraph(rtffile, "\\b \\fs20 *foc.area or total focal area is in Hectare unit \\b0 \\fs20 ")
  addNewLine(rtffile, n=1)
},error=function(e){cat("TECI Zonal statistics analysis :",conditionMessage(e), "\n")})
addNewLine(rtffile, n=1)
done(rtffile)

tryCatch({
  dbase.preques.name<-paste("QuES_B_database_", location,'_',T1,'_',T2,'.ldbase', sep='')
  save(lu1,lu2, zone, lookup_bh, lookup_z, T1, T2, location, mwfile.init,mwfile.final,habitat.degradation,habitat.loss.NA,habitat.gain.NA, habitat.recovery,file=dbase.preques.name)
},error=function(e){cat("QuES-B database production is failed, re-check your data :",conditionMessage(e), "\n")})

#====Land use change database export====
#QUESB.index=QUESB.index+1
eval(parse(text=(paste("QuES_B_data_", data[1,2], "_", data[2,2], "<-data", sep=""   ))))
newPre<-paste("QuES_B_data_", data[1,2], "_", data[2,2], sep="")

# DIFA TABLE INIT
QuESB_DIFA_init=paste( "_",pu_name,"_",data[1,2], sep="")
eval(parse(text=(paste("QuES_B_DIFA_table", QuESB_DIFA_init, "<-difa.table.init", sep=""   ))))
object_DIFAinit<-paste("QuES_B_DIFA_table", QuESB_DIFA_init, sep="")

#DIFA TABLE FINAL
QuESB_DIFA_final=paste( "_",pu_name,"_",data[2,2], sep="")
eval(parse(text=(paste("QuES_B_DIFA_table", QuESB_DIFA_final, "<-difa.table.final", sep=""   ))))
object_DIFAfinal<-paste("QuES_B_DIFA_table", QuESB_DIFA_final, sep="")

#AUC INIT
QuESB_AUC_init=paste( "_",pu_name,"_",data[1,2], sep="")
eval(parse(text=(paste("QuES_B_AUC", QuESB_AUC_init, "<-AUC.init", sep=""   ))))
object_AUC_init<-paste("QuES_B_AUC", QuESB_AUC_init, sep="")

#AUC FINAL
QuESB_AUC_final=paste( "_",pu_name,"_",data[2,2], sep="")
eval(parse(text=(paste("QuES_B_AUC", QuESB_AUC_final, "<-AUC.final", sep=""   ))))
object_AUC_final<-paste("QuES_B_AUC", QuESB_AUC_final, sep="")

command<-paste("resave(QUESB.index,",object_DIFAinit,",", newPre, ",",object_DIFAfinal, ",",object_AUC_init,",",object_AUC_final,",",  sep="")

setwd(dirname(proj.file))
command<-paste(command,"file='",basename(proj.file),"')", sep="")
eval(parse(text=(command)))

#====write LUMENS log file====
add.log<-data.frame(IDX=(QUESB.index), 
                    MODULE="QuES-B", 
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
                    GRIDRES=gridres,
                    WINDOWSIZE=windowsize,
                    WINDOWSHAPE=window.shape,
                    CLASSDESC=classdesc,
                    EDGECON=edgecon,
                    OUTPUT_FOLDER=result_dir, row.names=NULL)
log.quesb<-na.omit(rbind(log.quesb,add.log))
write.csv(log.quesb, paste(user_path,"/LUMENS/LUMENS_quesb.log", sep=""))