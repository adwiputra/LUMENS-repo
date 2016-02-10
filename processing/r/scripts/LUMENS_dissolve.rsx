##[LUMENS]=group
##admin_data=vector
##field_attribute=field admin_data
##admin_output=output vector

library(rgeos)

#create temp LUT
admin_df<-data.frame()
admin_df<-rbind(admin_df, admin_data@data)

#select unique attribute data
field_attribute<-as.character(field_attribute)
eval(parse(text=(paste("admin_df<-as.data.frame(admin_df$", field_attribute, ")", sep=""))))
admin_df[,1]<-as.character(admin_df[,1])
admin_df<-as.data.frame(unique(admin_df[,1]))
colnames(admin_df)=field_attribute

#add new field
admin_df$IDADM<-1:nrow(admin_df)

#merge LUT into polygon
eval(parse(text=(paste("admin_data@data$", field_attribute, "<-as.character(admin_data@data$", field_attribute, ")", sep=""))))
admin_data@data<-merge(admin_data@data, admin_df, by=field_attribute)

#match the polygon IDs, ensure shapefile row.names and polygon IDs are sensible
row.names(admin_data)<-row.names(admin_data@data)
admin_data<-spChFIDs(admin_data, row.names(admin_data))

#now dissolve
eval(parse(text=(paste("admin_data<-gUnaryUnion(admin_data, id=admin_data@data$", field_attribute, ")", sep=""))))

#and add the data back in
row.names(admin_data) <- as.character(1:length(admin_data))
admin_data <- SpatialPolygonsDataFrame(admin_data, admin_df)

admin_output <- admin_data
#writeOGR(admin_data, '.', "jypr_admin_data", driver="ESRI Shapefile", overwrite=TRUE)
#plot(admin_data)

