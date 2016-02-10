library(vegan)
library(reshapeGUI)


# Workflow:
#1. Input data dan penghitungan basal area setiap ind
#2. Reshape table
#3. Analisis vegdist
#4. Presentation

#1
raw_data <- read.csv("D:/LUMENS/QUES B Intro/Data-Sumsel.csv", sep = ",")


#defining standardized column. names
colnames(raw_data) <- c("District", "Sub.District", "Village","Site",
                        "Date", "Kode.Plot", "System","replication","Age",
                        "Latitude", "Longitude", "Sub.plot.size",
                        "Area", "Sp.code", "local.name", "sp.name", "plot.unit", "girth",
                        "diameter","height","density","indv")

#removing null record
raw_data <- raw_data[raw_data$indv != 0,]


#calculating basal area for each species in each LU Syst.

raw_data$bas_area <- pi*(raw_data$diameter/2)^2


#2
#producing table with cnames: "System"  "sp.name" "variable" "value"
mraw.dat <- melt(raw_data, id.vars = c("System","sp.name"), measure.vars = "bas_area")

#producing table which is ready to be calculated with reshapeGUI()
mraw.dat.cast <- dcast(data = mraw.dat, formula = System ~ sp.name, fun.aggregate = sum)
spec.bas.syst <- mraw.dat.cast[,2:ncol(mraw.dat.cast)]
row.names(spec.bas.syst) <- mraw.dat.cast$System

#3
#calculating Morisita-Horn similarity indices among different land use syst.
horn.ind <- vegdist(spec.bas.syst, method = "horn")

#4
#convert 
