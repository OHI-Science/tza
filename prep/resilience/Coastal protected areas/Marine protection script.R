
#Calculating mpa areas from shapefiles attributes
#Files fetched from DBF s in GIS folders

# TNT tools (Foreign package)
library(here)
library(foreign)
library(dplyr)
#Browsing file without memorising path etc
#myfile<-choose.files()
#Reading file-This section will be editied to loop so as to work on severla items at ago
library(raster)
mydata<-read.dbf("D:/git/zrasters/Resilience/tzn_mpa_intersect.dbf")

#VIewing your data
head(mydata)
#SElect data columns from dbf
total_MPA_region <- mydata[c(7,8,12)]
#View mydata95
mydata
write.csv(total_MPA_region, "D:/git/tza/prep/resilience/Coastal protected areas/Extracted_regional_value _csv/Total_MPA_region.csv")





