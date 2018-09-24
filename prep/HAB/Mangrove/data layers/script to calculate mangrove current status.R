#Objective-Calculating mangrove current status

library(dplyr)
csv2014<-read.csv("D:/git/tza/prep/HAB/Mangrove/Extracted_regional_value _csv/TZAmangrove_2014.csv")

csv2000<-read.csv("D:/git/tza/prep/HAB/Mangrove/Extracted_regional_value _csv/TZAmangrove_2000.csv")
#change header name
names(csv2000)[5]<-paste("ref year2000")

#Columnbind based on a common field
datamerged<-merge(csv2014, csv2000, by="rgn_name")

#Calculating curent status
datamerged$C_status<-(datamerged$area_km2/datamerged$`ref year2000`)


cal_status <- datamerged[c(1,2,4,10)]
cal_status["habitat"]<-"mangrove"

#Export calculated data

write.csv(cal_status,"D:/git/tza/prep/HAB/Mangrove/data layers/hab_mangrove_health_ken2018.csv")
