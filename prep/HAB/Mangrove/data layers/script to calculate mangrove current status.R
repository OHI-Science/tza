#Objective-Calculating mangrove current status

library(here)
#steps:
#assign the correct county (maybe under sector) to each site
setwd(here::here('prep/HAB/Mangrove/'))


library(dplyr)
csv2014<-read.csv("Extracted_regional_value _csv/TZAmangrove_2014.csv")

csv2000<-read.csv("Extracted_regional_value _csv/TZAmangrove_2000.csv")
#change header name
names(csv2000)[5]<-paste("ref year2000")

#Columnbind based on a common field
datamerged<-merge(csv2014, csv2000, by="rgn_name")

#Calculating curent status
datamerged$health<-(datamerged$area_km2/datamerged$`ref year2000`)


cal_status <- datamerged[c(2,3,4,10)]
# cal_status["habitat"]<-"mangrove"
colnames(cal_status)[1]<-'rgn_id'
colnames(cal_status)[2]<-'habitat'
colnames(cal_status)[3]<-'year'

cal_status<-cal_status[order(cal_status$rgn_id),]

#Export calculated data

write.csv(cal_status,"data layers/hab_mangrove_health_tan2018.csv",row.names = F)
