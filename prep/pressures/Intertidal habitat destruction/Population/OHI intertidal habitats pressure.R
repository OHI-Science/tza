#Script for dileneating human population count from the Gridded Population of the World (GPW), v4 across scales
#Requires
library(here)   #install.packages('here')
library(rgdal)
library(tidyverse)
library(raster)
library(rgeos)
library(dplyr)#for summarising data
##############################################################################################
#Import high resolution coastline
coastline<-readOGR("D:/git/zspatial_extent/wio_coastline.shp")  #navigates from ken folder

plot(coastline,col="red", main="WIO High resolution")#optional

#Creat buffer, this step requires that the shapefile be in projected crs (remember to check crs)

#Convert coastline projection to projected incase its not already in UTM
coastline_utm<-spTransform(coastline,CRS("+init=epsg:32737 +proj=utm +zone=37 +south +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"))

#Define buffer width
coast_5mile_buffer<-gBuffer(coastline_utm, width = 8046.72)

#reproject back to wgs-84
coastline_buffer_wgs84<-spTransform(coast_5mile_buffer,CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))

#Import raster dataset i.e. "GPW_V4 1km population 2015 in this case"

gpwv4_2015<-raster("D:/1 CORDIO GIS/2.1 Human Geography/Population/gpw-v4-population-count_2015.tif")#Define your own path
plot(gpwv4_2015)
#Crop and mask raster-use masking. Mask layer = to 5 mile bufferzone
#Can i combine line 32 to 37 to one command

gpwv4_2015_cropped<-crop(gpwv4_2015,coastline_buffer_wgs84)

gpwv4_2015_cropped <- mask(gpwv4_2015_cropped, coastline_buffer_wgs84)

#Remove null values from the masked layer
trim(gpwv4_2015_cropped, values = NA)

#preview-optional
plot(gpwv4_2015_cropped)#optional
plot(coastline_buffer_wgs84, add=TRUE)
##################################################################################################

#Get regional data.There are various ways of getting the data you need for each of your subregions.
#i.e. using raster::extract() and raster::zonal().extract()

# To get all values within each region-need to import ohi admin regions-rgn.This should be the land admin x

rgn<-readOGR("D:/git/zspatial_extent/TZA_OHI_Regions.shp")
vals = extract(gpwv4_2015_cropped,rgn,method='simple')%>%
  setNames(rgn@data$rgn_name)

# plot distribution of data per region

df <- data.frame(unlist(vals))%>%
  rename(value = unlist.vals.)%>%
  mutate(rgn_name = gsub("\\d+", "",row.names(.))) #the gsub here removes all numbers from the rgn_name

#now we have each value assigned to rgn_name.
head(df)

#################################################################################################

#use ddply to get total population for each region in 2015


total_df <- df%>%
  group_by(rgn_name)%>%
  summarise(total_pop2015 = sum(value,na.rm=T))

# pretty static table
knitr::kable(total_df)


#create a directory "total population" and export population data to csv

dir.create(file.path('./prep/HAB/Pressure/Population/','Extracted_regional_value _csv'), showWarnings = FALSE) #creates new sub folder


write.csv(total_df,"./prep/HAB/Pressure/Population/Extracted_regional_value _csv/2015_human_pop_county_at_5miles_buffer.csv",row.names = F)

#Normalising the population

library(here)
#steps:
#assign the correct county (maybe under sector) to each site
setwd(here::here('prep/pressures/Pressure-Intertidal habitat-5miles/Population/'))

tza_rgn_pop_5miles<-read.csv("Extracted_regional_value _csv/2015_human_pop_count_at_5miles_buffer.csv",header = T,stringsAsFactors = F)


tot_pop_5mile<-sum(tza_rgn_pop_5miles$pop_count)

#normalised pop
tza_rgn_pop_5miles$pressure_score<-(tza_rgn_pop_5miles$pop_count/sum(tza_rgn_pop_5miles$pop_count))

cal_status <- tza_rgn_pop_5miles[c(1,3,5)]

cal_status<-cal_status[order(cal_status$rgn_id),]

write.csv(cal_status,"Extracted_regional_value _csv/hd_intertidal_tan2018.csv",row.names = F)



