#Script for calculating population pressure across scales
#This script imports hight resolution coastline,reprojects the high resolution coastline to UTM for
#defination of buffer width in mtrs and later reprojects it back to CRS-WGS-84 for clipping
#Requires:
library(here)
library(dplyr)
library(rgdal)
library(tidyverse)
library(raster)
library(rgeos)

knitr::opts_chunk$set(message=F,warning=F, fig.width = 8, fig.height = 6,strip.white=TRUE)

options(scipen = 999) #this forces reporting in non scientific notation
##############################################################################################


#read in the high resolution coastline
setwd(here::here('prep/pressures/Demand for fish_25miles/'))

coastline<-readOGR("prep/pressures/Demand for fish_25miles/wio_coastline.shp")  #navigates from ken folder

plot(coastline,col="red", main="WIO High resolution")

#Crea buffer, requires projected crs to work with meters you can alter the width of the buffer
#by defining the width values in meters)

require(rgeos)#for reprjecting

#Convert to projected
coastline_utm<-spTransform(coastline,CRS("+init=epsg:32737 +proj=utm +zone=37 +south +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"))

coast_25mile_buffer<-gBuffer(coastline_utm, width = 41038.27)
plot(coast_25mile_buffer,col="transparent",main="WIO 25 mile buffer")

#for use in clipping, we need to re-project buffer back to WGS 84 to enhance clip
coastline_buffer_wgs84<-spTransform(coast_25mile_buffer,CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))

#Import raster dataset i.e. "GPW_V4 1km population 2015 in this case"

gpwv4_2015<-raster("//Brahmspc/cordio office 2018/OHI_Spatial_data/population/gpw-v4-population-count_2015.tif")#Define your own path

#Crop and mask raster-use masking. Mask layer = to 25 mile bufferzone

gpwv4_2015_cropped<-crop(gpwv4_2015,coastline_buffer_wgs84)

gpwv4_2015_cropped <- mask(gpwv4_2015_cropped, coastline_buffer_wgs84)

#Remove null values from the masked layer
trim(gpwv4_2015_cropped, values = NA)

#preview-optional
plot(gpwv4_2015_cropped)
plot(coastline_buffer_wgs84, add=TRUE)
##################################################################################################
#Get regional data
#There are various ways of getting the data you need for each of your subregions.
#i.e. using raster::extract() and raster::zonal().extract()

# To get all values within each region-need to import ohi regions-rgn

rgn<-readOGR("prep/pressures/Demand for fish_25miles/TZA_OHI_Regions.shp")


vals = extract(gpwv4_2015_cropped,rgn,method='simple')%>%
  setNames(rgn@data$rgn_name)

# plot distribution of data per region

df <- data.frame(unlist(vals))%>%
  rename(value = unlist.vals.)%>%
  mutate(rgn_name = gsub("\\d+", "",row.names(.))) #the gsub here removes all numbers from the rgn_name

#now we have each value assigned to rgn_name.
head(df)


#Add new col to data frame and populate it with year

df["year"]<-2015

#################################################################################################
#use ddply to get total population for each region in 2015

total_df <- df%>%
  group_by(rgn_name)%>%
  summarise(total_pop2015 = sum(value,na.rm=T))

total_df


#create a directory "total population" and export population data to csv
#dir.create(file.path('D:/git/ken/prep/pressures/Population/','Extracted_regional_value _csv'), showWarnings = FALSE) #creates new sub folder


write.csv(total_df,'/prep/pressures/Demand for fish_25miles/Extracted_regional_value _csv/2015_human_pop_count_at_25miles_buffer.csv',row.names = F)
write.

#Normalising the population

library(here)
#steps:
#assign the correct county (maybe under sector) to each site
setwd(here::here('prep/pressures/Demand for fish_25miles/'))

tza_rgn_pop_25miles<-read.csv("Extracted_regional_value _csv/fp_fish_demand_tz2018.csv",header = T,stringsAsFactors = F)

tot_pop_25mile<-sum(tza_rgn_pop_25miles$pop_count)

#normalised pop
tza_rgn_pop_25miles$pressure_score<-(tza_rgn_pop_25miles$pop_count/sum(tza_rgn_pop_25miles$pop_count))
cal_status <- tza_rgn_pop_25miles[c(1,2,3,5)]

cal_status<-cal_status[order(cal_status$rgn_id),]

write.csv(cal_status,"Extracted_regional_value _csv/hd_intertidal_tan2018.csv",row.names = F)


