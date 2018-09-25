#Working with Rasters...Plotting and clipping Rquires- rgdal and raster R libraries for this to work

knitr::opts_chunk$set(message=F,warning=F, fig.width = 8, fig.height = 6,strip.white=TRUE)

options(scipen = 999) #this forces reporting in non scientific notation
##############################################################################################
library(here)   #install.packages('here')
library(rgdal)
library(tidyverse)
library(raster)

setwd(here::here('prep/pressures/Nutrient pollution/'))

#Defining the wgs84 coordinate reference system
wgs_crs <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")

#.........Set regions of interest..........#

par(mfrow=c(1,2)) #set plotting window to show 2 plots side by side

# setwd(here::here('Chemical pollution'))   #setwd to a file path we all have - doesn't work, cant change wd

#read in the ROI shapefile and reproject to Mollweide
rgn  = readOGR('D:/git/zspatial_extent/Tanzania_ohi_eez_zones.shp')  #navigates from ken folder

plot(rgn,main = "TZA OHI regions \n WGS84 projection")

#define the mollweide projection coordinate reference system (crs)
mollCRS=CRS('+proj=moll +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +units=m +no_defs')

#reproject the shapefile to mollweide using spTransform
rgn_moll = spTransform(rgn, mollCRS)

#plot
#plot(rgn_moll, main = "TZA OHI regions \n Mollweide projection")



#########################################################################################
#...Bring in the your RASTER....#

#want to create a loop that repeats all these steps for each year of global data

#NOTE: MUST ALTER FOLDER NAME BASED ON WHERE GLOBAL TIF FILES ARE SAVED - SUBSTITUTE 'global tif' in lines below with folder name
#reason for this is global files are too large to upload onto github so each person will have it located in different folder

f <- list.files('D:/git/zrasters/nutrients/',pattern=".tif")     #lists all files in target folder with .tif in file name

for (i in 1:length(f)){

  myraster <- raster(paste('D:/git/zrasters/nutrients/',f[i],sep=""))

  # plot(myraster,main="Define your title",axes=F, legend.args=list(text='define your legend', side=4, font=2, line=2.5, cex=0.8))
  # plot(rgn_moll,add=T)

  ########################################################################################
  #Crop global data to your region

  myraster_crop <- crop(myraster,rgn_moll)

  # plot(myraster_crop,axes=F,
  #      legend.args=list(text='Define your text', side=4, font=2, line=2.5, cex=0.8))
  # plot(rgn_moll,add=T)
  ##################################################################################################
  #Get regional data
  #There are various ways of getting the data you might for each of your subregions.
  #Here we provide two ways of getting the average number of anomalous weeks per subregion
  #using raster::extract() and raster::zonal().extract()

  # get all values within each region
  vals = extract(myraster_crop,rgn_moll,method='simple')%>%
    setNames(rgn_moll@data$rgn_name)

  # plot distribution of data per region

  df <- data.frame(unlist(vals))%>%
    rename(value = unlist.vals.)%>%
    mutate(rgn_name = gsub("\\d+", "",row.names(.))) #the gsub here removes all numbers from the rgn_name

  #now we have each value assigned to rgn_name.
  head(df)


  #Add new col to data frame

  df['year']<-gsub(".*[_]([^.]+)[.].*", "\\1", f[i])    #keep only the year from the file name


  #########export csv
  #dir.create(file.path('./prep/pressures/Chemical pollution/', 'data layers'), showWarnings = FALSE) #creates new sub folder

  write.csv(df, file = paste('D:/git/tza/prep/pressures/Nutrient pollution/Extracted_regional_value _csv//',gsub("\\..*","",f[i]),'.csv',sep=""))   #removes the .tif from file name and replaces with .csv

  if (i==1){
    regional_values<-df
  }else{
    regional_values<-rbind(regional_values,df)
  }

}

write.csv(regional_values,"D:/git/tza/prep/pressures/Nutrient pollution/Extracted_regional_value _csv/tanzania_all_values_per_region_all_years.csv",row.names = F)

#################################################################################################
#use ddply to get average chemical pollution levels across years for each region
library(plyr)

#first step - yearly average per region
yearly_scores<-ddply(regional_values,c("rgn_name","year"),summarise,
                     pressure_score=round(mean(value,na.rm=T),3))

yearly_scores$rgn_id<-NA
yearly_scores$rgn_id[which(yearly_scores$rgn_name=='EEZ')]<-1
yearly_scores$rgn_id[which(yearly_scores$rgn_name=='Pwani')]<-2
yearly_scores$rgn_id[which(yearly_scores$rgn_name=='Dar es Salaam')]<-3
yearly_scores$rgn_id[which(yearly_scores$rgn_name=='Mtwara')]<-4
yearly_scores$rgn_id[which(yearly_scores$rgn_name=='Lindi')]<-5
yearly_scores$rgn_id[which(yearly_scores$rgn_name=='Tanga')]<-6
yearly_scores$rgn_id[which(yearly_scores$rgn_name=='Zanzibar')]<-7
yearly_scores$rgn_id[which(yearly_scores$rgn_name=='Pemba')]<-8

yearly_scores2<-yearly_scores[,c(4,2,3)]

write.csv(yearly_scores2,"D:/git/tza/prep/pressures/Nutrient pollution/data layers/po_Nutrient_12nm_tan2018.csv",row.names = F)


