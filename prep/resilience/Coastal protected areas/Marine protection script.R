#Objective. Dileneating MPA coverage for each region to quantify the level of marine protection within
#12nm
library(here)
#steps:
setwd(here::here('prep/resilience/Coastal protected areas/'))

# dir.create(file.path('data layers_noramlised'), showWarnings = FALSE) #creates new sub folder-optional

#Define projection-we will use the wgs84 coordinate reference system. You need Rgdal and raster
#required libraries
library(raster)#Compements shapefile manipulation
library(rgdal)#Complementaly use
library(rgeos)#for later use in shapefile manipulation-is intelligent with spatial and attribute data
library(sf)#complementaly
###############################################################################################################
#Define CRS- we use projected CRS since we will cal area and perfom spatial joins
wgs_crs <- CRS("+init=epsg:32737 +proj=utm +zone=37 +south +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0")

###############################################################################################################
##Import shapefiles MPA shapefie

mpa_wio<-readOGR("MPAs/WIO_M_PAs.shp")#note short file path fails

plot(mpa_wio, col="cyan1", border="blue", lwd=0.5, main="WIO MPAs")#optional

#Import the OHI regions-(12nm eez boundaries)

eez_rgn<-readOGR("MPAs/Tanzania_ohi_eez_zones.shp")

plot(eez_rgn, col="transparent", border="green", lwd=0.1, main="WIO MPAs & 12nm regions",add=TRUE)# optional

###############################################################################################################

##Intersect MPA layer with 12 nm ohi region

mpa_ohi_12nm<-raster::intersect(eez_rgn, mpa_wio)

#view-optional
plot(mpa_ohi_12nm, col = "red", border = "blue",add=TRUE)

###############################################################################################################

#Note-to calc area you need to convert your shapefiles CRS to projected if not already in this format
#E.g covert OHI region crs e.g covert_ohieez<-spTransform(eez_rgn,CRS("+init=epsg:32737 +proj=utm +zone=37 +south +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"))

##Calculate area of protected areas and coastal regions,(interesected) and attach to polygons

mpa_ohi_12nm@data$area_km2 <- gArea(mpa_ohi_12nm, byid = TRUE) / 1e6
eez_rgn@data$area_km2 <- gArea(eez_rgn, byid = TRUE) / 1e6

### Summarize the total protected area within each region-Requres dplyr

library(dplyr)

prot_area_df <- mpa_ohi_12nm@data %>%
  group_by(Region_ID, rgn_name) %>%
  summarize(prot_area_km2 = sum(area_km2)) %>%
  left_join(eez_rgn@data %>%
              select(Region_ID, tot_area_km2 = area_km2),
            by = 'Region_ID') %>%
  mutate(prot_area_pct = round(prot_area_km2 / tot_area_km2, 3) * 100)

# pretty static table
knitr::kable(prot_area_df)

prot_area_df$resilience_score<-round(prot_area_df$prot_area_pct/30,3)

prot_area_df$year<-'2015'

#Export data as regional raw
write.csv(prot_area_df,"Extracted_regional_value _csv/Total_MPA_region.csv",row.names = F)

prot_area_df1<-prot_area_df[,c(1,7,6)]

write.csv(prot_area_df1,"Extracted_regional_value _csv/hd_mpa_coast_tan2018.csv",row.names = F)




