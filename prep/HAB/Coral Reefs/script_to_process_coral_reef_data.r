#script will try to develop county level current status, trend and reference points for hard coral cover
library(here)
#steps:
#assign the correct county (maybe under sector) to each site
setwd(here::here('prep/HAB/Coral Reefs'))

tan<-read.csv("Tan_2017_GCRMN_benthic_dataset.csv",header = T,stringsAsFactors = F)


# cleaning sites  ---------------------------------------------------------

unique(tan$Site)
#10 different sites

#change Zanzibar to Unguja
#mafia, rufiji -> pwani
#mtwara -> mtwara
#songosongo -> lindi
#kwale -> unguja
#misali > pemba

f<-unique(tan[c('Site','Station')])
f<-f[order(f$Site),]

tan$Sector[which(tan$Site=='Zanzibar')]<-'Unguja'
tan$Sector[which(tan$Site=='Tanga')]<-'Tanga'
tan$Sector[which(tan$Site=='Mafia')]<-'Pwani'
tan$Sector[which(tan$Site=='Rufiji')]<-'Pwani'

tan$Sector[which(tan$Site=='Mtwara')]<-'Mtwara'
tan$Sector[which(tan$Site=='Songosongo')]<-'Lindi'

tan$Sector[which(tan$Site=='Kwale')]<-'Unguja'
tan$Sector[which(tan$Site=='Misali')]<-'Pemba'
tan$Sector[which(tan$Site=='Pemba')]<-'Pemba'
tan$Sector[which(tan$Site=='Dar_es_Salaam')]<-'Dar_es_Salaam'

unique(tan$Sector)

# ken$Sector[which(ken$Site=='')]<-'Tana River'

#next step - look at temporal range of data per county (sector)
library(plyr)
tmp<-ddply(tan,c("Sector"),summarise,
           start_yr=min(Year),
           latest_yr=max(Year),
           n_years=length(unique(Year)),
           n_sites=length(unique(Station)))

#also look at number of sites for each year
ste<-ddply(tan,c("Sector","Year"),summarise,
           n_sites=length(unique(Station)))

#calculate yearly mean for each county - aggregate across stations

#then for each station we have to sum by benthic_code and then we can aggregate across sectors

# n_test is to make sure that no multiple values per station
station_sum<-ddply(tan,c("Country","Year","Sector","Site","Station","Period","benthic_code"),summarise,
                   mean_cover=sum(cover,na.rm = T),
                   n_test=length(cover))

unique(station_sum$benthic_code[which(station_sum$n_test>1)]) #none/empty - all ok

#only take HC

station_sum<-station_sum[which(station_sum$benthic_code=='HC'),]
station_sum$mean_cover<-round(station_sum$mean_cover,2)


# calculate status --------------------------------------------------------

#some data pre-1998 in GCRMN dataset, need to decide if it is to be considered with pre98 data

pre98_GCRMN<-station_sum[which(station_sum$Period=='Pre'&station_sum$Year<1998),]
unique(pre98_GCRMN$Site) #data just for Zbar

summ_pre98GCRMN<-ddply(pre98_GCRMN,c("Sector","Site","Year"),summarise,
                       ave_cover=mean(mean_cover),
                       sd=sd(mean_cover),
                       n=length(unique(Station)))

#take those years where we had at least 4 stations monitored
summ_pre98GCRMN<-summ_pre98GCRMN[which(summ_pre98GCRMN$n>3),]

#now need to add this to pre98 before we produce ref_values
ref_value_unguja<-ddply(summ_pre98GCRMN,c("Sector"),summarise,
                        ref_cover=mean(ave_cover))

#only pre-98 values to calculate reference values for each site/area
pre98<-station_sum[which(station_sum$Period=='pre-1998'),]

#removing site specific data - use area aggregated data on its own
pre98<-pre98[-c(1),]  #21,22 are two lamu sites

#calculates reference averages for each site
ref_values<-ddply(pre98,c("Sector"),summarise,
                  ref_cover=mean(mean_cover))

#replace the value for Unguja in ref_values with ref_value_unguja

ref_values$ref_cover[ref_values$Sector=='Unguja']<-ref_value_unguja$ref_cover

#now to calculate the current cover values for each site based on values for 3 most recent years
#average over 2-3 years (current levels vs pre-1998)

#step 1 - get the year average for each Sector
site_ave<-ddply(station_sum,c("Sector","Year","benthic_code"),summarise,
                ave_cover=mean(mean_cover),
                n_sites=length(unique(Station)))

#step 2 - take the most recent 3 years of data for each site from site_ave
region_status_3yr<-ddply(site_ave,c("Sector","benthic_code"),summarise,
                         recent_cover=round(tail(ave_cover,n=3),2),
                         recent_Year=tail(Year,n=3),
                         n_site_recent=tail(n_sites,n=3))

#remove too old and erronous years - some regions left with less than 3 years - inconsistent
region_status_3yr<-region_status_3yr[-c(which(region_status_3yr$Sector=='Dar_es_Salaam'& region_status_3yr$recent_Year==1997),
                     which(region_status_3yr$Sector=='Dar_es_Salaam'& region_status_3yr$recent_Year==1998),
                     which(region_status_3yr$Sector=='Dar_es_Salaam'& region_status_3yr$recent_Year==1974),
                     # which(region_status_3yr$Sector=='Lindi'& region_status_3yr$recent_Year==2009),
                     # which(region_status_3yr$Sector=='Mtwara'& region_status_3yr$recent_Year==1999),
                     which(region_status_3yr$Sector=='Pemba'& region_status_3yr$recent_Year==1997)),]



#step 3: average across the most recent years of data for each region to get recent cover values
region_status_3yr_ave<-ddply(region_status_3yr,c("Sector","benthic_code"),summarise,
                             recent_cover_ave=round(mean(recent_cover),2))

#step 4: match the reference values to the recent values in region_status_3yr_ave

region_status_3yr_ave$ref_cover<-round(ref_values$ref_cover[match(region_status_3yr_ave$Sector,ref_values$Sector)],2)

region_status_3yr_ave$score<-round(region_status_3yr_ave$recent_cover_ave/region_status_3yr_ave$ref_cover,3)


#to get OHI scores curtailed to max of 1
region_status_3yr_ave$health<-round(region_status_3yr_ave$score,3)

region_status_3yr_ave$health[which(region_status_3yr_ave$score>1)]<-1

# region_scores<-ddply(region_status_3yr_ave,c("Sector"),summarise,
#                      health=round(mean(ohi_score),3)
#                      )

region_scores<-region_status_3yr_ave
region_scores$rgn_id<-NA
region_scores$rgn_id[which(region_scores$Sector=='Pwani')]<-2
region_scores$rgn_id[which(region_scores$Sector=='Mtwara')]<-4
region_scores$rgn_id[which(region_scores$Sector=='Dar_es_Salaam')]<-3
region_scores$rgn_id[which(region_scores$Sector=='Lindi')]<-5
region_scores$rgn_id[which(region_scores$Sector=='Tanga')]<-6
region_scores$rgn_id[which(region_scores$Sector=='Unguja')]<-7
region_scores$rgn_id[which(region_scores$Sector=='Pemba')]<-8

#steps to add the most recent_year to the dataframe
maxyear<-ddply(region_status_3yr,c("Sector"),summarise,
               maxyear=max(recent_Year))

region_scores$year<-maxyear$maxyear[match(region_scores$Sector,maxyear$Sector)]

region_scores$habitat<-'coral'

region_scores<-region_scores[,c(7,9,8,6)]

region_scores<-region_scores[order(region_scores$rgn_id),]

#save some intermediary outputs
write.csv(region_status_3yr_ave,"recent_and_reference_coral_cover_per_region.csv",row.names = F)

#save final output coral health layer
write.csv(region_scores,"hab_coral_health_tan2018.csv",row.names = F)

# other status methods -----------------------------------------------------


#this was done across the period break for 2016 - confirm if this is okay?
#new method: include Period, so we can compare post-2016 to pre-1998
county_ave<-ddply(station_sum,c("Sector","Site","Year","Period","benthic_code"),summarise,
                   ave_cover=mean(mean_cover),
                  n_sites=length(unique(Station)))


#this is only using 1 single year for ref point and status - can we use an average of 2 or 3 years
county_status<-ddply(county_ave,c("Sector","Site","Period","benthic_code"),summarise,
                    recent_cover=tail(ave_cover,n=3),
                    recent_Year=tail(Year,n=3),
                    n_site_recent=tail(n_sites,n=3),
                    reference_cover=head(ave_cover,n=3),
                    reference_Year=head(Year,n=3),
                    n_site_ref=head(n_sites,n=3))



# county_status3<-ddply(county_status2,c("Sector","benthic_code"),summarise,
#                       recent_cover_ave=round(mean(recent_cover),2),
#                       reference_cover_ave=round(mean(reference_cover),2))

#period averages - pre-1998 vs post-2016(current)

#this step is to average values from a particular site before averaging across period so that one site/area is not overly represented
period_ave<-ddply(station_sum,c("Sector","Site","Station","Period","benthic_code"),summarise,
                  ave_cover=mean(mean_cover),
                  n_sites=length(unique(Year)))

# period_ave<-ddply(station_sum,c("Sector","Period","benthic_code"),summarise,
#                   ave_cover=mean(mean_cover),
#                   n_sites=length(unique(Station)))

period_ave<-period_ave[which(period_ave$benthic_code=='HC'),]
period_ave$ave_cover<-round(period_ave$ave_cover,2)

period_ave2<-ddply(period_ave,c("Site","Period","benthic_code"),summarise,
                  mean_cover=mean(ave_cover),
                  n_sites=length(unique(Station)))

#change this period_ave if use the method of averaging across period
county_status4<-ddply(period_ave2,c("Sector","benthic_code"),summarise,
                      recent_cover=head(ave_cover,n=1),
                      n_site_recent=head(n_sites,n=1),
                      reference_cover=tail(ave_cover,n=1),
                      n_site_ref=tail(n_sites,n=1))

#can also consider doing comparisons per reef area and then aggregating the scores (0-1) by region/county

county_status$status<-county_status$recent_cover/county_status$reference_cover


# calculate 5 year trend ---------------------------------------------------------
#county level = count_ave dataframe

#step 1: get the yearly average - for each sector/county or site?

county_ave<-ddply(station_sum,c("Sector","Year","benthic_code"),summarise,
                  ave_cover=mean(mean_cover),
                  sd=sd(mean_cover),
                  n_sites=length(unique(Station)))

#remove erronous time points Mombasa 2015, Kwale and Lamu 2016 and Kilifi 2014
county_ave<-county_ave[-c(which(county_ave$Sector=='Kilifi'& county_ave$Year==2014),
                                        which(county_ave$Sector=='Lamu'& county_ave$Year==2016),
                                        which(county_ave$Sector=='Kwale'& county_ave$Year==2016),
                                        which(county_ave$Sector=='Mombasa'& county_ave$Year==2015)),]

#calculate trend - 5 most recent years for each county (contains some gaps in years)
# county_trend<-ddply(county_ave,c("Sector","benthic_code"),summarise,
#                     recent_cover=round(tail(ave_cover,n=6),2),
#                     sd=round(tail(sd,n=6),2),
#                     years=tail(Year,n=6),
#                     n=tail(n_sites,n=6))

# write.csv(county_trend,"coral_cover_trend_6years_per_county.csv",row.names = F)

library(dplyr)
#calculate trends
## minimum year here for illustration; it is based on data available
Year_min = 2006

#
r.trend <- county_ave %>%
  filter(Year >= Year_min) %>%
  filter(!is.na(ave_cover)) %>%
  group_by(Sector) %>%
  arrange(Year) %>%
  top_n(5, Year) %>%
  ungroup()


r.trend2 <- r.trend %>%
  group_by(Sector) %>%
  do(mdl = lm(ave_cover ~ Year, data=.),
     adjust_trend = .$ave_cover[.$Year == min(.$Year)]) %>%
  # summarize( region_id = Sector,
  #            trend = coef(mdl)['Year']*5) %>%
  # ungroup()%>%
#script from OHI core

dplyr::summarize(Sector, score = ifelse(coef(mdl)['Year']==0, 0, coef(mdl)['Year']/adjust_trend * 5))%>%
  dplyr::ungroup() %>%
  dplyr::mutate(score = ifelse(score>1, 1, score)) %>%
  dplyr::mutate(score = ifelse(score<(-1), (-1), score)) %>%
  dplyr::mutate(score = round(score, 4)) %>%
  dplyr::mutate(dimension = "trend") %>%
  dplyr::mutate(habitat = "coral") %>%
  dplyr::select(Sector,habitat ,score, dimension)

r.trend2$rgn_id<-NA
r.trend2$rgn_id[which(r.trend2$Sector=='Mombasa')]<-1
r.trend2$rgn_id[which(r.trend2$Sector=='Kwale')]<-2
r.trend2$rgn_id[which(r.trend2$Sector=='Kilifi')]<-3
r.trend2$rgn_id[which(r.trend2$Sector=='Lamu')]<-5

#change score name to trend

colnames(r.trend2)[3]<-'trend'

# add year column, latest year for each county
maxyear<-ddply(r.trend,c("Sector"),summarise,
      max_year=max(Year))

r.trend2$year<-maxyear$max_year[match(r.trend2$Sector,maxyear$Sector)]

r.trend2<-r.trend2[,c(5,2,6,3)]

r.trend2<-r.trend2[order(r.trend2$rgn_id),]

write.csv(r.trend2,"hab_coral_trend_ken2018.csv",row.names = F)

# status_data<-county_ave
#
# status_data$rgn_id<-NA
# status_data$rgn_id[which(status_data$Sector=='Mombasa')]<-1
# status_data$rgn_id[which(status_data$Sector=='Kwale')]<-2
# status_data$rgn_id[which(status_data$Sector=='Kilifi')]<-3
# status_data$rgn_id[which(status_data$Sector=='Lamu')]<-5
#
# colnames(status_data)[2]<-'year'
# colnames(status_data)[4]<-'status'
#
# CalculateTrend <- function(status_data, trend_years=trend_years){
#
#   if(sum(grepl("rgn_id", names(status_data))>0)){
#     names(status_data)[which(names(status_data)=="rgn_id")] <- "region_id"
#   }
#
#   if(sum(grepl("scenario_year", names(status_data)) > 0)) {
#     names(status_data)[which(names(status_data) == "scenario_year")] <- "year"
#   }
#
#   status_data <- status_data %>%
#     dplyr::select(region_id, year, status) %>%
#     dplyr::filter(year %in% trend_years) %>%
#     unique()
#
#   adj_trend_year <- min(trend_years)
#
#   r.trend = status_data %>%
#     dplyr::group_by(region_id) %>%
#     dplyr::do(mdl = lm(status ~ year, data=.),
#               adjust_trend = .$status[.$year == adj_trend_year]) %>%
#     dplyr::summarize(region_id, score = ifelse(coef(mdl)['year']==0, 0, coef(mdl)['year']/adjust_trend * 5)) %>%
#     dplyr::ungroup() %>%
#     dplyr::mutate(score = ifelse(score>1, 1, score)) %>%
#     dplyr::mutate(score = ifelse(score<(-1), (-1), score)) %>%
#     dplyr::mutate(score = round(score, 4)) %>%
#     dplyr::mutate(dimension = "trend") %>%
#     dplyr::select(region_id, score, dimension)
#
#   return(r.trend)
# }
#
# CalculateTrend(status_data)
#
