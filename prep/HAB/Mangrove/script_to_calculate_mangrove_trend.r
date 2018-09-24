#script will try to develop county level current status, trend and reference points for hard coral cover
library(here)
#steps:
#assign the correct county (maybe under sector) to each site
setwd(here::here('prep/HAB/Mangrove/Extracted_regional_value _csv'))


# calculate 5 year trend ---------------------------------------------------------

mang<-read.csv("hab_mangrove_extent_tan2018.csv",header = T,stringsAsFactors = F)
#county level = count_ave dataframe


library(dplyr)
#calculate trends
## minimum year here for illustration; it is based on data available
Year_min = 2010

#
r.trend <- mang %>%
  filter(year >= Year_min) %>%
  filter(!is.na(km2)) %>%
  group_by(rgn_id) %>%
  arrange(year) %>%
  top_n(5, year) %>%
  ungroup()


r.trend2 <- r.trend %>%
  group_by(rgn_id) %>%
  do(mdl = lm(km2 ~ year, data=.),
     adjust_trend = .$km2[.$year == min(.$year)]) %>%
  # summarize( region_id = rgn_id,
  #            trend = coef(mdl)['year']*5) %>%
  # ungroup()%>%
  #script from OHI core

  dplyr::summarize(rgn_id, trend = ifelse(coef(mdl)['year']==0, 0, coef(mdl)['year']/adjust_trend * 5))%>%
  dplyr::ungroup() %>%
  dplyr::mutate(trend = ifelse(trend>1, 1, trend)) %>%
  dplyr::mutate(trend = ifelse(trend<(-1), (-1), trend)) %>%
  dplyr::mutate(trend = round(trend, 4)) %>%
  # dplyr::mutate(dimension = "trend") %>%
  dplyr::mutate(habitat = "mangrove") %>%
  dplyr::select(rgn_id,habitat ,trend)

library(plyr)
# add year column, latest year for each county
maxyear<-ddply(r.trend,c("rgn_id"),summarise,
               max_year=max(year))

r.trend2$year<-maxyear$max_year[match(r.trend2$rgn_id,maxyear$rgn_id)]

r.trend2<-r.trend2[,c(1,2,4,3)]

r.trend2<-r.trend2[order(r.trend2$rgn_id),]

write.csv(r.trend2,"hab_mangrove_trend_tan2018.csv",row.names = F)
