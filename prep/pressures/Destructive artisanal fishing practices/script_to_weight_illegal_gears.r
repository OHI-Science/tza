library(plyr)
library(here)

setwd(here::here('prep//pressures/Destructive artisanal fishing practices/'))

illegal_gear<-read.csv(file = "illegal gears count and score per region per year.csv",header = T,stringsAsFactors = F)

unique(illegal_gear$Illegal.Gears)

illegal_gear$Illegal.Gears[which(illegal_gear$Illegal.Gears=='Beachseine')]<-'Beach Seine'

illegal_gear<-illegal_gear[-which(illegal_gear$Illegal.Gears=='TOTAL'),] #remove old total for illegal gears

illegal_gear$weight_no<-illegal_gear$No.

illegal_gear$weight_no[which(illegal_gear$Illegal.Gears=='Beach Seine')]<-3*illegal_gear$No.[which(illegal_gear$Illegal.Gears=='Beach Seine')]   #seines have the highest weight - highest damaging effect
# illegal_gear$weight_no[which(illegal_gear$Illegal.Gears=="Monofilament ")]<-2*illegal_gear$No.[which(illegal_gear$Illegal.Gears=="Monofilament ")]  #mon filament - weight of 2

#spearguns and harpoons - weight of 1

illegal_gear$diff<-illegal_gear$weight_no-illegal_gear$No.  #to find out how many additional 'gears' have been added due to the weights

#calculate the new weighted total of gears and illegal gears for each county per year
illegal_summ<-ddply(illegal_gear,c('Region',"Year"),summarise,
                    total_weighted_gears=sum(diff)+unique(All.gears.count),
                    total_weighted_illegal=sum(weight_no))

#new eighted proportion of illegal gears
illegal_summ$pressure_score<-round(illegal_summ$total_weighted_illegal/illegal_summ$total_weighted_gears,3)


#need to apply the values for mainland to each of the mainland regions

#repeat the mainland rows 5 times
q<-illegal_summ[rep(seq_len(nrow(illegal_summ[which(illegal_summ$Region=='Mainland'),])), each=5),]
q$rgn_id<-rep(2:6,5)

#MAKE SURE THEY HAVE THE SAME NUMBER OF COLUMNS FIRST

illegal_summ$rgn_id<-NA

q1<-rbind(q,illegal_summ[c(illegal_summ$Region=='Unguja'|illegal_summ$Region=='Pemba'),])

#add rgn_id row
q1$rgn_id[which(q1$Region=='Pemba')]<-8
q1$rgn_id[which(q1$Region=='Unguja')]<-7


final_results<-q1[,c(6,2,5)]

final_results<-final_results[order(final_results$rgn_id),]


write.csv(final_results,'fp_dest_art_tan2018.csv',row.names = F)
