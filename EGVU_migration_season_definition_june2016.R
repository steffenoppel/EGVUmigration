##########################################################################
# DEFINITION OF MIGRATORY SEASONS FOR SATELLITE-TRACKED EGYPTIAN VULTURES
# original script written by Steffen Oppel in September 2014
# modified in February 2016
# removed all ARGOS locations
# update 6 Feb 2016: revised data cleanup
# removed data prep and cleanup - see extra script 'EGVU_migration_data_prep2016.r'
# update 7 Feb 2016: manually identified spring migration events
# update 7 Feb 2016: collated all migration data and ran tripGrid to identify migration hotspots
# update 8 Feb 2016: added home range estimation, revised autumn migration definition for juveniles
# update 9 Feb 2016: added manual autumn migration definition for immature birds, removed home range estimation (now in EGVU_home_range_estimation.R), added time discretisation and philopatry analysis
# update 10 February: moved migration hotspot analysis into different script (EGVU_migration_hotspot_analysis.R)
# update 10 Feb 2016: changed season dates for spring to include lazaros (who died 1st of April)
# update 17 May 2016: included data from spring 2016
# update 17 June 2016: refine end_date of Dobromir and Sanie, delete Iliaz migrations 2015 (reached only north of Egypt)
# update 29 June 2016: calculation of nest_dist to define the end of the spring migration 
# update 13 July 2016: comparison of migration parameters between spring and autumn with linear regression moved in another script 'EGVU_migration_parameters_comparison.R'
##########################################################################

# Load necessary library
library(maptools)
library(sp)
library(rgdal)
require(maps)
require(mapdata)
require(geosphere)
require(rgeos)
library(raster)
library(move)
library(reshape)
library(rworldmap)
data(countriesLow)




#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# LOAD PREVIOUSLY SAVED DATA (prepared in script 2.EV-all-migration delineation.R)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Set working directory
setwd("C:\\STEFFEN\\MANUSCRIPTS\\in_prep\\EGVU_papers\\FrontiersMigrationPaper\\EGVUmigration")

# read in clean csv
locs = read.csv("EV-all_1ptperhr-filtered-utm-NSD-season.csv")
head(locs)

migs<-unique(locs$id.yr.season) ## specify the unique migration journeys
migs<-migs[!migs %in% c("Cabuk_2016_spring")]


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# CALCULATE TRAVEL DISTANCES AND SPEEDS 
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## this is not a particularly efficient way to calculate that, but it is easy to understand and manipulate
## run time ~25 min

migration<-data.frame()

migs<-migs[!migs %in% unique(migration$id.yr.season)]     ## use this line if loop was interrupted
for (a in migs){
  
  input<-locs %>% filter(id.yr.season==a) %>% 
    mutate(DateTime=ymd_h(paste(Year,Month, Day, Hour, sep=","))) %>%
    dplyr::select(id.yr.season,study,tag,id,DateTime, long, lat, NSD, ND,utm.e,utm.n) %>% arrange(DateTime) %>%
    mutate(step_dist=0,home_dist=0,cumul_dist=0,time_diff=0,speed=0)
  first<-SpatialPoints(data.frame(input$long[1], input$lat[1]), proj4string=CRS("+proj=longlat + datum=wgs84"))
  
  for (l in 2: dim(input)[1]){
    input$time_diff[l]<-as.numeric(difftime(input$DateTime[l],input$DateTime[l-1], units="hours"))
    fromloc<-SpatialPoints(data.frame(input$long[l-1], input$lat[l-1]), proj4string=CRS("+proj=longlat + datum=wgs84"))
    toloc<-SpatialPoints(data.frame(input$long[l], input$lat[l]), proj4string=CRS("+proj=longlat + datum=wgs84"))
    input$step_dist[l]<-spDistsN1(fromloc, toloc, longlat=T)
    input$home_dist[l]<-spDistsN1(first, toloc, longlat=T)
    input$cumul_dist[l]<-sum(input$step_dist)
    input$speed[l]<-input$step_dist[l]/input$time_diff[l]
  }
  
  migration<-rbind(migration, input)
  
}


migs<-unique(migration$id.yr.season) ## specify the unique migration journeys






#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# CREATE A BLANK TABLE THAT LISTS ALL MIGRATION EVENTS FOR INDIVIDUAL MIGRATION JOURNEYS
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

mig_summary<-migration %>% mutate(count=1) %>%
  group_by(id.yr.season,study,tag,id) %>%
  summarise(N_locs=sum(count)) %>%
  mutate(start_mig=NA,end_mig=NA,mig_days=0,mig_dist=0,mig_speed=0,mig_completed=0)






#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# CALCULATE START AND END DATES OF AUTUMN MIGRATION FOR INDIVIDUAL ANIMALS
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

### LOOP TO CALCULATE START AND END DATES FOR AUTUMN MIGRATION #######
### based on Evan's script to use models, but if model fails we use basic rules of thumb



### This loop extracts all locations associated with migration from the corrected location data set and compiles them in a new blank data frame for use in further analysis
### changed on 9 Feb 2016 to exclude immature birds - they are too complicated and must be added manually

all_migdata<-data.frame()		### create blank data frame that will hold all the data associated with proper autumn migration

for (a in migs){
  x<-migration %>% filter(id.yr.season==a)
  
  
  ### create LTRAJ object to fit movement model
  d2 <- as.ltraj(xy = x[, c("utm.e", "utm.n")], date = x$DateTime, id=a)
  d3 <- mvmtClass(d2)
  migr.dates <-  mvmt2dt(d3, p = 0.01, mod = "disperser")      ## replaced 0.05 with 0.01 as it includes most of the migration
  mig_time<-interval(start=migr.dates[[1]]$date[1],end=migr.dates[[1]]$date[2])
  
  ### write output in summary
  mig_summary$start_mig[mig_summary$id.yr.season==a]<- migr.dates[[1]]$date[1]
  mig_summary$start_mig[mig_summary$id.yr.season==a]<- migr.dates[[1]]$date[2]
  
  
  ### visually assess whether these dates make sense
  x<- x %>% mutate(MIG=if_else(DateTime %within% mig_time,2,3))
  plot(home_dist~DateTime, data=x,type='p', col=x$MIG, pch=16, cex=0.4)
  
  ### split data into premigration, migration, and post-migration
  premig <- x %>% filter(DateTime < migr.dates[[1]]$date[1])
  xmig<- x %>% filter(MIG==2)
  postmig <- x %>% filter(DateTime > migr.dates[[1]]$date[2])
  
  ### calculate mean travel speed for migration and outside
  migspeed<- x %>% mutate(Day=as.Date(DateTime)) %>% group_by(MIG,Day) %>%
    summarise(daily_speed=mean(speed), daily_dist=sum(step_dist)) %>% group_by(MIG) %>%
    summarise(min_mig_speed=min(daily_speed), max_mig_speed=max(daily_speed), min_daily_dist=min(daily_dist), max_daily_dist=max(daily_dist)) 
  
  ### calculate mean daily travel speed for migration and outside
  dailysummary<- x %>% mutate(Day=as.Date(DateTime)) %>% group_by(Day) %>%
    summarise(daily_speed=mean(speed), daily_dist=sum(step_dist))
  
  
  ## SIMPLE THRESHOLDS - MIGRATION STARTS WHEN DIST TO HOME CONTINUOUSLY INCREASES
  dailyhomedist<- x %>% mutate(Day=as.Date(DateTime)) %>% group_by(Day) %>%
    summarise(away=max(home_dist))
  
  ### find the first day where home_dist is greater than on any day before, and where home_dist is greater on any day afterwards
  start<-NA
  end<-NA
  for (d in 2:dim(dailyhomedist)[1]){
    maxbef<-max(dailyhomedist$away[1:(d-1)])
    minaft<-min(dailyhomedist$away[(d+1):dim(dailyhomedist)[1]])
    if(is.na(start)==TRUE) {     ## prevent that the first day gets overwritten by subsequent days
      if(dailyhomedist$away[d]>maxbef & dailyhomedist$away[d]<minaft){start<-dailyhomedist$Day[d]} 
    }
    
    if(is.na(start)==FALSE & is.na(end)==TRUE) {     ## prevent that the end is defined before the start
      if(dailyhomedist$away[d]>maxbef & dailyhomedist$away[d]>=minaft){end<-dailyhomedist$Day[d]}
    }
  }  # end loop over every day in the data set
    
    

}		#closes the animal loop




#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# MANUALLY DETERMINE START AND END DATES OF SPRING MIGRATION FOR INDIVIDUAL ANIMALS, AND AUTUMN MIGRATION FOR IMMATURE BIRDS
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## because spring migration is erratic for juveniles, this was determined on a case-by-case basis
## because autumn migration is erratic for immature birds that do not yet breed, this was determined on a case-by-case basis

## first, look at all the spring migration events we need to check:
season_summary[season_summary$season=="spring",c(2,10)]
season_summary[season_summary$Age == "juv" & season_summary$season == "autumn" & season_summary$year>season_summary$Tag_year,c(2,10)]


###### MANUALLY CHANGE AND INSPECT THE PLOT FOR EACH BIRD AND EACH YEAR ########
a="Iliaz"										### Enter name and year for each migration that need to be checked
y=2016

x<-subset(migration, Bird_ID==a)
x$year<-as.integer(format(x$Date, format="%Y"))
xy<-subset(x, year==y)
dim(xy)
xy<-xy[xy$Date>as.POSIXct(sprintf("%s-03-10",y),format="%Y-%m-%d"),]			### choose a broad period initially and refine it after first insight of migration
xy<-xy[xy$Date<as.POSIXct(sprintf("%s-06-10",y),format="%Y-%m-%d"),]
dim(xy)

### plot to explore 

par (mfrow=c(1,2), mar=c(3,4,1,0), oma=c(0,0,0,0))
plottitle<-paste(a,"spring migration", y, sep=" ")
plot(lat~Date, type='b', frame=F, data=xy, main=plottitle)

xy<-xy[order(xy$Bird_ID,xy$DateTime),]
MOBJ_all<-move(x=xy$long, y=xy$lat, time=xy$DateTime, proj=CRS("+proj=longlat +ellps=WGS84"))
plot(MOBJ_all, type='l', col='red', ylim=c(8,44),xlim=c(10,50))
plot(countriesLow, add=T)

###### END OF MANUAL EXPLORATION PLOTS #######################




#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
### REMOVE THE ENTRIES IN SEASON SUMMARY WHERE THE BIRD DID NOT PERFORM A PROPER MIGRATION #######
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
dim(season_summary)
### NEED TO ENTER ALL THE NONSENSICAL POST-JUV MIGRATION PERIODS
season_summary<-season_summary[!(season_summary$Age == "juv" & season_summary$year==(season_summary$Tag_year+1)),]				# juveniles : remove entries in the year after the tagging (no migration)

dim(season_summary)
season_summary<-season_summary[!(season_summary$Name=="Levkipos" & season_summary$season == "spring" & season_summary$year== "2013"),]		# remove Levkipos for spring 2013
season_summary<-season_summary[!(season_summary$Name=="Levkipos" & season_summary$year== "2014"),]
season_summary<-season_summary[!(season_summary$Name=="Levkipos" & season_summary$season == "spring" & season_summary$year== "2015"),]
season_summary<-season_summary[!(season_summary$Name=="Volen" & season_summary$season == "spring" & season_summary$year== "2013"),]
season_summary<-season_summary[!(season_summary$Name=="Svetlina" & season_summary$season == "spring" & season_summary$year== "2013"),]
season_summary<-season_summary[!(season_summary$Name=="Svetlina" & season_summary$season == "spring" & season_summary$year== "2014"),]
season_summary<-season_summary[!(season_summary$Name=="Iliaz" & season_summary$season == "spring" & season_summary$year== "2013"),]
season_summary<-season_summary[!(season_summary$Name=="Iliaz" & season_summary$season == "autumn" & season_summary$year== "2015"),]
season_summary<-season_summary[!(season_summary$Name=="Iliaz" & season_summary$year== "2014"),]
season_summary<-season_summary[!(season_summary$Name=="Sanie" & season_summary$season == "spring" & season_summary$year== "2014"),]
season_summary<-season_summary[!(season_summary$Name=="Dobromir" & season_summary$season == "spring" & season_summary$year== "2013"),]
season_summary<-season_summary[!(season_summary$Name=="Iliaz" & season_summary$season == "spring" & season_summary$year=="2015"),]
dim(season_summary)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
### MANUALLY ENTER THE START AND END DATES OF SPRING AND AUTUMN MIGRATION FOR ALL ANIMALS #######
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

### SPRING MIGRATION ####
season_summary$start_mig[season_summary$Name=="Dobromir" & season_summary$season == "spring" & season_summary$year==2014]<-"2014-04-18"
season_summary$end_mig[season_summary$Name=="Dobromir" & season_summary$season == "spring" & season_summary$year==2014]<-"2014-06-03"
season_summary$start_mig[season_summary$Name=="Dobromir" & season_summary$season == "spring" & season_summary$year==2015]<-"2015-04-30"
season_summary$end_mig[season_summary$Name=="Dobromir" & season_summary$season == "spring" & season_summary$year==2015]<-"2015-05-15"
season_summary$start_mig[season_summary$Name=="Volen" & season_summary$season == "spring" & season_summary$year==2014]<-"2014-04-29"
season_summary$end_mig[season_summary$Name=="Volen" & season_summary$season == "spring" & season_summary$year==2014]<-"2014-06-25"
season_summary$start_mig[season_summary$Name=="Sanie" & season_summary$season == "spring" & season_summary$year==2015]<-"2015-05-04"
season_summary$end_mig[season_summary$Name=="Sanie" & season_summary$season == "spring" & season_summary$year==2015]<-"2015-06-18"
season_summary$start_mig[season_summary$Name=="Castor" & season_summary$season == "spring" & season_summary$year==2015]<-"2015-03-07"
season_summary$end_mig[season_summary$Name=="Castor" & season_summary$season == "spring" & season_summary$year==2015]<-"2015-04-05"
season_summary$start_mig[season_summary$Name=="Lazaros" & season_summary$season == "spring" & season_summary$year==2013]<-"2013-03-08"
season_summary$end_mig[season_summary$Name=="Lazaros" & season_summary$season == "spring" & season_summary$year==2013]<-"2013-03-31"
season_summary$start_mig[season_summary$Name=="Iliaz" & season_summary$season == "spring" & season_summary$year==2016]<-"2016-03-17"
season_summary$end_mig[season_summary$Name=="Iliaz" & season_summary$season == "spring" & season_summary$year==2016]<-"2016-05-09"
season_summary$start_mig[season_summary$Name=="Boris" & season_summary$season == "spring" & season_summary$year==2016]<-"2016-03-01"
season_summary$end_mig[season_summary$Name=="Boris" & season_summary$season == "spring" & season_summary$year==2016]<-"2016-03-20"
season_summary$start_mig[season_summary$Name=="Jenny" & season_summary$season == "spring" & season_summary$year==2016]<-"2016-03-16"
season_summary$end_mig[season_summary$Name=="Jenny" & season_summary$season == "spring" & season_summary$year==2016]<-"2016-04-16"
season_summary$start_mig[season_summary$Name=="Dobromir" & season_summary$season == "spring" & season_summary$year==2016]<-"2016-05-01"
season_summary$start_mig[season_summary$Name=="Sanie" & season_summary$season == "spring" & season_summary$year==2016]<-"2016-04-13"
season_summary$start_mig[season_summary$Name=="Aoos" & season_summary$season == "spring" & season_summary$year==2016]<-"2016-03-16"
season_summary$end_mig[season_summary$Name=="Aoos" & season_summary$season == "spring" & season_summary$year==2016]<-"2016-04-18"
season_summary$end_mig[season_summary$Name=="Dobromir" & season_summary$season == "spring" & season_summary$year==2016]<-"2016-05-30"
season_summary$end_mig[season_summary$Name=="Sanie" & season_summary$season == "spring" & season_summary$year==2016]<-"2016-05-13"


season_summary$mig_completed[season_summary$Name=="Dobromir" & season_summary$season == "spring" & season_summary$year==2014]<-1
season_summary$mig_completed[season_summary$Name=="Dobromir" & season_summary$season == "spring" & season_summary$year==2015]<-1
season_summary$mig_completed[season_summary$Name=="Volen" & season_summary$season == "spring" & season_summary$year==2014]<-1
season_summary$mig_completed[season_summary$Name=="Sanie" & season_summary$season == "spring" & season_summary$year==2015]<-1
season_summary$mig_completed[season_summary$Name=="Castor" & season_summary$season == "spring" & season_summary$year==2015]<-1
season_summary$mig_completed[season_summary$Name=="Lazaros" & season_summary$season == "spring" & season_summary$year==2013]<-1
season_summary$mig_completed[season_summary$Name=="Iliaz" & season_summary$season == "spring" & season_summary$year==2016]<-1
season_summary$mig_completed[season_summary$Name=="Boris" & season_summary$season == "spring" & season_summary$year==2016]<-1
season_summary$mig_completed[season_summary$Name=="Jenny" & season_summary$season == "spring" & season_summary$year==2016]<-1
season_summary$mig_completed[season_summary$Name=="Aoos" & season_summary$season == "spring" & season_summary$year==2016]<-1
season_summary$mig_completed[season_summary$Name=="Sanie" & season_summary$season == "spring" & season_summary$year==2016]<-1
season_summary$mig_completed[season_summary$Name=="Dobromir" & season_summary$season == "spring" & season_summary$year==2016]<-1
#season_summary



### AUTUMN MIGRATION #### 
## Enter manually start of migration ##
season_summary$start_mig[season_summary$Name=="Sanie" & season_summary$season == "autumn" & season_summary$year==2015]<-"2015-07-20"
season_summary$start_mig[season_summary$Name=="Dobromir" & season_summary$season == "autumn" & season_summary$year==2014]<-"2014-07-27"
season_summary$start_mig[season_summary$Name=="Dobromir" & season_summary$season == "autumn" & season_summary$year==2015]<-"2015-08-22"
season_summary$start_mig[season_summary$Name=="Iliaz" & season_summary$season == "autumn" & season_summary$year==2015]<-"2015-05-23"
season_summary$start_mig[season_summary$Name=="Arda" & season_summary$season == "autumn" & season_summary$year==2012]<-"2012-09-18"

season_summary$mig_completed[season_summary$Name=="Sanie" & season_summary$season == "autumn" & season_summary$year==2015]<-1
season_summary$mig_completed[season_summary$Name=="Dobromir" & season_summary$season == "autumn" & season_summary$year==2015]<-1
season_summary$mig_completed[season_summary$Name=="Dobromir" & season_summary$season == "autumn" & season_summary$year==2014]<-1
season_summary$mig_completed[season_summary$Name=="Iliaz" & season_summary$season == "autumn" & season_summary$year==2015]<-1


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# MATCH END DATES OF SPRING MIGRATION FOR INDIVIDUAL ANIMALS based on nearest DISTANCE TO NEST
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
head(season_summary)
head(nest_dist_juv)

## figure out mismatch for juveniles ##
season_summary$RecordNr<-seq(1:dim(season_summary)[1])			### this helps you to re-insert the info later on - it is basically the line number!
date_compare<-merge(nest_dist_juv, season_summary[season_summary$season=="spring",c(1,3,4,9:12,17)], by=c("Transmitter_ID","year"), all.x=T)
date_compare<-date_compare[!(is.na(date_compare$Sex)),]
difftime(date_compare$DateTime, date_compare$end_mig, unit="days")	### Check the difference between the dates - if it is too large, check the track to see if there is a significant movement between the dates


## update records in season_summary
# We only use the closest dist for Dobromir in 2014, Sanie in 2015 and in 2016, because they were not really stopping in one place but kept flying around

date_compare<-date_compare[(date_compare$RecordNr %in% c(9,21,23)),]
season_summary$end_mig[match(date_compare$RecordNr,season_summary$RecordNr)]<-as.character(as.Date(date_compare$DateTime, format="%Y-%m-%d")+1)


## figure out mismatch for adults ##
date_compare<-merge(nest_dist_ad, season_summary[season_summary$season=="spring",c(1,3,4,9:12,17)], by=c("Transmitter_ID","year"), all.x=T)
date_compare<-date_compare[!(is.na(date_compare$Sex)),]
difftime(date_compare$DateTime, date_compare$end_mig, unit="days")

## update records in season_summary
#season_summary$end_mig[match(date_compare$RecordNr,season_summary$RecordNr)]<-as.character(as.Date(date_compare$DateTime, format="%Y-%m-%d")+1)	# Don't run that because for adults we keep the date manually determined (they are not flying around like juveniles)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
### CALCULATE mig_days, mig_dist and mig_speed FOR SPRING AND AUTUMN MIGRATION ######## #######
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

fallmigs<-season_summary[season_summary$Age == "juv" & season_summary$season == "autumn" & season_summary$year>season_summary$Tag_year,c(2,10:12,9)]		### creates small data frame for the autumn migration events that we need to fill in
springmigs<-season_summary[season_summary$season=="spring",c(2,10:12,9)]		### creates small data frame for the spring migration events that we need to fill in
springmigs<-rbind(springmigs,fallmigs)

for (a in 1:dim(springmigs)[1]){								### starts loop over all the spring migration events
x<-subset(migration, Bird_ID==springmigs[a,1])
x$year<-as.integer(format(x$Date, format="%Y"))
xy<-subset(x, year==springmigs[a,2])
start<-as.POSIXct(springmigs[a,3], format="%Y-%m-%d")
end<-as.POSIXct(springmigs[a,4], format="%Y-%m-%d")
xy<-xy[!xy$Date<start,]
xy<-xy[!xy$Date>end,]
xy$season=springmigs$season[a]

all_migdata<-rbind(all_migdata,xy)				### this adds the spring migration data to the data frame that holds all the autumn migration data

season_summary$mig_days[season_summary$Name==springmigs[a,1] & season_summary$season == springmigs$season[a] & season_summary$year==springmigs[a,2]]<-end-start
season_summary$mig_dist[season_summary$Name==springmigs[a,1] & season_summary$season == springmigs$season[a] & season_summary$year==springmigs[a,2]]<-sum(xy$step_dist)
season_summary$mig_speed[season_summary$Name==springmigs[a,1] & season_summary$season == springmigs$season[a] & season_summary$year==springmigs[a,2]]<-mean(xy$speed)
}

season_summary$travel_speed<-season_summary$mig_dist/season_summary$mig_days						# [in km/day]: calculate overall travel speed (different from mean speed already in table)
dim(season_summary)
write.table(season_summary,"EGVU_migration_summaries.csv",sep=",", row.names=F)			#### EXPORT TABLE 





#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# REMOVE DATA NOT ASSOCIATED WITH MIGRATION
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
head(all_migdata)
dim(all_migdata)
all_migdata$MigID<-paste(all_migdata$Bird_ID,all_migdata$season,all_migdata$year, sep="_")
season_summary$MigID<-paste(season_summary$Name,season_summary$season,season_summary$year, sep="_")
all_migdata<-all_migdata[all_migdata$MigID %in% season_summary$MigID,]
dim(all_migdata)
unique(all_migdata$MigID)


