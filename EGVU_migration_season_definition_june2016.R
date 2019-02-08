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
# LOAD PREVIOUSLY SAVED WORKSPACE (from 'EGVU_migration_data_prep2016.r')
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Set working directory

setwd("C:\\STEFFEN\\RSPB\\Bulgaria\\Analysis\\Migration")
setwd("S:\\ConSci\\DptShare\\SteffenOppel\\RSPB\\Bulgaria\\Analysis\\Migration")
setwd("C:\\Users\\Clémentine\\Documents\\Stage M2\\EGVU_Bulgaria")

load("C:\\STEFFEN\\RSPB\\Bulgaria\\Analysis\\Migration\\EGVU_migration_raw_dat.RData")
load("S:\\ConSci\\DptShare\\SteffenOppel\\RSPB\\Bulgaria\\Analysis\\Migration\\EGVU_migration_raw_dat.RData")
load("C:\\Users\\Clémentine\\Documents\\Stage M2\\EGVU_Bulgaria\\EGVU_migration_raw_dat.RData")



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# DATA EXPLORATION TO VISUALISE TRAVEL: PLOT OF CUMULATIVE DISTANCE TRAVELLED
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

par (mfrow=c(6,4), mar=c(3,4,1,0), oma=c(0,0,0,0))
for (a in anim){
input<-migration[migration$Bird_ID == a,]
plottitle<-paste(a,birds$Transmitter_ID[match(a,birds$Name)], sep=" ")
plot(cumul_dist~Date, type='l', frame=F, data=input, main=plottitle)
}



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# CREATE A BLANK TABLE THAT LISTS ALL MIGRATION EVENTS FOR INDIVIDUAL ANIMALS
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
anim<-unique(birds$Name)
mig_summary<-birds[,c(1:7,13)]
mig_summary$season<-NA
mig_summary$year<-0
mig_summary$start_mig<-NA
mig_summary$end_mig<-NA
mig_summary$mig_days<-0
mig_summary$mig_dist<-0
mig_summary$mig_speed<-0
mig_summary$mig_completed<-0




#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# COMPILE ALL MIGRATION SEASONS DURING WHICH AN ANIMAL WAS ALIVE AND TAGGED
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
season_summary<-data.frame()
for (a in anim){

x<-subset(migration, Bird_ID==a)
x$year<-as.integer(format(x$Date, format="%Y"))

for(y in unique(x$year)){

xy<-subset(x, year==y)
seasonbird<-mig_summary[mig_summary$Name==a,]
seasonbird$year<-y
if(min(xy$Date)<as.POSIXct(sprintf("%s-02-01",y),format="%Y-%m-%d")){
if(max(xy$Date)>as.POSIXct(sprintf("%s-03-30",y),format="%Y-%m-%d")){

seasonbird$season<-"spring"
season_summary<-rbind(season_summary,seasonbird)}}

if(max(xy$Date)>as.POSIXct(sprintf("%s-11-20",y),format="%Y-%m-%d")){
seasonbird$season<-"autumn"
season_summary<-rbind(season_summary,seasonbird)}
}
}

#season_summary[season_summary$Name=="Lazaros",]




#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# CALCULATE START AND END DATES OF AUTUMN MIGRATION FOR INDIVIDUAL ANIMALS
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

### LOOP TO CALCULATE START AND END DATES FOR AUTUMN MIGRATION #######
### This loop extracts all locations associated with migration from the corrected location data set and compiles them in a new blank data frame for use in further analysis
### changed on 9 Feb 2016 to exclude immature birds - they are too complicated and must be added manually

all_migdata<-data.frame()		### create blank data frame that will hold all the data associated with proper autumn migration

for (a in anim){
x<-subset(migration, Bird_ID==a)
x$year<-as.integer(format(x$Date, format="%Y"))

years<-season_summary$year[season_summary$Name==a & season_summary$season == "autumn"]

for (y in years){

xy<-subset(x, year==y)
xy<-xy[xy$Date>as.POSIXct(sprintf("%s-07-15",y),format="%Y-%m-%d"),]				## take only second half of year for autumn migration
dailydist<-aggregate(step_dist~Date, data=xy, FUN=sum)						## calculate daily travel distance

if(birds$Age[match(a, birds$Name)]=="juv" & birds$Tag_year[match(a, birds$Name)]==y){
start<-as.Date(min(dailydist$Date[dailydist$step_dist>50]), format="%Y-%m-%d")+1		## migration starts for a juvenile when bird travels >50 km per day
}
if(birds$Age[match(a, birds$Name)]=="ad"){
start<-as.Date(min(dailydist$Date[dailydist$step_dist>150]), format="%Y-%m-%d")+1		## migration starts for an adult when bird travels >150 km per day
}

subSahara<-xy[xy$lat<22,]
if (dim(subSahara)[1]>0){									## calculate end of migration only for birds that crossed the Sahara
season_summary$mig_completed[season_summary$Name==a & season_summary$season == "autumn" & season_summary$year==y]<-1
southward<-aggregate(home_dist~Date, data=subSahara, FUN=mean)			## calculate average distance to home for each day
southward$reversal<-0

for (d in 2:dim(southward)[1]){
southward$reversal[d]<-southward$home_dist[d]-southward$home_dist[d-1]		## calculate difference in distance to origin
}
end<-as.Date(min(southward$Date[southward$reversal<0], na.rm=T), format="%Y-%m-%d") ## migration ends when dist to home no longer increases after the bird is south of 22 N
}else{end<-as.Date(max(xy$Date), format="%Y-%m-%d")
season_summary$mig_completed[season_summary$Name==a & season_summary$season == "autumn" & season_summary$year==y]<-0}				## for birds that died on migration the migration ends when the bird is dead
### write important dates into table ###
season_summary$end_mig[season_summary$Name==a & season_summary$season == "autumn" & season_summary$year==y]<-as.character(end)

if(format(start, format="%Y")==y){								## THIS CONDITION PREVENTS THE INCLUSION OF ODD AUTUMN MIGRATION OF IMMATURE BIRDS
season_summary$start_mig[season_summary$Name==a & season_summary$season == "autumn" & season_summary$year==y]<-as.character(start)
season_summary$mig_days[season_summary$Name==a & season_summary$season == "autumn" & season_summary$year==y]<-end-start

migdata<-xy[as.Date(xy$Date)>=start,]
migdata<-migdata[as.Date(migdata$Date)<=end,]

if((end-start)>3){all_migdata<-rbind(all_migdata, migdata)}
season_summary$mig_dist[season_summary$Name==a & season_summary$season == "autumn" & season_summary$year==y]<-sum(migdata$step_dist)
season_summary$mig_speed[season_summary$Name==a & season_summary$season == "autumn" & season_summary$year==y]<-mean(migdata$speed)
}		#closes the if statement that ensures that only current year data are used [to exclude immatures after tag_year]
}		#closes the year loop
}		#closes the animal loop

### no longer needed corrections from earlier versions
#season_summary<-season_summary[!(season_summary$mig_days<3 & season_summary$season == "autumn"),]		# remove non-existing migrations - keeps every line where mig_days is not <3 in autumn season
#season_summary[season_summary$Age == "juv" & season_summary$season == "autumn" & season_summary$year>season_summary$Tag_year,13:15]<-0	# SET MIGRATION TIMES OF IMMATURES TO 0

season_summary

all_migdata$season<-"autumn"			## add the season to the migration data



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# CALCULATE THE MINIMUM DISTANCE TO THE NEST IN EVERY YEAR AFTER THE TAG YEAR FOR EVERY JUVENILE
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

nest_dist_juv<-data.frame()

head(birds)
juveniles<-birds[(birds$Age=="juv"),c(2,4,5,6,7)]
juveniles<-juveniles[juveniles$Name %in% season_summary$Name,]

for(j in juveniles$Name){
x<-subset(migration, Bird_ID==j)
x$year<-as.integer(format(x$Date, format="%Y"))
years<-unique(x$year)
years<-years[-1]
for(y in years){

xy<-subset(x, year==y)
if(max(xy$DateTime)>as.POSIXct(sprintf("%s-05-30",y),format="%Y-%m-%d")){
mindist<-xy[xy$home_dist==min(xy$home_dist,na.rm = TRUE),c(6,1,17,8:9,19)]
mindist$year<-y
nest_dist_juv<-rbind(nest_dist_juv,mindist[1,])
}}}		## Close the if statement, year loop, juveniles loop

write.table(nest_dist_juv, "EGVU_philopatry_distance_juv.csv", sep=";", row.names=F)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# CALCULATE THE MINIMUM DISTANCE TO THE NEST IN EVERY YEAR FOR EVERY ADULT
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
nest_dist_ad<-data.frame()

head(birds)
adults<-birds[(birds$Age=="ad"),c(2,4,5,6,7)]
adults<-adults[adults$Name %in% season_summary$Name,]

for(a in adults$Name){
x<-subset(migration, Bird_ID==a)
x$year<-as.integer(format(x$Date, format="%Y"))
years<-unique(x$year)
for(y in years){

xy<-subset(x, year==y)
if(max(xy$DateTime)>as.POSIXct(sprintf("%s-05-30",y),format="%Y-%m-%d")){		### Which date should we set for the spring migration ?
mindist<-xy[xy$home_dist==min(xy$home_dist,na.rm = TRUE),c(6,1,17,8:9,19)]
mindist$year<-y
nest_dist_ad<-rbind(nest_dist_ad,mindist)
}}}		## Close the if statement, year loop, adults loop

write.table(nest_dist_ad, "EGVU_philopatry_distance_ad.csv", sep=",", row.names=F)


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



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# SAVE WORKSPACE (MIGRATORY BOTTLENECK ANALYSIS AND HOME RANGE ESTIMATION MOVED TO ANOTHER SCRIPT)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Set working directory

setwd("C:\\STEFFEN\\RSPB\\Bulgaria\\Analysis\\Migration")
setwd("S:\\ConSci\\DptShare\\SteffenOppel\\RSPB\\Bulgaria\\Analysis\\Migration")
setwd("C:\\Users\\Clémentine\\Documents\\Stage M2\\EGVU_Bulgaria")

save.image("C:\\STEFFEN\\RSPB\\Bulgaria\\Analysis\\Migration\\EGVU_migration_Jun2016.RData")
save.image("S:\\ConSci\\DptShare\\SteffenOppel\\RSPB\\Bulgaria\\Analysis\\Migration\\EGVU_migration_Jun2016.RData")
save.image("C:\\Users\\Clémentine\\Documents\\Stage M2\\EGVU_Bulgaria\\EGVU_migration_Jun2016.RData")

