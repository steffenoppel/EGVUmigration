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
library(adehabitatLT)
library(adehabitatHR)
library(nlme)
library(lubridate)
library(ggplot2)
library(scales)
library(tidyverse)
library(migrateR)
library(data.table)




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
  summarise(N_locs=sum(count),start_mig=min(DateTime),end_mig=max(DateTime))






#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# CALCULATE START AND END DATES OF AUTUMN MIGRATION FOR INDIVIDUAL ANIMALS
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

### LOOP TO CALCULATE START AND END DATES FOR AUTUMN MIGRATION #######
### based on Evan's script to use models, but if model fails we use basic rules of thumb
### output is written as dates into a summary table and migration data are collated in a data.frame
### non-existent migrations are purged from the dataset

all_migdata<-data.frame()		### create blank data frame that will hold all the data associated with proper autumn migration

for (a in migs){
  x<-migration %>% filter(id.yr.season==a) %>% mutate(Day=as.Date(DateTime))
  
  if (dim(x)[1] <20 | max(x$home_dist)<500) {
    mig_summary$start_mig[mig_summary$id.yr.season==a]<- NA
    mig_summary$end_mig[mig_summary$id.yr.season==a]<- NA
  } else {
  
  
  plot(home_dist~DateTime, data=x,type='l', col=3)
  plot(lat~long, data=x,type='p', col=3, pch=16)
  
  
  ### create LTRAJ object to fit movement model
  d2 <- as.ltraj(xy = x[, c("utm.e", "utm.n")], date = x$DateTime, id=a)
  try(d3 <- mvmtClass(d2),silent=T)
  migr.dates <-  mvmt2dt(d3, p = 0.01, mod = "disperser")      ## replaced 0.05 with 0.01 as it includes most of the migration

  ### write output in summary if models converge and produce output
  if(is.null(migr.dates[[1]])==FALSE){
    mig_summary$start_mig[mig_summary$id.yr.season==a]<- migr.dates[[1]]$date[1]
    mig_summary$end_mig[mig_summary$id.yr.season==a]<- migr.dates[[1]]$date[2]
  }
  

  ## ALTERNATIVE DEFINITION WITH SIMPLE THRESHOLDS - MIGRATION STARTS WHEN DIST TO HOME CONTINUOUSLY INCREASES
  dailyhomedist<- x  %>% group_by(Day) %>%
    summarise(away=max(home_dist))
  
  ### find the first day where home_dist is greater than on any day before, and where home_dist is greater on any day afterwards
  start<-NA
  for (d in 2:(dim(dailyhomedist)[1]-1)){
    maxbef<-max(dailyhomedist$away[1:(d-1)])
    minaft<-min(dailyhomedist$away[(d+1):dim(dailyhomedist)[1]])
    dmax<-d
    if(is.na(start)==TRUE) {     ## prevent that the first day gets overwritten by subsequent days
      if(dailyhomedist$away[d]>maxbef & dailyhomedist$away[d]<minaft){start<-dailyhomedist$Day[d]} 
    }
    if(is.na(start)==FALSE) break
  }  # end loop over every day in the data set
  
  
  ### going backwards, find the first day where home_dist is smaller than on any day afterwards, and where home_dist is smaller on any day before
  end<-NA
  for (d in (dim(dailyhomedist)[1]):dmax){
    maxbef<-max(dailyhomedist$away[dmax:(d-1)])
    minaft<-min(dailyhomedist$away[(d):dim(dailyhomedist)[1]])
    if(is.na(end)==TRUE) {     ## prevent that the first day gets overwritten by subsequent days
      if(dailyhomedist$away[d]>maxbef & dailyhomedist$away[d]<=minaft){end<-dailyhomedist$Day[d]} 
    }
    
    # if(is.na(start)==FALSE & is.na(end)==TRUE) {     ## prevent that the end is defined before the start
    #   if(dailyhomedist$away[d]>maxbef & dailyhomedist$away[d]>=minaft){end<-dailyhomedist$Day[d]}
    # }
    if(is.na(end)==FALSE) break
  }  # end loop over every day in the data set
  

  
  
  
  ###  if this simple threshold cannot find an end date, then there is something suspicious and we either use the model output or the start date to flag up that the migration is suspicious
  if(is.na(end)==TRUE ){
    end<-ifelse((is.null(migr.dates[[1]])==FALSE),migr.dates[[1]]$date[2],start)
  }
  
  
  ### if this simple threshold cannot find a start date, then there is no migration
  if(is.na(start)==TRUE){
    mig_summary$start_mig[mig_summary$id.yr.season==a]<- NA
    mig_summary$end_mig[mig_summary$id.yr.season==a]<- NA
  } else {
    

    
    
    ### overwrite output in summary if it extends migration - THIS MAY NEED ADJUSTMENT IF THE ALGORITHM IDENTIFIES TOO MUCH?
    if(is.null(migr.dates[[1]])==FALSE){
      if(start<migr.dates[[1]]$date[1]){mig_summary$start_mig[mig_summary$id.yr.season==a]<- min(x$DateTime[x$Day==start])}
      if(end>migr.dates[[1]]$date[2]){mig_summary$end_mig[mig_summary$id.yr.season==a]<- max(x$DateTime[x$Day==end])}
    }else{
      mig_summary$start_mig[mig_summary$id.yr.season==a]<- min(x$DateTime[x$Day==start])
      mig_summary$end_mig[mig_summary$id.yr.season==a]<- max(x$DateTime[x$Day==end])
    }
    
    
    
    ### visually assess whether these dates make sense
    mig_time<-interval(start=mig_summary$start_mig[mig_summary$id.yr.season==a],end=mig_summary$end_mig[mig_summary$id.yr.season==a])
    x<- x %>% mutate(MIG=if_else(DateTime %within% mig_time,2,3))
    plot(home_dist~DateTime, data=x,type='p', col=x$MIG, pch=16, cex=0.4)

    xmig<- x %>% filter(MIG==2)
    all_migdata<- rbind(all_migdata,xmig)
    
    
    
    
    }  ## close second else loop for migrations with a start date
  
  }  ## close first else loop for insufficient data sets


}		#closes the animal loop





#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# SAVE MIGRATION DATA AND MIGRATION SUMMARY
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
head(all_migdata)
dim(all_migdata)
mig_summary
fwrite(mig_summary,"EGVU_migration_start_end_dates.csv")
fwrite(all_migdata,"EGVU_migration_hourly_data.csv")

