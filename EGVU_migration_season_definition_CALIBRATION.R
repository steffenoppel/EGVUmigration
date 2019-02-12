##########################################################################
# DEFINITION OF MIGRATORY SEASONS FOR SATELLITE-TRACKED EGYPTIAN VULTURES
# original script written by Steffen Oppel in September 2014
# modified in February 2016
# re-written on 9 February 2019 to include Evan Buechley's NSD model approach
##########################################################################

## updated 10 Feb to include date comparison with manually annotated tracks
## updated 11 Feb to add more refined methods from migrateR package for start location and refine p.Est
## updated 12 February to calibrate algorithmic start and end definition using manually annotated migrations

# Load necessary library
library(maptools)
library(sp)
library(rgdal)
require(maps)
require(mapdata)
require(geosphere)
library(adehabitatLT)
library(nlme)
library(lubridate)
library(ggplot2)
library(scales)
library(tidyverse)
library(migrateR)
library(data.table)
library(readxl)



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


migsDATA<-unique(migration$id.yr.season) ## specify the unique migration journeys






#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# READ IN MANUALLY ANNOTATED DATA FOR INDIVIDUAL MIGRATION JOURNEYS
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


# Set working directory
setwd("C:\\STEFFEN\\MANUSCRIPTS\\in_prep\\EGVU_papers\\FrontiersMigrationPaper\\EGVUmigration")

# read in results tables
manudates<- read_xlsx("EV_mig_calibration.xlsx", sheet="complete migrations")
manudates <- manudates %>% rename(id.yr.season=group,season=season,start_mig_MANU=begin, end_mig_MANU=end)
head(manudates)


### MANUAL ANNNOTATION FROM CLEMENTINE BOUGAIN's THESIS ####

manudates$end_mig_MANU[manudates$id.yr.season=="Dobromir_2014_spring"]<-ymd("2014-06-03")
manudates$start_mig_MANU[manudates$id.yr.season=="Dobromir_2015_spring"]<-ymd("2015-04-30")
manudates$end_mig_MANU[manudates$id.yr.season=="Dobromir_2015_spring"]<-ymd("2015-05-15")
manudates$start_mig_MANU[manudates$id.yr.season=="Sanie_2015_spring"]<-ymd("2015-05-04")
manudates$end_mig_MANU[manudates$id.yr.season=="Sanie_2015_spring"]<-ymd("2015-06-18")
manudates$start_mig_MANU[manudates$id.yr.season=="Castor_2015_spring"]<-ymd("2015-03-07")
manudates$end_mig_MANU[manudates$id.yr.season=="Castor_2015_spring"]<-ymd("2015-04-05")
manudates$start_mig_MANU[manudates$id.yr.season=="Lazaros_2013_spring"]<-ymd("2013-03-08")
manudates$end_mig_MANU[manudates$id.yr.season=="Lazaros_2013_spring"]<-ymd("2013-03-31")
manudates$start_mig_MANU[manudates$id.yr.season=="Iliaz_2016_spring"]<-ymd("2016-03-17")
manudates$end_mig_MANU[manudates$id.yr.season=="Iliaz_2016_spring"]<-ymd("2016-05-09")
manudates$start_mig_MANU[manudates$id.yr.season=="Boris_2016_spring"]<-ymd("2016-03-01")
manudates$end_mig_MANU[manudates$id.yr.season=="Boris_2016_spring"]<-ymd("2016-03-20")
manudates$start_mig_MANU[manudates$id.yr.season=="Jenny_2016_spring"]<-ymd("2016-03-16")
manudates$end_mig_MANU[manudates$id.yr.season=="Jenny_2016_spring"]<-ymd("2016-04-16")
manudates$start_mig_MANU[manudates$id.yr.season=="Dobromir_2016_spring"]<-ymd("2016-05-01")
manudates$start_mig_MANU[manudates$id.yr.season=="Sanie_2016_spring"]<-ymd("2016-04-13")
manudates$start_mig_MANU[manudates$id.yr.season=="Aoos_2016_spring"]<-ymd("2016-03-16")
manudates$end_mig_MANU[manudates$id.yr.season=="Aoos_2016_spring"]<-ymd("2016-04-18")
manudates$end_mig_MANU[manudates$id.yr.season=="Dobromir_2016_spring"]<-ymd("2016-05-30")
manudates$end_mig_MANU[manudates$id.yr.season=="Sanie_2016_spring"]<-ymd("2016-05-13")
manudates$start_mig_MANU[manudates$id.yr.season=="Sanie_2015_fall"]<-ymd("2015-07-20")
manudates$start_mig_MANU[manudates$id.yr.season=="Dobromir_2015_fall"]<-ymd("2015-08-22")



### remove journeys that have no annotated dates and are useless for calibration

manudates<- manudates %>% filter(!is.na(start_mig_MANU))
dim(manudates)
migsCALIB<-unique(manudates$id.yr.season)

migs<-migsCALIB[migsCALIB %in% migsDATA]


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# CALCULATE START AND END DATES OF AUTUMN MIGRATION FOR INDIVIDUAL ANIMALS
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

### MASSIVE SENSITIVITY OF DATES TO SPECIFIED PERCENTAGE OF MIGRATION< HENCE WE LOOP OVER SOLUTIONS
propMig<-seq(0.001,0.05,0.001)



### LOOP TO CALCULATE START AND END DATES FOR AUTUMN MIGRATION #######
### COMPARE WITH MANUALLY ANNOTATED DATES AND CALCULATE DIFFERENCE
### based on Evan's script to use models, but if model fails we use basic rules of thumb


mig_calibration<-data.frame()		### create blank data frame that will hold all the data to evaluate accuracy of algorithmic start and end definition

for (a in migs){
  x<-migration %>% filter(id.yr.season==a) %>% mutate(Day=as.Date(DateTime))
  
  ### ~~~~~~~~~ 1. EXTRACT MANUAL START AND END DATES ~~~~~~~~~~~~~~~~ ###
  
  MANU_START<-manudates$start_mig_MANU[manudates$id.yr.season==a]
  MANU_END<-manudates$end_mig_MANU[manudates$id.yr.season==a]
  
  
  
  ### ~~~~~~~~~ 2. DEFINE START AND END DATES WITH SIMPLE THRESHOLDS ~~~~~~~~~~~~~~~~ ###
  ## MIGRATION STARTS WHEN DIST TO HOME CONTINUOUSLY INCREASES
  
  dailyhomedist<- x  %>% group_by(Day) %>%
    summarise(away=max(home_dist))
  
  ### find the first day where home_dist is greater than on any day before, and where home_dist is greater on any day afterwards
  THRESH_start<-NA
  for (d in 2:(dim(dailyhomedist)[1]-1)){
    maxbef<-max(dailyhomedist$away[1:(d-1)])
    minaft<-min(dailyhomedist$away[(d+1):dim(dailyhomedist)[1]])
    dmax<-d
    if(is.na(THRESH_start)==TRUE) {     ## prevent that the first day gets overwritten by subsequent days
      if(dailyhomedist$away[d]>maxbef & dailyhomedist$away[d]<minaft){THRESH_start<-dailyhomedist$Day[d]} 
    }
    if(is.na(THRESH_start)==FALSE) break
  }  # end loop over every day in the data set
  
  
  ### going backwards, find the first day where home_dist is smaller than on any day afterwards, and where home_dist is smaller on any day before
  THRESH_end<-NA
  for (d in (dim(dailyhomedist)[1]):dmax){
    maxbef<-max(dailyhomedist$away[dmax:(d-1)])
    minaft<-min(dailyhomedist$away[(d):dim(dailyhomedist)[1]])
    if(is.na(THRESH_end)==TRUE) {     ## prevent that the first day gets overwritten by subsequent days
      if(dailyhomedist$away[d]>maxbef & dailyhomedist$away[d]<=minaft){THRESH_end<-dailyhomedist$Day[d]} 
    }
    
    # if(is.na(start)==FALSE & is.na(end)==TRUE) {     ## prevent that the end is defined before the start
    #   if(dailyhomedist$away[d]>maxbef & dailyhomedist$away[d]>=minaft){end<-dailyhomedist$Day[d]}
    # }
    if(is.na(THRESH_end)==FALSE) break
  }  # end loop over every day in the data set
  
  
  ### CAPTURE OUTPUT FOR CALIBRATION 
  THRESH_calib<-data.frame('id.yr.season'=a, 'point'=c('start','end'),'prop'=0,'DIFF'=NA)
  if(is.na(THRESH_start)==FALSE){THRESH_calib$DIFF[1]<-as.numeric(difftime(MANU_START,THRESH_start,units='days'))} 
  if(is.na(THRESH_end)==FALSE){THRESH_calib$DIFF[2]<-as.numeric(difftime(MANU_END,THRESH_end,units='days'))} 
  mig_calibration<-rbind(mig_calibration,THRESH_calib)
  rm(THRESH_calib,THRESH_end,THRESH_start)
 
  
  
   
  ### ~~~~~~~~~ 3. DEFINE START AND END DATES WITH rNSD MODEL ~~~~~~~~~~~~~~~~ ###

  ### create LTRAJ object to fit movement model
  d2 <- as.ltraj(xy = x[, c("utm.e", "utm.n")], date = x$DateTime, id=a)
  
  ### identify the start locations rather than just using the first point
  try(rlocs<- findrloc(d2), silent=T)
  
  ### fit the classification model with or without an optimal number of starting points
  if(rlocs %in% ls()){
    try(d3 <- mvmtClass(d2, rloc=rlocs$rloc),silent=T)
  }else{
    try(d3 <- mvmtClass(d2),silent=T)
  }
  
  ### refine the classification model given that we know delta should be >500 km
  informPrior<-pEst(l.d=250000)   ## poor documentation of function - no idea in what unit this is required!
  try(d3<-refine(d3,p.est=informPrior),silent=T)
  
  
  
  ### extract start and end dates FOR EVERY PROPORTION OF MIGRATION 
  
  for (m in propMig){
    MODEL_calib<-data.frame('id.yr.season'=a, 'point'=c('start','end'),'prop'=m,'DIFF'=NA)
    migr.dates <-  mvmt2dt(d3, p = m, mod = "disperser")      ## EXTRACT DATES FOR A GIVEN PROPORTION OF MIGRATION

    ### write output in summary if models converge and produce output
    if(is.null(migr.dates[[1]])==FALSE){
      MODEL_calib$DIFF[1]<-as.numeric(difftime(MANU_START,migr.dates[[1]]$date[1],units='days'))
      MODEL_calib$DIFF[2]<-as.numeric(difftime(MANU_END,migr.dates[[1]]$date[2],units='days'))
    }
    
    ## capture output and reset
    mig_calibration<-rbind(mig_calibration,MODEL_calib)
    rm(MODEL_calib,migr.dates)
  } ## end loop over all proportions of migration
  
  rm(d3,rlocs)
  

fwrite(mig_calibration,"EGVU_migration_calibration_dates.csv")

}		#closes the animal loop





#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# PLOT THE PROPORTION MIGRATION THRESHOLD FOR START / END FOR EACH SEASON
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

mig_calibration <- mig_calibration %>% mutate(season=ifelse(grepl("spring",id.yr.season)==TRUE,"spring","fall")) %>%
  mutate(season=factor(season, labels=c("fall","spring"))) %>%
  mutate(point=factor(point, labels=c("end","start")))
  
head(mig_calibration)
dim(mig_calibration)

pdf("EGVU_mig_def_calbration.pdf", width=8, height=8)
ggplot(mig_calibration, aes(x=prop,y=DIFF)) +
  geom_point(size=1) +
  geom_smooth() +
  facet_grid(season ~ point,scales = "fixed") + 
  xlab("proportion of migration remaining") +
  ylab("Difference between manual and model (days)") +
  theme(panel.background=element_rect(fill="white", colour="black"), 
        axis.text=element_text(size=16, color="black"), 
        axis.title=element_text(size=20),
        strip.text.x=element_text(size=18, color="black"),
        strip.text.y=element_text(size=18, color="black"),
        strip.background=element_rect(fill="white", colour="black"),
        panel.margin.x=unit(1, "lines"), 		###panel.spacing.x
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        panel.border = element_blank())

dev.off()
  


