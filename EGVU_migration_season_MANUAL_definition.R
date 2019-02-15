##########################################################################
# DEFINITION OF MIGRATORY SEASONS FOR SATELLITE-TRACKED EGYPTIAN VULTURES
# original script written by Steffen Oppel in September 2014
# modified in February 2016
# re-written on 9 February 2019 to include Evan Buechley's NSD model approach
##########################################################################

## updated 10 Feb to include date comparison with manually annotated tracks
## updated 14 Feb to facilitate easy manual annotation of remaining tracks

# Load necessary library
library(maptools)
library(sp)
require(maps)
require(mapdata)
require(geosphere)
library(lubridate)
library(ggplot2)
basemap <- map_data("world")
library(scales)
library(tidyverse)
library(data.table)
library(readxl)
library(plotly)


### DEFINE FUNCTIONS TO MANUALLY ENTER DATES ###

readStartDate <- function(){
   no <- readline(prompt="Is the suggested START date correct? (y/n)")
   if(no=='n'){
       start <- ymd(readline(prompt="Enter correct start date as YYYY-mm-dd"))
      }
}
readEndDate <- function(){
  no <- readline(prompt="Is the suggested END date correct? (y/n)")
  if(no=='n'){
    start <- ymd(readline(prompt="Enter correct end date as YYYY-mm-dd"))
  }
}



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# LOAD PREVIOUSLY SAVED DATA (prepared in script 2.EV-all-migration delineation.R)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Set working directory
setwd("C:\\STEFFEN\\MANUSCRIPTS\\in_prep\\EGVU_papers\\FrontiersMigrationPaper\\EGVUmigration")

# # read in clean csv
# locs = read.csv("EV-all_1ptperhr-filtered-utm-NSD-season.csv")
# head(locs)
# 
# migs<-unique(locs$id.yr.season) ## specify the unique migration journeys
# migs<-migs[!migs %in% c("Cabuk_2016_spring")]


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# CALCULATE TRAVEL DISTANCES AND SPEEDS 
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## this is not a particularly efficient way to calculate that, but it is easy to understand and manipulate
## run time ~25 min

# migration<-data.frame()
# 
# migs<-migs[!migs %in% unique(migration$id.yr.season)]     ## use this line if loop was interrupted
# for (a in migs){
#   
#   input<-locs %>% filter(id.yr.season==a) %>% 
#     mutate(DateTime=ymd_h(paste(Year,Month, Day, Hour, sep=","))) %>%
#     dplyr::select(id.yr.season,study,tag,id,DateTime, long, lat, NSD, ND,utm.e,utm.n) %>% arrange(DateTime) %>%
#     mutate(step_dist=0,home_dist=0,cumul_dist=0,time_diff=0,speed=0)
#   first<-SpatialPoints(data.frame(input$long[1], input$lat[1]), proj4string=CRS("+proj=longlat + datum=wgs84"))
#   
#   for (l in 2: dim(input)[1]){
#     input$time_diff[l]<-as.numeric(difftime(input$DateTime[l],input$DateTime[l-1], units="hours"))
#     fromloc<-SpatialPoints(data.frame(input$long[l-1], input$lat[l-1]), proj4string=CRS("+proj=longlat + datum=wgs84"))
#     toloc<-SpatialPoints(data.frame(input$long[l], input$lat[l]), proj4string=CRS("+proj=longlat + datum=wgs84"))
#     input$step_dist[l]<-spDistsN1(fromloc, toloc, longlat=T)
#     input$home_dist[l]<-spDistsN1(first, toloc, longlat=T)
#     input$cumul_dist[l]<-sum(input$step_dist)
#     input$speed[l]<-input$step_dist[l]/input$time_diff[l]
#   }
#   
#   migration<-rbind(migration, input)
#   
# }
# 
#fwrite(migration,"EGVU_migration_preformatted.csv")




#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# READ IN MANUALLY ANNOTATED DATA FOR INDIVIDUAL MIGRATION JOURNEYS
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


# Set working directory
setwd("C:\\STEFFEN\\MANUSCRIPTS\\in_prep\\EGVU_papers\\FrontiersMigrationPaper\\EGVUmigration")

# read in raw migration data (prepared in script 2.EV-all-migration delineation.R and EGVU_migration_season_definition_CALIBRATION.R)
migration<-fread("EGVU_migration_preformatted.csv")

## remove non-existent migrations [they should already be excluded...]

migration<-migration[!(migration$id.yr.season=="Agri_2013_spring"),]
migration<-migration[!(migration$id.yr.season=="Agri_2014_fall"),]
migration<-migration[!(migration$id.yr.season=="Ardahan_2013_spring"),]
migration<-migration[!(migration$id.yr.season=="Haydi_2014_spring"),]
migration<-migration[!(migration$id.yr.season=="Iste_2014_spring"),]
migration<-migration[!(migration$id.yr.season=="Serhat_2014_spring"),]
migration<-migration[!(migration$id.yr.season=="Tuzluca_2013_spring"),]
migration<-migration[!(migration$id.yr.season=="Tuzluca_2016_fall"),]
migration<-migration[!(migration$id.yr.season=="Batuecasa_2017_spring"),]
migration<-migration[!(migration$id.yr.season=="Huebra_2017_spring"),]
migration<-migration[!(migration$id.yr.season=="2HN_2017_fall"),]
migration<-migration[!(migration$id.yr.season=="2HN_2017_spring"),]
migration<-migration[!(migration$id.yr.season=="Douro_2017_spring"),]
migration<-migration[!(migration$id.yr.season=="Faia_2017_spring"),]
migration<-migration[!(migration$id.yr.season=="Faia_2018_fall"),]
migration<-migration[!(migration$id.yr.season=="Poiares_2017_spring"),]
migration<-migration[!(migration$id.yr.season=="Akaga_2018_spring"),]
migration<-migration[!(migration$id.yr.season=="Aoos_2015_spring"),]
migration<-migration[!(migration$id.yr.season=="Boyana_2018_spring"),]
migration<-migration[!(migration$id.yr.season=="Castor_2014_spring"),]
migration<-migration[!(migration$id.yr.season=="Batuecasa_2017_spring"),]
migration<-migration[!(migration$id.yr.season=="Lazaros_2012_spring"),]
migration<-migration[!(migration$id.yr.season=="Polya_2018_spring"),]
migration<-migration[!(migration$id.yr.season=="Volen_2014_fall"),]
migration<-migration[!(migration$id.yr.season=="A75658_2011_spring"),]
migration<-migration[!(migration$id.yr.season=="A75658_2011_fall"),]
migration<-migration[!(migration$id.yr.season=="A75659_2011_fall"),]
migration<-migration[!(migration$id.yr.season=="A80420_2013_fall"),]
migration<-migration[!(migration$id.yr.season=="A89731_2013_fall"),]
migration<-migration[!(migration$id.yr.season=="Pyrenees_2016_fall"),]
migration<-migration[!(migration$id.yr.season=="A89731_2012_fall"),]
migration<-migration[!(migration$id.yr.season=="Ardahan_2014_fall"),]
migration<-migration[!(migration$id.yr.season=="95R_2017_spring"),]
migration<-migration[!(migration$id.yr.season=="2HN_2016_spring"),]
migration<-migration[!(migration$id.yr.season=="2HN_2016_fall"),]
migration<-migration[!(migration$id.yr.season=="95R_2017_fall"),]
migration<-migration[!(migration$id.yr.season=="A75658_2010_spring"),]
migration<-migration[!(migration$id.yr.season=="A75658_2010_fall"),]
migration<-migration[!(migration$id.yr.season=="Anna_2018_fall"),]
migration<-migration[!(migration$id.yr.season=="BatuecasP_2017_fall"),]
migration<-migration[!(migration$id.yr.season=="Iliaz_2013_spring"),]
migration<-migration[!(migration$id.yr.season=="Iliaz_2013_fall"),]
migration<-migration[!(migration$id.yr.season=="Iliaz_2014_fall"),]
migration<-migration[!(migration$id.yr.season=="Levkipos_2013_spring"),]
migration<-migration[!(migration$id.yr.season=="Levkipos_2013_fall"),]
migration<-migration[!(migration$id.yr.season=="Mille_2014_fall"),]
migration<-migration[!(migration$id.yr.season=="Sanie_2014_fall"),]
migration<-migration[!(migration$id.yr.season=="Svetlina_2013_fall"),]
migration<-migration[!(migration$id.yr.season=="Volen_2013_spring"),]






## remove other data sets that are marginal ##
migsDATA<-unique(migration$id.yr.season) ## specify all the unique migration journeys
dim(migration)
for (a in migsDATA){
  x<-migration %>% filter(id.yr.season==a) %>% mutate(Day=as.Date(DateTime))
  if (dim(x)[1] <20 | max(x$home_dist)<500) {
  print(sprintf("%s is not a proper migratory journey",a))
  migration<-migration %>% filter(id.yr.season != a)
  } 
}
dim(migration)
migsDATA<-unique(migration$id.yr.season) ## specify all the unique migration journeys that have passed the basic filter


# # read in results tables
# manudates<- read_xlsx("EV_mig_calibration.xlsx", sheet="complete migrations")
# manudates <- manudates %>% rename(id.yr.season=group,season=season,start_mig_MANU=begin, end_mig_MANU=end)
# head(manudates)
# 
# 
# ### MANUAL ANNNOTATION FROM CLEMENTINE BOUGAIN's THESIS ####
# manudates$end_mig_MANU[manudates$id.yr.season=="Dobromir_2014_spring"]<-ymd("2014-06-03")
# manudates$start_mig_MANU[manudates$id.yr.season=="Dobromir_2015_spring"]<-ymd("2015-04-30")
# manudates$end_mig_MANU[manudates$id.yr.season=="Dobromir_2015_spring"]<-ymd("2015-05-15")
# manudates$start_mig_MANU[manudates$id.yr.season=="Sanie_2015_spring"]<-ymd("2015-05-04")
# manudates$end_mig_MANU[manudates$id.yr.season=="Sanie_2015_spring"]<-ymd("2015-06-18")
# manudates$start_mig_MANU[manudates$id.yr.season=="Castor_2015_spring"]<-ymd("2015-03-07")
# manudates$end_mig_MANU[manudates$id.yr.season=="Castor_2015_spring"]<-ymd("2015-04-05")
# manudates$start_mig_MANU[manudates$id.yr.season=="Lazaros_2013_spring"]<-ymd("2013-03-08")
# manudates$end_mig_MANU[manudates$id.yr.season=="Lazaros_2013_spring"]<-ymd("2013-03-31")
# manudates$start_mig_MANU[manudates$id.yr.season=="Iliaz_2016_spring"]<-ymd("2016-03-17")
# manudates$end_mig_MANU[manudates$id.yr.season=="Iliaz_2016_spring"]<-ymd("2016-05-09")
# manudates$start_mig_MANU[manudates$id.yr.season=="Boris_2016_spring"]<-ymd("2016-03-01")
# manudates$end_mig_MANU[manudates$id.yr.season=="Boris_2016_spring"]<-ymd("2016-03-20")
# manudates$start_mig_MANU[manudates$id.yr.season=="Jenny_2016_spring"]<-ymd("2016-03-16")
# manudates$end_mig_MANU[manudates$id.yr.season=="Jenny_2016_spring"]<-ymd("2016-04-16")
# manudates$start_mig_MANU[manudates$id.yr.season=="Dobromir_2016_spring"]<-ymd("2016-05-01")
# manudates$start_mig_MANU[manudates$id.yr.season=="Sanie_2016_spring"]<-ymd("2016-04-13")
# manudates$start_mig_MANU[manudates$id.yr.season=="Aoos_2016_spring"]<-ymd("2016-03-16")
# manudates$end_mig_MANU[manudates$id.yr.season=="Aoos_2016_spring"]<-ymd("2016-04-18")
# manudates$end_mig_MANU[manudates$id.yr.season=="Dobromir_2016_spring"]<-ymd("2016-05-30")
# manudates$end_mig_MANU[manudates$id.yr.season=="Sanie_2016_spring"]<-ymd("2016-05-13")
# manudates$start_mig_MANU[manudates$id.yr.season=="Sanie_2015_fall"]<-ymd("2015-07-20")
# manudates$start_mig_MANU[manudates$id.yr.season=="Dobromir_2015_fall"]<-ymd("2015-08-22")
#fwrite(manudates,"EGVU_migration_dates_manually_classified.csv")
#manudates<-fread("EGVU_migration_dates_manually_classified.csv")



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# COMPILE ALL THE DATASETS THAT HAVE ALREADY BEEN MANUALLY ANNOTATED
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

mig_dates1<-fread("EGVU_migration_dates_manually_classified_PART2.csv")%>%
  mutate(start=ymd(start)) %>%
  mutate(end=ymd(end)) 
mig_dates2<-fread("EGVU_migration_dates_manually_classified.csv")%>%
  mutate(start=as.Date(start_mig_MANU)) %>%
  mutate(end=as.Date(end_mig_MANU))  %>%
  filter(!(start_mig_MANU=="")) %>%
  dplyr::select(id.yr.season,start,end)
mig_dates3<-fread("migration.dates.mideast.csv") %>%
  mutate(start=as.Date(start, format="%m/%d/%y")) %>%
  mutate(end=as.Date(end, format="%m/%d/%y")) ## opened, modified, and saved in MS Excel in US date format
mig_dates1<-fread("EGVU_migration_dates_manually_classified_PART3.csv")%>%
  filter(!(id.yr.season %in% mig_dates1$id.yr.season))%>%
  mutate(start=ymd(start)) %>%
  mutate(end=ymd(end)) 

all_migdates<-rbind(mig_dates1,mig_dates2,mig_dates3)
fwrite(all_migdates,"EGVU_manually_classified_migration_dates.csv")




#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
### IDENTIFY THOSE JOURNEYS THAT STILL NEED TO BE ANNOTATED
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

mig_dates<-fread("EGVU_manually_classified_migration_dates.csv")
mig_dates$start<-ymd(mig_dates$start) ## use dmy if you opened, modified, and saved in MS Excel
mig_dates$end<-ymd(mig_dates$end) ## use dmy if you opened, modified, and saved in MS Excel
NEEDEDmigs<-migsDATA[!(migsDATA %in% mig_dates$id.yr.season)]
counter=1




#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# MANUALLY REPEAT THE CODE FROM THIS LINE ONWARDS
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
### THIS INTERACTIVE CODE DOES NOT RUN IN A LOOP!!
### IT REQUIRES MANUAL INCREMENTS TO GO THROUGH EACH MIGRATORY JOURNEY

counter=counter+1


a=NEEDEDmigs[counter]  
source("manual_threshold_function.R")

### ~~~~~~~~~ 2. SHOW THE INTERACTIVE GRAPH OF DISTANCE TO SELECT APPROPRIATE DATES ~~~~~~~~~~~~~~~~ ###
## visually assess whether the threshold dates make sense

distgraph<-ggplot(x) + geom_point(aes(x=DateTime, y=home_dist, col=MIG)) + scale_x_datetime(date_breaks="2 weeks", date_labels="%b-%Y")
ggplotly(distgraph)

### ~~~~~~~~~ 3. SHOW A MAP WITH MIGRATION LOCATIONS ~~~~~~~~~~~~~~~~ ###
## geographically assess whether the threshold dates make sense

if(dim(xmig)[1]>5){  
    ggplot() + geom_polygon(data = basemap, aes(x=long, y = lat, group = group)) + 
      coord_fixed(xlim = xlim,  ylim = ylim, ratio = 1.3)+
      geom_path(data=x, aes(x=long, y=lat))+
      geom_point(data=xmig, aes(x=long, y=lat),col='darkred',size=1.2)
    }else{
      ggplot() + geom_polygon(data = basemap, aes(x=long, y = lat, group = group)) + 
        coord_fixed(xlim = c(min(x$long)-3,max(x$long)+3),  ylim = c(min(x$lat)-6,max(x$lat)+6), ratio = 1.3)+
        geom_path(data=x, aes(x=long, y=lat))
      }

### ~~~~~~~~~ 4. FILL IN START AND END DATE MANUALLY ~~~~~~~~~~~~~~~~ ###
source('C:/STEFFEN/MANUSCRIPTS/in_prep/EGVU_papers/FrontiersMigrationPaper/EGVUmigration/manual_annotation_function.R')






#### IF THERE WAS NO MIGRATION REMOVE THE LINE 
mig_dates<-mig_dates[!mig_dates$id.yr.season==a,]







    
### THIS BELOW DID NOT WORK #################################################
    
    # 
    # 
    # 
    # #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # # MANUALLY ANNOTATE START AND END DATES OF MIGRATION FOR INDIVIDUAL ANIMALS
    # #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # 
    # 
    # ### LOOP TO CALCULATE START AND END DATES FOR AUTUMN MIGRATION #######
    # ### COMPARE WITH MANUALLY ANNOTATED DATES AND CALCULATE DIFFERENCE
    # ### based on Evan's script to use models, but if model fails we use basic rules of thumb
    # 
    # 
    # #mig_dates<-data.frame()		### create blank data frame that will hold all the data to evaluate accuracy of algorithmic start and end definition
    # mig_dates<-fread("EGVU_migration_dates_manually_classified_PART2.csv")
    # mig_dates$start<-ymd(mig_dates$start)
    # mig_dates$end<-ymd(mig_dates$end)
    # 
    # NEEDEDmigs<-NEEDEDmigs[!(NEEDEDmigs %in% mig_dates$id.yr.season)]
    # 
    # for (a in NEEDEDmigs){
    #   
    #   
    #   
    #   ### SELECT THE DATA FOR THIS ANIMAL
    #   x<-migration %>% filter(id.yr.season==a) %>% mutate(Day=as.Date(DateTime))
    #   
    #   if (dim(x)[1] <20 | max(x$home_dist)<500) {
    #     print(sprintf("%s is not a proper migratory journey",a))
    #   } else {
    #     
    #     print(sprintf("starting with migration journey %s",a))
    #     
    #     ### ~~~~~~~~~ 1. DEFINE START AND END DATES WITH SIMPLE THRESHOLDS ~~~~~~~~~~~~~~~~ ###
    #     ## MIGRATION STARTS WHEN DIST TO HOME CONTINUOUSLY INCREASES
    #     
    #     dailyhomedist<- x  %>% group_by(Day) %>%
    #       summarise(away=max(home_dist))
    #     
    #     ### find the first day where home_dist is greater than on any day before, and where home_dist is greater on any day afterwards
    #     THRESH_start<-NA
    #     for (d in 2:(dim(dailyhomedist)[1]-1)){
    #       maxbef<-max(dailyhomedist$away[1:(d-1)])
    #       minaft<-min(dailyhomedist$away[(d+1):dim(dailyhomedist)[1]])
    #       dmax<-d
    #       if(is.na(THRESH_start)==TRUE) {     ## prevent that the first day gets overwritten by subsequent days
    #         if(dailyhomedist$away[d]>maxbef & dailyhomedist$away[d]<minaft){THRESH_start<-dailyhomedist$Day[d]} 
    #       }
    #       if(is.na(THRESH_start)==FALSE) break
    #     }  # end loop over every day in the data set
    #     
    #     
    #     ### going backwards, find the first day where home_dist is smaller than on any day afterwards, and where home_dist is smaller on any day before
    #     THRESH_end<-NA
    #     for (d in (dim(dailyhomedist)[1]):dmax){
    #       maxbef<-max(dailyhomedist$away[dmax:(d-1)])
    #       minaft<-min(dailyhomedist$away[(d):dim(dailyhomedist)[1]])
    #       if(is.na(THRESH_end)==TRUE) {     ## prevent that the first day gets overwritten by subsequent days
    #         if(dailyhomedist$away[d]>maxbef & dailyhomedist$away[d]<=minaft){THRESH_end<-dailyhomedist$Day[d]} 
    #       }
    #       
    #       # if(is.na(start)==FALSE & is.na(end)==TRUE) {     ## prevent that the end is defined before the start
    #       #   if(dailyhomedist$away[d]>maxbef & dailyhomedist$away[d]>=minaft){end<-dailyhomedist$Day[d]}
    #       # }
    #       if(is.na(THRESH_end)==FALSE) break
    #     }  # end loop over every day in the data set
    #     
    #     
    #     
    #     
    #     ### ~~~~~~~~~ 2. SHOW THE INTERACTIVE GRAPH OF DISTANCE TO SELECT APPROPRIATE DATES ~~~~~~~~~~~~~~~~ ###
    #     ## visually assess whether the threshold dates make sense
    #     
    #     mig_time<-interval(start=THRESH_start,end=THRESH_end)
    #     x<- x %>% mutate(MIG=if_else(Day %within% mig_time,"migrating","stationary")) %>%
    #       mutate(MIG=if_else(is.na(MIG),"stationary",MIG))
    #     
    #     
    #     distgraph<-ggplot(x) + geom_point(aes(x=DateTime, y=home_dist, col=MIG))
    #     ggplotly(distgraph)
    #     
    #     
    #     
    #     ### ~~~~~~~~~ 3. SHOW A MAP WITH MIGRATION LOCATIONS ~~~~~~~~~~~~~~~~ ###
    #     ## geographically assess whether the threshold dates make sense
    #     
    #     xmig<- x %>% filter(MIG=="migrating")
    #     xlim<-c(min(xmig$long)-3,max(xmig$long)+3)
    #     ylim<-c(min(xmig$lat)-3,max(xmig$lat)+3)
    #     
    #     if(dim(xmig)[1]>5){  
    #       ggplot() + geom_polygon(data = basemap, aes(x=long, y = lat, group = group)) + 
    #         coord_fixed(xlim = xlim,  ylim = ylim, ratio = 1.3)+
    #         geom_path(data=x, aes(x=long, y=lat))+
    #         geom_point(data=xmig, aes(x=long, y=lat),col='darkred',size=1.2)
    #     }else{
    #       ggplot() + geom_polygon(data = basemap, aes(x=long, y = lat, group = group)) + 
    #         coord_fixed(xlim = c(min(x$long)-3,max(x$long)+3),  ylim = c(min(x$lat)-3,max(x$lat)+3), ratio = 1.3)+
    #         geom_path(data=x, aes(x=long, y=lat))
    #     }
    #     
    #     
    #     
    #     ### ~~~~~~~~~ 4. FILL IN START AND END DATE MANUALLY ~~~~~~~~~~~~~~~~ ###
    #     ## only need to adjust the dates that are wrong
    #     #fix(THRESH_calib)
    #     
    #     StartDate <- readStartDate()
    #     EndDate <- readEndDate()
    #     
    #     ### CAPTURE OUTPUT FOR CALIBRATION 
    #     THRESH_calib<-data.frame('id.yr.season'=a) %>%
    #       mutate(start=if_else(is.null(StartDate),THRESH_start,StartDate)) %>%
    #       mutate(end=if_else(is.null(EndDate),THRESH_end,EndDate))
    #     
    #     
    #     ### ~~~~~~~~~ 5. SAVE DATA AND CLEAN UP ~~~~~~~~~~~~~~~~ ###
    #     mig_dates<-rbind(mig_dates,THRESH_calib)
    #     fwrite(mig_dates,"EGVU_migration_dates_manually_classified_PART2.csv")
    #     dev.off()
    #     rm(THRESH_end,THRESH_start,x,xmig,xlim,ylim,mig_time,distgraph,THRESH_calib)
    #     pause()
    #     
    #     print(sprintf("finished with migration journey %s",a))
    #     
    #   }}		#closes the else loop for migrations and the animal loop
    # 
    # 
    # 
    # 
