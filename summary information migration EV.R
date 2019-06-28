#############################################################################
## CALCULATION OF BASIC MIGRATION SUMMARY INFORMATION EGYPTIAN VULTURES 
#############################################################################
# after John Fieberg course on data visualization and analysis 
# with  animal movement data from Movebank and the animal movement tools package (amt)

# updated by Steffen Oppel on 18 June 2019 after revising data to exclude captive bred birds
## COMPARE between original and 1-pt per day data

# Load libraries
library(knitr)
library(lubridate)
library(maptools)
library(raster)
library(move)
library(amt) 
library(ggmap)
library(tibble)
library(leaflet)
library(dplyr)
options(width=165,digits.secs = 3)
opts_chunk$set(fig.width=12,fig.height=4.5, error=TRUE,cache = F)

# Record time for running all code
ptm<-proc.time()

###Set working directory
setwd("E://Documentos//PUBLICACIONES//Articulo migracion alimoches FRONTIERS ECOL EVOL//analysis")
setwd("C:\\STEFFEN\\MANUSCRIPTS\\Submitted\\FrontiersMigrationPaper\\Analysis\\EGVUmigration")

## data upload
# SELECT THE ORIGINAL DATASET OR EVAN's new 1pt per day set
#data <- fread(file="EGVU_manually_selected_migration_data.csv")
data <- fread(file="EGVU_Final_complete_migrations_only_1ptperday.csv")
head(data)
filter(data, id.yr.season== "Lazaros_2013_spring") 

### LAZAROS spring 2013 is missing
# datsupp <- fread("EV-all-1ptperday-filtered-utm-NSD-season.csv")
# datsupp <- filter(datsupp, id.yr.season== "Lazaros_2013_spring") %>%
#   mutate(DateTime=date,step_dist=0,home_dist=0,cumul_dist=0,time_diff=0,speed=0) %>%
#   select(V1,id.yr.season,study,tag,id,DateTime,long,lat,NSD,ND,utm.e,utm.n,step_dist,home_dist,cumul_dist,time_diff,speed,Day,date,Year,Month,Hour,id.yr,season)
# #str(datsupp)
# #str(data)
# data<-rbind(data, datsupp) %>% mutate(DateTime=ymd_hms(DateTime))
d<-filter(data, id.yr.season== "Malysh_2017_fall") 
#fwrite(data,"EGVU_Final_complete_migrations_only_1ptperday.csv")


#### Data cleaning

# Delete observations where missing lat or long or a timestamp.  
ind<-complete.cases(data[,c("lat", "long", "DateTime")])
EV.dat<-data[ind==TRUE,]


# Check for duplicated observations (ones with same lat, long, timestamp,
# and individual identifier). 
ind2<-EV.dat %>% select(DateTime, long, lat, id.yr.season) %>%
  duplicated
sum(ind2) # no duplicates
EV.dat<-EV.dat[ind2!=TRUE,]

# Make timestamp a date/time variable
#EV.dat$DateTime <-as.POSIXct(EV.dat$DateTime, format="%Y-%m-%d T%H:%M:%OS", tz="UTC")


# REMOVE CAPTIVE BIRDS
EV.dat<-EV.dat %>% filter(!(id %in% c("Akaga", "Blanka", "Boyana", "Elodie","Polya","Lomets","Regina","Anna","Zighmund","Panteley","Akaga")))

# some visual inspection plots 
plot(EV.dat$utm.e, EV.dat$utm.n)
show(EV.dat)
summary(EV.dat)
filter(EV.dat, id.yr.season== "Lazaros_2013_spring")
filter(EV.dat, is.na(DateTime))

## COMPUTATION OF BASIC SUMMARY STATISTICS

### Creating a track in amt
# 
# Before we can use the amt package to calculate step lengths, turn angles, and bearings
# for Egyptian vulture data, we need to add a class (track) to the data. Then, we can summarize 
# the data by individual, month, etc. 

# First, create a track using geographic coordinates and the timestamp.
# this will allow us to determine time of day 
trk <- mk_track(EV.dat, .x=long, .y=lat, .t=DateTime, id = id.yr.season, 
                crs = CRS("+init=epsg:4326"))

# calculate day/night with either movement track
trk <- trk %>% time_of_day()

# transform back to geographic coordinates
trk <- transform_coords(trk, CRS("+init=epsg:32630"))

# Save the class here (and apply it later after adding columns to the object)
trk.class<-class(trk)


            ### OPTIONAL ###
            ### From here:
            ## -----------------------------------------------------------------------------------------
            ### Movement Characteristics
            # 
            # - dir_abs will calculate absolute angles for steps
            # - dir_rel will calculate turning angles (relative angles)
            # - step_lengths will calculate distances between points
            # - nsd = Net Squared Displacement (distance from first point)
            # 
            # Arguments direction_abs:
            # 
            # - full_circle will calculate between 0 and 360 (rather than -180 and 180)
            # - zero gives the direction = 0 for absolute angle
            # 
            # Note:  we have to calculate these characteristics separately for each 
            # individual (to avoid calculating a distance between say the last observation
            # of the first individual and the first observation of the second individual).
            # 
            # 
            # To do this, we could loop through individuals, calculate these
            # characteristics for each individual, then rbind the data 
            # back together.  Or, use nested data frames and the map function
            # in the purrr library to do this with very little code. 
            # 
            # To see how nesting works, we can create a nested object by individual
            nesttrk<-trk%>%nest(-id)
            nesttrk
            
            # Each row contains data from an individual.  For example, we can access data
            # from the first individual using:
            nesttrk$data[[1]]
            
            # We could calculate movement characteristics by individual using:
            temp<-direction_rel(nesttrk$data[[1]])
            head(temp)
            
            
            # Or, we can add a columns to each nested column of data using purrr::map
            trk<-trk %>% nest(-id) %>% 
              mutate(dir_abs = map(data, direction_abs,full_circle=TRUE, zero="N"), 
                     dir_rel = map(data, direction_rel), 
                     sl = map(data, step_lengths),
                     nsd_=map(data, nsd))%>%unnest()
            ### to here
            #### -------------------------------------------------------------------------------------------------


# Calculate month, year, hour, week of each observation and append these to the dataset
# Unlike the movement charactersitics, these calculations can be done all at once, 
# since they do not utilize successive observations (like step lengths and turn angles do).
trk<-trk%>% 
  mutate(
    week=week(t_),
    month = month(t_, label=TRUE), 
    year=year(t_),
    hour = hour(t_)
  )

# Now, we need to again tell R that this is a track (rather 
# than just a data frame)
class(trk)
class(trk)<-trk.class

# Lets take a look at what we created
trk

## visual inspection of data on an interactive map (cool map!)
inspect(trk)

##############################################################
## summary movement parameters (Pascual)
##############################################################

### further information on movement parameters is available at

## Abrahms B, Seidel DP, Dougherty E, Hazen EL, Bograd SJ, Wilson AM, McNutt JW, Costa DP,
## Blake S, Brashares JS and others (2017). "Suite of simple metrics reveals common movement syndromes
## across vertebrate taxa." Movement ecology, 5(1), pp. 12. 

## Almeida PJ, Vieira MV, Kajin M, Forero-Medina G and Cerqueira R (2010)
## "Indices of movement behaviour: conceptual background,
## effects of scale and location errors." Zoologia (Curitiba), 27(5), pp. 674-680.


## check class of the tracks as "track_xyt" (time included)
class(trk)

## split tracks per id.year.season (it creates a list)
EV_data1 <- split(trk, trk$id)

## summary of sampling rates
sampling_rate <- lapply(EV_data1,summarize_sampling_rate)
sampling_rate1 <- as.data.frame(sampling_rate)
sampling_rate2 <- t(sampling_rate1)
sampling_rate3 <- as.data.frame(sampling_rate2)
names (sampling_rate3)[1] = "sampling rate"
sampling_rate4 <- sampling_rate3 %>% rownames_to_column("ID")
head(sampling_rate4)

## total distance
total_distance <- lapply(EV_data1,tot_dist)
total_distance1 <- as.data.frame(total_distance)
total_distance2 <- t(total_distance1)
total_distance3 <- as.data.frame(total_distance2)
names (total_distance3)[1] = "total distance"
total_distance4 <- total_distance3 %>% rownames_to_column("ID")
head(total_distance4)


## cumulative distance
cumulative_distance <- lapply(EV_data1,cum_dist)
cumulative_distance1 <- as.data.frame(cumulative_distance)
cumulative_distance2 <- t(cumulative_distance1)
cumulative_distance3 <- as.data.frame(cumulative_distance2)
names (cumulative_distance3)[1] = "cumulative distance"
cumulative_distance4 <- cumulative_distance3 %>% rownames_to_column("ID")
head(cumulative_distance4)

## straightness
straightness <- lapply(EV_data1,straightness)
straightness1 <- as.data.frame(straightness)
straightness2 <- t(straightness1)
straightness3 <- as.data.frame(straightness2)
names (straightness3)[1] = "straightness"
straightness4 <- straightness3 %>% rownames_to_column("ID")
head(straightness4)

## mean squred displacement (msd)
msd <- lapply(EV_data1,msd)
msd1 <- as.data.frame(msd)
msd2 <- t(msd1)
msd3 <- as.data.frame(msd2)
names (msd3)[1] = "msd"
msd4 <- msd3 %>% rownames_to_column("ID")
head(msd4)

## intensity use
intensity <- lapply(EV_data1,intensity_use)
intensity1 <- as.data.frame(intensity)
intensity2 <- t(intensity1)
intensity3 <- as.data.frame(intensity2)
names (intensity3)[1] = "intensity use"
intensity4 <- intensity3 %>% rownames_to_column("ID")
head(intensity4)

## sinuosity
sinuosity <- lapply(EV_data1,sinuosity)
sinuosity1 <- as.data.frame(sinuosity)
sinuosity2 <- t(sinuosity1)
sinuosity3 <- as.data.frame(sinuosity2)
names (sinuosity3)[1] = "sinuosity"
sinuosity4 <- sinuosity3 %>% rownames_to_column("ID")
head(sinuosity4)

### mean turn angle correlation (tac) of a track
tac <- lapply(EV_data1,tac)
tac1 <- as.data.frame(tac)
tac2 <- t(tac1)
tac3 <- as.data.frame(tac2)
names (tac3)[1] = "tac"
tac4 <- tac3 %>% rownames_to_column("ID")
head(tac4)

## speed (not very useful)
## I do not know how to create a data.frame with multiple values within each element in a list
# speed <- lapply(EV_data1,speed)
# speed1 <- as.data.frame(speed)
# speed2 <- t(speed1)
# speed3 <- as.data.frame(speed2)
# names (speed3)[1] = "speed"
# speed4 <- speed3 %>% rownames_to_column("ID")
# head(speed4)

## track duration
duration <- lapply(EV_data1,from_to)
duration1 <- as.data.frame(duration)
duration2 <- t(duration1)
duration3 <- as.data.frame(duration2)
names (duration3)[1] = "start"
names (duration3)[2] = "end"
duration4 <- duration3 %>% rownames_to_column("ID")
head(duration4) 

summary_EV_migration_parameters <- cbind(total_distance4,cumulative_distance4,straightness4,msd4, 
                                         intensity4,sinuosity4,tac4,duration4)

## computing migration duration (elapsed time between start and end dates)
time.interval <- summary_EV_migration_parameters$start %--% summary_EV_migration_parameters$end
time.duration <- as.duration(time.interval)
time.duration1 <- as.numeric(time.duration, "days")
migration_duration <- as.data.frame(time.duration1)
names (migration_duration)[1] = "time duration (days)"
head(migration_duration) 


## add migration duration to summary information
summary_EV_migration_parameters <- cbind(total_distance4,cumulative_distance4,straightness4,msd4, 
                                         intensity4,sinuosity4,tac4,duration4,migration_duration)

## clean data (unnecessary columns)
summary_EV_migration_parameters <- summary_EV_migration_parameters[,-c(3,5,7,9,11,13,15)]

## add Julian day for start and end migration dates
summary_EV_migration_parameters$julian_start <- yday(summary_EV_migration_parameters$start)
summary_EV_migration_parameters$julian_end <- yday(summary_EV_migration_parameters$end)

## export
fwrite(summary_EV_migration_parameters, file="summary_EV_migration_parameters_1PTPERDAY.csv")
filter(summary_EV_migration_parameters, ID== "Lazaros_2013_spring") 


############ COMPARE BETWEEN ORIGINAL AND REDUCED DATA #######################
# during revision we decided to go for 1 pt per day thinning of data
# how does that affect travel distance and straightness
highresdat<-fread("summary_EV_migration_parameters_HIGHRES.csv")
lowresdat<-fread("summary_EV_migration_parameters_1PTPERDAY.csv")


head(lowresdat)
head(highresdat)



lowresdat<-lowresdat %>% select(ID,`total distance`,`cumulative distance`,straightness,sinuosity,msd,`time duration (days)`) %>%
  group_by(ID) %>%
  gather(key=metric, value=value,-ID) %>%
  mutate(Res="daily")
highresdat<-highresdat %>% select(ID,`total distance`,`cumulative distance`,straightness,sinuosity,msd,`time duration (days)`) %>%
  group_by(ID) %>%
  gather(key=metric, value=value,-ID) %>%
  mutate(Res="hourly")
COMP<-rbind(lowresdat,highresdat) %>%
  spread(key=Res, value=value)



### plot correlation ###

ggplot(COMP) + geom_point(aes(x=daily,y=hourly)) +facet_wrap(~metric, scales="free")

