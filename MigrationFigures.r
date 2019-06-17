##########################################################################
# PREPARATION OF FIGURES FOR MIGRATION OF EGYPTIAN VULTURES
##########################################################################

## written 14 June 2019



# Load necessary library
library(lubridate)
library(tidyverse)
library(data.table)
library(ggplot2)
filter <- dplyr::filter
select <- dplyr::select


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# LOAD PREVIOUSLY SAVED MIGRATION DATA (prepared in script 2.EV-all-migration delineation.R)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Set working directory
setwd("C:\\STEFFEN\\MANUSCRIPTS\\Submitted\\FrontiersMigrationPaper\\EGVUmigration")

# read in clean csv
migdat<- fread("EGVU_manually_selected_migration_data.csv")
migdat$DateTime<- ymd_hms(migdat$DateTime)
migdat<-migdat[order(migdat$id.yr.season,migdat$DateTime),]
migdat$Season<- ifelse(grepl('spring',migdat$id.yr.season),'spring','fall')
head(migdat)


try(setwd("C:\\STEFFEN\\RSPB\\Bulgaria\\Raw_Data\\Tracking_data"), silent=T)
#try(setwd("S:\\ConSci\\DptShare\\SteffenOppel\\RSPB\\Bulgaria\\Raw_Data\\Tracking_data"), silent=T)
load("EGVU_tracking_data.RData")


setwd("C:\\STEFFEN\\MANUSCRIPTS\\in_press\\MigrationBookChapter")
#setwd("S:\\ConSci\\DptShare\\SteffenOppel\\MANUSCRIPTS\\in_prep\\EGVU_papers\\MigrationBookChapter")
birdsSpain<-fread("Lopez_Deployments.csv")



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# FILTER OUT BAD LOCATIONS
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

dim(migdat)
migdat<-migdat[!is.na(migdat$long),]
migdat<-migdat[!is.na(migdat$lat),]
migdat<-migdat[!(migdat$long>60),]
dim(migdat)


#### TROUBLESHOOT AND REMOVE LOCATIONS WITH DISPLACEMENTS >2 degrees

migdat %>% group_by(id.yr.season) %>%
  mutate(latdispl=lat-lag(lat), longdispl=long-lag(long)) %>%
  filter(longdispl>10 | latdispl >10) %>%
  arrange(desc(longdispl))








#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# PLOT A GENERAL OVERVIEW MAP OF THE FLYWAY
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
basemap <- map_data("world")

#### CREATE AND SAVE PLOT ###
pdf("EGVUmigration_Balkan.pdf", width=9, height=6)
postscript("EGVUmigration_Balkan.eps", width=9, height=6)
#jpeg("Figure1.jpg", width=9, height=6, units="in", res=600, quality=100)


ggplot() + geom_polygon(data = basemap, aes(x=long, y = lat, group = group)) + 
  coord_fixed(xlim = c(-15,56),  ylim = c(-1,50), ratio = 1.3)+

## beautification of the axes
theme(panel.background=element_rect(fill="white", colour="black"), 
      axis.text=element_text(size=18, color="black"), 
      axis.title=element_text(size=20),
      legend.background = element_rect(),
      legend.title = element_text(size=16),
      legend.key = element_blank(),
      legend.text=element_text(size=12),
      #legend.key = element_rect(fill = NA, colour = NA, size = 0.25),
      strip.text.x=element_text(size=18, color="black"), 
      strip.background=element_rect(fill="white", colour="black")) +
  ylab("Longitude") +
  xlab("Latitude") +
  geom_path(data=migdat, aes(x=long, y=lat, group=id.yr.season, col=Season))+
  
  ### PLOT DEPLOYMENT LOCATIONS
  geom_point(data=birds, aes(x=Tag_long, y=Tag_lat) ,color="gold1", size=2, pch=16)+
  geom_point(data=birdsSpain, aes(x=`deploy-on-longitude`, y=`deploy-on-latitude`) ,color="gold1", size=2, pch=16)+            ##SPAIN deployments
  geom_point(aes(x=c(46.355,43.67,40.968759), y=c(39.496,40.08,11.713795)) ,color="gold1", size=2, pch=16)+                 ## MIDDLE EAST DEPLOYMENTS
  geom_point(aes(y=41.073, x=-6.666) ,color="gold1", size=2, pch=16)                                                        ## SALORO DEPLOYMENTS
  
dev.off()



#### TURKEY FOCUS
Turkey <- basemap %>% filter(region=="Turkey")

#jpeg("EGVU_migration_Turkey.jpg", width=9, height=6, units="in", res=600, quality=100)


ggplot() + geom_polygon(data = basemap, aes(x=long, y = lat, group = group)) + 
	geom_polygon(data = Turkey, aes(x=long, y = lat, group = group), fill= "olivedrab") + 
  	coord_fixed(xlim = c(25,45),  ylim = c(35,43), ratio = 1.3)+

## beautification of the axes
theme(panel.background=element_rect(fill="white", colour="black"), 
      axis.text=element_text(size=18, color="black"), 
      axis.title=element_text(size=20),
      legend.background = element_rect(),
      legend.title = element_text(size=16),
      legend.key = element_blank(),
      legend.text=element_text(size=12),
      #legend.key = element_rect(fill = NA, colour = NA, size = 0.25),
      strip.text.x=element_text(size=18, color="black"), 
      strip.background=element_rect(fill="white", colour="black")) +
  ylab("Longitude") +
  xlab("Latitude") +
  geom_path(data=migdat, aes(x=long, y=lat, group=id.yr.season, col=Season))+
  
  ### PLOT DEPLOYMENT LOCATIONS
  geom_point(data=birds, aes(x=Tag_long, y=Tag_lat) ,color="gold1", size=2, pch=16)+
  geom_point(data=birdsSpain, aes(x=`deploy-on-longitude`, y=`deploy-on-latitude`) ,color="gold1", size=2, pch=16)+            ##SPAIN deployments
  geom_point(aes(x=c(46.355,43.67,40.968759), y=c(39.496,40.08,11.713795)) ,color="gold1", size=2, pch=16)+                 ## MIDDLE EAST DEPLOYMENTS
  geom_point(aes(y=41.073, x=-6.666) ,color="gold1", size=2, pch=16)                                                        ## SALORO DEPLOYMENTS

  


dev.off()



