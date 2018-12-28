###################################################################
#### EGYPTIAN VULTURE MIGRATION ANALYSIS			 ####
###################################################################
## initiated 28 Dec 2018 by steffen.oppel@rspb.org.uk
## example contribution for wind analysis and overlap of home ranges



# Load necessary library
library(move)
library(tidyverse)
library(sp)
require(geosphere)
library(RNCEP)




#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# DOWNLOAD DATA FROM MOVEBANK
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
MyLOGIN <- movebankLogin(username="Steffen", password="xxxxxxxxxxxxxxxx")
EVlocs<-getMovebankData(study="Neophron percnopterus Bulgaria/Greece",login=MyLOGIN, removeDuplicatedTimestamps=T)
EVanimals<-getMovebankAnimals(study="Neophron percnopterus Bulgaria/Greece",login=MyLOGIN)
head(EVlocs)
str(EVlocs)
head(EVanimals)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# DEFINE MIGRATORY PERIODS FOR EACH ANIMAL TRACK
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


## TO BE DONE - based on Evan's Net Displacement analysis [insert code]




#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# CALCULATE STEP DISTANCES, TIME INTERVALS, SPEED AND WIND FOR EACH ANIMAL TRACK
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## NOTE: THIS REQUIRES DEFINITION OF BEGIN AND END OF MIGRATION
## for purpose of demonstration done here on all locations

alltrips<-as.numeric(unique(EVlocs@data$deployment_id))
migration<-data.frame()

for (a in alltrips){

input<-EVlocs@data[EVlocs@data$deployment_id == a,] %>% filter(sensor_type=="GPS") %>%
  dplyr::select(deployment_id,tag_id,location_long,location_lat,timestamp) %>%
  arrange(timestamp) %>%
  mutate(step_dist=0,home_dist=0,cumul_dist=0,time_diff=0,speed=0,tailwind=0,sidewind=0)

first<-SpatialPoints(data.frame(input$location_lat[1], input$location_long[1]), proj4string=CRS("+proj=longlat + datum=wgs84"))

for (l in 2: dim(input)[1]){
input$time_diff[l]<-as.numeric(difftime(input$timestamp[l],input$timestamp[l-1], units="hours"))
fromloc<-SpatialPoints(data.frame(input$location_lat[l-1], input$location_long[l-1]), proj4string=CRS("+proj=longlat + datum=wgs84"))
toloc<-SpatialPoints(data.frame(input$location_lat[l], input$location_long[l]), proj4string=CRS("+proj=longlat + datum=wgs84"))
input$step_dist[l]<-spDistsN1(fromloc, toloc, longlat=T)
input$home_dist[l]<-spDistsN1(first, toloc, longlat=T)
input$cumul_dist[l]<-sum(input$step_dist)
input$speed[l]<-input$step_dist[l]/input$time_diff[l]

### get wind information ###

fldat<-NCEP.flight(beg.loc=c(input$location_lat[l-1], input$location_long[l-1]), end.loc=c(input$location_lat[l], input$location_long[l]), begin.dt=as.character(input$timestamp[l-1]), flow.assist='NCEP.Tailwind',
  fa.args=list(airspeed=12), path='great.circle', when2stop=list('latitude','longitude',50),cutoff=-500,
  levels2consider='surface', hours=3, evaluation.interval=60, id=1, land.if.bad=FALSE, reanalysis2 = FALSE)
input$tailwind[l]<-ifelse(length(fldat$tailwind[1])>0,fldat$tailwind[1],NA)
input$sidewind[l]<-ifelse(length(fldat$sidewind[1])>0,fldat$sidewind[1],NA)

}

migration<-rbind(migration, input)

}




#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# COMPARE TAILWIND AND SIDEWIND BETWEEN SEASONS / FLYWAYS etc.
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

aggregate(tailwind~Season, migration, FUN=mean)

#pdf("MUPE_tailwind_histogram.pdf", width=10, height=5)
ggplot(migration, aes(x=tailwind))+
geom_histogram(aes(y=..count../sum(..count..),col=TripType, fill=TripType),binwidth=0.5,center=0)+
geom_vline(xintercept=0, col='darkred', size=1.2, linetype="dashed")+
facet_wrap(~Season, ncol=5)+

  theme(panel.background=element_rect(fill="white", colour="black"), 
        axis.text.x=element_text(size=18, color="black"),
        axis.text.y=element_blank(), 
        axis.title=element_text(size=20), 
        strip.text.x=element_text(size=18, color="black"), 
        strip.background=element_rect(fill="white", colour="black"), 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        panel.border = element_blank(), 
        panel.background = element_blank()) +
  ylab("Relative frequency of trip segments") +
  xlab("Tailwind (m/s)")
dev.off()












