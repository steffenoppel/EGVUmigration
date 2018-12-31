###################################################################
#### EGYPTIAN VULTURE MIGRATION ANALYSIS			 ####
###################################################################
## initiated 28 Dec 2018 by steffen.oppel@rspb.org.uk
## example contribution for overlap of home ranges
## this is a mock demo, and will require proper assignment of 'winter' and 'summer' home ranges


# Load necessary library
library(move)
library(tidyverse)
library(sp)
require(geosphere)
library(adehabitatHR)
library(lubridate)
library(raster)
library(R.utils)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# DOWNLOAD DATA FROM MOVEBANK
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
MyLOGIN <- movebankLogin(username="Steffen", password="xxxx")
EVlocs<-getMovebankData(study="Neophron percnopterus Bulgaria/Greece",login=MyLOGIN, removeDuplicatedTimestamps=T)
EVanimals<-getMovebankAnimals(study="Neophron percnopterus Bulgaria/Greece",login=MyLOGIN)
head(EVlocs)
str(EVlocs)
head(EVanimals)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# DEFINE WINTER PERIODS FOR EACH ANIMAL AND YEAR
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


## TO BE DONE - based on Evan's Net Displacement analysis [insert code]



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# CALCULATE 95% MCP FOR EACH ANIMAL in SUMMER AND WINTER
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## NOTE: THIS REQUIRES DEFINITION OF BEGIN AND END OF MIGRATION
## for purpose of demonstration done here on all locations with random assignment of seasons!


#### START LOOP OVER ALL ANIMALS ####

alltrips<-as.numeric(unique(EVlocs@data$deployment_id))
HomeRangeOverlap<-data.frame()

for (a in alltrips){


### DATA PREPARATION ###

input<-EVlocs@data[EVlocs@data$deployment_id == a,] %>% filter(sensor_type=="GPS") 
nlocs<-as.integer((dim(input)[1])/4)			### this is only needed for the random assignment of seasons
adds<-dim(input)[1]-nlocs*4

input<-input %>%
  dplyr::select(deployment_id,tag_id,location_long,location_lat,timestamp) %>%
  arrange(timestamp) %>%
  mutate(Season=c(rep(c("Summer1","Summer2","Winter1","Winter2"), each=nlocs),rep("Winter2",adds))) %>%
  mutate(Season=as.factor(Season))


### TRANSFORM TO equidistant projection

DataGroup.Wgs <- SpatialPointsDataFrame(SpatialPoints(data.frame(input$location_long, input$location_long), proj4string=CRS("+proj=longlat +datum=WGS84")), data = input[,5:6], match.ID=F)
DataGroup.Wgs$timestamp<-NULL
input <- spTransform(DataGroup.Wgs , CRSobj="+proj=aeqd +ellps=WGS84", center=TRUE)

### CALCULATE Bhattacharyya's OVERLAP INDEX
HR <- kernelUD(input,grid=1000, same4all=TRUE)
OL1<-kerneloverlaphr(HR, meth="BA", conditional=TRUE)		### set conditional=T will make overlap smaller because it sets everything to 0 outside overlap zone


### CALCULATE EARTH MOVERS DISTANCE INDEX
## this is an extremely computationally intensive calculation, so reduce grid size before doing it

# emdthresh<-sqrt((diff(range(coordinates(input)[,1]))^2)+(diff(range(coordinates(input)[,2]))^2))	## needed to avoid internal error 9
# for (gr in c(100,75,50)){
#   try(rm(emdout),silent=T)
#   KDE.Small <- adehabitatHR::kernelUD(input, grid=gr, same4all=T)	## REDUCED GRID TO LIMIT COMPUTATION TIME
#   udspdf <- estUDm2spixdf(KDE.Small)
#   all<-stack(udspdf)
#   withTimeout({try(emdout<-emd(all, threshold=emdthresh), silent=T)}, timeout=500, onTimeout="silent")
#   if('emdout' %in% ls()){break}
# }



### FORMAT FOR OUTPUT
out<-as.data.frame(OL1) %>% mutate(Season2=row.names(OL1)) %>%
	gather(key="Season", value="BA",-Season2) %>%
	mutate(deployment_id=a)

HomeRangeOverlap<-rbind(HomeRangeOverlap,out)

} ### end loop over all deployments




#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# SUMMARISE OUTPUT 
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

HomeRangeOverlap %>% mutate(SameSeason=ifelse(Season==Season2,1,0))%>%
	filter(SameSeason==0) %>%
	mutate(SameSeason=ifelse(grepl("Summer",Season,perl=T)==grepl("Summer",Season2,perl=T),1,0))%>%
	filter(SameSeason==1) %>%
	filter(Season %in% c("Summer1","Winter1")) %>%

ggplot(aes(x=BA))+
geom_histogram(aes(y=..count../sum(..count..),col=Season, fill=Season),binwidth=0.01,center=0)+
geom_vline(xintercept=0.5, col='darkred', size=1.2, linetype="dashed")+
#facet_wrap(~Season, ncol=5)+

  theme(panel.background=element_rect(fill="white", colour="black"), 
        axis.text.x=element_text(size=18, color="black"),
        axis.text.y=element_blank(), 
        axis.title=element_text(size=20), 
        strip.text.x=element_text(size=18, color="black"), 
        strip.background=element_rect(fill="white", colour="black"), 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        panel.border = element_blank()) +
  ylab("Relative frequency of overlap") +
  xlab("Bhattacharyya's Affinity index")








