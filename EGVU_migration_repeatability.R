##########################################################################
# QUANTIFICATION OF REPEATABILITY FOR MIGRATING EGYPTIAN VULTURES
# original script written by Steffen Oppel in March 2019
# based on script developed for Ascension Frigatebirds in 2015
##########################################################################


### REVISED 17 JUNE 2019 after reviews
### removed captive-bred individuals from analysis
## removed incomplete migrations
## updated fixed factors to match GLMM results

### revised 20 June to include Lazaros_2013_spring


# Load necessary library
#library(RInSp)
library(rptR)
library(lubridate)
library(tidyverse)
library(data.table)
filter<-dplyr::filter
select<-dplyr::select



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# LOAD PREVIOUSLY SAVED MIGRATION DATA (sent by Pascual Lopez-Lopez on 15 March 2019)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Set working directory
setwd("C:\\STEFFEN\\MANUSCRIPTS\\Submitted\\FrontiersMigrationPaper\\Analysis\\EGVUmigration")
ancdata<-fread("migration_parameters_completed_migrations_clean.csv")
data<-fread("summary_EV_migration_parameters_1PTPERDAY.csv")   ## updated on 18 June to use latest 1-pt-perday dataset provided by Evan
head(data)
head(ancdata)
# ## remove uncomplete migrations
# data_by_migrations <- split.data.frame(data,data$full_migration)
# data_complete_migrations <- as.data.frame(data_by_migrations$y)
# 
# ## remove Israelian birds
# levels(data_complete_migrations$country)
# data_complete_migrations1 <- data_complete_migrations[data_complete_migrations$country != "Israel", ]
# levels(data_complete_migrations1$country)
# 
# ## data ready for analyses
# data_ok <- data_complete_migrations1
ancdata<- ancdata %>% select(country,subpopulation,route,ID,year,season,full_migration,agedeploy,agemigr,startlong,startlat,endlat,endlong) %>%
  mutate(id.year.season=paste(ID,year,season,sep="_")) %>%
  filter(full_migration=="y")

migs <- data %>% mutate(DateTime=ymd_hms(start)) %>%
  mutate(DateTime=if_else(is.na(DateTime),ymd_hms(paste(start,"08:00:00",sep=" ")),DateTime)) %>%
  rename(id.year.season=ID, totaldistkm=`total distance`, cumulativedistkm=`cumulative distance`,durationdays=`time duration (days)`) %>%
  mutate(totaldistkm=totaldistkm/1000,cumulativedistkm=cumulativedistkm/1000) %>%
  mutate(SPEED=cumulativedistkm/durationdays) %>%
  full_join(ancdata, by="id.year.season") %>%
  filter(subpopulation!="Israel") %>%
  filter(!(ID %in% c("Akaga", "Blanka", "Boyana", "Elodie","Polya","Lomets","Regina","Anna","Zighmund","Panteley","Akaga"))) %>%
  filter(!(id.year.season %in% c("Macedonia_2011_fall", "Faia_2018_fall", "Camaces_2017_fall", "Arpacai_2012_fall","Ikaros_2012_fall","Asparuh_2013_fall","Berenice_2013_fall",
                     "Heracles_2013_fall","Ibrahim_2013_fall","Ilina_2013_fall","Katerina_2013_fall","Redcliff_2013_fall","Ardahan_2014_fall","Volen_2014_fall"))) %>%  
  mutate(year=as.factor(year(DateTime)),agedeploy=as.factor(agedeploy),agemigr=as.factor(agemigr)) %>%
  select(country,subpopulation,route,id.year.season,ID,year,season,full_migration,agedeploy,agemigr,totaldistkm,cumulativedistkm,straightness,durationdays,julian_start,julian_end,SPEED)


head(migs)
dim(migs)
unique(migs$subpopulation)

migs %>% filter(id.year.season=="Lazaros_2013_spring")
ancdata %>% filter(ID=="Lazaros")


####~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~####
####~~~~~ 			SIMPLE SUMMARY											     ~~~~~~####
####~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~####
mig.summ<- migs %>% group_by(season,subpopulation) %>%
  summarise(dist=mean(cumulativedistkm), dur=mean(durationdays), speed=mean(SPEED))



####~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~####
####~~~~~ 			SIMPLE PLOTS 											     ~~~~~~####
####~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~####
pdf("EGVU_mig_distances.pdf", width=8, height=6)
ggplot(migs, aes(x=cumulativedistkm, y=durationdays,col=subpopulation,pch=season)) + geom_point(size=2.5)+
  
  theme(panel.background=element_rect(fill="white", colour="black"), 
        axis.text=element_text(size=18, color="black"),
        axis.title=element_text(size=20), 
        strip.text=element_text(size=18, color="black"), 
        strip.background=element_rect(fill="white", colour="black"), 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        panel.border = element_blank()) +
  ylab("Migration duration (days)") +
  xlab("Total travel distance (km)")

dev.off()


####~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~####
####~~~~~ 			REPEATABILITY OF MIGRATION PARAMETERS											     ~~~~~~####
####~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~####

## QUESTION: ARE MIGRATION PARAMETERS MORE CONSISTENT (=repeatable) WITHIN RATHER THAN BETWEEN POPULATIONS?
## while accounting for known seasonal and age variation (based on Pascual's analysis)

#### abandoned approach using RInSp, which requires a full matrix (i.e. similar number of trips among individuals)
# RISinput<-migs %>% 
#   group_by(ID) %>%
#   arrange(DateTime) %>%
#   mutate(TripSeq=row_number()) %>%
#   dplyr::select(durationdays, ID, TripSeq) %>%
#   spread(key=TripSeq, value=durationdays)
# RISinput[is.na(RISinput)]<-0
# RISdat<-import.RInSp(RISinput, col.header = T, info.cols = 1,subset.column=c(2:14), data.type = "double", print.messages=TRUE)
# out<-WTcMC(RISdat, replicates = 10000, weight = "N_items", print.ris=TRUE)


## TOTAL DISTANCE ###
totdistREP<-rpt(totaldistkm~(1|subpopulation), grname="subpopulation",data=migs, datatype="Gaussian", npermut=1000, parallel=T, ncores=4)

## CUMULATIVE DISTANCE ###
cumdistREP<-rpt(cumulativedistkm~season+(1|subpopulation), grname="subpopulation",data=migs, datatype="Gaussian", npermut=1000, parallel=T, ncores=4)

## STRAIGHTNESS ###
straightREP<-rpt(straightness~season+(1|subpopulation), grname="subpopulation",data=migs, datatype="Gaussian", npermut=1000, parallel=T, ncores=4)

## TRIP DURATION ###
durREP<-rpt(durationdays~agemigr+season+(1|subpopulation), grname="subpopulation",data=migs, datatype="Gaussian", npermut=1000, parallel=T, ncores=4)

## SPEED ###
speedREP<-rpt(SPEED~agemigr+season+(1|subpopulation), grname="subpopulation",data=migs, datatype="Gaussian", npermut=1000, parallel=T, ncores=4)

## START DATE ###
startREP<-rpt(julian_start~agemigr+season+(1|subpopulation), grname="subpopulation",data=migs, datatype="Gaussian", npermut=1000, parallel=T, ncores=4)

## END DATE ###
endREP<-rpt(julian_end~agemigr+season+(1|subpopulation), grname="subpopulation",data=migs, datatype="Gaussian", npermut=1000, parallel=T, ncores=4)



####~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~####
####~~~~~ 			COMBINE OUTPUT 											     ~~~~~~####
####~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~####
rep_summary<-data.frame(parameter=c("totaldistkm","cumulativedistkm","durationdays","straightness","start","end","speed"),
                        R=0,se=0,lcl=0,ucl=0,P_lrt=0)

outlist<-list(totdistREP,cumdistREP,durREP,straightREP,startREP,endREP,speedREP)

for (l in 1:nrow(rep_summary)){
  xobj<-outlist[[l]]
  rep_summary$R[l]<-xobj$R[1,1]
  rep_summary$se[l]<-xobj$se[1,1]
  rep_summary$lcl[l]<-xobj$CI_emp[1,1]
  rep_summary$ucl[l]<-xobj$CI_emp[1,2]
  rep_summary$P_lrt[l]<-xobj$P[1,1]
}
  

fwrite(rep_summary,"EGVU_repeatability_migration_subpop.csv")
  
  
  