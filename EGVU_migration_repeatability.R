##########################################################################
# QUANTIFICATION OF REPEATABILITY FOR MIGRATING EGYPTIAN VULTURES
# original script written by Steffen Oppel in March 2019
# based on script developed for MAscension Frigatebirds in 2015
##########################################################################


# Load necessary library
library(RInSp)
library(rptR)
library(lubridate)
library(tidyverse)
library(data.table)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# LOAD PREVIOUSLY SAVED MIGRATION DATA (sent by Pascual Lopez-Lopez on 15 March 2019)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Set working directory
setwd("C:\\STEFFEN\\MANUSCRIPTS\\in_prep\\EGVU_papers\\FrontiersMigrationPaper\\EGVUmigration")

# read in clean csv
migs<- fread("migration_parameters_completed_migrations_clean.csv")
migs<- migs %>% mutate(DateTime=mdy_hm(migs$start)) %>%
  #filter(subpopfine!="Israel") %>%
  mutate(SPEED=cumulativedistkm/(durationdays*24))
head(migs)
unique(migs$subpopulation)




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
cumdistREP<-rpt(cumulativedistkm~agemigr+season+(1|subpopulation), grname="subpopulation",data=migs, datatype="Gaussian", npermut=1000, parallel=T, ncores=4)

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
rep_summary<-data.frame(parameter=c('direct-line distance','travel distance', 'straightness','duration','speed','start date','end date'),
                        R=0,se=0,lcl=0,ucl=0,P_lrt=0)

outlist<-list(totdistREP,cumdistREP,straightREP,durREP,speedREP,startREP,endREP)

for (l in 1:nrow(rep_summary)){
  xobj<-outlist[[l]]
  rep_summary$R[l]<-xobj$R[1,1]
  rep_summary$se[l]<-xobj$se[1,1]
  rep_summary$lcl[l]<-xobj$CI_emp[1,1]
  rep_summary$ucl[l]<-xobj$CI_emp[1,2]
  rep_summary$P_lrt[l]<-xobj$P[1,1]
}
  

fwrite(rep_summary,"EGVU_repeatability_migration_subpop.csv")
  
  
  