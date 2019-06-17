##########################################################################
# QUANTIFICATION OF THERMAL UPLIFT SUPPORT FOR MIGRATING EGYPTIAN VULTURES
# original script written by Steffen Oppel in April 2019
# based on script for tailwind support from same study
##########################################################################


# Load necessary library
library(lubridate)
library(tidyverse)
library(data.table)
filter <- dplyr::filter
select <- dplyr::select


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# LOAD PREVIOUSLY SAVED MIGRATION DATA (prepared in script 2.EV-all-migration delineation.R)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Set working directory
setwd("C:\\STEFFEN\\MANUSCRIPTS\\in_prep\\EGVU_papers\\FrontiersMigrationPaper\\EGVUmigration")

# read in clean csv
migdat<- fread("EGVU_manually_selected_migration_data.csv")
migdat$DateTime<- ymd_hms(migdat$DateTime)

head(migdat)




#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# READ IN THERMAL UPLIFT DATA FROM MOVEBANK AND ADD TO MIG DATA
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
uplift<- fread("thermal_uplift.csv") %>% mutate(DateTime=ymd_hms(timestamp)) %>%
  rename(id='individual-local-identifier',uplift=`Movebank Thermal Uplift (from ECMWF)`) %>%
  select(id,DateTime,uplift)
head(uplift)

migration.lift<-merge(migdat,uplift, by=c('id','DateTime'), all=T)




#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# SUMMARISE UPLIFT BETWEEN SEASONS AND SUBPOPULATIONS
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


#### REMOVE INVALID DATA AND CREATE SUMMARY
uplift.summary<-migration.lift %>% filter(!is.na(uplift)) %>%
  filter(uplift!=9999) %>%
  filter(!study=="efrat-israel") %>%
  mutate(subpopulation=ifelse(study=="oppel-balkans",'Balkans',
                              ifelse(study %in% c('buechley-mideast', 'karyakin-russia'),'Caucasus','West'))) %>%
  mutate(season=ifelse(grepl('spring',id.yr.season),'spring','fall')) %>%
  
  group_by(subpopulation, season) %>%
  summarise(mean.lift=median(uplift), sd.lift=sd(uplift), lcl=quantile(uplift,0.25), ucl=quantile(uplift,0.75),min=min(uplift), max=max(uplift))
  

uplift.summary

fwrite(uplift.summary,"EGVU_uplift_summary.csv")




#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# TEST FOR SIGNIFICANT DIFFERENCES IN uplift BETWEEN SEASONS AND SUBPOPULATIONS
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
library(lme4)
#### REMOVE INVALID DATA
test.lift<-migration.lift %>% filter(!is.na(uplift)) %>%
  filter(uplift!=9999) %>%
  filter(!study=="efrat-israel") %>%
  mutate(subpopulation=ifelse(study=="oppel-balkans",'Balkans',
                              ifelse(study %in% c('buechley-mideast', 'karyakin-russia'),'Caucasus','West'))) %>%
  mutate(season=ifelse(grepl('spring',id.yr.season),'spring','fall')) %>%
  mutate(Year=year(Day))




#### RUN SIMPLE GLMM models
M.null<-lmer(uplift~-1+season+(1|id.yr.season), data=test.lift)
M.subpop<-lmer(uplift~-1+subpopulation*season+(1|id.yr.season), data=test.lift)
anova(M.null,M.subpop)
summary(M.subpop)


#### RUN SIMPLE GLMM model with nested random effect [requested by Pascual - just to show it is the same]
M.null<-lmer(uplift~-1+season+(1|Year/id/season), data=test.lift)
M.subpop<-lmer(uplift~-1+subpopulation*season+(1|Year/id/season), data=test.lift)
anova(M.null,M.subpop)
summary(M.subpop)






######### abandoned plot ####################

migration.lift %>% filter(!is.na(uplift)) %>%
  filter(uplift!=9999) %>%
  filter(!study=="efrat-israel") %>%
  mutate(subpopulation=ifelse(study=="oppel-balkans",'Balkans',
                              ifelse(study %in% c('buechley-mideast', 'karyakin-russia'),'Caucasus','West'))) %>%
  mutate(season=ifelse(grepl('spring',id.yr.season),'spring','fall')) %>%
  filter(uplift>-10) %>%
  filter(uplift<10) %>%
  
  
  ggplot(aes(x=uplift))+
  geom_histogram(binwidth=0.25,center=0)+
  geom_vline(data=uplift.summary,aes(xintercept=mean.lift), col='darkred', size=1.2)+
  facet_grid(subpopulation~season, scales="fixed")+
  
  theme(panel.background=element_rect(fill="white", colour="black"), 
        axis.text.x=element_text(size=18, color="black"),
        axis.text.y=element_blank(), 
        axis.title=element_text(size=20), 
        strip.text=element_text(size=18, color="black"), 
        strip.background=element_rect(fill="white", colour="black"), 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        panel.border = element_blank()) +
  ylab("Relative frequency of trip segments") +
  xlab("uplift (m/s)") +
  scale_x_continuous(breaks = seq(-10, 10, 2))




