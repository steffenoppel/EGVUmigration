##########################################################################
# QUANTIFICATION OF TAILWIND SUPPORT FOR MIGRATING EGYPTIAN VULTURES
# original script written by Steffen Oppel in March 2019
# based on script developed for Murphy's Petrels in 2017
##########################################################################

## updated 10 Feb to include date comparison with manually annotated tracks
## updated 14 Feb to facilitate easy manual annotation of remaining tracks

# Load necessary library
library(RNCEP)
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
# CALCULATE WIND SUPPORT FOR STEP OF EACH MIGRATION TRACK
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

allmigs<-unique(migdat$id.yr.season)
migration.wind<-fread("EGVU_tailwind_support.csv")
migration.wind$DateTime<- ymd_hms(migration.wind$DateTime)

# allmigs <- allmigs[!allmigs %in% unique(migration.wind$id.yr.season)]
# for (a in allmigs){
# 
# input<-migdat %>% filter(id.yr.season==a) %>%  		## used to be filter(sensor_type=="GPS")
# 	filter(step_dist>0) %>%
# 	arrange(DateTime) %>%
# 	mutate(tailwind=0,sidewind=0)
# 
# for (l in 2: dim(input)[1]){
# 
# ### get wind information ###
# input$long[l]<-ifelse(input$long[l-1]==input$long[l],input$long[l-1]+0.00001,input$long[l])
# input$lat[l]<-ifelse(input$lat[l-1]==input$lat[l],input$lat[l-1]+0.00001,input$lat[l])
# if(hour(input$DateTime[l-1])==0 & minute(input$DateTime[l-1])==0){minute(input$DateTime[l-1])<-1}
# 
# try(fldat<-NCEP.flight(beg.loc=c(input$lat[l-1], input$long[l-1]), end.loc=c(input$lat[l], input$long[l]), begin.dt=as.character(input$DateTime[l-1]), flow.assist='NCEP.Tailwind',
#   fa.args=list(airspeed=12), path='great.circle', when2stop=list('longitude',50),cutoff=-500,
#   levels2consider='surface', hours=3, evaluation.interval=60, id=1, land.if.bad=FALSE, reanalysis2 = FALSE), silent=T)
# input$tailwind[l]<-ifelse(length(fldat$tailwind[1])>0,fldat$tailwind[1],NA)
# input$sidewind[l]<-ifelse(length(fldat$sidewind[1])>0,fldat$sidewind[1],NA)
# fldat$tailwind[1]<-9999   		## set to NA value in case next iteration fails
# }
# 
# migration.wind<-rbind(migration.wind, input)
# fwrite(migration.wind,"EGVU_tailwind_support.csv")
# }




#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# SUMMARISE TAILWIND BETWEEN SEASONS AND SUBPOPULATIONS
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Set working directory
setwd("C:\\STEFFEN\\MANUSCRIPTS\\in_prep\\EGVU_papers\\FrontiersMigrationPaper\\EGVUmigration")
migration.wind<-fread("EGVU_tailwind_support.csv")
migration.wind2<-fread("EGVU_tailwind_support_HOME.csv")


## FIND DUPLICATES
donehome <- unique(migration.wind2$id.yr.season)
donework <- unique(migration.wind$id.yr.season)
allmigs[!allmigs %in% c(donehome,donework)]   ### all done!
duplic.migs<-donehome[donehome %in% donework]

migration.wind<-migration.wind %>% filter(!id.yr.season %in% duplic.migs) %>%
  bind_rows(migration.wind2)

notdoneyet <- allmigs[!allmigs %in% unique(migration.wind$id.yr.season)]
notdoneyet

head(migration.wind)
unique(migration.wind$study)


#### REMOVE INVALID DATA AND CREATE SUMMARY
wind.summary<-migration.wind %>% filter(!is.na(tailwind)) %>%
  filter(tailwind!=9999) %>%
  filter(!tailwind==sidewind) %>%
  filter(!study=="efrat-israel") %>%
  mutate(subpopulation=ifelse(study=="oppel-balkans",'Balkans',
                              ifelse(study %in% c('buechley-mideast', 'karyakin-russia'),'Caucasus','West'))) %>%
  mutate(season=ifelse(grepl('spring',id.yr.season),'spring','fall')) %>%
  
  group_by(subpopulation, season) %>%
  summarise(mean.tail=median(tailwind), sd.tail=sd(tailwind), lcl=quantile(tailwind,0.25), ucl=quantile(tailwind,0.75),min=min(tailwind), max=max(tailwind))
  
fwrite(wind.summary,"EGVU_tailwind_summary.csv")




#### CREATE HISTOGRAM OF WIND DISTRIBUTION  

pdf("EGVU_tailwind_histogram.pdf", width=8, height=8) 
  
migration.wind %>% filter(!is.na(tailwind)) %>%
  filter(tailwind!=9999) %>%
  filter(!tailwind==sidewind) %>%
  filter(!study=="efrat-israel") %>%
  mutate(subpopulation=ifelse(study=="oppel-balkans",'Balkans',
                              ifelse(study %in% c('buechley-mideast', 'karyakin-russia'),'Caucasus','West'))) %>%
  mutate(season=ifelse(grepl('spring',id.yr.season),'spring','fall')) %>%
  filter(tailwind>-10) %>%
  filter(tailwind<10) %>%


ggplot(aes(x=tailwind))+
geom_histogram(binwidth=0.25,center=0)+
geom_vline(data=wind.summary,aes(xintercept=mean.tail), col='darkred', size=1.2)+
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
  xlab("Tailwind (m/s)") +
 scale_x_continuous(breaks = seq(-10, 10, 2))


dev.off()



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# TEST FOR SIGNIFICANT DIFFERENCES IN TAILWIND BETWEEN SEASONS AND SUBPOPULATIONS
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
library(lme4)
#### REMOVE INVALID DATA
test.wind<-migration.wind %>% filter(!is.na(tailwind)) %>%
  filter(tailwind!=9999) %>%
  filter(!tailwind==sidewind) %>%
  filter(!study=="efrat-israel") %>%
  mutate(subpopulation=ifelse(study=="oppel-balkans",'Balkans',
                              ifelse(study %in% c('buechley-mideast', 'karyakin-russia'),'Caucasus','West'))) %>%
  mutate(season=ifelse(grepl('spring',id.yr.season),'spring','fall')) %>%
  mutate(Year=year(Day))




#### RUN SIMPLE GLMM models
M.null<-lmer(tailwind~-1+season+(1|id.yr.season), data=test.wind)
M.subpop<-lmer(tailwind~-1+subpopulation*season+(1|id.yr.season), data=test.wind)
anova(M.null,M.subpop)
summary(M.subpop)


#### RUN SIMPLE GLMM model with nested random effect [requested by Pascual - just to show it is the same]
M.null<-lmer(tailwind~-1+season+(1|Year/id/season), data=test.wind)
M.subpop<-lmer(tailwind~-1+subpopulation*season+(1|Year/id/season), data=test.wind)
anova(M.null,M.subpop)
summary(M.subpop)






