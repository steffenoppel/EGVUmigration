##########################################################################
# CHECK TEMPORAL RESOLUTION BETWEEN LOCATIONS
##########################################################################

# Load necessary library
library(tidyverse)
library(data.table)
library(ggplot2)




#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# LOAD PREVIOUSLY SAVED DATA (prepared in script 2.EV-all-migration delineation.R)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Set working directory
setwd("C:\\STEFFEN\\MANUSCRIPTS\\Submitted\\FrontiersMigrationPaper\\Analysis")

# # read in clean csv
locs = read.csv("EV-all_1ptperhr-filtered-utm-NSD-season.csv")
head(locs)



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# CALCULATE TIME GAPS
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


  input<-locs %>% 
    mutate(DateTime=ymd_h(paste(Year,Month, Day, Hour, sep=","))) %>%
    dplyr::select(id.yr.season,id,DateTime, long, lat) %>%
    group_by(id.yr.season) %>% arrange(DateTime) %>%
    mutate(gap=as.numeric(lead(DateTime)-DateTime)) %>%
    mutate(season=ifelse(grepl("fall",id.yr.season)==T,"fall","spring")) %>%
    filter(gap<24)
  

head(input)

ggplot(input) + geom_histogram(aes(x=gap,y=(..count..)/tapply(..count..,..PANEL..,sum)[..PANEL..]),binwidth=1)+                               
  facet_wrap("season", ncol=2, scales = "fixed")

