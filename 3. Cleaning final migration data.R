#Set WD
setwd("~/Google Drive/Research Projects/Egyptian Vulture/Frontiers in Ecology & Evolution Paper/Latest/Data/EVmigration_data")

##Clear workspace
rm(list = ls())

###Load relevant libraries###
library(adehabitatLT)
library(adehabitatHR)
library(plotrix)
library(lattice)
library(gdata)
library(nlme)
library(maps)
library(lubridate)
library(ggplot2)
library(scales)


#load data
 d<-read.csv("EGVU_manually_selected_migration_data.csv", header=T, sep=",")
 summary(d)
 d$date <- ymd_hms(d$DateTime)
 head(d)
 
 # Pull year, month, day, hour from the DateTime variable
 d$Year <- year(d$date)
 d$Month <- month(d$date)
 d$Day = day(d$date)
 d$Hour <- hour(d$date)
 
 # burst by id and year
 d$id.yr <- c(paste(d$id,d$Year,sep="_")) 
 head(d)
 
 #assign a season variable (spring / fall)
 d$season <- ifelse(d$Month >= 7,
                         c("fall"), c("spring")) 
 summary(d)
 
 ## remove non-existent ds [they should already be excluded...]
 d<-d[!(d$id.yr.season=="Agri_2013_spring"),]
 d<-d[!(d$id.yr.season=="Agri_2014_fall"),]
 d<-d[!(d$id.yr.season=="Ardahan_2013_spring"),]
 d<-d[!(d$id.yr.season=="Haydi_2014_spring"),]
 d<-d[!(d$id.yr.season=="Iste_2014_spring"),]
 d<-d[!(d$id.yr.season=="Serhat_2014_spring"),]
 d<-d[!(d$id.yr.season=="Tuzluca_2013_spring"),]
 d<-d[!(d$id.yr.season=="Tuzluca_2016_fall"),]
 d<-d[!(d$id.yr.season=="Batuecasa_2017_spring"),]
 d<-d[!(d$id.yr.season=="Huebra_2017_spring"),]
 d<-d[!(d$id.yr.season=="2HN_2017_fall"),]
 d<-d[!(d$id.yr.season=="2HN_2017_spring"),]
 d<-d[!(d$id.yr.season=="Douro_2017_spring"),]
 d<-d[!(d$id.yr.season=="Faia_2017_spring"),]
 d<-d[!(d$id.yr.season=="Faia_2018_fall"),]
 d<-d[!(d$id.yr.season=="Poiares_2017_spring"),]
 d<-d[!(d$id.yr.season=="Akaga_2018_spring"),]
 d<-d[!(d$id.yr.season=="Aoos_2015_spring"),]
 d<-d[!(d$id.yr.season=="Boyana_2018_spring"),]
 d<-d[!(d$id.yr.season=="Castor_2014_spring"),]
 d<-d[!(d$id.yr.season=="Batuecasa_2017_spring"),]
 d<-d[!(d$id.yr.season=="Lazaros_2012_spring"),]
 d<-d[!(d$id.yr.season=="Polya_2018_spring"),]
 d<-d[!(d$id.yr.season=="Volen_2014_fall"),]
 d<-d[!(d$id.yr.season=="A75658_2011_spring"),]
 d<-d[!(d$id.yr.season=="A75658_2011_fall"),]
 d<-d[!(d$id.yr.season=="A75659_2011_fall"),]
 d<-d[!(d$id.yr.season=="A80420_2013_fall"),]
 d<-d[!(d$id.yr.season=="A89731_2013_fall"),]
 d<-d[!(d$id.yr.season=="Pyrenees_2016_fall"),]
 d<-d[!(d$id.yr.season=="A89731_2012_fall"),]
 d<-d[!(d$id.yr.season=="Ardahan_2014_fall"),]
 d<-d[!(d$id.yr.season=="95R_2017_spring"),]
 d<-d[!(d$id.yr.season=="2HN_2016_spring"),]
 d<-d[!(d$id.yr.season=="2HN_2016_fall"),]
 d<-d[!(d$id.yr.season=="95R_2017_fall"),]
 d<-d[!(d$id.yr.season=="A75658_2010_spring"),]
 d<-d[!(d$id.yr.season=="A75658_2010_fall"),]
 d<-d[!(d$id.yr.season=="Anna_2018_fall"),]
 d<-d[!(d$id.yr.season=="BatuecasP_2017_fall"),]
 d<-d[!(d$id.yr.season=="Iliaz_2013_spring"),]
 d<-d[!(d$id.yr.season=="Iliaz_2013_fall"),]
 d<-d[!(d$id.yr.season=="Iliaz_2014_fall"),]
 d<-d[!(d$id.yr.season=="Levkipos_2013_spring"),]
 d<-d[!(d$id.yr.season=="Levkipos_2013_fall"),]
 d<-d[!(d$id.yr.season=="Mille_2014_fall"),]
 d<-d[!(d$id.yr.season=="Sanie_2014_fall"),]
 d<-d[!(d$id.yr.season=="Svetlina_2013_fall"),]
 d<-d[!(d$id.yr.season=="Volen_2013_spring"),]
 
 #subset by study to have manageable datasets
 #then check dataset visually and remove non-migrant id.yr's
 summary(d$study)
 d1 = subset(d, study == "buechley-mideast")
 plot = ggplot(d1, aes(date, ND), color = id.yr) + geom_line() + facet_wrap(~ id, ncol = 3)
 plot = plot + labs(x = "Date", y = "Net Displacement") + theme_bw()
 plot
 d<-d[!(d$id=="Arpacay"),]

 #on to next study
 summary(d$study)
 d1 = subset(d, study == "douro-spain")
 plot = ggplot(d1, aes(date, ND), color = id.yr) + geom_line() + facet_wrap(~ id, ncol = 3)
 plot = plot + labs(x = "Date", y = "Net Displacement") + theme_bw() 
 plot
 d<-d[!(d$id=="Camaces"),]
 
 #on to next study
 summary(d$study)
 d1 = subset(d, study == "efrat-israel")
 plot = ggplot(d1, aes(date, ND), color = id.yr) + geom_line() + facet_wrap(~ id, ncol = 3)
 plot = plot + labs(x = "Date", y = "Net Displacement") + theme_bw() 
 plot
 
 #on to next study
 summary(d$study)
 d1 = subset(d, study == "grefa-spain")
 plot = ggplot(d1, aes(date, ND), color = id.yr) + geom_line() + facet_wrap(~ id, ncol = 3)
 plot = plot + labs(x = "Date", y = "Net Displacement") + theme_bw() 
 plot
 d<-d[!(d$id=="9FC"),]
 d<-d[!(d$id=="2HP"),]
 
 #on to next study
 summary(d$study)
 d1 = subset(d, study == "karyakin-russia")
 plot = ggplot(d1, aes(date, ND), color = id.yr) + geom_line() + facet_wrap(~ id, ncol = 3)
 plot = plot + labs(x = "Date", y = "Net Displacement") + theme_bw() 
 plot
 d<-d[!(d$id=="Kapchug"),]
 d<-d[!(d$id=="Sarygush"),]
 
 #on to next study
 summary(d$study)
 d1 = subset(d, study == "kobierzycki-france")
 plot = ggplot(d1, aes(date, ND), color = id.yr) + geom_line() + facet_wrap(~ id, ncol = 3)
 plot = plot + labs(x = "Date", y = "Net Displacement") + theme_bw() 
 plot
 
 #on to next study
 summary(d$study)
 d1 = subset(d, study == "life.rupis-spain")
 plot = ggplot(d1, aes(date, ND), color = id.yr) + geom_line() + facet_wrap(~ id, ncol = 3)
 plot = plot + labs(x = "Date", y = "Net Displacement") + theme_bw() 
 plot
 
 #on to next study
 summary(d$study)
 d1 = subset(d, study == "migra-spain")
 plot = ggplot(d1, aes(date, ND), color = id.yr) + geom_line() + facet_wrap(~ id, ncol = 3)
 plot = plot + labs(x = "Date", y = "Net Displacement") + theme_bw() 
 plot
 d<-d[!(d$id=="Endoia"),]
 
 #on to next study
 summary(d$study)
 d1 = subset(d, study == "oppel-balkans")
 plot = ggplot(d1, aes(date, ND), color = id.yr) + geom_line() + facet_wrap(~ id, ncol = 3)
 plot = plot + labs(x = "Date", y = "Net Displacement") + theme_bw() 
 plot
 d<-d[!(d$id=="Anna"),]
 d<-d[!(d$id=="Katerina"),]
 d<-d[!(d$id=="Ilin"),]
 d<-d[!(d$id=="Panteley"),]
 d<-d[!(d$id=="Zikmund"),]
 d<-d[!(d$id=="Odiseas"),]
 d<-d[!(d$id=="Asparuh"),]
 d<-d[!(d$id=="Belgin"),]
 d<-d[!(d$id=="Berenice"),]
 d<-d[!(d$id=="Heracles"),]
 d<-d[!(d$id=="Ibrahim"),]
 d<-d[!(d$id=="Ikaros"),]
 d<-d[!(d$id=="Ilina"),]
 d<-d[!(d$id=="Lomets"),]
 d<-d[!(d$id=="Macedonia"),]
 d<-d[!(d$id=="Maria"),]
 d<-d[!(d$id=="Redcliff"),]
 d<-d[!(d$id=="Regina"),]
 
 #on to next study
 summary(d$study)
 d1 = subset(d, study == "terra.natura-spain")
 plot = ggplot(d1, aes(date, ND), color = id.yr) + geom_line() + facet_wrap(~ id, ncol = 3)
 plot = plot + labs(x = "Date", y = "Net Displacement") + theme_bw() 
 plot
 
 ###############################
 #now, visually inspect by id.yr and remove non-migrations years
 ################################
 summary(d)
 summary(d$study)
 d1 = subset(d, study == "buechley-mideast")
 plot = ggplot(d1, aes(date, ND), color = id.yr) + geom_line() + facet_wrap(~ id.yr, ncol = 3)
 plot = plot + labs(x = "Date", y = "Net Displacement") + theme_bw() 
 plot
 d<-d[!(d$id.yr=="Aras_2013"),]
 d<-d[!(d$id.yr=="Cabuk_2016"),]
 d<-d[!(d$id.yr=="Haydi_2016"),]
 d<-d[!(d$id.yr=="Igdir_2015"),]
 d<-d[!(d$id.yr=="Logiya_2013"),]
 d<-d[!(d$id.yr=="Orada_2017"),]
 
 #on to next study
 summary(d$study)
 d1 = subset(d, study == "douro-spain")
 plot = ggplot(d1, aes(date, ND), color = id.yr) + geom_line() + facet_wrap(~ id.yr, ncol = 3)
 plot = plot + labs(x = "Date", y = "Net Displacement") + theme_bw() 
 plot
 
 #on to next study
 summary(d$study)
 d1 = subset(d, study == "efrat-israel")
 plot = ggplot(d1, aes(date, ND), color = id.yr) + geom_line() + facet_wrap(~ id.yr, ncol = 3)
 plot = plot + labs(x = "Date", y = "Net Displacement") + theme_bw() 
 plot
 
 #on to next study
 summary(d$study)
 d1 = subset(d, study == "grefa-spain")
 plot = ggplot(d1, aes(date, ND), color = id.yr) + geom_line() + facet_wrap(~ id.yr, ncol = 3)
 plot = plot + labs(x = "Date", y = "Net Displacement") + theme_bw() 
 plot
 d<-d[!(d$id.yr=="2HN_2016"),]
 d<-d[!(d$id.yr=="95R_2017"),]
 d<-d[!(d$id.yr=="95R_2018"),]
 d<-d[!(d$id.yr=="9FH_2018"),]
 d<-d[!(d$id.yr=="9FJ_2018"),]
 
 #on to next study
 summary(d$study)
 d1 = subset(d, study == "karyakin-russia")
 plot = ggplot(d1, aes(date, ND), color = id.yr) + geom_line() + facet_wrap(~ id.yr, ncol = 3)
 plot = plot + labs(x = "Date", y = "Net Displacement") + theme_bw() 
 plot
 d<-d[!(d$id.yr=="Gurman_2018"),]
 
 #on to next study
 summary(d$study)
 d1 = subset(d, study == "kobierzycki-france")
 plot = ggplot(d1, aes(date, ND), color = id.yr) + geom_line() + facet_wrap(~ id.yr, ncol = 3)
 plot = plot + labs(x = "Date", y = "Net Displacement") + theme_bw() 
 plot
 d<-d[!(d$id.yr=="Pyrenees_2015"),]
 
 #on to next study
 summary(d$study)
 d1 = subset(d, study == "life.rupis-spain")
 plot = ggplot(d1, aes(date, ND), color = id.yr) + geom_line() + facet_wrap(~ id.yr, ncol = 3)
 plot = plot + labs(x = "Date", y = "Net Displacement") + theme_bw() 
 plot
 d<-d[!(d$id.yr=="Bruco_2018"),]
 
 #on to next study
 summary(d$study)
 d1 = subset(d, study == "migra-spain")
 #plot = ggplot(d1, aes(date, ND), color = id.yr) + geom_line() + facet_wrap(~ id.yr, ncol = 3)
 #plot = plot + labs(x = "Date", y = "Net Displacement") + theme_bw() 
 #plot
 d<-d[!(d$id.yr=="Brieva_2017"),]
 d<-d[!(d$id.yr=="Quel_2017"),]
 d<-d[!(d$id.yr=="Quel_2018"),]
 #on to next study
 summary(d$study)
 d1 = subset(d, study == "oppel-balkans")
 #plot = ggplot(d1, aes(date, ND), color = id.yr) + geom_line() + facet_wrap(~ id.yr, ncol = 3)
 #plot = plot + labs(x = "Date", y = "Net Displacement") + theme_bw() 
 #plot
 d<-d[!(d$id.yr=="Arda_2013"),]
 d<-d[!(d$id.yr=="Castor_2016"),]
 d<-d[!(d$id.yr=="Dobromir_2013"),]
 d<-d[!(d$id.yr=="Elodie_2017"),]
 d<-d[!(d$id.yr=="Iliaz_2013"),]
 d<-d[!(d$id.yr=="Iliaz_2014"),]
 d<-d[!(d$id.yr=="Iliaz_2015"),]
 d<-d[!(d$id.yr=="Lazaros_2013"),]
 d<-d[!(d$id.yr=="Levkipos_2013"),]
 d<-d[!(d$id.yr=="Levkipos_2014"),]
 d<-d[!(d$id.yr=="Levkipos_2015"),]
 d<-d[!(d$id.yr=="Paschalis_2014"),]
 d<-d[!(d$id.yr=="Sanie_2014"),]
 d<-d[!(d$id.yr=="Spartacus_2011"),]
 d<-d[!(d$id.yr=="Svetlina_2013"),]
 d<-d[!(d$id.yr=="Svetlina_2014"),]
 d<-d[!(d$id.yr=="Volen_2013"),]
 #on to next study
 summary(d$study)
 d1 = subset(d, study == "terra.natura-spain")
 #plot = ggplot(d1, aes(date, ND), color = id.yr) + geom_line() + facet_wrap(~ id.yr, ncol = 3)
 #plot = plot + labs(x = "Date", y = "Net Displacement") + theme_bw() 
 #plot
 d<-d[!(d$id.yr=="A75658_2010"),]
 d<-d[!(d$id.yr=="A75658_2012"),]
 
 
 
 ###############################
 #now, visually inspect by id.yr.season and remove non-migrations years
 ################################
 summary(d$study)
 d1 = subset(d, study == "buechley-mideast")
 plot = ggplot(d1, aes(date, ND)) + geom_line() + facet_wrap(~ id.yr.season, ncol = 3)
 plot = plot + labs(x = "Date", y = "Net Displacement") + theme_bw() 
 plot
 d<-d[!(d$id.yr.season=="Agri_2013_spring"),]
 d<-d[!(d$id.yr.season=="Agri_2014_fall"),]
 d<-d[!(d$id.yr.season=="Ardahan_2013_spring"),]
 d<-d[!(d$id.yr.season=="Haydi_2014_spring"),]
 d<-d[!(d$id.yr.season=="Iste_2014_spring"),]
 d<-d[!(d$id.yr.season=="Serhat_2014_spring"),]
 d<-d[!(d$id.yr.season=="Tuzluca_2013_spring"),]
 d<-d[!(d$id.yr.season=="Tuzluca_2016_fall"),]
 #on to next study
 summary(d$study)
 d1 = subset(d, study == "douro-spain")
 #plot = ggplot(d1, aes(date, ND), color = id.yr) + geom_line() + facet_wrap(~ id.yr.season, ncol = 3)
 #plot = plot + labs(x = "Date", y = "Net Displacement") + theme_bw() 
 #plot
 d<-d[!(d$id.yr.season=="Batuecasa_2017_spring"),]
 d<-d[!(d$id.yr.season=="Huebra_2017_spring"),]
 #on to next study
 summary(d$study)
 d1 = subset(d, study == "efrat-israel")
 #plot = ggplot(d1, aes(date, ND), color = id.yr) + geom_line() + facet_wrap(~ id.yr.season, ncol = 3)
 #plot = plot + labs(x = "Date", y = "Net Displacement") + theme_bw() 
 #plot
 #on to next study
 summary(d$study)
 d1 = subset(d, study == "grefa-spain")
 #plot = ggplot(d1, aes(date, ND), color = id.yr) + geom_line() + facet_wrap(~ id.yr.season, ncol = 3)
 #plot = plot + labs(x = "Date", y = "Net Displacement") + theme_bw() 
 #plot
 d<-d[!(d$id.yr.season=="2HN_2017_fall"),]
 d<-d[!(d$id.yr.season=="2HN_2017_spring"),]
 #on to next study
 summary(d$study)
 d1 = subset(d, study == "karyakin-russia")
 #plot = ggplot(d1, aes(date, ND), color = id.yr) + geom_line() + facet_wrap(~ id.yr.season, ncol = 3)
 #plot = plot + labs(x = "Date", y = "Net Displacement") + theme_bw() 
 #plot
 #on to next study
 summary(d$study)
 d1 = subset(d, study == "kobierzycki-france")
 #plot = ggplot(d1, aes(date, ND)) + geom_line() + facet_wrap(~ id.yr.season, ncol = 3)
 #plot = plot + labs(x = "Date", y = "Net Displacement") + theme_bw() 
 #plot
 d<-d[!(d$id.yr.season=="Pyrenees_2016_spring"),]
 #on to next study
 summary(d$study)
 d1 = subset(d, study == "life.rupis-spain")
 #plot = ggplot(d1, aes(date, ND), color = id.yr) + geom_line() + facet_wrap(~ id.yr.season, ncol = 3)
 #plot = plot + labs(x = "Date", y = "Net Displacement") + theme_bw() 
 #plot
 d<-d[!(d$id.yr.season=="Douro_2017_spring"),]
 d<-d[!(d$id.yr.season=="Faia_2017_spring"),]
 d<-d[!(d$id.yr.season=="Faia_2018_fall"),]
 d<-d[!(d$id.yr.season=="Poiares_2017_spring"),]
 #on to next study
 summary(d$study)
 d1 = subset(d, study == "migra-spain")
 plot = ggplot(d1, aes(date, ND), color = id.yr) + geom_line() + facet_wrap(~ id.yr.season, ncol = 3)
 plot = plot + labs(x = "Date", y = "Net Displacement") + theme_bw() 
 plot
 #on to next study
 summary(d$study)
 d1 = subset(d, study == "oppel-balkans")
 #plot = ggplot(d1, aes(date, ND), color = id.yr) + geom_line() + facet_wrap(~ id.yr.season, ncol = 3)
 #plot = plot + labs(x = "Date", y = "Net Displacement") + theme_bw() 
 #plot
 summary(d$id)
 d<-d[!(d$id.yr.season=="Akaga_2018_spring"),]
 d<-d[!(d$id.yr.season=="Aoos_2015_spring"),]
 d<-d[!(d$id.yr.season=="Boyana_2018_spring"),]
 d<-d[!(d$id.yr.season=="Castor_2014_spring"),]
 d<-d[!(d$id.yr.season=="Batuecasa_2017_spring"),]
 d<-d[!(d$id.yr.season=="Lazaros_2012_spring"),]
 d<-d[!(d$id.yr.season=="Polya_2018_spring"),]
 d<-d[!(d$id.yr.season=="Volen_2014_fall"),]
 #on to next study
 summary(d$study)
 d1 = subset(d, study == "terra.natura-spain")
 #plot = ggplot(d1, aes(date, ND), color = id.yr) + geom_line() + facet_wrap(~ id.yr.season, ncol = 3)
 #plot = plot + labs(x = "Date", y = "Net Displacement") + theme_bw() 
 #plot
 summary(d1$id)
 d<-d[!(d$id.yr.season=="A75658_2011_spring"),]
 d<-d[!(d$id.yr.season=="A75658_2011_fall"),]
 d<-d[!(d$id.yr.season=="A75659_2011_fall"),]
 d<-d[!(d$id.yr.season=="A80420_2013_fall"),]
 d<-d[!(d$id.yr.season=="A89731_2013_fall"),]
 
 #additional incomplete migrations identified through later processes
 d<-d[!(d$id.yr.season=="Pyrenees_2016_fall"),]
 d<-d[!(d$id.yr.season=="A89731_2012_fall"),]
 d<-d[!(d$id.yr.season=="Ardahan_2014_fall"),]

 ###########################################################
 #remove captive raised birds ( Akaga, Blanka, Boyana, Elodie and Polya)
 ################################
 d<-d[!(d$id=="Akaga"),]
 d<-d[!(d$id=="Blanka"),] 
 d<-d[!(d$id=="Boyana"),] 
 d<-d[!(d$id=="Elodie"),] 
 d<-d[!(d$id=="Polya"),] 
 
 #
 summary(d)
 write.csv(d, "EGVU_Final_complete_migrations_only.csv")
 
 #limit to 1 pt per day
 d1 = d[!duplicated(d[,c('id', 'Year', 'Month', 'Day')]),]
 write.csv(d1, "EGVU_Final_complete_migrations_only_1ptperday.csv") 
           
           
           
 