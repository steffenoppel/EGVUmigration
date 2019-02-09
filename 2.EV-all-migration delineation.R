#Set WD
setwd("~/Google Drive/Egyptian Vulture/Frontiers in Ecology & Evolution Paper/Latest/Data/EVmigration_data")
setwd("C:\\STEFFEN\\MANUSCRIPTS\\in_prep\\EGVU_papers\\FrontiersMigrationPaper\\EGVUmigration")

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
# 
# #load data
# d<-read.csv("EV-all-1ptperday-filtered-utm-NSD.csv", header=T, sep=",")
# summary(d)
# d$date <- ymd_hms(d$date)
# head(d)
# 
# #assign a season variable (spring / fall)
# d$season <- ifelse(d$Month >= 7,
#                         c("fall"), c("spring")) 
# d$season = d$season
# summary(d)
# 
# #add id.yr.season variable
# d$id.yr.season <- c(paste(d$id.yr,d$season,sep="_")) 
# head(d)
# write.csv(d, "EV-all-1ptperday-filtered-utm-NSD-season.csv")

#########################################################
d = read.csv("EV-all-1ptperday-filtered-utm-NSD-season.csv")
summary(d)
d$date <- ymd_hms(d$date)

#subset by study to have manageable datasets
#then check dataset visually and remove non-migrant id.yr's
summary(d$study)
d1 = subset(d, study == "buechley-mideast")
plot = ggplot(d1, aes(date, ND), color = id.yr) + geom_line() + facet_wrap(~ id, ncol = 3)
plot = plot + labs(x = "Date", y = "Net Displacement") + theme_bw()
plot
#inspect visually, then remove id's for birds that did not COMPLETE a migration from the original dataset
#i'm defining a migration here roughly as anything that passess a migration midpoint (from that study) and that is completed
d<-d[!(d$id=="Djibouti"),]
d<-d[!(d$id=="Mille"),]
d<-d[!(d$id=="Arpacay"),]

#on to next study
summary(d$study)
d1 = subset(d, study == "douro-spain")
#plot = ggplot(d1, aes(date, ND), color = id.yr) + geom_line() + facet_wrap(~ id, ncol = 3)
#plot = plot + labs(x = "Date", y = "Net Displacement") + theme_bw() 
#plot
d<-d[!(d$id=="BatuecasP"),]
d<-d[!(d$id=="Camaces"),]
#on to next study
summary(d$study)
d1 = subset(d, study == "efrat-israel")
#plot = ggplot(d1, aes(date, ND), color = id.yr) + geom_line() + facet_wrap(~ id, ncol = 3)
#plot = plot + labs(x = "Date", y = "Net Displacement") + theme_bw() 
#plot
#on to next study
summary(d$study)
d1 = subset(d, study == "grefa-spain")
#plot = ggplot(d1, aes(date, ND), color = id.yr) + geom_line() + facet_wrap(~ id, ncol = 3)
#plot = plot + labs(x = "Date", y = "Net Displacement") + theme_bw() 
#plot
d<-d[!(d$id=="9FC"),]
d<-d[!(d$id=="2HP"),]
#on to next study
summary(d$study)
d1 = subset(d, study == "karyakin-russia")
#plot = ggplot(d1, aes(date, ND), color = id.yr) + geom_line() + facet_wrap(~ id, ncol = 3)
#plot = plot + labs(x = "Date", y = "Net Displacement") + theme_bw() 
#plot
d<-d[!(d$id=="Kapchug"),]
d<-d[!(d$id=="Sarygush"),]
#on to next study
summary(d$study)
d1 = subset(d, study == "kobierzycki-france")
#plot = ggplot(d1, aes(date, ND), color = id.yr) + geom_line() + facet_wrap(~ id, ncol = 3)
#plot = plot + labs(x = "Date", y = "Net Displacement") + theme_bw() 
#plot
#on to next study
summary(d$study)
d1 = subset(d, study == "life.rupis-spain")
#plot = ggplot(d1, aes(date, ND), color = id.yr) + geom_line() + facet_wrap(~ id, ncol = 3)
#plot = plot + labs(x = "Date", y = "Net Displacement") + theme_bw() 
#plot
#on to next study
summary(d$study)
d1 = subset(d, study == "migra-spain")
#plot = ggplot(d1, aes(date, ND), color = id.yr) + geom_line() + facet_wrap(~ id, ncol = 3)
#plot = plot + labs(x = "Date", y = "Net Displacement") + theme_bw() 
#plot
d<-d[!(d$id=="Endoia"),]
#on to next study
summary(d$study)
d1 = subset(d, study == "oppel-balkans")
#plot = ggplot(d1, aes(date, ND), color = id.yr) + geom_line() + facet_wrap(~ id, ncol = 3)
#plot = plot + labs(x = "Date", y = "Net Displacement") + theme_bw() 
#plot
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
#plot = ggplot(d1, aes(date, ND), color = id.yr) + geom_line() + facet_wrap(~ id, ncol = 3)
#plot = plot + labs(x = "Date", y = "Net Displacement") + theme_bw() 
#plot

###############################
#now, visually inspect by id.yr and remove non-migrations years
################################
summary(d$study)
d1 = subset(d, study == "buechley-mideast")
#plot = ggplot(d1, aes(date, ND), color = id.yr) + geom_line() + facet_wrap(~ id.yr, ncol = 3)
#plot = plot + labs(x = "Date", y = "Net Displacement") + theme_bw() 
#plot
d<-d[!(d$id.yr=="Aras_2013"),]
d<-d[!(d$id.yr=="Cabuk_2016"),]
d<-d[!(d$id.yr=="Haydi_2016"),]
d<-d[!(d$id.yr=="Igdir_2015"),]
d<-d[!(d$id.yr=="Logiya_2013"),]
d<-d[!(d$id.yr=="Orada_2017"),]
#on to next study
summary(d$study)
d1 = subset(d, study == "douro-spain")
#plot = ggplot(d1, aes(date, ND), color = id.yr) + geom_line() + facet_wrap(~ id.yr, ncol = 3)
#plot = plot + labs(x = "Date", y = "Net Displacement") + theme_bw() 
#plot
#on to next study
summary(d$study)
d1 = subset(d, study == "efrat-israel")
#plot = ggplot(d1, aes(date, ND), color = id.yr) + geom_line() + facet_wrap(~ id.yr, ncol = 3)
#plot = plot + labs(x = "Date", y = "Net Displacement") + theme_bw() 
#plot
#on to next study
summary(d$study)
d1 = subset(d, study == "grefa-spain")
#plot = ggplot(d1, aes(date, ND), color = id.yr) + geom_line() + facet_wrap(~ id.yr, ncol = 3)
#plot = plot + labs(x = "Date", y = "Net Displacement") + theme_bw() 
#plot
d<-d[!(d$id.yr=="2HN_2016"),]
d<-d[!(d$id.yr=="95R_2017"),]
d<-d[!(d$id.yr=="95R_2018"),]
d<-d[!(d$id.yr=="9FH_2018"),]
d<-d[!(d$id.yr=="9FJ_2018"),]
#on to next study
summary(d$study)
d1 = subset(d, study == "karyakin-russia")
#plot = ggplot(d1, aes(date, ND), color = id.yr) + geom_line() + facet_wrap(~ id.yr, ncol = 3)
#plot = plot + labs(x = "Date", y = "Net Displacement") + theme_bw() 
#plot
d<-d[!(d$id.yr=="Gurman_2018"),]
#on to next study
summary(d$study)
d1 = subset(d, study == "kobierzycki-france")
#plot = ggplot(d1, aes(date, ND), color = id.yr) + geom_line() + facet_wrap(~ id.yr, ncol = 3)
#plot = plot + labs(x = "Date", y = "Net Displacement") + theme_bw() 
#plot
d<-d[!(d$id.yr=="Pyrenees_2015"),]
#on to next study
summary(d$study)
d1 = subset(d, study == "life.rupis-spain")
#plot = ggplot(d1, aes(date, ND), color = id.yr) + geom_line() + facet_wrap(~ id.yr, ncol = 3)
#plot = plot + labs(x = "Date", y = "Net Displacement") + theme_bw() 
#plot
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
#plot = ggplot(d1, aes(date, ND)) + geom_line() + facet_wrap(~ id.yr.season, ncol = 3)
#plot = plot + labs(x = "Date", y = "Net Displacement") + theme_bw() 
#plot
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

#######################################################
##write trimmed "completed migrations" dataset to csv
write.csv(d, "EV-CompletedMigrations-1ptperday.csv")

#######################################################
#Convert to ltraj object
######################################################
##Clear workspace
rm(list = ls())

#read in clean csv
d = read.csv("EV-CompletedMigrations-1ptperday.csv")
d$date = ymd_hms(d$date)
summary(d$id.yr.season)

########################################################
#convert to ltraj
########################################################
d2 = as.ltraj(xy = d[, c("utm.e", "utm.n")], date = d$date, id = d$id.yr.season)
d2
#plot(d2)

#save ltraj as dataframe
# R2n is the squared distance between the first relocation 
# of the trajectory and the current relocation in map units
nsds <- do.call(rbind, d2)
summary(nsds) 

###################################################
#Use migrateR package to delineate migration periods
###################################################
require(migrateR)
require(plyr)
par(mfrow=c(1,1)) 

#Fitting Models
#?mvmtClass
d3 <- mvmtClass(d2) 
d3
ev.migr.summary = data.frame(unclass(summary(d3)))
ev.migr.summary
#write.csv(ev.migr.summary, "ev.migr.summary.1ptperday.csv")

#plot model fits
plot(d3, ncol = 1)				# Will cycle through all elements [esc!]	
# or plot a specific plot (like those not classified as dispersers)
plot(d3[[181]])

# Timing parameters to calendar  dates, setting p = 0.05 
# means that 95% of the migration distance is included in 
# the parameter estimates (i.e. Buechley et al 2018)
# using 'disperser' model type here because a-priori, we know that the species
#is a long distance migrant, and I pre-scanned the NSD to remove non-migratory
# or partial migrations. Thus, all completed migrations should follow a 
#disperser model
#?mvmt2dt
ev.all.migr.dates =  mvmt2dt(d3, p = 0.05, mod = "disperser")
#check error messages -- 2 id's without 'disperser'models
ev.all.migr.dates
ev.all.migr.dates.df <- ldply (ev.all.migr.dates, data.frame)
ev.all.migr.dates.df
ev.all.migr.dates.df$model = "disperser"
write.csv(ev.all.migr.dates.df, "disperser-dates-1ptperday.csv")
#
#ev.all.migr.dates =  mvmt2dt(d3, p = 0.01, mod = "migrant")
#ev.all.migr.dates
#ev.all.migr.dates.df <- ldply (ev.all.migr.dates, data.frame)
#ev.all.migr.dates.df
#ev.all.migr.dates.df$model = "migrant"
#write.csv(ev.all.migr.dates.df, "migrant.dates.csv")
#
#ev.all.migr.dates =  mvmt2dt(d3, p = 0.01, mod = "mixmig")
#ev.all.migr.dates
#ev.all.migr.dates.df <- ldply (ev.all.migr.dates, data.frame)
#ev.all.migr.dates.df
#ev.all.migr.dates.df$model = "mixmig"
#write.csv(ev.all.migr.dates.df, "mixmig.dates.csv")

###################################################
#merge migr.summary with disperser dates
migr.summary = read.csv("ev.migr.summary.1ptperday.csv")
disperser.dates = read.csv("disperser-dates-1ptperday.csv")
#add burst column to match migr summary
disperser.dates$burst = disperser.dates$.id
disperser.dates$.id = NULL
disperser.dates$X = NULL
migr.summary$X = NULL
# check datasets
head(migr.summary)
head(disperser.dates)
tail(migr.summary)
summary(migr.summary)
summary(disperser.dates)
#merge by burst
migr.summary2 = merge(migr.summary,disperser.dates, by = "burst", all = TRUE)
tail(migr.summary2)
# add start-end column with repeating start, end, start, end, etc
startend = rep(c("start", "end"), times = 193)
startend
migr.summary2$startend = startend
head(migr.summary2)
write.csv(migr.summary2, "EV-migration-summary-final-1ptperday.csv")

#######################################
# at this point, there are 2 models that did not fit
#I inspected them, adjusted model fit and extracted start/end dates manually
#then inserted into "EV-migration-summary-final.csv"
########################################
d = read.csv("EV-CompletedMigrations-1ptperday.csv")
#
A89730_2010_spring = subset(d, d$id.yr.season=="A89730_2010_spring")
A89730_2010_spring
write.csv(A89730_2010_spring, "A89730_2010_spring.csv")
d = read.csv("A89730_2010_spring.csv")
d$date = ymd_hms(d$date)
d2 = as.ltraj(xy = d[, c("utm.e", "utm.n")], date = d$date, id = d$id.yr.season)
d3 <- mvmtClass(d2) 
plot(d3)
#adjusting the start date fixes the conversion problem for the disperser model
d3 <- mvmtClass(d2, stdt = "02-01")
plot(d3)
mvmt2dt(d3, p = 0.05, mod = "disperser")
##########################################################
d = read.csv("EV-CompletedMigrations-1ptperday.csv")
#
Sanie_2016_fall = subset(d, d$id.yr.season=="Sanie_2016_fall")
write.csv(Sanie_2016_fall, "Sanie_2016_fall.csv")
d = read.csv("Sanie_2016_fall.csv")
d$date = ymd_hms(d$date)
d
#
d2 = as.ltraj(xy = d[, c("utm.e", "utm.n")], date = d$date, id = d$id.yr.season)
d2
#?mvmtClass
d3 <- mvmtClass(d2) 
plot(d3)
d3 <- mvmtClass(d2, stdt = "08-01")
plot(d3)
mvmt2dt(d3, p = 0.05, mod = "disperser")

# extract parameters from top model (i.e. migrant, mix-migrant, disperser, etc.)
# use terms 'mrho', 'mdelta' = 500' to set minimum residency and minimum distacne, respetcively
# e.g. top.model.params <- topmvmt(d3, mrho = 21, mdelta = 500)
#?topmvmt
#top.model.params <- topmvmt(d3)
#top.model.params
#top.model.params.df = mvmt2df(top.model.params)
#top.model.params.df
#disperser = top.model.params.df$disperser
#disperser
#write.csv(disperser, "top.mod.disperser.csv")
#migrant = top.model.params.df$migrant
#migrant
#write.csv(migrant, "top.mod.migrant.csv")
#mixmig = top.model.params.df$mixmig
#mixmig
#write.csv(mixmig, "top.mod.mixmig.csv")

##############################
#dtheta2(d3)  # "timing of return migration"
#delta2(d3)  # "distance of return migration"

# Paired model and xy plots (work in progress...)
# spatmig(d2, d3)		# Will cycle through all elements [esc!]

############################################################################
# assign migration start and end dates to complete dataset
############################################################################
full.data = read.csv("EV-CompletedMigrations.csv")
head(full.data)
full.data$X.3 = NULL
full.data$X.2 = NULL
full.data$X.1 = NULL
full.data$X = NULL
full.data$date = ymd_hms(full.data$date)
head(full.data)
#
migr.sum = read.csv("EV-migration-summary-final-1ptperday-allmodels-excelmodified.csv")
head(migr.sum)
#migr.sum$date = mdy_hm(migr.sum$date)
#head(migr.sum)
migr.sum$X = NULL
migr.sum$id.yr.season = migr.sum$burst
migr.sum$burst = NULL
head(migr.sum)
#
head(migr.sum)
head(full.data)
#
d = merge(full.data,migr.sum, by = "id.yr.season", all = T)
head(d)
#
d$migr.start.date = mdy_hm(d$migr.start.date)
d$migr.end.date = mdy_hm(d$migr.end.date)
d$date = ymd_hms(d$date)
head(d)
d1 = d[((d$date > d$migr.start.date) & (d$date < d$migr.end.date)),]
head(d)
head(d1)
tail(d1)
write.csv(d1, "EV-migrations.only-1ptperhour.csv")

###########################################################################
#plot migrations only
##########################################################################
pcks <- list('plyr', 'lubridate', 'ggmap', 'maptools', 'rgdal', 'maps', 'mapdata', 'move', 'mapproj')
sapply(pcks, require, character = TRUE)
##Clear workspace
rm(list = ls())
#load the data
d = read.csv("EV-migrations.only-1ptperday.csv")
head(d)
d$date <- ymd_hms(d$date)
class(d$date)

#convert to ltraj
d2 = as.ltraj(xy = d[, c("utm.e", "utm.n")], date = d$date, id = d$id.yr.season)
d2
plot(d2)

#save ltraj as dataframe
# R2n is the squared distance between the first relocation 
# of the trajectory and the current relocation in map units
nsds <- do.call(rbind, d2)
summary(nsds) 

##Check the order of IDs before performing this step
d$NSD2 <- nsds$R2n
d$ND2 <- sqrt(d$NSD)
head(d)

summary(d$study)
mideast = subset(d, study == "buechley-mideast")
tn = subset(d, study == "terra.natura-spain")
israel = subset(d, study == "efrat-israel")
douro = subset(d, study == "douro-spain")
grefa = subset(d, study == "grefa-spain")
russia = subset(d, study == "karyakin-russia")
france = subset(d, study == "kobierzycki-france")
lr = subset(d, study == "life.rupis-spain")
migra = subset(d, study == "migra-spain")
balkans = subset(d, study == "oppel-balkans")

plot = ggplot(migra, aes(date, ND2)) + geom_line() + facet_wrap(~ id.yr.season, ncol = 3)
plot = plot + labs(x = "Date", y = "Net Displacement") + theme_bw() 
plot

plot = ggplot(d, aes(date, ND2)) + geom_line() + facet_wrap(~ id.yr.season, ncol = 6)
plot = plot + labs(x = "Date", y = "Net Displacement") + theme_bw() 
plot

#Personal Google API Key - careful not to publish or share
api <- "INSERT HERE" 
register_google(key = api)
ggmap_credentials()

#Mapping
summary(d)
names(d)
map = get_map("Chad", zoom = 3)
ggmap(map)
map1 = ggmap(map) + geom_point(data = d, aes(x = long, y = lat, color = id))
map1
ggplot(d, aes(x=long, y=lat, col=id.yr.season)) + geom_path(alpha = 0.5) + geom_point(alpha = 0.5) + coord_fixed()
#
summary(d)
latrange <- range(-2,60)
longrange <- range(-10,52)
ev.map <- get_map(location = c(longrange[1], latrange[1], longrange[2], latrange[2]), maptype = "terrain", source="google")
ev.plot = ggmap(ev.map) + geom_point(data = d, mapping = aes(x = long, y = lat, col=id), alpha = 0.5, size=0.5) +
  geom_path(data = d, mapping = aes(x = long, y = lat, col=id), alpha = 0.5, size=0.5) +
  coord_map() + labs(x = "Longitude", y = "Latitude") + 
  scale_color_discrete(l = 30) + 
  guides(color=guide_legend(ncol=4))
ev.plot

######################################################
#plot original dataset and final migration dataset
#for comparison
#######################################################
#Set WD
setwd("~/Google Drive/Egyptian Vulture/Frontiers in Ecology & Evolution Paper/Latest/Data/EVmigration_data")

#Clear workspace
rm(list = ls())

#load packages
library(sp)
library(maptools)

#read in datasets
orig<-read.csv("EV-all-1ptperday-filtered-utm-NSD-season.csv")
migr = read.csv("EV-migrations.only-1ptperhour.csv")
head(migr)

points_to_line <- function(data, long, lat, id_field = NULL, sort_field = NULL) {
  
  # Convert to SpatialPointsDataFrame
  coordinates(data) <- c(long, lat)
  
  # If there is a sort field...
  if (!is.null(sort_field)) {
    if (!is.null(id_field)) {
      data <- data[order(data[[id_field]], data[[sort_field]]), ]
    } else {
      data <- data[order(data[[sort_field]]), ]
    }
  }
  
  # If there is only one path...
  if (is.null(id_field)) {
    
    lines <- SpatialLines(list(Lines(list(Line(data)), "id")))
    
    return(lines)
    
    # Now, if we have multiple lines...
  } else if (!is.null(id_field)) {  
    
    # Split into a list by ID field
    paths <- sp::split(data, data[[id_field]])
    
    sp_lines <- SpatialLines(list(Lines(list(Line(paths[[1]])), "line1")))
    
    # I like for loops, what can I say...
    for (p in 2:length(paths)) {
      id <- paste0("line", as.character(p))
      l <- SpatialLines(list(Lines(list(Line(paths[[p]])), id)))
      sp_lines <- spRbind(sp_lines, l)
    }
    
    return(sp_lines)
  }
}

#
library(leaflet)
migr_lines <- points_to_line(data = migr, 
                          long = "long", 
                          lat = "lat", 
                          id_field = "id.yr.season", 
                          sort_field = "date")

leaflet(data = migr_lines) %>%
  addTiles() %>%
  addPolylines()
#
head(orig)
orig_lines <- points_to_line(data = orig, 
                             long = "long", 
                             lat = "lat", 
                             id_field = "id.yr.season", 
                             sort_field = "date")

leaflet(data = orig_lines) %>%
  addTiles() %>%
  addPolylines()

