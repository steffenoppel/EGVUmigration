######################################################################
#Migration delineation
######################################################################
#Set WD
setwd("~/Google Drive/Egyptian Vulture/Frontiers in Ecology & Evolution Paper/Latest/Data/EVmigration_data")

##Clear workspace
rm(list = ls())

###Load relevant libraries###
##install MigrateR
#install.packages("devtools")
library(devtools)
#install_github("dbspitz/migrateR/migrateR", build_vignettes = T)
library(adehabitatLT)
library(adehabitatHR)
library(plotrix)
library(lattice)
library(gdata)
library(nlme)
library(maps)
library(lubridate)

# read in original, raw data files one by one, check & clean 
mideast = read.csv("EgyptianVulture_MiddleEast.csv")
galicia = read.csv("GREFA_Galicia_Spain.csv")
israel = read.csv("Israel_wild.csv")
dagestan = read.csv("Karyakin_Dagestan.csv")
france = read.csv("Kobierzycki_France.csv")
spain1 = read.csv("MIGRA_Gipuzkoa_Spain.csv")
spain2 = read.csv("MIGRA_Rioja_Spain.csv")
balkans = read.csv("Oppel_Bulgaria_Greece.csv")
spain3 = read.csv("Saloro_Spain.csv")
spain4 = read.csv("TerraNaturaSpain.csv")
rupis = read.csv("VCF_LIFERupis.csv")

###################################################
##Clean and format data
###################################################
#Store DateTime as POSIXt object
#summary(balkans)
balkans = balkans[complete.cases(balkans[ ,2:4]),] #remove any rows that don't have date, lat or long
balkans$date <- dmy_hms(balkans$study.local.timestamp)
#head(balkans)
#
#summary(dagestan)
dagestan = dagestan[complete.cases(dagestan[ ,2:4]),] #remove any rows that don't have date, lat or long
dagestan$date <- dmy_hms(dagestan$study.local.timestamp)
#head(dagestan)
#
#summary(france) # note large number of NAs for lat / long -- these are accessory measurements than can be deleted
france = france[complete.cases(france[ ,2:4]),] #remove any rows that don't have date, lat or long
france$date <- dmy_hms(france$timestamp)
#head(france)
#
#summary(galicia)# note large number of NAs for lat / long -- these are accessory measurements than can be deleted
galicia = galicia[complete.cases(galicia[ ,2:4]),] #remove any rows that don't have date, lat or long
galicia$date <- dmy_hms(galicia$study.local.timestamp)
#head(galicia)
#
#summary(israel)
israel = israel[complete.cases(israel[ ,2:4]),] #remove any rows that don't have date, lat or long
israel$date <- dmy_hms(israel$study.local.timestamp)
#head(israel)
#
#summary(mideast)
mideast = mideast[complete.cases(mideast[ ,2:4]),] #remove any rows that don't have date, lat or long
mideast$date <- dmy_hms(mideast$study.local.timestamp)
#head(mideast)
#this study had 2 trabnsmitters that collected data every 2 mins. 
#so this next section of code subset this data down to 1 pt per hour, 
#because this dataset is huge otherwise and will slow down all computations
mideast$Year <- year(mideast$date)
mideast$Month <- month(mideast$date)
mideast$Day = day(mideast$date)
mideast$Hour <- hour(mideast$date)
names(mideast)
mideast = mideast[!duplicated(mideast[,c('individual.local.identifier', 'Year', 'Month', 'Day', 'Hour')]),]
mideast$Year = NULL
mideast$Month = NULL
mideast$Day = NULL
mideast$Hour = NULL
#
#summary(rupis)
rupis = rupis[complete.cases(rupis[ ,2:4]),] #remove any rows that don't have date, lat or long
rupis$date <- dmy_hms(rupis$study.local.timestamp)
#head(rupis)
#
#summary(spain1)
spain1 = spain1[complete.cases(spain1[ ,2:4]),] #remove any rows that don't have date, lat or long
spain1$date <- dmy_hms(spain1$study.local.timestamp)
#head(spain1)
#
#summary(spain2)
spain2 = spain2[complete.cases(spain2[ ,2:4]),] #remove any rows that don't have date, lat or long
spain2$date <- dmy_hms(spain2$study.local.timestamp)
#head(spain2)
#
#summary(spain3)
spain3 = spain3[complete.cases(spain3[ ,2:4]),] #remove any rows that don't have date, lat or long
spain3$date <- dmy_hms(spain3$study.local.timestamp)
#head(spain3)
#
#summary(spain4)
spain4 = spain4[complete.cases(spain4[ ,2:4]),] #remove any rows that don't have date, lat or long
spain4$date <- dmy_hms(spain4$study.local.timestamp)
#head(spain4)
#need to reclassify ID as factor because it is read as numeric, because the ids start with numbers
spain4$individual.local.identifier = as.factor(spain4$individual.local.identifier) 
#class(spain4$id)

#################################################################
#Merge datasets
#################################################################
names(balkans) == names(dagestan)
d = rbind(balkans, dagestan)
names(d) == names(france)
d = rbind(d,france)
names(d) == names(galicia)
d = rbind(d, galicia)
names(d) == names(israel)
d = rbind(d, israel)
names(d) == names(mideast)
d = rbind(d, mideast)
names(d) == names(rupis)
d = rbind(d, rupis)
names(d) == names(spain1)
d = rbind(d, spain1)
names(d) == names(spain2)
d = rbind(d, spain2)
names(d) == names(spain3)
d = rbind(d, spain3)
names(d) == names(spain4)
d = rbind(d, spain4)
summary(d)

#write merged dataset file
write.csv(d, "EV-all-merged.csv")

################################################################
# Continue data cleaning
################################################################
##Clear workspace
rm(list = ls())

#read in 
d = read.csv("EV-all-merged.csv")
summary(d)
d$date <- ymd_hms(d$date)

# Pull year, month, day, hour from date
d$Year <- year(d$date)
d$Month <- month(d$date)
d$Day = day(d$date)
d$Hour <- hour(d$date)

#Limit to 1 point per hour per ID (note -- i did this above for 1 study, but now for all)
names(d)
summary(d)
d1 = d[!duplicated(d[,c('id', 'Year', 'Month', 'Day', 'Hour')]),]
summary(d1)

## Remove duplicates
any(duplicated(d, fromLast=F))
any(duplicated(d, fromLast=T))
d = d[!duplicated(d),]

#Pascual Lopez-Lopez: To filter outliers I prefer to use a filter by altitude. 
#I have found that normally outliers (i.e., incorrect lat&long values) 
#correspond to incorrect annotation of altitude. 
#To solve this I usually make this filter in R:
#summary(d)
#d <- subset(d, d$height.above.ellipsoid < 5000)
  #Evan: Unfortunately not all of the data in our study has altitude, 
  #so we can't apply this across the entire dataset

#Limit to 1 point per hour per ID (note - this will simply save the first point per id in every hour)
summary(d)
d1 = d[!duplicated(d[,c('individual.local.identifier', 'Year', 'Month', 'Day', 'Hour')]),]
write.csv(d1, "EV-all-1ptperhr.csv")

#Limit to 1 point per day per ID (note - this will simply save the first point per id in every day)
d2 = d[!duplicated(d[,c('individual.local.identifier', 'Year', 'Month', 'Day')]),]
write.csv(d2, "EV-all-1ptperday.csv")

#########################################################################
###Read in data file - now working with the 1pt per hour dataset
#########################################################################
rm(list = ls())
d<-read.csv("EV-all-1ptperhr.csv")
summary(d)

#re-store date as POSIXt object
class(d$date)
d$date <- ymd_hms(d$date)
class(d$date)

#remove data from 2019 (setting a cutoff point for the study data)
d <- subset(d, date <= as.POSIXct('2018-12-30 23:59'))
summary(d$date)

#rename/simplify column headers
names(d)
colnames(d)[colnames(d)=="location.long"] <- "long"
colnames(d)[colnames(d)=="location.lat"] <- "lat"
colnames(d)[colnames(d)=="tag.local.identifier"] <- "tag"
colnames(d)[colnames(d)=="individual.local.identifier"] <- "id"
colnames(d)[colnames(d)=="study.name"] <- "study"
names(d)

#rename/simplify study names
names(d)
summary(d$study)
d$study = as.character(d$study)
d$study[d$study == "_zh Neophron percnopterus"] <- "life.rupis-spain"
d$study[d$study == "Egyptian vulture EB Terra Natura UA Spain"] <- "terra.natura-spain"
d$study[d$study == "Egyptian Vulture in Gipuzkoa - Migra Program in Spain"] <- "migra-spain"
d$study[d$study == "Egyptian Vulture in La Rioja - Migra Program in Spain"] <- "migra-spain"
d$study[d$study == "Egyptian vulture Kobierzycki Gardon "] <- "kobierzycki-france"
d$study[d$study == "Egyptian vulture Kobierzycki Pyrenees"] <- "kobierzycki-france"
d$study[d$study == "Egyptian vulture Kobierzycki Vaucluse"] <- "kobierzycki-france"
d$study[d$study == "Egyptian Vulture wild-birds Israel"] <- "efrat-israel"
d$study[d$study == "Egyptian vultures Dagestan2"] <- "karyakin-russia"
d$study[d$study == "EgyptianVulture_MiddleEast"] <- "buechley-mideast"
d$study[d$study == "Neophron percnopterus Bulgaria/Greece"] <- "oppel-balkans"
d$study[d$study == "Neophron percnopterus. GREFA. Spain"] <- "grefa-spain"
d$study[d$study == "SaloroDouro"] <- "douro-spain"
d$study = as.factor(d$study)
summary(d$study)

#rename/simplify IDs
summary(d$id)
d$id = as.character(d$id)
d$id[d$id == "Amud H08 Blue"] <- "Amud"
d$id[d$id == "75657"] <- "A75657"
d$id[d$id == "75658"] <- "A75658"
d$id[d$id == "75659"] <- "A75659"
d$id[d$id == "80419"] <- "A80419"
d$id[d$id == "80420"] <- "A80420"
d$id[d$id == "89730"] <- "A89730"
d$id[d$id == "89731"] <- "A89731"
d$id[d$id == "BatuecasA"] <- "Batuecasa"
d$id[d$id == "Gamla 2018 H36 Green"] <- "H36"
d$id[d$id == "Gamla 2018 J4107"] <- "J4107"
d$id[d$id == "Gardon_2016_Ad_wild_MW707 "] <- "Gardon"
d$id[d$id == "Guip\xfazcoa 01 - Zuri"] <- "Zuri"
d$id[d$id == "Guip\xfazcoa 02 - Endoia"] <- "Endoia"
d$id[d$id == "Guip\xfazcoa 03 - Ekhi - 5533 "] <- "Ekhi"
d$id[d$id == "La Rioja 01-Brieva"] <- "Brieva"
d$id[d$id == "La Rioja 02-Quel"] <- "Quel"
d$id[d$id == "La Rioja 03 - Lidia -REKI38"] <- "Lidia"
d$id[d$id == "NeoPer_Bruco"] <- "Bruco"
d$id[d$id == "NeoPer_Douro"] <- "Douro"
d$id[d$id == "NeoPer_Faia"] <- "Faia"
d$id[d$id == "NeoPer_Poiares"] <- "Poiares"
d$id[d$id == "NeoPer_Rupis"] <- "Rupis"
d$id[d$id == "Ossau_2018_Ad_wild_OR17996_PC0"] <- "Ossau"
d$id[d$id == "Provence_2016_Ad_wild_EO5018_Salome_8P"] <- "Provence"
d$id[d$id == "Pyrenees_2015_Imm_wild_EC48601087785"] <- "Pyrenees"
d$id[d$id == "Red Cliff"] <- "Redcliff"
d$id = as.factor(d$id)
summary(d$id)

###Create burst by ID and Year
head(d)
d$id.yr <- c(paste(d$id,d$Year,sep="_")) 
d$id.yr <- as.factor(d$id.yr) 
summary(d$id.yr)

# order by ID and datetime
head(d)
attach(d)
d = d[order(id, date),]
detach(d)
head(d)

#last check at cleaned dataset
summary(d)
names(d)
summary(d$date)
summary(d$id)
summary(d$study)
summary(d$Year)

write.csv(d, "Ev-all-1ptperhr-clean.csv")

###########################################################
#AT this point, I loaded the cleaned dataset into movebank
#and used movebank's 'simple outlier' filter to remove outliers
# max plausible speed == 25 m/s (km/hr = 25*1000 / 60*60), error radius == 30m 
#parameters determined through experimentation
############################################################
##Clear workspace
rm(list = ls())

#reload dataset after outliers filtered in movebank
#note -- some of the non-essential columns were lost while importing into movebank
d = read.csv("EV-all-filtered.csv")

#rename/simplify column headers
names(d)
head(d)
colnames(d)[colnames(d)=="location.long"] <- "long"
colnames(d)[colnames(d)=="location.lat"] <- "lat"
colnames(d)[colnames(d)=="tag.local.identifier"] <- "tag"
colnames(d)[colnames(d)=="individual.local.identifier"] <- "id"
colnames(d)[colnames(d)=="comments"] <- "study"
colnames(d)[colnames(d)=="timestamp"] <- "date"
d$event.id = NULL
d$visible = NULL
d$algorithm.marked.outlier = NULL
d$manually.marked.outlier = NULL
d$individual.taxon.canonical.name = NULL
d$study.name = NULL
d$sensor.type = NULL
names(d)
summary(d)

#restore date as POSIXt object
summary(d$date)
d$date <- ymd_hms(d$date)
class(d$date)

# Pull year, month, day, hour from the DateTime variable
d$Year <- year(d$date)
d$Month <- month(d$date)
d$Day = day(d$date)
d$Hour <- hour(d$date)
head(d)

###Create burst by ID and Year
head(d)
d$id.yr <- c(paste(d$id,d$Year,sep="_")) 
d$id.yr <- as.factor(d$id.yr) 
summary(d$id.yr)

# order by ID and datetime
head(d)
attach(d)
d = d[order(id, date),]
detach(d)
head(d)

#############
# get UTM
#############
d1 = d

#create spatialpointsdataframe
coords <- d1[ , c("long", "lat")] 
crs <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +towgs84=0,0,0")
d2 = SpatialPointsDataFrame(coords = coords,
                            data = d1, 
                            proj4string = crs)
summary(d2)

#reproject to utm (note - units are in km)
projection <- CRS("+proj=utm +zone=34 +ellps=WGS84 +datum=WGS84 +units=km +no_defs ")
ev.projected <- spTransform(d2, projection)

#extract utm data columns
ev.projected.df <- data.frame(ev.projected)
names(ev.projected.df)
head(ev.projected.df)
ev.projected.df$utm.e = ev.projected.df$long.1
ev.projected.df$utm.n = ev.projected.df$lat.1 
ev.projected.df$long.1 = NULL
ev.projected.df$lat.1 = NULL
ev.projected.df$optional = NULL
head(ev.projected.df)
d1 = ev.projected.df
head(d1)
summary(d1)
write.csv(d1, "EV-all-1ptperhr-filtered-utm.csv")

#Limit to 1 point per day per ID (note - this will simply save the first point per id in every day)
d2 = d1[!duplicated(d1[,c('id', 'Year', 'Month', 'Day')]),]
write.csv(d2, "EV-all-1ptperday-filtered-utm.csv")

################################################################################
# Visualizing dataset
#########################################################################
#load packages
pcks <- list('plyr', 'lubridate', 'ggmap', 'maptools', 'rgdal', 'maps', 'mapdata', 'move', 'mapproj')
sapply(pcks, require, character = TRUE)
##Clear workspace
rm(list = ls())
#load the data
d = read.csv("EV-all-1ptperhr-filtered-utm.csv")
head(d)
d$date <- ymd_hms(d$date)
class(d$date)

#Personal Google API Key - careful not to publish or share
api <- "Input here!" 
register_google(key = api)
ggmap_credentials()

#Mapping
map = get_map("Chad", zoom = 3)
ggmap(map)
map1 = ggmap(map) + geom_point(data = d, aes(x = long, y = lat, color = id))
map1
ggplot(d, aes(x=long, y=lat, col=id)) + geom_path(alpha = 0.5) + geom_point(alpha = 0.5) + coord_fixed()
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

#######################################################
#Convert to ltraj object
#######################################################
#load the data
#d = read.csv("EV-all-1ptperhr-filtered-utm.csv")
#OR
d = read.csv("EV-all-1ptperday-filtered-utm.csv")
d$date <- ymd_hms(d$date)

#using 1 point per hour dataset
summary(d)
d3 = as.ltraj(xy = d[, c("utm.e", "utm.n")], date = d$date, id = d$id)
d3

#plot ltraj
plot.ltraj(d3)

#save ltraj as dataframe
# R2n is the squared distance between the first relocation 
# of the trajectory and the current relocation in map units
nsds <- do.call(rbind, d3)
summary(nsds) 

##Check the order of IDs before performing this step
d$NSD <- nsds$R2n
d$ND <- sqrt(d$NSD)
head(d)

#save file with NSD
write.csv(d, "EV-all-1ptperday-filtered-utm-NSD.csv")

####################################################
##Check the NSDs visually
####################################################
par(mfrow=c(1,1)) 
library(ggplot2)
library(scales)

plot = ggplot(d, aes(date, ND), color = id) + geom_line() + facet_wrap(~ id, ncol = 3)
plot = plot + labs(x = "Date", y = "Net Displacement") + theme_bw() 
plot

d$Date <- as.Date(d$date)
plot1<-qplot(Date, ND, data = d, geom = "path", colour = id)
plot1
#
plot2<-qplot(Date, lat, data = d, geom = "path", colour = id)
plot2
#
Plot2 <- plot2 +  geom_path(size = 1) + 
theme(axis.title.y = element_text(size = 12, vjust = 0.3, face = "bold")) + 
theme(axis.title.x = element_text(size = 12, vjust = 0.5, face = "bold")) + 
scale_x_date(labels = date_format("%b")) +
theme(panel.border = element_rect(colour = "black", fill = "NA")) +
theme(panel.background = element_rect(fill = "white")) + 
theme(panel.grid.minor.x = element_line(colour="white")) + 
theme(panel.grid.minor.y = element_line(colour="white")) + xlab("Date") + ylab("Net Displacement") +  
theme(axis.text.y = element_text(face = "bold", size = rel(1.3))) + 
theme(axis.text.x = element_text(face = "bold", size = rel(1.3)))
Plot2
#
Plot2 + facet_wrap(~id, nrow = 2)

####################
#plot net displacement by study
####################
summary(d)
summary(d$study)
plot = ggplot(d, aes(date, ND), color = id) + geom_line() + facet_wrap(~ id, ncol = 6)
plot = plot + labs(x = "Date", y = "Net Displacement (km)") + theme_bw() 
plot #export 1200 x 1200
mideast = subset(d, study == "buechley-mideast")
plot = ggplot(mideast, aes(date, ND), color = id) + geom_line() + facet_wrap(~ id, ncol = 3)
plot = plot + labs(x = "Date", y = "Net Displacement (km)") + theme_bw() 
plot
douro = subset(d, study == "douro-spain")
plot = ggplot(douro, aes(date, ND), color = id) + geom_line() + facet_wrap(~ id, ncol = 3)
plot = plot + labs(x = "Date", y = "Net Displacement") + theme_bw() 
plot
israel = subset(d, study == "efrat-israel")
plot = ggplot(israel, aes(date, ND), color = id) + geom_line() + facet_wrap(~ id, ncol = 3)
plot = plot + labs(x = "Date", y = "Net Displacement") + theme_bw() 
plot
grefa = subset(d, study == "grefa-spain")
plot = ggplot(grefa, aes(date, ND), color = id) + geom_line() + facet_wrap(~ id, ncol = 3)
plot = plot + labs(x = "Date", y = "Net Displacement") + theme_bw() 
plot
#2hp looks like it was deployed twice
twohp = subset(d, id == "2HP")
plot = ggplot(twohp, aes(date, ND), color = id) + geom_line() + facet_wrap(~ id, ncol = 3)
plot = plot + labs(x = "Date", y = "Net Displacement") + theme_bw() 
plot
#
summary(d$study)
russia = subset(d, study == "karyakin-russia")
plot = ggplot(russia, aes(date, ND), color = id) + geom_line() + facet_wrap(~ id, ncol = 3)
plot = plot + labs(x = "Date", y = "Net Displacement") + theme_bw() 
plot
france = subset(d, study == "kobierzycki-france")
plot = ggplot(france, aes(date, ND), color = id) + geom_line() + facet_wrap(~ id, ncol = 3)
plot = plot + labs(x = "Date", y = "Net Displacement") + theme_bw() 
plot
rupis = subset(d, study == "life.rupis-spain")
plot = ggplot(rupis, aes(date, ND), color = id) + geom_line() + facet_wrap(~ id, ncol = 3)
plot = plot + labs(x = "Date", y = "Net Displacement") + theme_bw() 
plot
migra = subset(d, study == "migra-spain")
plot = ggplot(migra, aes(date, ND), color = id) + geom_line() + facet_wrap(~ id, ncol = 3)
plot = plot + labs(x = "Date", y = "Net Displacement") + theme_bw() 
plot
balkans = subset(d, study == "oppel-balkans")
plot = ggplot(balkans, aes(date, ND), color = id) + geom_line() + facet_wrap(~ id, ncol = 3)
plot = plot + labs(x = "Date", y = "Net Displacement") + theme_bw() 
plot
tarra.natura = subset(d, study == "terra.natura-spain")
plot = ggplot(terra.natura, aes(date, ND), color = id) + geom_line() + facet_wrap(~ id, ncol = 3)
plot = plot + labs(x = "Date", y = "Net Displacement") + theme_bw() 
plot


