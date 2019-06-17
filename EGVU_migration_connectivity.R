##########################################################################
# QUANTIFICATION OF MIGRATORY CONNECTIVITY FOR EGYPTIAN VULTURES
# original script written by Steffen Oppel in JUNE 2019
##########################################################################

### ABANDONED 17 JUNE 2019
### THIS IS ONLY FOR DISCRETE POPULATIONS
### EGVU DO NOT WINTER IN 'DISCRETE' POPULATIONS, hence difficult to specify a priori
### cannot use this to quantify connectivity within e.g. the Balkan subpopulation


# Load necessary library
library(lubridate)
library(tidyverse)
library(data.table)
library(sp)
library("rnaturalearth")
library("rnaturalearthdata")
library(sf)
library(devtools)
library(ggplot2)
library(maptools)
devtools::install_github("SMBC-NZP/MigConnectivity", build_vignettes = TRUE)
#library(MigConnectivity)
source("C:\\STEFFEN\\MANUSCRIPTS\\Submitted\\FrontiersMigrationPaper\\estConnectivity.R")
source("C:\\STEFFEN\\MANUSCRIPTS\\Submitted\\FrontiersMigrationPaper\\calcConnectivity.R")
source("C:\\STEFFEN\\MANUSCRIPTS\\Submitted\\FrontiersMigrationPaper\\utilityFunctions.R")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# LOAD PREVIOUSLY SAVED MIGRATION DATA (sent by Pascual Lopez-Lopez on 15 March 2019)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Set working directory
setwd("C:\\STEFFEN\\MANUSCRIPTS\\Submitted\\FrontiersMigrationPaper\\EGVUmigration")

# read in clean csv
migs<- fread("migration_parameters_completed_migrations_clean.csv")
migs<- migs %>% mutate(DateTime=mdy_hm(migs$start)) %>%
	dplyr::filter(season=="fall") %>%
	dplyr::filter(startlat>30) 			## remove Iliaz 2015 fall migration from Chad to Sudan

head(migs)
unique(migs$subpopulation)


####~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~####
####~~~~~ 			PREPARE DATA TO ESTIMATE MIGRATORY CONNECTIVITY										     ~~~~~~####
####~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~####
###############################################################################
#' Estimate MC from abundance and/or transition probability estimates OR
#' geolocator and/or GPS data OR intrinsic markers.
#'
#' Resampling of uncertainty for MC from RMark psi matrix estimates and/or JAGS
#' relative abundance MCMC samples OR SpatialPoints geolocators and/or GPS
#' data OR intrinsic markers such as isotopes.
#'
#' @param originDist Distances between the B origin sites.  Symmetric B by B
#'  matrix.

origins<- migs %>% group_by(season,subpopulation) %>%
  summarise(long=mean(startlong),lat=mean(startlat))

# Convert capture locations into SpatialPoints #
CapLocs<-sp::SpatialPoints(as.matrix(origins[,3:4]),proj4string=CRS("+proj=longlat +datum=WGS84"))
originDist<-spDists(CapLocs)




#' @param targetDist Distances between the W target sites.  Symmetric W by W
#'  matrix.  Optional for intrinsic data.

arrivals<- migs %>% group_by(season,subpopulation) %>%
  summarise(long=mean(endlong),lat=mean(endlat))
# Convert target locations into SpatialPoints #
WinLocs<-sp::SpatialPoints(as.matrix(arrivals[,3:4]),proj4string=CRS("+proj=longlat +datum=WGS84"))
targetDist<-spDists(WinLocs)



#' @param originRelAbund Relative abundance estimates at B origin sites. Either
#'  a numeric vector of length B that sums to 1 or an mcmc object with
#'  \code{nSamples} rows  and columns including 'relN[1]' through 'relN[B]'.
#'  Currently, an mcmc object doesn't work with geolocator, GPS, or intrinsic
#'  data.

originRelAbund<-c(70,1000,1300)  ## rough estimate of breeding pairs for Balkans, Middle East, and West (Iberian) populations from BL Datazone
originRelAbund<-originRelAbund/sum(originRelAbund)


#' @param sampleSize Total sample size of animals that psi will be estimated from.
#'    Should be the number of animals released in one of the origin sites and
#'    observed in one of the target sites.  Optional, but recommended, unless
#'    you are estimating MC from GPS, geolocator, or intrinsic data (in which
#'    case the function can calculate it for you).

sampleSize<-dim(migs)[1]


#' @param originSites If \code{psi} is a MARK object, this must be a numeric
#'  vector indicating which sites are origin.  If using GPS, geolocator, or
#'  intrinsic data, this can be the geographic definition of sites in the
#'  release season as "SpatialPolygonsDataFrame"

basemap <- ne_countries(scale = "medium", returnclass = "sf")
sort(unique(basemap$sovereignt))
Balkan <- basemap %>% dplyr::filter(sovereignt %in% c("Greece","Bulgaria","Albania","Macedonia","Romania")) %>% st_union
MiddleEast <- basemap %>% dplyr::filter(sovereignt %in% c("Russia","Turkey","Armenia","Azerbaijan","Iran","Syria","Lebanon","Israel","Iraq")) %>% st_union
West <- basemap %>% dplyr::filter(sovereignt %in% c("Spain","Portugal","Andorra","France")) %>% st_union
Balkan <- as(Balkan,"Spatial")
MiddleEast <- as(MiddleEast,"Spatial")
West <- as(West,"Spatial")
Balkan@polygons[[1]]@ID<-"Balkan"
MiddleEast@polygons[[1]]@ID<-"MiddleEast"
West@polygons[[1]]@ID<-"West"
list_of_SPols<-list(Balkan,MiddleEast,West)
IDs <- sapply(list_of_SPols, function(x)
  slot(slot(x, "polygons")[[1]], "ID"))

#Checking
length(unique(IDs)) == length(list_of_SPols)

#Making SpatialPolygons from list of polygons
Spol <- SpatialPolygons(lapply(list_of_SPols,
                                function(x) slot(x, "polygons")[[1]]))

#Creating a dataframe with Spol IDs
Spol_df<- as.data.frame(sapply(slot(Spol, "polygons"), function(x) slot(x, "ID")))

#Making the IDs row names 
row.names(Spol_df) <- sapply(slot(Spol, "polygons"), function(x) slot(x, "ID"))

# Making the spatial polygon data frame
originSites <- SpatialPolygonsDataFrame(Spol, data =Spol_df)
proj4string(originSites)<-CRS("+proj=longlat +datum=WGS84")



#' @param targetSites If \code{psi} is a MARK object, this must be a numeric
#'  vector indicating which sites are target.  If using GPS, geolocator, or
#'  intrinsic data, this must be the geographic definition of sites in the
#'  non-release season.  Optional for intrinsic data; if left out, the function
#'  will use the \code{targetSites} defined in \code{targetIntrinsic}.


WestAfr <- basemap %>% dplyr::filter(sovereignt %in% c("Algeria","Morocco","Mauritania","Senegal","Mali","Gambia","Burkina Faso")) %>% st_union
EastAfr <- basemap %>% dplyr::filter(sovereignt %in% c("Kenya","Niger","Nigeria","Chad","Sudan","South Sudan","Ethiopia","Eritrea","Somalia","Cameroon","Central African Republic","Djibouti","Somaliland")) %>% st_union
Saudi <- basemap %>% dplyr::filter(sovereignt %in% c("Saudi Arabia","Yemen","Oman","Iraq")) %>% st_union
WestAfr <- as(WestAfr,"Spatial")
EastAfr <- as(EastAfr,"Spatial")
Saudi <- as(Saudi,"Spatial")
WestAfr@polygons[[1]]@ID<-"WestAfr"
EastAfr@polygons[[1]]@ID<-"EastAfr"
Saudi@polygons[[1]]@ID<-"Saudi"
list_of_SPols<-list(WestAfr,EastAfr,Saudi)
IDs <- sapply(list_of_SPols, function(x)
  slot(slot(x, "polygons")[[1]], "ID"))

#Checking
length(unique(IDs)) == length(list_of_SPols)

#Making SpatialPolygons from list of polygons
Spol <- SpatialPolygons(lapply(list_of_SPols,
                                function(x) slot(x, "polygons")[[1]]))

#Creating a dataframe with Spol IDs
Spol_df<- as.data.frame(sapply(slot(Spol, "polygons"), function(x) slot(x, "ID")))

#Making the IDs row names 
row.names(Spol_df) <- sapply(slot(Spol, "polygons"), function(x) slot(x, "ID"))

# Making the spatial polygon data frame
targetSites <- SpatialPolygonsDataFrame(Spol, data =Spol_df)
proj4string(targetSites)<-CRS("+proj=longlat +datum=WGS84")





#' @param originPoints A \code{SpatialPoints} object, with length number of
#'    animals tracked.  Each point indicates the release location of an animal.

originPoints<-sp::SpatialPoints(as.matrix(migs[migs$season=="fall",27:28]),proj4string=CRS("+proj=longlat +datum=WGS84"))
plot(originPoints)
plot(basemap, add=T)
plot(originPoints, add=T, col='red')

#' @param targetPoints For GL or GPS data, a \code{SpatialPoints} object, with
#'    length number ofanimals tracked.  Each point indicates the point estimate
#'    location in the non-release season.

targetPoints<-sp::SpatialPoints(as.matrix(migs[migs$season=="fall",30:29]),proj4string=CRS("+proj=longlat +datum=WGS84"))
plot(targetPoints)
plot(basemap, add=T)
plot(targetPoints, add=T, col='red')

#' @param originAssignment Assignment of \code{originPoints} to release season
#'    sites. Integer vector with length number of animals tracked. Optional,
#'    but if using GL or GPS data, either \code{originAssignment} or
#'    \code{originSites} and \code{originPoints} should be defined.

originReg<-over(originPoints,originSites)
names(originReg)[1]<-"Region"
originPoints[is.na(originReg$Region),]

originAssignment<-migs %>% dplyr::filter(season=="fall") %>% group_by(subpopulation) %>% summarise(n=length(ID))
originAssignment<-originAssignment$n


#' @param targetAssignment Optional. Point estimate assignment of
#'    \code{targetPoints} to non-release season sites. Integer vector with
#'    length number of animals tracked.

targetReg<-over(targetPoints,targetSites)
names(targetReg)[1]<-"Region"
targetAssignment<-targetReg %>% group_by(Region) %>% summarise(n=length(Region))
targetAssignment<-targetAssignment$n[!is.na(targetAssignment$Region)]


#' @param originNames Optional. Vector of names for the release season sites.

originNames<-c("Balkan","MiddleEast","West")


#' @param targetNames Optional. Vector of names for the non-release season
#'    sites.

targetNames<-c("WestAfrica","Arabia","EastAfrica")



#' @param nSamples Number of times to resample \code{psi} and/or
#'    \code{originRelAbund} OR number of times to resample \code{targetPoints}
#'    for intrinsic data OR number of bootstrap runs for GL or GPS data. In
#'    the last two cases, animals are sampled with replacement for each. For all,
#'    the purpose is to estimate sampling uncertainty.

nSamples<-1000


#' @param nSim Tuning parameter for GL or intrinsic data. Affects only the
#'    speed; 1000 seems to work well with our GL data and 10 for our intrinsic
#'    data, but your results may vary.  Should be integer > 0.

nSim<-10

#' @param isGL Indicates whether or which animals were tracked with geolocators.
#'    Should be either single TRUE or FALSE value, or vector with length of
#'    number of animals tracked, with TRUE for animals in
#'    \code{targetPoints} with geolocators and FALSE for animals with GPS.

isGL<-FALSE

#' @param verbose 0 (default) to 3. 0 prints no output during run. 1 prints
#'  a line every 100 samples or bootstraps and a summary every 10.  2 prints a
#'  line and summary every sample or bootstrap. 3 also prints the number of
#'  draws (for tuning nSim for GL/intrinsic data only).
#' @param calcCorr In addition to MC, should function also estimate Mantel
#'    correlation between release and non-release locations (GPS or GL data
#'    only)?  Default is FALSE.
#' @param alpha Level for confidence/credible intervals provided.
#' @param approxSigTest Should function compute approximate one-sided
#'    significance tests (p-values) for MC from the bootstrap?  Default is
#'    FALSE.
#' @param sigConst Value to compare MC to in significance test.
#'    Default is 0.
#' @param resampleProjection Projection when sampling from geolocator
#'    bias/error. This projection needs units = m. Default is Equidistant
#'    Conic. The default setting preserves distances around latitude = 0 and
#'    longitude = 0. Other projections may work well, depending on the location
#'    of \code{targetSites}.  Ignored unless data are geolocator or GPS.
#' @param maxTries Maximum number of times to run a single GL/intrinsic
#'    bootstrap before exiting with an error.  Default is 300.  Set to NULL to
#'    never stop.  Thisparameter was added to prevent GL setups where some
#'    sample points never land on target sites from running indefinitely.
#' @param targetIntrinsic For intrinsic tracking data, the results of
#'    \code{isoAssign} or a similar function, of class \code{intrinsicAssign}.
#' @param isIntrinsic Logical indicating whether the animals are tracked via
#'    intrinsic marker (e.g. isotopes) or not.  Currently estMC will only estimate
#'    connectivity for all intrinsically marked animals or all extrinsic (e.g.,
#'    bands, GL, or GPS), so isIntrinsic should be a single TRUE or FALSE.






####~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~####
####~~~~~ 			ESTIMATE MIGRATORY CONNECTIVITY	FOR WHOLE DATASET										     ~~~~~~####
####~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~####

M<-estMC(isGL=isGL, # Logical vector: light-level geolocator(T)/GPS(F)
	targetDist = targetDist, # Target location distance matrix
	originDist = originDist, # Origin location distance matrix
	targetSites = targetSites, # Non-breeding / target sites
	originSites = originSites, # Breeding / origin sites
	originPoints = originPoints, # Capture Locations
	targetPoints = targetPoints, # Target locations from devices
	originRelAbund = originRelAbund, # Origin relative abundances
	resampleProjection = proj4string(targetPoints),
	verbose = 0, # output options - see help ??estMC
	nSamples = 1000) # This is set low for example

M$meanMC
M$seMC







####~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~####
####~~~~~ 			ESTIMATE MIGRATORY CONNECTIVITY	FOR WEST POPULATION									     ~~~~~~####
####~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~####
#Creating a dataframe with Spol IDs
Spol_df<- as.data.frame(sapply(slot(West, "polygons"), function(x) slot(x, "ID")))
row.names(Spol_df) <- sapply(slot(West, "polygons"), function(x) slot(x, "ID"))
originSites <- SpatialPolygonsDataFrame(West, data=Spol_df)
proj4string(originSites)<-CRS("+proj=longlat +datum=WGS84")

WestMC<-estMC(isGL=isGL, # Logical vector: light-level geolocator(T)/GPS(F)
         targetDist = targetDist, # Target location distance matrix
         originDist = matrix(0, nrow=1, ncol=1), # Origin location distance matrix
         targetSites = targetSites, # Non-breeding / target sites
         originSites = originSites, # Breeding / origin sites
         originPoints = sp::SpatialPoints(as.matrix(migs[migs$season=="fall" & migs$subpopulation=="West",27:28]),proj4string=CRS("+proj=longlat +datum=WGS84")), # Capture Locations
         targetPoints = sp::SpatialPoints(as.matrix(migs[migs$season=="fall" & migs$subpopulation=="West",30:29]),proj4string=CRS("+proj=longlat +datum=WGS84")), # Target locations from devices
         originRelAbund = 1, # Origin relative abundances
         resampleProjection = proj4string(targetPoints),
         verbose = 0, # output options - see help ??estMC
         nSamples = 1000) # This is set low for example

WestMC$meanMC
WestMC$seMC






###### CHECK WHETHER THIS WORKS WITH JUST LOCATIONS, but it throws obscure error message

Mmin<-estMC(isGL=rep(FALSE,sum(originAssignment)), # Logical vector: light-level geolocator(T)/GPS(F)
	targetDist = targetDist, # Target location distance matrix
	originDist = originDist, # Origin location distance matrix
	originAssignment=originAssignment,
	targetAssignment=targetAssignment,
	originPoints = originPoints, # Capture Locations
	targetPoints = targetPoints, # Target locations from devices
	originRelAbund = originRelAbund, # Origin relative abundances
	resampleProjection = proj4string(targetPoints),
	verbose = 0, # output options - see help ??estMC
	nSamples = 1000) # This is set low for example

M$meanMC
M$seMC







