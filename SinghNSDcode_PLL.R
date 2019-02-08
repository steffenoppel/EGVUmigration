######################################################################
#Moose migrations across different start dates
######################################################################

###Load relevant libraries###

library(adehabitatLT)
library(adehabitatHR)
library(plotrix)
library(lattice)
library(gdata)
library(nlme)

###Set working directory
setwd("E://Documentos//PUBLICACIONES//Articulo migracion alimoches FRONTIERS ECOL EVOL//Articulo MSD PLOS ONE")


###Read in data file for one of the study areas. Note, this related to the first element of the study where we determine
###how the extent of movement influences the performance of NSD model. Therefore, all data from all study areas was used
###in this first part, but the NSD model was run on each study area separately to reduce sample size and improve
###model convergence success. 

### First problem: the AA_BD.csv dataset is not available 
## authors kindly ask you to contact them to access the original dataset
## therefore I cannot see data structure
DayPos5<-read.csv("AA_BD.csv")

## Nonetheless I try to use Evan's cleaned dataset with 1 point per day
DayPos5<-read.csv("E://Documentos//PUBLICACIONES//Articulo migracion alimoches FRONTIERS ECOL EVOL//Datos Evan//EV-all-1ptperday-filtered-utm-NSD-season.csv")

###Set date
DayPos5$date<-as.POSIXct(DayPos5$date, format = "%Y-%m-%d %H:%M:%S","GMT")
summary(DayPos5)

############################

litr <- as.ltraj(xy=DayPos5[,5:6], date=DayPos5$date, id = DayPos5$id.yr.season, typeII=TRUE)

nsds <- do.call(rbind, litr)

##Check the order of IDs before performing this step
DayPos5$NSDm <- nsds$R2n/1000000

summary(DayPos5)

##Now estimate MSD using 30 time steps
library(plyr)
library(zoo)

DayPos6 <- ddply(DayPos5, .(id.yr.season), function(DayPos5) {
  z <- zoo(DayPos5$NSDm, DayPos5$X.1)
  DayPos5$rollmean <- rollapply(z, width=30, align = "right", partial=TRUE, FUN=mean, na.rm=TRUE)
  DayPos5
})

DayPos5$MSD <- coredata(DayPos6$rollmean)

head(DayPos5)
#### OK!

##Check the NSDs visually

library(ggplot2)
library(scales)

DayPos5$Date <- as.Date(DayPos5$date)

plot2<-qplot(Date, sqrt(NSDm), data = DayPos5, geom = "path", colour = id)
plot2

Plot2 <- plot2 +  geom_path(size = 1) + 
theme(axis.title.y = element_text(size = 12, vjust = 0.3, face = "bold")) + 
theme(axis.title.x = element_text(size = 12, vjust = 0.5, face = "bold")) + 
scale_x_date(labels = date_format("%b")) +
theme(panel.border = element_rect(colour = "black", fill = "NA")) +
theme(panel.background = element_rect(fill = "white")) + 
theme(panel.grid.minor.x = element_line(colour="white")) + 
theme(panel.grid.minor.y = element_line(colour="white")) + xlab("Date") + ylab("Distance (km)") +  
theme(axis.text.y = element_text(face = "bold", size = rel(1.3))) + 
theme(axis.text.x = element_text(face = "bold", size = rel(1.3)))
Plot2

Plot2 + facet_wrap(~yrMooseID, scales = "free")
Plot2 + facet_wrap(~id, ncol = 5, scales = "free")

Plot2 + facet_grid(. ~ id, scales = "free")
  

###Now we  fit each of the models described in Bunnefeld et al. 2011 and Singh et al. 2012

###First a null model
null.HRmod <- nlme(MSD ~ A,
                   data = DayPos5,
                   fixed = A ~ 1,
                   random = A ~ 1,
                   groups = ~ id.yr.season,
                   start = c(A = mean(DayPos5[,'MSD'])))

### change starting values if no conv (=mean), such as a specific value, or median etc.


###Sedentary HR model
######################################
asym.HRmod <- nlme(MSD ~ Asym*(1 - exp(lrc*X.1)),
                   data     = DayPos5,
                   fixed     = Asym + lrc ~ 1,
                   random     = Asym ~ 1,
                   groups     = ~ id.yr.season,
                   start     = c(Asym = summary(null.HRmod)$tTable[1], 
                                 lrc = -0.002))

##Dispersal model with distance (Asym) and timing (xmid) varying between individuals. 
##Providing a start improve model convergence, which can also be obtained by using parameters from previously estimated models
########################################################
ranef2.Dispmod <- nlme(MSD ~ Asym/(1 + exp((xmid-nDaysYr)/scal)),
              data = DayPos5,
              fixed = Asym + xmid + scal ~ 1,
              random = Asym + xmid ~ 1,
              groups = ~ yrMooseID,
                      na.action = na.exclude,
                      start = c(Asym = summary(asym.HRmod)$tTable[1], xmid = 30, scal = 2))
summary(ranef2.Dispmod)

ranef2.Dispmod <- nlme(MSD ~ Asym/(1 + exp((xmid-X.1)/scal)),
                       data = DayPos5,
                       fixed = Asym + xmid + scal ~ 1,
                       random = Asym + xmid ~ 1,
                       groups = ~ id.yr.season,
                       na.action = na.exclude,
                       start = c(Asym = summary(asym.HRmod)$tTable[1], xmid = 30, scal = 2))
summary(ranef2.Dispmod)
## PASCUAL: the model does not converge
## error: "maximum number of iterations (maxIter = 50) reached without convergence"

##Dispersal model with Distance, timing and duration (scal) varying between individuals. 
########################################################
full.Dispmod <- nlme(MSD ~ Asym/(1 + exp((xmid-nDaysYr)/scal)),
         data = DayPos5,
        fixed = Asym + xmid + scal ~ 1,
        random = Asym + xmid + scal ~ 1,
        groups = ~ yrMooseID,
               start = c(Asym = summary(ranef2.Dispmod)$tTable[1],
	xmid = summary(ranef2.Dispmod)$tTable[2], scal = summary(ranef2.Dispmod)$tTable[3]))

summary(full.Dispmod)

full.Dispmod <- nlme(MSD ~ Asym/(1 + exp((xmid-X.1)/scal)),
                     data = DayPos5,
                     fixed = Asym + xmid + scal ~ 1,
                     random = Asym + xmid + scal ~ 1,
                     groups = ~ id.yr.season,
                     start = c(Asym = summary(ranef2.Dispmod)$tTable[1],
                               xmid = summary(ranef2.Dispmod)$tTable[2], scal = summary(ranef2.Dispmod)$tTable[3]))
## it does not converge
summary(full.Dispmod)

##Simple Migration model with only distance varying between individuals
########################################################
asym.Migrmod <- nlme(MSD ~ asym/(1+exp((xmidA-X.1)/scale1)) + (-asym /(1 + exp((xmidB-X.1)/scale2))),
                     data=DayPos5,
                     fixed = asym+ xmidA + xmidB + scale1 + scale2 ~ 1,
                     random = asym ~ 1,
                     groups =~ id,
                     start = c(asym = 5515,xmidA = 90, xmidB = 242, scale1 = 4, scale2 = 4))
                     
summary(asym.Migrmod)

### PASCUAL: I stop here. 
## error: "step halving factor reduced below minimum in PNLS step"
## as far as I understand, the model does not converge probably due to minScale parameter in nlmeControl() 
## there are some 

##More complex migration model with distance, timing of Migration 1 (xmidA) and timing of migration 2 (xmidB)
########################################################
ranef2.Migrmod <- nlme(MSD ~ asym/(1+exp((xmidA-nDaysYr)/scale1)) + (-asym /(1 + exp((xmidB-nDaysYr)/scale2))),
           data=DayPos5,
           fixed = asym + xmidA + xmidB + scale1 + scale2 ~ 1,
            random = asym + xmidA + xmidB ~ 1|yrMooseID,
            #groups =~ yrMooseID, control=contr,
           start = c(asym = 2200, xmidA = 100,
               xmidB = 295, scale1 = 10, scale2 = 15))
summary(ranef2.Migrmod)

##Complete migration model with all parameters varying between individuals. Normally this will struggle to converge in a dataset
##that contain a mix of migrants and non-migrants, and is more important in later stages when estimating timing of movements for 
##migratory individuals only using the NSD rather than the MSD 
ranef4.Migrmod <- nlme(MSD ~ asym/(1+exp((xmidA-nDaysYr)/scale1)) + (-asym /(1 + exp((xmidB-nDaysYr)/scale2))),
           data=DayPos5,
           fixed = list(asym + xmidA + xmidB + scale1 + scale2 ~ 1),
            random = asym + xmidA + xmidB + scale1 + scale2 ~ 1|yrMooseID,
            #groups =~ yrMooseID, control=contr,
           start = c(asym = summary(ranef2.Migrmod)$tTable[1], xmidA = summary(ranef2.Migrmod)$tTable[2],
               xmidB = summary(ranef2.Migrmod)$tTable[3], scale1 = 5, 
		scale2 = 10))
summary(ranef4.Migrmod)

##Mixed Migratory Model with distance and timing of Migration 1, and distance and timing of Migration 2, varying between individuals. 
mix.Migrmod <- nlme(MSD ~ asymA/(1+exp((xmidA-nDaysYr)/scale1)) + (-asymB /(1 + exp((xmidB-nDaysYr)/scale2))),
           data=DayPos5,
           fixed = asymA + asymB + xmidA + xmidB + scale1 + scale2 ~ 1,
            random = asymA + asymB +xmidA + xmidB~ 1,
            groups =~ yrMooseID,
           start = c(asymA = 10000, asymB = 8000, xmidA = 110, xmidB = 300, scale1 = 8, scale2 = 12))
summary(mix.Migrmod)

##Nomadic model
#####################################################################################################
all.linear <- nlme(MSD ~ 4*D*nDaysYr,
              data = DayPos5,
              fixed = D ~ 1,
              random = D ~ 1,
              groups = ~ yrMooseID,
              start = c(D = 3))
########################################################
########################################################
### GOF indiv level
### change this list below to include the model that fitted (with highest number of random effects) for each movement type

HRmod <- asym.HRmod
NullMod <- null.HRmod
Dispmod <- full.Dispmod  ##here we managed to a fit a disp mod with distance, timing and duration varying between all individuals
MigrMod <- ranef2.Migrmod ##here we could only fit a model with distance and timing varying between individuals (in these initial classification stages)
MigrModC <-mix.Migrmod
nomadMod <- all.linear


###Now perform the calculations for the goodness of fit (i.e. concordance criterion). 
### CC1-IDlev
########################################################

# check first that yrMooseID levels are the same

all.equal(levels(DayPos5$yrMooseID),rownames(coef(HRmod)))

#[1] TRUE    # same for Dispmod etc.


###
#HR
###
CC1ID.HRmod <- numeric(length(levels(DayPos5$yrMooseID)))

for(k in 1:length(levels(DayPos5$yrMooseID))) {

CC1ID.HRmod[k] <- 1 - (sum((DayPos5[which(DayPos5$yrMooseID == levels(DayPos5$yrMooseID)[k]),'MSD'] -
         fitted(HRmod)[which(DayPos5$yrMooseID ==
levels(DayPos5$yrMooseID)[k])])^2)) / (
         (sum((DayPos5[which(DayPos5$yrMooseID == levels(DayPos5$yrMooseID)[k]),'MSD'] -
         mean(DayPos5[which(DayPos5$yrMooseID ==
levels(DayPos5$yrMooseID)[k]),'MSD']))^2)) +
         (sum((fitted(HRmod)[which(DayPos5$yrMooseID == levels(DayPos5$yrMooseID)[k])] -
         mean(fitted(HRmod)[which(DayPos5$yrMooseID ==
levels(DayPos5$yrMooseID)[k])]))^2)) +
         (length(DayPos5[which(DayPos5$yrMooseID ==
levels(DayPos5$yrMooseID)[k]),'MSD'])*((mean(DayPos5[which(DayPos5$yrMooseID==levels(DayPos5$yrMooseID)[k]),'MSD'])
-
         mean(fitted(HRmod)[which(DayPos5$yrMooseID ==
levels(DayPos5$yrMooseID)[k])]))^2))
         )
         }
###
#NULL
##
CC1ID.NullMod <- numeric(length(levels(DayPos5$yrMooseID)))

for(k in 1:length(levels(DayPos5$yrMooseID))) {

CC1ID.NullMod[k] <- 1 - (sum((DayPos5[which(DayPos5$yrMooseID == levels(DayPos5$yrMooseID)[k]),'MSD'] -
         fitted(NullMod)[which(DayPos5$yrMooseID ==
levels(DayPos5$yrMooseID)[k])])^2)) / (
         (sum((DayPos5[which(DayPos5$yrMooseID == levels(DayPos5$yrMooseID)[k]),'MSD'] -
         mean(DayPos5[which(DayPos5$yrMooseID ==
levels(DayPos5$yrMooseID)[k]),'MSD']))^2)) +
         (sum((fitted(NullMod)[which(DayPos5$yrMooseID == levels(DayPos5$yrMooseID)[k])] -
         mean(fitted(NullMod)[which(DayPos5$yrMooseID ==
levels(DayPos5$yrMooseID)[k])]))^2)) +
         (length(DayPos5[which(DayPos5$yrMooseID ==
levels(DayPos5$yrMooseID)[k]),'MSD'])*((mean(DayPos5[which(DayPos5$yrMooseID==levels(DayPos5$yrMooseID)[k]),'MSD'])
-
         mean(fitted(NullMod)[which(DayPos5$yrMooseID ==
levels(DayPos5$yrMooseID)[k])]))^2))
         )
         }

###
#Dispersal
##
CC1ID.Dispmod <- numeric(length(levels(DayPos5$yrMooseID)))

for(k in 1:length(levels(DayPos5$yrMooseID))) {

CC1ID.Dispmod[k] <- 1 - (sum((DayPos5[which(DayPos5$yrMooseID == levels(DayPos5$yrMooseID)[k]),'MSD'] -
         fitted(Dispmod)[which(DayPos5$yrMooseID ==
levels(DayPos5$yrMooseID)[k])])^2)) / (
         (sum((DayPos5[which(DayPos5$yrMooseID == levels(DayPos5$yrMooseID)[k]),'MSD'] -
         mean(DayPos5[which(DayPos5$yrMooseID ==
levels(DayPos5$yrMooseID)[k]),'MSD']))^2)) +
         (sum((fitted(Dispmod)[which(DayPos5$yrMooseID == levels(DayPos5$yrMooseID)[k])] -
         mean(fitted(Dispmod)[which(DayPos5$yrMooseID ==
levels(DayPos5$yrMooseID)[k])]))^2)) +
         (length(DayPos5[which(DayPos5$yrMooseID ==
levels(DayPos5$yrMooseID)[k]),'MSD'])*((mean(DayPos5[which(DayPos5$yrMooseID==levels(DayPos5$yrMooseID)[k]),'MSD'])
-
         mean(fitted(Dispmod)[which(DayPos5$yrMooseID ==
levels(DayPos5$yrMooseID)[k])]))^2))
         )
         }

###
#Migration
###
CC1ID.MigrMod <- numeric(length(levels(DayPos5$yrMooseID)))

for(k in 1:length(levels(DayPos5$yrMooseID))) {

CC1ID.MigrMod[k] <- 1 - (sum((DayPos5[which(DayPos5$yrMooseID == levels(DayPos5$yrMooseID)[k]),'MSD'] -
         fitted(MigrMod)[which(DayPos5$yrMooseID ==
levels(DayPos5$yrMooseID)[k])])^2)) / (
         (sum((DayPos5[which(DayPos5$yrMooseID == levels(DayPos5$yrMooseID)[k]),'MSD'] -
         mean(DayPos5[which(DayPos5$yrMooseID ==
levels(DayPos5$yrMooseID)[k]),'MSD']))^2)) +
         (sum((fitted(MigrMod)[which(DayPos5$yrMooseID == levels(DayPos5$yrMooseID)[k])] -
         mean(fitted(MigrMod)[which(DayPos5$yrMooseID ==
levels(DayPos5$yrMooseID)[k])]))^2)) +
         (length(DayPos5[which(DayPos5$yrMooseID ==
levels(DayPos5$yrMooseID)[k]),'MSD'])*((mean(DayPos5[which(DayPos5$yrMooseID==levels(DayPos5$yrMooseID)[k]),'MSD'])
-
         mean(fitted(MigrMod)[which(DayPos5$yrMooseID ==
levels(DayPos5$yrMooseID)[k])]))^2))
         )
         }

###
#MigrationC
###
CC1ID.MigrModC <- numeric(length(levels(DayPos5$yrMooseID)))

for(k in 1:length(levels(DayPos5$yrMooseID))) {

CC1ID.MigrModC[k] <- 1 - (sum((DayPos5[which(DayPos5$yrMooseID == levels(DayPos5$yrMooseID)[k]),'MSD'] -
         fitted(MigrModC)[which(DayPos5$yrMooseID ==
levels(DayPos5$yrMooseID)[k])])^2)) / (
         (sum((DayPos5[which(DayPos5$yrMooseID == levels(DayPos5$yrMooseID)[k]),'MSD'] -
         mean(DayPos5[which(DayPos5$yrMooseID ==
levels(DayPos5$yrMooseID)[k]),'MSD']))^2)) +
         (sum((fitted(MigrModC)[which(DayPos5$yrMooseID == levels(DayPos5$yrMooseID)[k])] -
         mean(fitted(MigrModC)[which(DayPos5$yrMooseID ==
levels(DayPos5$yrMooseID)[k])]))^2)) +
         (length(DayPos5[which(DayPos5$yrMooseID ==
levels(DayPos5$yrMooseID)[k]),'MSD'])*((mean(DayPos5[which(DayPos5$yrMooseID==levels(DayPos5$yrMooseID)[k]),'MSD'])
-
         mean(fitted(MigrModC)[which(DayPos5$yrMooseID ==
levels(DayPos5$yrMooseID)[k])]))^2))
         )
         }

###
#Nomad
###
CC1ID.nomadMod <- numeric(length(levels(DayPos5$yrMooseID)))

for(k in 1:length(levels(DayPos5$yrMooseID))) {

CC1ID.nomadMod[k] <- 1 - (sum((DayPos5[which(DayPos5$yrMooseID == levels(DayPos5$yrMooseID)[k]),'MSD'] -
         fitted(nomadMod)[which(DayPos5$yrMooseID ==
levels(DayPos5$yrMooseID)[k])])^2)) / (
         (sum((DayPos5[which(DayPos5$yrMooseID == levels(DayPos5$yrMooseID)[k]),'MSD'] -
         mean(DayPos5[which(DayPos5$yrMooseID ==
levels(DayPos5$yrMooseID)[k]),'MSD']))^2)) +
         (sum((fitted(nomadMod)[which(DayPos5$yrMooseID == levels(DayPos5$yrMooseID)[k])] -
         mean(fitted(nomadMod)[which(DayPos5$yrMooseID ==
levels(DayPos5$yrMooseID)[k])]))^2)) +
         (length(DayPos5[which(DayPos5$yrMooseID ==
levels(DayPos5$yrMooseID)[k]),'MSD'])*((mean(DayPos5[which(DayPos5$yrMooseID==levels(DayPos5$yrMooseID)[k]),'MSD'])
-
         mean(fitted(nomadMod)[which(DayPos5$yrMooseID ==
levels(DayPos5$yrMooseID)[k])]))^2))
         )
         }

###############################################
###Create a dataframe of the results
SpaceUseClass <- data.frame(CC1ID.HRmod = CC1ID.HRmod,CC1ID.NullMod = CC1ID.NullMod,CC1ID.Dispmod = CC1ID.Dispmod,
		CC1ID.MigrMod = CC1ID.MigrMod,CC1ID.MigrModC = CC1ID.MigrModC,CC1ID.nomadMod = CC1ID.nomadMod, yrMooseID = levels(DayPos5$yrMooseID))

### add column with space use classification according to highest CC1 value
maxCC1ID <- (apply(SpaceUseClass[,1:6], 1, which.max))
SpaceUseClass$bestMod.CC1ID <- factor(ifelse(maxCC1ID == 1, "HRmod", ifelse(maxCC1ID == 2, "NullMod", ifelse(maxCC1ID == 3, "Dispmod",
		ifelse(maxCC1ID == 4, "MigrMod", ifelse(maxCC1ID == 5, "MigrModC", ifelse(maxCC1ID == 6, "nomadMod", NA)))))))

###Save results of movement classifications and goodness of fits.  
coeffs<-coef(MigrModC)
params<-data.frame(coeffs,SpaceUseClass)
sink("BD MSD Output.txt")
summary(MigrModC)
sink()
write.csv(params,"BD MSD Parameters.csv")

###Check classifications. Things to note
##Check values of asymA and asymB for individuals that were Mixed Mig. If these are only 1 or 2km apart (for Moose), it is probably just migratory
##Check value of asym for migratory, it may be that Migrmod provided the best fit, but all models may provide a poor fit
##On this point, check the value of the concordance criterion. We have found that migratory models with a CC<0.7 are often incorrect
##Check values of xmidB and scal2. Since it is not possible to constrain nlme, the migration may return to zero after the last data point, 
##for e.g. xmidB = 400, but data only has 365 days. This is in fact a dispersal then

##Afterwards, continue by only selecting individuals identified as migratory to obtain more accurate estimates for distance, timing
###and duration using the NSD and including all movement parameters as random effects in the nlme. 

params <- read.csv("BD MSD Parameters 2.csv")

SpaceUseClassMigr <- params[params$bestMod.CC1ID %in% c("MigrMod", "MigrModC"),]
SpaceUseClassMigr <- droplevels(SpaceUseClassMigr)

migrMoose <- SpaceUseClassMigr$yrMooseID
DayPos6 <- DayPos5[DayPos5$yrMooseID %in% migrMoose,]
DayPos6 <- droplevels(DayPos6)
summary(DayPos6)

############Now reapply models using the NSD#######
##These final NSD models were also applied to the second part of our study that examines the effect of starting date and location, and how data resolution influence model performance. 

##First a simpler mode to obtain starting values for the sampled dataset (i.e. it now only contains migrators)
ranef2.MigrmodLD <- nlme(NSDm ~ asym/(1+exp((xmidA-nDaysYr)/scale1)) + (-asym /(1 + exp((xmidB-nDaysYr)/scale2))),
           data=DayPos6,
           fixed = asym+ xmidA + xmidB + scale1 + scale2 ~ 1,
            random = asym + xmidA + xmidB ~ 1|yrMooseID,
            start = c(asym = summary(ranef2.Migrmod)$tTable[1], xmidA = summary(ranef2.Migrmod)$tTable[2],
            xmidB = summary(ranef2.Migrmod)$tTable[3], scale1 = 8, scale2 = 10))
summary(ranef2.MigrmodLD)

##Then a complex model that hopefully converges. This step may still prove a challenge. Here we have two versions, the second uses results from the above
##model as starting values, whereas the first I manually manipulate to try improve model convergence. Viewing plots of the movement trajecotry may assist
##in identifying appropriate starting values for the model. If convergence continues to fail, it may be necessary
##to sub-sample the dataset, or remove individuals with abnormal NSD patterns. 
ranef4.MigrmodLD <- nlme(NSDm ~ asym/(1+exp((xmidA-nDaysYr)/scale1)) + (-asym /(1 + exp((xmidB-nDaysYr)/scale2))),
           data=DayPos6,
           fixed = list(asym+ xmidA + xmidB + scale1 + scale2 ~ 1),
            random = asym + xmidA + xmidB + scale1 + scale2 ~ 1,
            groups =~ yrMooseID,
           start = c(asym = 1805, xmidA = 85,
               xmidB = 299, scale1 = 1.5, 
		scale2 = 6))
summary(ranef4.MigrmodLD)

ranef4.MigrmodLD <- nlme(NSDm ~ asym/(1+exp((xmidA-nDaysYr)/scale1)) + (-asym /(1 + exp((xmidB-nDaysYr)/scale2))),
           data=DayPos6,
           fixed = asym+ xmidA + xmidB + scale1 + scale2 ~ 1,
            random = asym + xmidA + xmidB + scale1 + scale2 ~ 1,
            groups =~ yrMooseID,
           start = c(asym = summary(ranef2.MigrmodLD)$tTable[1], xmidA = summary(ranef2.MigrmodLD)$tTable[2],
               xmidB = summary(ranef2.MigrmodLD)$tTable[3], scale1 = 5, scale2 = 4))
summary(ranef4.MigrmodLD)

##Obtain GOF for new model

MigrModLD <-ranef4.MigrmodLD

CC1ID.MigrModLD <- numeric(length(levels(DayPos6$yrMooseID)))

for(k in 1:length(levels(DayPos6$yrMooseID))) {

CC1ID.MigrModLD[k] <- 1 - (sum((DayPos6[which(DayPos6$yrMooseID == levels(DayPos6$yrMooseID)[k]),'MSD'] -
         fitted(MigrModLD)[which(DayPos6$yrMooseID ==
levels(DayPos6$yrMooseID)[k])])^2)) / (
         (sum((DayPos6[which(DayPos6$yrMooseID == levels(DayPos6$yrMooseID)[k]),'MSD'] -
         mean(DayPos6[which(DayPos6$yrMooseID ==
levels(DayPos6$yrMooseID)[k]),'MSD']))^2)) +
         (sum((fitted(MigrModLD)[which(DayPos6$yrMooseID == levels(DayPos6$yrMooseID)[k])] -
         mean(fitted(MigrModLD)[which(DayPos6$yrMooseID ==
levels(DayPos6$yrMooseID)[k])]))^2)) +
         (length(DayPos6[which(DayPos6$yrMooseID ==
levels(DayPos6$yrMooseID)[k]),'MSD'])*((mean(DayPos6[which(DayPos6$yrMooseID==levels(DayPos6$yrMooseID)[k]),'MSD'])
-
         mean(fitted(MigrModLD)[which(DayPos6$yrMooseID ==
levels(DayPos6$yrMooseID)[k])]))^2))
         )
         }

SpaceUseClassLD <- data.frame(CC1ID.MigrModLD = CC1ID.MigrModLD, yrMooseID = levels(DayPos6$yrMooseID))

coeffs<-coef(ranef4.MigrmodLD)
params<-data.frame(coeffs,SpaceUseClassLD)
write.csv(params,"BD NSD MigrMod results.csv")


