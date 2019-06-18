#======================================================================================================#
# GLMMs analyses Egyptian vulture data 
#======================================================================================================#

### originally provided by Pascual Lopez-Lopez
### revised 17 June 2019 by Steffen Oppel - removed captive-bred birds
## removed incomplete migrations

## ADD MODELS FOR SPEED, START AND END DATES!
## REMOVE MODELS FOR INTENSITY AND TAC AND MSD

# Load necessary libraries
library(lme4)
library(lubridate)
library(tidyverse)
library(data.table)
filter<-dplyr::filter
select<-dplyr::select
library(fitdistrplus)
library(corrplot)
library(dplyr)
library(tibble)
library(lmerTest)
library(MuMIn)
library(ggplot2)
library(gridExtra)
require(car)
library(afex)
library(piecewiseSEM)

# Set working directory
setwd("C:\\STEFFEN\\MANUSCRIPTS\\Submitted\\FrontiersMigrationPaper\\Analysis\\EGVUmigration")
#setwd("E://Documentos//PUBLICACIONES//Articulo migracion alimoches FRONTIERS ECOL EVOL//analysis")
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

data_ok <- data %>% mutate(DateTime=ymd_hms(start)) %>%
  rename(id.year.season=ID, totaldistkm=`total distance`, cumulativedistkm=`cumulative distance`,durationdays=`time duration (days)`) %>%
  mutate(totaldistkm=totaldistkm/1000,cumulativedistkm=cumulativedistkm/1000) %>%
  mutate(speed=cumulativedistkm/durationdays) %>%
  full_join(ancdata, by="id.year.season") %>%
  filter(!(ID %in% c("Akaga", "Blanka", "Boyana", "Elodie","Polya","Lomets","Regina","Anna","Zighmund","Panteley","Akaga"))) %>%
  filter(subpopulation!="Israel") %>%
  filter(!(id.year.season %in% c("Macedonia_fall_2011", "Faia_fall_2018", "Camaces_fall_2017", "Arpacai_fall_2012","Ikaros_fall_2012","Asparuh_fall_2013","Berenice_fall_2013",
                     "Heracles_fall_2013","Ibrahim_fall_2013","Ilina_fall_2013","Katerina_fall_2013","Redcliff_fall_2013","Lazaros_spring_2013","Ardahan_fall_2014","Volen_fall_2014"))) %>%  
  mutate(year=as.factor(year(DateTime)),agedeploy=as.factor(agedeploy),agemigr=as.factor(agemigr)) %>%
  select(country,subpopulation,id.year.season,ID,year,season,full_migration,agedeploy,agemigr,totaldistkm,cumulativedistkm,straightness,durationdays,julian_start,julian_end,speed)
  #mutate(msd=as.numeric(msd),msdkm=as.numeric(msdkm)) %>%

levels(data_ok$subpopulation)
head(data_ok)
dim(data_ok)

### SAMPLE SIZE
data_ok %>% group_by(country) %>% summarise (n_ind=length(unique(ID)))
data_ok %>% group_by(season) %>% summarise (n_ind=length(unique(id.year.season)))
data_ok %>% group_by(agedeploy) %>% summarise (n_ind=length(unique(ID)))
data_ok %>% group_by(agemigr) %>% summarise (n_ind=length(unique(id.year.season)))



########~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~################
######## SAMPLE SIZES AND TABLE 1 SUMMARIES  ################
########~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~################
Table1n<- data_ok %>% select(subpopulation,id.year.season,ID,season,agemigr) %>%
  mutate(agemigr=ifelse(agemigr==1,"juvenile",ifelse(agemigr==6,"adult","immature"))) %>%
  group_by(subpopulation,season,agemigr) %>%
  summarise(n_ind=length(unique(ID)),n_mig=length(unique(id.year.season)))

Table1<- data_ok %>% select(subpopulation,id.year.season,ID,year,season,agemigr,totaldistkm,cumulativedistkm,straightness,durationdays,julian_start,julian_end,speed) %>%
  mutate(agemigr=ifelse(agemigr==1,"juvenile",ifelse(agemigr==6,"adult","immature"))) %>%
  gather(key="mig_metric", value="value",-subpopulation,-id.year.season,-ID,-year,-season,-agemigr) %>%
  group_by(subpopulation,season,agemigr,mig_metric) %>%
  summarise(mean=median(value, na.rm=T), min=min(value, na.rm=T),max=max(value, na.rm=T)) %>%
  mutate(mean=ifelse(mean>10,round(mean,0),ifelse(mean<1,round(mean,3),round(mean,1)))) %>%
  mutate(min=ifelse(min>10,round(min,0),ifelse(min<1,round(min,3),round(min,1)))) %>%
  mutate(max=ifelse(max>10,round(max,0),ifelse(max<1,round(max,3),round(max,1)))) %>%
  
  ## fix dates
  mutate(mean=ifelse(mig_metric %in% c("julian_start","julian_end"),format(as.Date(mean, origin=ymd("2018-01-01")),format="%d-%b"),mean)) %>%
  mutate(min=ifelse(mig_metric %in% c("julian_start","julian_end"),format(as.Date(min, origin=ymd("2018-01-01")),format="%d-%b"),min)) %>%
  mutate(max=ifelse(mig_metric %in% c("julian_start","julian_end"),format(as.Date(max, origin=ymd("2018-01-01")),format="%d-%b"),max)) %>%
  
  ## compile and relabel
  mutate(out=paste(mean," (",min," - ",max,")",sep="")) %>%
  mutate(mig_metric=ifelse(mig_metric=="totaldistkm","direct-line distance",mig_metric)) %>%
  mutate(mig_metric=ifelse(mig_metric=="cumulativedistkm","travel distance",mig_metric)) %>%
  mutate(mig_metric=ifelse(mig_metric=="durationdays","duration",mig_metric)) %>%
  select(mig_metric,subpopulation,season,agemigr,out) %>%
  
  ## reshape as desired
  dcast(mig_metric +agemigr ~ subpopulation + season, value.var = "out") 

fwrite(Table1, "Table1_migration_summaries.csv")
fwrite(Table1n, "Table1_migration_sample_sizes.csv")




#==========================================we are ready to fit our models====================================================
#==========================================GLMMs=====================================
#================some packages of interest below==========================================================================



#attach(data_ok)
names (data_ok)

#### start model fitting (with lme4 package) 

#=============================================
## dependent variable = totaldistkm 
#=============================================
## model fitting with gaussian family and dependent variable log transformed
model0 <- lmer(log(totaldistkm) ~ (1|year/ID), data = data_ok)
# summary(model0)
# Anova(model0)
model1 <- lmer(log(totaldistkm) ~ factor(subpopulation) + (1|year/ID), data = data_ok)
# summary(model1)
# Anova(model1)
model2 <- lmer(log(totaldistkm) ~ factor(season) + (1|year/ID), data = data_ok)
# summary(model2)
# Anova(model2)
model3 <- glmer(log(totaldistkm) ~ factor(agemigr) + (1|year/ID), data = data_ok)
# summary(model3)
# Anova(model3)
model4 <- lmer(log(totaldistkm) ~ factor(subpopulation) + factor(season) + (1|year/ID), data = data_ok)
# summary(model4)
# Anova(model4)
model5 <- lmer(log(totaldistkm) ~ factor(subpopulation) + factor(agemigr) + (1|year/ID), data = data_ok)
# summary(model5)
# Anova(model5)
model6 <- lmer(log(totaldistkm) ~ factor(season) + factor (agemigr) + (1|year/ID), data = data_ok)
# summary(model6)
# Anova(model6)
model7 <- lmer(log(totaldistkm) ~ factor(subpopulation) + factor(season) + factor (agemigr) + (1|year/ID), data = data_ok)
# summary(model7)
# Anova(model7)

### multimodel comparison
model_rank_totaldistkm <- model.sel(model0, model1, model2, model3, model4, model5, model6, model7, rank=AIC)
model_rank_totaldistkm$class<-NULL


#############################
## BEST MODEL is model1 #####
#############################

##model averaging with those models whose AICw sum > 0.9
model_avg_totaldistkm <- model.avg(model1,model4)
summary(model_avg_totaldistkm)

## export results
best_totaldistkm <- as.data.frame(Anova(model1))
# write.csv2(Anova(model1), "best_model_totaldistkm.csv")
# write.csv2(model_rank_totaldistkm, "model_rank_totaldistkm.csv")

## graphic validation results of the best model
{Res <- residuals(model1, type = "response")
  Fit <- fitted(model1)
  par(mfrow = c(2, 2))
  plot(Res ~ Fit, xlab = "Fitted values", ylab = "Residuals", main = "Residuals vs. fitted")
  abline(h = 0)
  hist(Res, main = "Histogram of residuals", xlab = "Residuals")
  qqnorm(Res)
  qqline(Res)
  plot(Res ~ subpopulation, xlab = "subpopulation", ylab = "Residuals", main = "Total distance (km)")
  abline(h = 0, lty = 3)}

## normality test for residuals
shapiro.test(Res)

## computation of marginal R2 (only fixed effects) and conditional R2 (attributable to fixed + random effects)
model1R2 <- rsquared(model1, method=NULL)
model1R2

## R2 of random effects is the substraction between conditional R2 - marginal R2
R2random_model1 <- model1R2$Conditional - model1R2$Marginal
R2random_model1

## export results
R2_totaldistkm <- cbind(model1R2$Marginal,R2random_model1)
R2_totaldistkm <- as.data.frame(R2_totaldistkm)
names(R2_totaldistkm)[1] = "R2_fixed_effects"
names(R2_totaldistkm)[2] = "R2_random_effects"
#write.csv2(R2_totaldistkm, "R2_values_totaldistkm.csv")

rm(model0, model1, model2, model3, model4, model5, model6, model7)
graphics.off()


#================================================
## dependent variable = cumulative distance (km) 
#================================================

## model fitting with gaussian family and dependent variable log transformed
model0 <- lmer(log(cumulativedistkm) ~ (1|year/ID), data = data_ok)
# summary(model0)
# Anova(model0)
model1 <- lmer(log(cumulativedistkm) ~ factor(subpopulation) + (1|year/ID), data = data_ok)
# summary(model1)
# Anova(model1)
model2 <- lmer(log(cumulativedistkm) ~ factor(season) + (1|year/ID), data = data_ok)
# summary(model2)
# Anova(model2)
model3 <- lmer(log(cumulativedistkm) ~ factor(agemigr) + (1|year/ID), data = data_ok)
# summary(model3)
# Anova(model3)
model4 <- lmer(log(cumulativedistkm) ~ factor(subpopulation) + factor(season) + (1|year/ID), data = data_ok)
# summary(model4)
# Anova(model4)
model5 <- lmer(log(cumulativedistkm) ~ factor(subpopulation) + factor(agemigr) + (1|year/ID), data = data_ok)
# summary(model5)
# Anova(model5)
model6 <- lmer(log(cumulativedistkm) ~ factor(season) + factor (agemigr) + (1|year/ID), data = data_ok)
# summary(model6)
# Anova(model6)
model7 <- lmer(log(cumulativedistkm) ~ factor(subpopulation) + factor(season) + factor (agemigr) + (1|year/ID), data = data_ok)
# summary(model7)
# Anova(model7)

### multimodel comparison
model_rank_cumulativedistkm <- model.sel(model0, model1, model2, model3, model4, model5, model6, model7, rank=AIC)
model_rank_cumulativedistkm

#############################
## BEST MODEL is model4 #####
#############################

##model averaging with those models whose AICw sum > 0.9
model_avg_cumulativedistkm <- model.avg(model7,model4)
summary(model_avg_cumulativedistkm)

## export results
best_cumulativedistkm <- as.data.frame(Anova(model4))
# write.csv2(Anova(model7), "best_model_cumulativedistkm.csv")
# write.csv2(model_rank_cumulativedistkm, "model_rank_cumulativedistkm.csv")

## graphic validation results of the best model
{Res <- residuals(model4, type = "response")
  Fit <- fitted(model4)
  par(mfrow = c(2, 2))
  plot(Res ~ Fit, xlab = "Fitted values", ylab = "Residuals", main = "Residuals vs. fitted")
  abline(h = 0)
  hist(Res, main = "Histogram of residuals", xlab = "Residuals")
  qqnorm(Res)
  qqline(Res)
  plot(Res ~ subpopulation, xlab = "subpopulation", ylab = "Residuals", main = "Total distance (km)")
  abline(h = 0, lty = 3)}

## normality test for residuals
shapiro.test(Res)

## computation of marginal R2 (only fixed effects) and conditional R2 (attributable to fixed + random effects)
model1R2 <- rsquared(model4, method=NULL)
model1R2

## R2 of random effects is the substraction between conditional R2 - marginal R2
R2random_model1 <- model1R2$Conditional - model1R2$Marginal
R2random_model1

## export results
R2_cumulativedistkm <- cbind(model1R2$Marginal,R2random_model1)
R2_cumulativedistkm <- as.data.frame(R2_cumulativedistkm)
names(R2_cumulativedistkm)[1] = "R2_fixed_effects"
names(R2_cumulativedistkm)[2] = "R2_random_effects"
#write.csv2(R2_cumulativedistkm, "R2_values_cumulativedistkm.csv")

rm(model0, model1, model2, model3, model4, model5, model6, model7)
graphics.off()



#=============================================
## dependent variable = straightness 
#=============================================
## model fitting with gaussian family 
model0 <- lmer(straightness ~ (1|year/ID), data = data_ok)
# summary(model0)
# Anova(model0)
model1 <- lmer(straightness ~ factor(subpopulation) + (1|year/ID), data = data_ok)
# summary(model1)
# Anova(model1)
model2 <- lmer(straightness ~ factor(season) + (1|year/ID), data = data_ok)
# summary(model2)
# Anova(model2)
model3 <- lmer(straightness ~ factor(agemigr) + (1|year/ID), data = data_ok)
# summary(model3)
# Anova(model3)
model4 <- lmer(straightness ~ factor(subpopulation) + factor(season) + (1|year/ID), data = data_ok)
# summary(model4)
# Anova(model4)
model5 <- lmer(straightness ~ factor(subpopulation) + factor(agemigr) + (1|year/ID), data = data_ok)
# summary(model5)
# Anova(model5)
model6 <- lmer(straightness ~ factor(season) + factor (agemigr) + (1|year/ID), data = data_ok)
# summary(model6)
# Anova(model6)
model7 <- lmer(straightness ~ factor(subpopulation) + factor(season) + factor (agemigr) + (1|year/ID), data = data_ok)
# summary(model7)
# Anova(model7)

### multimodel comparison
model_rank_straightness <- model.sel(model0, model1, model2, model3, model4, model5, model6, model7, rank=AIC)
model_rank_straightness

#############################
## BEST MODEL is model4 #####
#############################

##model averaging with those models whose AICw sum > 0.9
model_avg_straightness <- model.avg(model4,model1)
summary(model_avg_straightness)

## export results
best_straightness <- as.data.frame(Anova(model4))
# write.csv2(Anova(model4), "best_model_straightness.csv")
# write.csv2(model_rank_straightness, "model_rank_straightness.csv")

## graphic validation results of the best model
{Res <- residuals(model4, type = "response")
  Fit <- fitted(model4)
  par(mfrow = c(2, 2))
  plot(Res ~ Fit, xlab = "Fitted values", ylab = "Residuals", main = "Residuals vs. fitted")
  abline(h = 0)
  hist(Res, main = "Histogram of residuals", xlab = "Residuals")
  qqnorm(Res)
  qqline(Res)
  plot(Res ~ subpopulation, xlab = "subpopulation", ylab = "Residuals", main = "Total distance (km)")
  abline(h = 0, lty = 3)}

## normality test for residuals
shapiro.test(Res)

## computation of marginal R2 (only fixed effects) and conditional R2 (attributable to fixed + random effects)
model1R2 <- rsquared(model4, method=NULL)
model1R2

## R2 of random effects is the substraction between conditional R2 - marginal R2
R2random_model1 <- model1R2$Conditional - model1R2$Marginal
R2random_model1

## export results
R2_straightness <- cbind(model1R2$Marginal,R2random_model1)
R2_straightness <- as.data.frame(R2_straightness)
names(R2_straightness)[1] = "R2_fixed_effects"
names(R2_straightness)[2] = "R2_random_effects"
#write.csv2(R2_straightness, "R2_values_straightness.csv")

rm(model0, model1, model2, model3, model4, model5, model6, model7)
graphics.off()





#=============================================
## dependent variable = durationdays 
#=============================================
## model fitting with gaussian family 
model0 <- lmer(durationdays ~ (1|year/ID), data = data_ok)
# summary(model0)
# Anova(model0)
model1 <- lmer(durationdays ~ factor(subpopulation) + (1|year/ID), data = data_ok)
# summary(model1)
# Anova(model1)
model2 <- lmer(durationdays ~ factor(season) + (1|year/ID), data = data_ok)
# summary(model2)
# Anova(model2)
model3 <- lmer(durationdays ~ factor(agemigr) + (1|year/ID), data = data_ok)
# summary(model3)
# Anova(model3)
model4 <- lmer(durationdays ~ factor(subpopulation) + factor(season) + (1|year/ID), data = data_ok)
# summary(model4)
# Anova(model4)
model5 <- lmer(durationdays ~ factor(subpopulation) + factor(agemigr) + (1|year/ID), data = data_ok)
# summary(model5)
# Anova(model5)
model6 <- lmer(durationdays ~ factor(season) + factor (agemigr) + (1|year/ID), data = data_ok)
# summary(model6)
# Anova(model6)
model7 <- lmer(durationdays ~ factor(subpopulation) + factor(season) + factor (agemigr) + (1|year/ID), data = data_ok)
# summary(model7)
# Anova(model7)

### multimodel comparison
model_rank_durationdays <- model.sel(model0, model1, model2, model3, model4, model5, model6, model7, rank=AIC)
model_rank_durationdays

#############################
## BEST MODEL is model7 #####
#############################

##model averaging with those models whose AICw sum > 0.9
model_avg_durationdays <- model.avg(model7,model5)
summary(model_avg_durationdays)

## export results
best_durationdays <- as.data.frame(Anova(model7))
# write.csv2(Anova(model7), "best_model_durationdays.csv")
# write.csv2(model_rank_durationdays, "model_rank_durationdays.csv")

## graphic validation results of the best model
{Res <- residuals(model7, type = "response")
  Fit <- fitted(model7)
  par(mfrow = c(2, 2))
  plot(Res ~ Fit, xlab = "Fitted values", ylab = "Residuals", main = "Residuals vs. fitted")
  abline(h = 0)
  hist(Res, main = "Histogram of residuals", xlab = "Residuals")
  qqnorm(Res)
  qqline(Res)
  plot(Res ~ subpopulation, xlab = "subpopulation", ylab = "Residuals", main = "Total distance (km)")
  abline(h = 0, lty = 3)}

## normality test for residuals
shapiro.test(Res)

## computation of marginal R2 (only fixed effects) and conditional R2 (attributable to fixed + random effects)
model1R2 <- rsquared(model7, method=NULL)
model1R2

## R2 of random effects is the substraction between conditional R2 - marginal R2
R2random_model1 <- model1R2$Conditional - model1R2$Marginal
R2random_model1

## export results
R2_durationdays <- cbind(model1R2$Marginal,R2random_model1)
R2_durationdays <- as.data.frame(R2_durationdays)
names(R2_durationdays)[1] = "R2_fixed_effects"
names(R2_durationdays)[2] = "R2_random_effects"
#write.csv2(R2_durationdays, "R2_values_durationdays.csv")

rm(model0, model1, model2, model3, model4, model5, model6, model7)
graphics.off()









#=============================================
## dependent variable = start date
#=============================================

## model fitting with gaussian family and dependent variable log transformed
model0 <- lmer(julian_start ~ (1|year/ID), data = data_ok)
# summary(model0)
# Anova(model0)
model1 <- lmer(julian_start ~ factor(subpopulation) + (1|year/ID), data = data_ok)
# summary(model1)
# Anova(model1)
model2 <- lmer(julian_start ~ factor(season) + (1|year/ID), data = data_ok)
# summary(model2)
# Anova(model2)
model3 <- lmer(julian_start ~ factor(agemigr) + (1|year/ID), data = data_ok)
# summary(model3)
# Anova(model3)
model4 <- lmer(julian_start ~ factor(subpopulation) + factor(season) + (1|year/ID), data = data_ok)
# summary(model4)
# Anova(model4)
model5 <- lmer(julian_start ~ factor(subpopulation) + factor(agemigr) + (1|year/ID), data = data_ok)
# summary(model5)
# Anova(model5)
model6 <- lmer(julian_start ~ factor(season) + factor (agemigr) + (1|year/ID), data = data_ok)
# summary(model6)
# Anova(model6)
model7 <- lmer(julian_start ~ factor(subpopulation) + factor(season) + factor (agemigr) + (1|year/ID), data = data_ok)
# summary(model7)
# Anova(model7)

### multimodel comparison
model_rank_start <- model.sel(model0, model1, model2, model3, model4, model5, model6, model7, rank=AIC)
model_rank_start

#############################
## BEST MODEL is model7 #####
#############################

##model averaging with those models whose AICw sum > 0.9
model_avg_start <- model.avg(model7,model6)
summary(model_avg_start)

## export results
best_start <- as.data.frame(Anova(model7))
# write.csv2(Anova(model7), "best_model_start.csv")
# write.csv2(model_rank_start, "model_rank_start.csv")

## graphic validation results of the best model
{Res <- residuals(model7, type = "response")
  Fit <- fitted(model7)
  par(mfrow = c(2, 2))
  plot(Res ~ Fit, xlab = "Fitted values", ylab = "Residuals", main = "Residuals vs. fitted")
  abline(h = 0)
  hist(Res, main = "Histogram of residuals", xlab = "Residuals")
  qqnorm(Res)
  qqline(Res)
  plot(Res ~ subpopulation, xlab = "subpopulation", ylab = "Residuals", main = "Total distance (km)")
  abline(h = 0, lty = 3)}

## normality test for residuals
shapiro.test(Res)

## computation of marginal R2 (only fixed effects) and conditional R2 (attributable to fixed + random effects)
model1R2 <- rsquared(model7, method=NULL)
model1R2

## R2 of random effects is the substraction between conditional R2 - marginal R2
R2random_model1 <- model1R2$Conditional - model1R2$Marginal
R2random_model1

## export results
R2_start <- cbind(model1R2$Marginal,R2random_model1)
R2_start <- as.data.frame(R2_start)
names(R2_start)[1] = "R2_fixed_effects"
names(R2_start)[2] = "R2_random_effects"
#write.csv2(R2_start, "R2_values_start.csv")

rm(model0, model1, model2, model3, model4, model5, model6, model7)
graphics.off()






#=============================================
## dependent variable = end date 
#=============================================
## model fitting with gaussian family 
model0 <- lmer(julian_end ~ (1|year/ID), data = data_ok)
# summary(model0)
# Anova(model0)
model1 <- lmer(julian_end ~ factor(subpopulation) + (1|year/ID), data = data_ok)
# summary(model1)
# Anova(model1)
model2 <- lmer(julian_end ~ factor(season) + (1|year/ID), data = data_ok)
# summary(model2)
# Anova(model2)
model3 <- lmer(julian_end ~ factor(agemigr) + (1|year/ID), data = data_ok)
# summary(model3)
# Anova(model3)
model4 <- lmer(julian_end ~ factor(subpopulation) + factor(season) + (1|year/ID), data = data_ok)
# summary(model4)
# Anova(model4)
model5 <- lmer(julian_end ~ factor(subpopulation) + factor(agemigr) + (1|year/ID), data = data_ok)
# summary(model5)
# Anova(model5)
model6 <- lmer(julian_end ~ factor(season) + factor (agemigr) + (1|year/ID), data = data_ok)
# summary(model6)
# Anova(model6)
model7 <- lmer(julian_end ~ factor(subpopulation) + factor(season) + factor (agemigr) + (1|year/ID), data = data_ok)
# summary(model7)
# Anova(model7)

### multimodel comparison
model_rank_end <- model.sel(model0, model1, model2, model3, model4, model5, model6, model7, rank=AIC)
model_rank_end

############################################
## BEST MODEL is model7 #####
############################################

##model averaging with those models whose AICw sum > 0.9
model_avg_end <- model.avg(model7,model6)
summary(model_avg_end)

## export results
best_end <- as.data.frame(Anova(model7))
# write.csv2(Anova(model7), "best_model_end.csv")
# write.csv2(model_rank_end, "model_rank_end.csv")

## graphic validation results of the best model
{Res <- residuals(model7, type = "response")
  Fit <- fitted(model7)
  par(mfrow = c(2, 2))
  plot(Res ~ Fit, xlab = "Fitted values", ylab = "Residuals", main = "Residuals vs. fitted")
  abline(h = 0)
  hist(Res, main = "Histogram of residuals", xlab = "Residuals")
  qqnorm(Res)
  qqline(Res)
  plot(Res ~ subpopulation, xlab = "subpopulation", ylab = "Residuals", main = "Total distance (km)")
  abline(h = 0, lty = 3)}

## normality test for residuals
shapiro.test(Res)

## computation of marginal R2 (only fixed effects) and conditional R2 (attributable to fixed + random effects)
model1R2 <- rsquared(model7, method=NULL)
model1R2

## R2 of random effects is the substraction between conditional R2 - marginal R2
R2random_model1 <- model1R2$Conditional - model1R2$Marginal
R2random_model1

## export results
R2_end <- cbind(model1R2$Marginal,R2random_model1)
R2_end <- as.data.frame(R2_end)
names(R2_end)[1] = "R2_fixed_effects"
names(R2_end)[2] = "R2_random_effects"
#write.csv2(R2_end, "R2_values_end.csv")

rm(model0, model1, model2, model3, model4, model5, model6, model7)
graphics.off()






#=============================================
## dependent variable = speed 
#=============================================
## model fitting with gaussian family 
model0 <- lmer(speed ~ (1|year/ID), data = data_ok)
# summary(model0)
# Anova(model0)
model1 <- lmer(speed ~ factor(subpopulation) + (1|year/ID), data = data_ok)
# summary(model1)
# Anova(model1)
model2 <- lmer(speed ~ factor(season) + (1|year/ID), data = data_ok)
# summary(model2)
# Anova(model2)
model3 <- lmer(speed ~ factor(agemigr) + (1|year/ID), data = data_ok)
# summary(model3)
# Anova(model3)
model4 <- lmer(speed ~ factor(subpopulation) + factor(season) + (1|year/ID), data = data_ok)
# summary(model4)
# Anova(model4)
model5 <- lmer(speed ~ factor(subpopulation) + factor(agemigr) + (1|year/ID), data = data_ok)
# summary(model5)
# Anova(model5)
model6 <- lmer(speed ~ factor(season) + factor (agemigr) + (1|year/ID), data = data_ok)
# summary(model6)
# Anova(model6)
model7 <- lmer(speed ~ factor(subpopulation) + factor(season) + factor (agemigr) + (1|year/ID), data = data_ok)
# summary(model7)
# Anova(model7)

### multimodel comparison
model_rank_speed <- model.sel(model0, model1, model2, model3, model4, model5, model6, model7, rank=AIC)
model_rank_speed

############################################
## BEST MODEL is model7 #####
############################################

##model averaging with those models whose AICw sum > 0.9
## no alternative models. Nothing to do here.
#model_avg_speed <- model.avg(model0)
#summary(model_avg_speed)

## export results
best_speed <- as.data.frame(Anova(model7))
# write.csv2(Anova(model7), "best_model_speed.csv")
# write.csv2(model_rank_speed, "model_rank_speed.csv")

## graphic validation results of the best model
{Res <- residuals(model7, type = "response")
  Fit <- fitted(model7)
  par(mfrow = c(2, 2))
  plot(Res ~ Fit, xlab = "Fitted values", ylab = "Residuals", main = "Residuals vs. fitted")
  abline(h = 0)
  hist(Res, main = "Histogram of residuals", xlab = "Residuals")
  qqnorm(Res)
  qqline(Res)
  plot(Res ~ subpopulation, xlab = "subpopulation", ylab = "Residuals", main = "Total distance (km)")
  abline(h = 0, lty = 3)}

## normality test for residuals
shapiro.test(Res)

## computation of marginal R2 (only fixed effects) and conditional R2 (attributable to fixed + random effects)
model1R2 <- rsquared(model7, method=NULL)
model1R2

## R2 of random effects is the substraction between conditional R2 - marginal R2
R2random_model1 <- model1R2$Conditional - model1R2$Marginal
R2random_model1

## export results
R2_speed <- cbind(model1R2$Marginal,R2random_model1)
R2_speed <- as.data.frame(R2_speed)
names(R2_speed)[1] = "R2_fixed_effects"
names(R2_speed)[2] = "R2_random_effects"
#write.csv2(R2_speed, "R2_values_speed.csv")

rm(model0, model1, model2, model3, model4, model5, model6, model7)
graphics.off()









#======================================================================================
## exporting results
#======================================================================================

## model ranking results
model_rank <- rbind.data.frame(model_rank_totaldistkm,model_rank_cumulativedistkm,model_rank_durationdays,model_rank_straightness,model_rank_start,
                               model_rank_end,model_rank_speed)
model_rank$variable <- rep(c("totaldistkm","cumulativedistkm","durationdays","straightness","start","end","speed"), each=8)
model_rank
#write.csv2(model_rank, "model_rank_results.csv")

## marginal R2 (fixed effects) and conditional R2 (attributable to fixed + random effects) 
R2_values <- rbind.data.frame(R2_totaldistkm,R2_cumulativedistkm,R2_durationdays,R2_straightness,R2_start,R2_end,R2_speed)
R2_values
# variable names
variable <- c("totaldistkm","cumulativedistkm","durationdays","straightness","start","end","speed")
R2_values_OK <- cbind.data.frame(variable,R2_values)
write.csv2(R2_values_OK, "R2_values_results.csv")



## best models p_values
best_models <- rbind.data.frame(best_totaldistkm,best_cumulativedistkm,best_straightness,best_durationdays,best_start,best_end,best_speed,
                                make.row.names=TRUE)
best_models$parameter <- row.names(best_models)
#variable names
best_models$variable <- c(rep("totaldistkm",nrow(best_totaldistkm)),
                          rep("cumulativedistkm",nrow(best_cumulativedistkm)),
                          rep("straightness",nrow(best_straightness)),
                          rep("durationdays",nrow(best_durationdays)),
                          rep("start",nrow(best_start)),
                          rep("end",nrow(best_end)),
                          rep("speed",nrow(best_speed)))

fwrite(best_models, "best_models_p_value_results.csv")




#======================================================================================
## SUMMARISE INTO TABLE 3 FOR PAPER
#======================================================================================


## read in results of repeatability analysis
repeatabilities<-fread("EGVU_repeatability_migration_subpop.csv")
names(repeatabilities)[1]<-"variable"
head(repeatabilities)

## add AICweight to R2_values
R2_values_OK<-R2_values_OK %>% mutate(wAIC=model_rank$weight[model_rank$weight>0.5])

Table3<- best_models %>% 
  left_join(R2_values_OK, by="variable") %>%
  left_join(repeatabilities, by="variable")%>%
  mutate(variable=ifelse(variable=="totaldistkm","direct-line distance",variable)) %>%
  mutate(variable=ifelse(variable=="cumulativedistkm","travel distance",variable)) %>%
  mutate(variable=ifelse(variable=="durationdays","duration",variable)) %>%
  mutate(Repeatability=paste(round(R,3)," (",round(lcl,3)," - ",round(ucl,3),")",sep="")) %>%
  #mutate(`Fixed effect(s)`=str_replace(parameter, "factor(.*\\)", "")) %>%
  mutate(`Fixed effect(s)`=gsub("[\\(\\)]", "", regmatches(as.character(parameter), gregexpr("\\(.*?\\)", as.character(parameter)))))%>%
  select(variable,`Fixed effect(s)`,wAIC,R2_fixed_effects,R2_random_effects,Repeatability)


fwrite(Table3, "Table3_model_results.csv")






#################### EXPLORATORY ANALYSIS MOVED TO BOTTOM ####################################################


#==========data exploratory analysis==============
#===============we can choose which distirbution fits better to our data==========================
#==============we use fitdistrplus package in R====================================================


#========================discrete True if data is discrete (FALSE if it is continuous)================
str(data_ok)
par(mfrow = c(1, 1))
# descdist(data$var.dep, discrete = T)
descdist(data_ok$totaldistkm, discrete = F, boot = 1000)
descdist(data_ok$cumulativedistkm, discrete = F, boot = 1000)
descdist(data_ok$straightness, discrete = F, boot = 1000)
descdist(data_ok$msdkm, discrete = F, boot = 1000)
descdist(data_ok$end, discrete = F, boot = 1000)
descdist(data_ok$sinuosity, discrete = F, boot = 1000)
descdist(data_ok$tac, discrete = F, boot = 1000)
descdist(data_ok$durationdays, discrete = F, boot = 1000)

## fitting distributions
## totaldistkm
fit.norm_totaldistkm <- fitdist(data$totaldistkm, "norm")
fit.beta_totaldistkm <- fitdist(data$totaldistkm, "beta")
fit.weibull_totaldistkm <- fitdist(data_ok$totaldistkm, "weibull")
fit.gamma_totaldistkm <- fitdist(data_ok$totaldistkm, "gamma")
fit.lognormal_totaldistkm <- fitdist(data_ok$totaldistkm, "lnorm")
plot(fit.norm_totaldistkm)
plot(fit.beta_totaldistkm)
plot(fit.weibull_totaldistkm)
plot(fit.gamma_totaldistkm)
plot(fit.lognormal_totaldistkm)

## cumulativedistkm
fit.norm_cumulativedistkm <- fitdist(data$cumulativedistkm, "norm")
fit.beta_cumulativedistkm <- fitdist(data$cumulativedistkm, "beta")
fit.weibull_cumulativedistkm <- fitdist(data_ok$cumulativedistkm, "weibull")
fit.gamma_cumulativedistkm <- fitdist(data_ok$cumulativedistkm, "gamma")
fit.lognormal_cumulativedistkm <- fitdist(data_ok$cumulativedistkm, "lnorm")
plot(fit.norm_cumulativedistkm)
plot(fit.beta_cumulativedistkm)
plot(fit.weibull_cumulativedistkm)
plot(fit.gamma_cumulativedistkm)
plot(fit.lognormal_cumulativedistkm)

## straightness
fit.norm_straightness <- fitdist(data$straightness, "norm")
fit.beta_straightness <- fitdist(data$straightness, "beta")
fit.weibull_straightness <- fitdist(data_ok$straightness, "weibull")
fit.gamma_straightness <- fitdist(data_ok$straightness, "gamma")
fit.lognormal_straightness <- fitdist(data_ok$straightness, "lnorm")
plot(fit.norm_straightness)
plot(fit.beta_straightness)
plot(fit.weibull_straightness)
plot(fit.gamma_straightness)
plot(fit.lognormal_straightness)

## msdkm
# fit.norm_msdkm <- fitdist(data$msdkm, "norm")
# fit.beta_msdkm <- fitdist(data$msdkm, "beta")
# fit.weibull_msdkm <- fitdist(data_ok$msdkm, "weibull")
# fit.gamma_msdkm <- fitdist(data_ok$msdkm, "gamma")
# fit.lognormal_msdkm <- fitdist(data_ok$msdkm, "lnorm")
# plot(fit.norm_msdkm)
# plot(fit.beta_msdkm)
# plot(fit.weibull_msdkm)
# plot(fit.gamma_msdkm)
# plot(fit.lognormal_msdkm)

## intensityuse
# fit.norm_intensityuse <- fitdist(data$intensityuse, "norm")
# fit.beta_intensityuse <- fitdist(data$intensityuse, "beta")
# fit.weibull_intensityuse <- fitdist(data_ok$intensityuse, "weibull")
# fit.gamma_intensityuse <- fitdist(data_ok$intensityuse, "gamma")
# fit.lognormal_intensityuse <- fitdist(data_ok$intensityuse, "lnorm")
# plot(fit.norm_intensityuse)
# plot(fit.beta_intensityuse)
# plot(fit.weibull_intensityuse)
# plot(fit.gamma_intensityuse)
# plot(fit.lognormal_intensityuse)

## sinuosity
# fit.norm_sinuosity <- fitdist(data$sinuosity, "norm")
# fit.beta_sinuosity <- fitdist(data$sinuosity, "beta")
# fit.weibull_sinuosity <- fitdist(data_ok$sinuosity, "weibull")
# fit.gamma_sinuosity <- fitdist(data_ok$sinuosity, "gamma")
# fit.lognormal_sinuosity <- fitdist(data_ok$sinuosity, "lnorm")
# plot(fit.norm_sinuosity)
# plot(fit.beta_sinuosity)
# plot(fit.weibull_sinuosity)
# plot(fit.gamma_sinuosity)
# plot(fit.lognormal_sinuosity)

## tac
# fit.norm_tac <- fitdist(data$tac, "norm")
# fit.beta_tac <- fitdist(data$tac, "beta")
# fit.weibull_tac <- fitdist(data_ok$tac, "weibull")
# fit.gamma_tac <- fitdist(data_ok$tac, "gamma")
# fit.lognormal_tac <- fitdist(data_ok$tac, "lnorm")
# plot(fit.norm_tac)
# plot(fit.beta_tac)
# plot(fit.weibull_tac)
# plot(fit.gamma_tac)
# plot(fit.lognormal_tac)

## durationdays
fit.norm_durationdays <- fitdist(data$durationdays, "norm")
fit.beta_durationdays <- fitdist(data$durationdays, "beta")
fit.weibull_durationdays <- fitdist(data_ok$durationdays, "weibull")
fit.gamma_durationdays <- fitdist(data_ok$durationdays, "gamma")
fit.lognormal_durationdays <- fitdist(data_ok$durationdays, "lnorm")
plot(fit.norm_durationdays)
plot(fit.beta_durationdays)
plot(fit.weibull_durationdays)
plot(fit.gamma_durationdays)
plot(fit.lognormal_durationdays)


## OPTIONAL ###
#==============================now we know which is the most convenient distribution===================
#===============================in a second step we will calculate any correlation among our variables if exist===========
#===============================To this end, we can use some figures as those showing explicitly correlation among covariates====
#================================we use corrplot, which is based in spearman pairwise correlations=======================


M <- data_ok[,c("totaldistkm","cumulativedistkm","straightness","msdkm","intensityuse","sinuosity","tac","durationdays")]

corrplot(cor(M), method="circle")  ## alternative methods "square"
corrplot(cor(M), method="ellipse")
#=====================================we can use different plot types to visualize correlogram==============================
corrplot(cor(M), method="color") ## alternative method "pie"
#==============also, we can represent correlations in number format=========================================================
corrplot(cor(M), method="number")
## mixed format
corrplot.mixed(cor(M), upper = "circle", lower = "number", tl.cex = 0.8, number.cex = 1)


#==============================we can finally do a significance test and add it to correlogram===============================
# mat : is a matrix of data
# ... : further arguments to pass to the native R cor.test function
cor.mtest <- function(mat, ...) {
  mat <- as.matrix(mat)
  n <- ncol(mat)
  p.mat<- matrix(NA, n, n)
  diag(p.mat) <- 0
  for (i in 1:(n - 1)) {
    for (j in (i + 1):n) {
      tmp <- cor.test(mat[, i], mat[, j], ...)
      p.mat[i, j] <- p.mat[j, i] <- tmp$p.value
    }
  }
  colnames(p.mat) <- rownames(p.mat) <- colnames(mat)
  p.mat
}
# matrix of the p-value of the correlation
p.mat <- cor.mtest(M)
p.mat
## optional 
## head(p.mat[, 1:5])


#==============================================================================
#========================now, we can simply plot our test results and integrate them into our correlogram=============

# Specialized the insignificant value according to the significant level
corrplot(cor(M), type="upper", order="hclust", 
         p.mat = p.mat, sig.level = 0.05)

## mark unsignificant values with a crox (X) 
corrplot.mixed(cor(M), upper = "circle", lower = "number", tl.cex = 0.8, number.cex = 1, p.mat = p.mat, sig.level = 0.05) ## optional sig.level = 0.01
corrplot.mixed(cor(M), upper = "circle", lower = "number", tl.cex = 0.8, number.cex = 1, p.mat = p.mat, insig = "blank") 

## correlation plot with significant values highlighted with asterisks
corrplot(cor(M), p.mat = p.mat, method = "color", type = "upper", sig.level = c(.001, .01, .05), pch.cex = .9, 
         insig = "label_sig", pch.col = "white", order = "AOE")





