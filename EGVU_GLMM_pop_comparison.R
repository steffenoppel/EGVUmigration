#======================================================================================================#
# GLMMs analyses Egyptian vulture data 
#======================================================================================================#

setwd("E://Documentos//PUBLICACIONES//Articulo migracion alimoches FRONTIERS ECOL EVOL//analysis")
data<-read.csv2("migration_parameters_completed_migrations_clean.csv",header=TRUE)

data$year <- as.factor(data$year)
data$agedeploy <- as.factor(data$agedeploy)
data$agemigr <- as.factor(data$agemigr)


## remove uncomplete migrations
data_by_migrations <- split.data.frame(data,data$full_migration)
data_complete_migrations <- as.data.frame(data_by_migrations$y)

## remove Israelian birds
levels(data_complete_migrations$country)
data_complete_migrations1 <- data_complete_migrations[data_complete_migrations$country != "Israel", ]
levels(data_complete_migrations1$country)

## data ready for analyses
data_ok <- data_complete_migrations1
levels(data_ok$subpopulation)


#==========data exploratory analysis==============
#===============we can choose which distirbution fits better to our data==========================
#==============we use fitdistrplus package in R====================================================
library(fitdistrplus)

#========================discrete True if data is discrete (FALSE if it is continuous)================

par(mfrow = c(1, 1))
# descdist(data$var.dep, discrete = T)
descdist(data_ok$totaldistkm, discrete = F, boot = 1000)
descdist(data_ok$cumulativedistkm, discrete = F, boot = 1000)
descdist(data_ok$straightness, discrete = F, boot = 1000)
descdist(data_ok$msdkm, discrete = F, boot = 1000)
descdist(data_ok$intensityuse, discrete = F, boot = 1000)
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
fit.norm_msdkm <- fitdist(data$msdkm, "norm")
fit.beta_msdkm <- fitdist(data$msdkm, "beta")
fit.weibull_msdkm <- fitdist(data_ok$msdkm, "weibull")
fit.gamma_msdkm <- fitdist(data_ok$msdkm, "gamma")
fit.lognormal_msdkm <- fitdist(data_ok$msdkm, "lnorm")
plot(fit.norm_msdkm)
plot(fit.beta_msdkm)
plot(fit.weibull_msdkm)
plot(fit.gamma_msdkm)
plot(fit.lognormal_msdkm)

## intensityuse
fit.norm_intensityuse <- fitdist(data$intensityuse, "norm")
fit.beta_intensityuse <- fitdist(data$intensityuse, "beta")
fit.weibull_intensityuse <- fitdist(data_ok$intensityuse, "weibull")
fit.gamma_intensityuse <- fitdist(data_ok$intensityuse, "gamma")
fit.lognormal_intensityuse <- fitdist(data_ok$intensityuse, "lnorm")
plot(fit.norm_intensityuse)
plot(fit.beta_intensityuse)
plot(fit.weibull_intensityuse)
plot(fit.gamma_intensityuse)
plot(fit.lognormal_intensityuse)

## sinuosity
fit.norm_sinuosity <- fitdist(data$sinuosity, "norm")
fit.beta_sinuosity <- fitdist(data$sinuosity, "beta")
fit.weibull_sinuosity <- fitdist(data_ok$sinuosity, "weibull")
fit.gamma_sinuosity <- fitdist(data_ok$sinuosity, "gamma")
fit.lognormal_sinuosity <- fitdist(data_ok$sinuosity, "lnorm")
plot(fit.norm_sinuosity)
plot(fit.beta_sinuosity)
plot(fit.weibull_sinuosity)
plot(fit.gamma_sinuosity)
plot(fit.lognormal_sinuosity)

## tac
fit.norm_tac <- fitdist(data$tac, "norm")
fit.beta_tac <- fitdist(data$tac, "beta")
fit.weibull_tac <- fitdist(data_ok$tac, "weibull")
fit.gamma_tac <- fitdist(data_ok$tac, "gamma")
fit.lognormal_tac <- fitdist(data_ok$tac, "lnorm")
plot(fit.norm_tac)
plot(fit.beta_tac)
plot(fit.weibull_tac)
plot(fit.gamma_tac)
plot(fit.lognormal_tac)

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
library(corrplot)

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




#==========================================we are ready to fit our models====================================================
#==========================================GLMMs=====================================
#================some packages of interest below==========================================================================
library(lmerTest)
library(MuMIn)
library(ggplot2)
library(gridExtra)
library(lme4)
require(car)
library(afex)
library(piecewiseSEM)


attach(data_ok)
names (data_ok)

#### start model fitting (with lme4 package) 

#=============================================
## dependent variable = totaldistkm 
#=============================================
## model fitting with gaussian family and dependent variable log transformed
model0 <- glmer(log(totaldistkm) ~ (1|year/ID), data = data_ok, family = gaussian)
summary(model0)
Anova(model0)
model1 <- glmer(log(totaldistkm) ~ factor(subpopulation) + (1|year/ID), data = data_ok, family = gaussian)
summary(model1)
Anova(model1)
model2 <- glmer(log(totaldistkm) ~ factor(season) + (1|year/ID), data = data_ok, family = gaussian)
summary(model2)
Anova(model2)
model3 <- glmer(log(totaldistkm) ~ factor(agemigr) + (1|year/ID), data = data_ok, family = gaussian)
summary(model3)
Anova(model3)
model4 <- glmer(log(totaldistkm) ~ factor(subpopulation) + factor(season) + (1|year/ID), data = data_ok, family = gaussian)
summary(model4)
Anova(model4)
model5 <- glmer(log(totaldistkm) ~ factor(subpopulation) + factor(agemigr) + (1|year/ID), data = data_ok, family = gaussian)
summary(model5)
Anova(model5)
model6 <- glmer(log(totaldistkm) ~ factor(season) + factor (agemigr) + (1|year/ID), data = data_ok, family = gaussian)
summary(model6)
Anova(model6)
model7 <- glmer(log(totaldistkm) ~ factor(subpopulation) + factor(season) + factor (agemigr) + (1|year/ID), data = data_ok, family = gaussian)
summary(model7)
Anova(model7)

### multimodel comparison
model_rank_totaldistkm <- model.sel(model0, model1, model2, model3, model4, model5, model6, model7, rank=AIC)
model_rank_totaldistkm


#############################
## BEST MODEL is model1 #####
#############################

##model averaging with those models whose AICw sum > 0.9
model_avg_totaldistkm <- model.avg(model1,model4)
summary(model_avg_totaldistkm)

## export results
best_totaldistkm <- as.data.frame(Anova(model1))
write.csv2(Anova(model1), "best_model_totaldistkm.csv")
write.csv2(model_rank_totaldistkm, "model_rank_totaldistkm.csv")

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
write.csv2(R2_totaldistkm, "R2_values_totaldistkm.csv")

rm(model0, model1, model2, model3, model4, model5, model6, model7)
graphics.off()


#================================================
## dependent variable = cumulative distance (km) 
#================================================

## model fitting with gaussian family and dependent variable log transformed
model0 <- glmer(log(cumulativedistkm) ~ (1|year/ID), data = data_ok, family = gaussian)
summary(model0)
Anova(model0)
model1 <- glmer(log(cumulativedistkm) ~ factor(subpopulation) + (1|year/ID), data = data_ok, family = gaussian)
summary(model1)
Anova(model1)
model2 <- glmer(log(cumulativedistkm) ~ factor(season) + (1|year/ID), data = data_ok, family = gaussian)
summary(model2)
Anova(model2)
model3 <- glmer(log(cumulativedistkm) ~ factor(agemigr) + (1|year/ID), data = data_ok, family = gaussian)
summary(model3)
Anova(model3)
model4 <- glmer(log(cumulativedistkm) ~ factor(subpopulation) + factor(season) + (1|year/ID), data = data_ok, family = gaussian)
summary(model4)
Anova(model4)
model5 <- glmer(log(cumulativedistkm) ~ factor(subpopulation) + factor(agemigr) + (1|year/ID), data = data_ok, family = gaussian)
summary(model5)
Anova(model5)
model6 <- glmer(log(cumulativedistkm) ~ factor(season) + factor (agemigr) + (1|year/ID), data = data_ok, family = gaussian)
summary(model6)
Anova(model6)
model7 <- glmer(log(cumulativedistkm) ~ factor(subpopulation) + factor(season) + factor (agemigr) + (1|year/ID), data = data_ok, family = gaussian)
summary(model7)
Anova(model7)

### multimodel comparison
model_rank_cumulativedistkm <- model.sel(model0, model1, model2, model3, model4, model5, model6, model7, rank=AIC)
model_rank_cumulativedistkm

#############################
## BEST MODEL is model7 #####
#############################

##model averaging with those models whose AICw sum > 0.9
model_avg_cumulativedistkm <- model.avg(model7,model4)
summary(model_avg_cumulativedistkm)

## export results
best_cumulativedistkm <- as.data.frame(Anova(model7))
write.csv2(Anova(model7), "best_model_cumulativedistkm.csv")
write.csv2(model_rank_cumulativedistkm, "model_rank_cumulativedistkm.csv")

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
R2_cumulativedistkm <- cbind(model1R2$Marginal,R2random_model1)
R2_cumulativedistkm <- as.data.frame(R2_cumulativedistkm)
names(R2_cumulativedistkm)[1] = "R2_fixed_effects"
names(R2_cumulativedistkm)[2] = "R2_random_effects"
write.csv2(R2_cumulativedistkm, "R2_values_cumulativedistkm.csv")

rm(model0, model1, model2, model3, model4, model5, model6, model7)
graphics.off()



#=============================================
## dependent variable = straightness 
#=============================================
## model fitting with gaussian family 
model0 <- glmer(straightness ~ (1|year/ID), data = data_ok, family = gaussian)
summary(model0)
Anova(model0)
model1 <- glmer(straightness ~ factor(subpopulation) + (1|year/ID), data = data_ok, family = gaussian)
summary(model1)
Anova(model1)
model2 <- glmer(straightness ~ factor(season) + (1|year/ID), data = data_ok, family = gaussian)
summary(model2)
Anova(model2)
model3 <- glmer(straightness ~ factor(agemigr) + (1|year/ID), data = data_ok, family = gaussian)
summary(model3)
Anova(model3)
model4 <- glmer(straightness ~ factor(subpopulation) + factor(season) + (1|year/ID), data = data_ok, family = gaussian)
summary(model4)
Anova(model4)
model5 <- glmer(straightness ~ factor(subpopulation) + factor(agemigr) + (1|year/ID), data = data_ok, family = gaussian)
summary(model5)
Anova(model5)
model6 <- glmer(straightness ~ factor(season) + factor (agemigr) + (1|year/ID), data = data_ok, family = gaussian)
summary(model6)
Anova(model6)
model7 <- glmer(straightness ~ factor(subpopulation) + factor(season) + factor (agemigr) + (1|year/ID), data = data_ok, family = gaussian)
summary(model7)
Anova(model7)

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
write.csv2(Anova(model4), "best_model_straightness.csv")
write.csv2(model_rank_straightness, "model_rank_straightness.csv")

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
write.csv2(R2_straightness, "R2_values_straightness.csv")

rm(model0, model1, model2, model3, model4, model5, model6, model7)
graphics.off()



#=============================================
## dependent variable = msdkm 
#=============================================
## model fitting with gaussian family and dependent variable log transformed
model0 <- glmer(log(msdkm) ~ (1|year/ID), data = data_ok, family = gaussian)
summary(model0)
Anova(model0)
model1 <- glmer(log(msdkm) ~ factor(subpopulation) + (1|year/ID), data = data_ok, family = gaussian)
summary(model1)
Anova(model1)
model2 <- glmer(log(msdkm) ~ factor(season) + (1|year/ID), data = data_ok, family = gaussian)
summary(model2)
Anova(model2)
model3 <- glmer(log(msdkm) ~ factor(agemigr) + (1|year/ID), data = data_ok, family = gaussian)
summary(model3)
Anova(model3)
model4 <- glmer(log(msdkm) ~ factor(subpopulation) + factor(season) + (1|year/ID), data = data_ok, family = gaussian)
summary(model4)
Anova(model4)
model5 <- glmer(log(msdkm) ~ factor(subpopulation) + factor(agemigr) + (1|year/ID), data = data_ok, family = gaussian)
summary(model5)
Anova(model5)
model6 <- glmer(log(msdkm) ~ factor(season) + factor (agemigr) + (1|year/ID), data = data_ok, family = gaussian)
summary(model6)
Anova(model6)
model7 <- glmer(log(msdkm) ~ factor(subpopulation) + factor(season) + factor (agemigr) + (1|year/ID), data = data_ok, family = gaussian)
summary(model7)
Anova(model7)

### multimodel comparison
model_rank_msdkm <- model.sel(model0, model1, model2, model3, model4, model5, model6, model7, rank=AIC)
model_rank_msdkm

#############################
## BEST MODEL is model1 #####
#############################

##model averaging with those models whose AICw sum > 0.9
model_avg_msdkm <- model.avg(model1,model4)
summary(model_avg_msdkm)

## export results
best_msdkm <- as.data.frame(Anova(model1))
write.csv2(Anova(model1), "best_model_msdkm.csv")
write.csv2(model_rank_msdkm, "model_rank_msdkm.csv")

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
R2_msdkm <- cbind(model1R2$Marginal,R2random_model1)
R2_msdkm <- as.data.frame(R2_msdkm)
names(R2_msdkm)[1] = "R2_fixed_effects"
names(R2_msdkm)[2] = "R2_random_effects"
write.csv2(R2_msdkm, "R2_values_msdkm.csv")

rm(model0, model1, model2, model3, model4, model5, model6, model7)
graphics.off()


#=============================================
## dependent variable = intensityuse 
#=============================================
## model fitting with gaussian family 
model0 <- glmer(intensityuse ~ (1|year/ID), data = data_ok, family = gaussian)
summary(model0)
Anova(model0)
model1 <- glmer(intensityuse ~ factor(subpopulation) + (1|year/ID), data = data_ok, family = gaussian)
summary(model1)
Anova(model1)
model2 <- glmer(intensityuse ~ factor(season) + (1|year/ID), data = data_ok, family = gaussian)
summary(model2)
Anova(model2)
model3 <- glmer(intensityuse ~ factor(agemigr) + (1|year/ID), data = data_ok, family = gaussian)
summary(model3)
Anova(model3)
model4 <- glmer(intensityuse ~ factor(subpopulation) + factor(season) + (1|year/ID), data = data_ok, family = gaussian)
summary(model4)
Anova(model4)
model5 <- glmer(intensityuse ~ factor(subpopulation) + factor(agemigr) + (1|year/ID), data = data_ok, family = gaussian)
summary(model5)
Anova(model5)
model6 <- glmer(intensityuse ~ factor(season) + factor (agemigr) + (1|year/ID), data = data_ok, family = gaussian)
summary(model6)
Anova(model6)
model7 <- glmer(intensityuse ~ factor(subpopulation) + factor(season) + factor (agemigr) + (1|year/ID), data = data_ok, family = gaussian)
summary(model7)
Anova(model7)

### multimodel comparison
model_rank_intensityuse <- model.sel(model0, model1, model2, model3, model4, model5, model6, model7, rank=AIC)
model_rank_intensityuse

############################################
## BEST MODEL is model0, the NULL model #####
############################################

##model averaging with those models whose AICw sum > 0.9
model_avg_intensityuse <- model.avg(model0,model3)
summary(model_avg_intensityuse)

## export results
best_intensityuse <- as.data.frame(Anova(model0))
write.csv2(Anova(model0), "best_model_intensityuse.csv")
write.csv2(model_rank_intensityuse, "model_rank_intensityuse.csv")

## graphic validation results of the best model
{Res <- residuals(model0, type = "response")
  Fit <- fitted(model0)
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
model1R2 <- rsquared(model0, method=NULL)
model1R2

## R2 of random effects is the substraction between conditional R2 - marginal R2
R2random_model1 <- model1R2$Conditional - model1R2$Marginal
R2random_model1

## export results
R2_intensityuse <- cbind(model1R2$Marginal,R2random_model1)
R2_intensityuse <- as.data.frame(R2_intensityuse)
names(R2_intensityuse)[1] = "R2_fixed_effects"
names(R2_intensityuse)[2] = "R2_random_effects"
write.csv2(R2_intensityuse, "R2_values_intensityuse.csv")

rm(model0, model1, model2, model3, model4, model5, model6, model7)
graphics.off()


#=============================================
## dependent variable = sinuosity 
#=============================================
## model fitting with gaussian family 
model0 <- glmer(sinuosity ~ (1|year/ID), data = data_ok, family = gaussian)
summary(model0)
Anova(model0)
model1 <- glmer(sinuosity ~ factor(subpopulation) + (1|year/ID), data = data_ok, family = gaussian)
summary(model1)
Anova(model1)
model2 <- glmer(sinuosity ~ factor(season) + (1|year/ID), data = data_ok, family = gaussian)
summary(model2)
Anova(model2)
model3 <- glmer(sinuosity ~ factor(agemigr) + (1|year/ID), data = data_ok, family = gaussian)
summary(model3)
Anova(model3)
model4 <- glmer(sinuosity ~ factor(subpopulation) + factor(season) + (1|year/ID), data = data_ok, family = gaussian)
summary(model4)
Anova(model4)
model5 <- glmer(sinuosity ~ factor(subpopulation) + factor(agemigr) + (1|year/ID), data = data_ok, family = gaussian)
summary(model5)
Anova(model5)
model6 <- glmer(sinuosity ~ factor(season) + factor (agemigr) + (1|year/ID), data = data_ok, family = gaussian)
summary(model6)
Anova(model6)
model7 <- glmer(sinuosity ~ factor(subpopulation) + factor(season) + factor (agemigr) + (1|year/ID), data = data_ok, family = gaussian)
summary(model7)
Anova(model7)

### multimodel comparison
model_rank_sinuosity <- model.sel(model0, model1, model2, model3, model4, model5, model6, model7, rank=AIC)
model_rank_sinuosity

############################################
## BEST MODEL is model0, the NULL model #####
############################################

##model averaging with those models whose AICw sum > 0.9
## no alternative models. Nothing to do here.
#model_avg_sinuosity <- model.avg(model0)
#summary(model_avg_sinuosity)

## export results
best_sinuosity <- as.data.frame(Anova(model0))
write.csv2(Anova(model0), "best_model_sinuosity.csv")
write.csv2(model_rank_sinuosity, "model_rank_sinuosity.csv")

## graphic validation results of the best model
{Res <- residuals(model0, type = "response")
  Fit <- fitted(model0)
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
model1R2 <- rsquared(model0, method=NULL)
model1R2

## R2 of random effects is the substraction between conditional R2 - marginal R2
R2random_model1 <- model1R2$Conditional - model1R2$Marginal
R2random_model1

## export results
R2_sinuosity <- cbind(model1R2$Marginal,R2random_model1)
R2_sinuosity <- as.data.frame(R2_sinuosity)
names(R2_sinuosity)[1] = "R2_fixed_effects"
names(R2_sinuosity)[2] = "R2_random_effects"
write.csv2(R2_sinuosity, "R2_values_sinuosity.csv")

rm(model0, model1, model2, model3, model4, model5, model6, model7)
graphics.off()


#=============================================
## dependent variable = tac 
#=============================================
## model fitting with gaussian family 
model0 <- glmer(tac ~ (1|year/ID), data = data_ok, family = gaussian)
summary(model0)
Anova(model0)
model1 <- glmer(tac ~ factor(subpopulation) + (1|year/ID), data = data_ok, family = gaussian)
summary(model1)
Anova(model1)
model2 <- glmer(tac ~ factor(season) + (1|year/ID), data = data_ok, family = gaussian)
summary(model2)
Anova(model2)
model3 <- glmer(tac ~ factor(agemigr) + (1|year/ID), data = data_ok, family = gaussian)
summary(model3)
Anova(model3)
model4 <- glmer(tac ~ factor(subpopulation) + factor(season) + (1|year/ID), data = data_ok, family = gaussian)
summary(model4)
Anova(model4)
model5 <- glmer(tac ~ factor(subpopulation) + factor(agemigr) + (1|year/ID), data = data_ok, family = gaussian)
summary(model5)
Anova(model5)
model6 <- glmer(tac ~ factor(season) + factor (agemigr) + (1|year/ID), data = data_ok, family = gaussian)
summary(model6)
Anova(model6)
model7 <- glmer(tac ~ factor(subpopulation) + factor(season) + factor (agemigr) + (1|year/ID), data = data_ok, family = gaussian)
summary(model7)
Anova(model7)

### multimodel comparison
model_rank_tac <- model.sel(model0, model1, model2, model3, model4, model5, model6, model7, rank=AIC)
model_rank_tac

#############################
## BEST MODEL is model1 #####
#############################

##model averaging with those models whose AICw sum > 0.9
model_avg_tac <- model.avg(model1,model4)
summary(model_avg_tac)

## export results
best_tac <- as.data.frame(Anova(model1))
write.csv2(Anova(model1), "best_model_tac.csv")
write.csv2(model_rank_tac, "model_rank_tac.csv")

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
R2_tac <- cbind(model1R2$Marginal,R2random_model1)
R2_tac <- as.data.frame(R2_tac)
names(R2_tac)[1] = "R2_fixed_effects"
names(R2_tac)[2] = "R2_random_effects"
write.csv2(R2_tac, "R2_values_tac.csv")

rm(model0, model1, model2, model3, model4, model5, model6, model7)
graphics.off()




#=============================================
## dependent variable = durationdays 
#=============================================
## model fitting with gaussian family 
model0 <- glmer(durationdays ~ (1|year/ID), data = data_ok, family = gaussian)
summary(model0)
Anova(model0)
model1 <- glmer(durationdays ~ factor(subpopulation) + (1|year/ID), data = data_ok, family = gaussian)
summary(model1)
Anova(model1)
model2 <- glmer(durationdays ~ factor(season) + (1|year/ID), data = data_ok, family = gaussian)
summary(model2)
Anova(model2)
model3 <- glmer(durationdays ~ factor(agemigr) + (1|year/ID), data = data_ok, family = gaussian)
summary(model3)
Anova(model3)
model4 <- glmer(durationdays ~ factor(subpopulation) + factor(season) + (1|year/ID), data = data_ok, family = gaussian)
summary(model4)
Anova(model4)
model5 <- glmer(durationdays ~ factor(subpopulation) + factor(agemigr) + (1|year/ID), data = data_ok, family = gaussian)
summary(model5)
Anova(model5)
model6 <- glmer(durationdays ~ factor(season) + factor (agemigr) + (1|year/ID), data = data_ok, family = gaussian)
summary(model6)
Anova(model6)
model7 <- glmer(durationdays ~ factor(subpopulation) + factor(season) + factor (agemigr) + (1|year/ID), data = data_ok, family = gaussian)
summary(model7)
Anova(model7)

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
write.csv2(Anova(model7), "best_model_durationdays.csv")
write.csv2(model_rank_durationdays, "model_rank_durationdays.csv")

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
write.csv2(R2_durationdays, "R2_values_durationdays.csv")

rm(model0, model1, model2, model3, model4, model5, model6, model7)
graphics.off()


#======================================================================================
## exporting results
#======================================================================================

## model ranking results
model_rank <- rbind.data.frame(model_rank_totaldistkm,model_rank_cumulativedistkm,model_rank_straightness,model_rank_msdkm,
                               model_rank_intensityuse,model_rank_sinuosity, model_rank_tac,model_rank_durationdays,
                               make.row.names=TRUE)
model_rank
write.csv2(model_rank, "model_rank_results.csv")

## marginal R2 (fixed effects) and conditional R2 (attributable to fixed + random effects) 
R2_values <- rbind.data.frame(R2_totaldistkm,R2_cumulativedistkm,R2_straightness,R2_msdkm,R2_intensityuse,R2_sinuosity,R2_tac,R2_durationdays)
R2_values
# variable names
variable <- c("totaldistkm","cumulativedistkm","straightness","msdkm","intensityuse","sinuosity","tac","durationdays")
R2_values_OK <- cbind.data.frame(variable,R2_values)
write.csv2(R2_values_OK, "R2_values_results.csv")

library(dplyr)
library(tibble)

## best models p_values
best_models <- rbind.data.frame(best_totaldistkm,best_cumulativedistkm,best_straightness,best_msdkm,best_tac,best_durationdays,
                                make.row.names=TRUE)
#variable names
dep_var <- c("totaldistkm","cumulativedistkm","cumulativedistkm","cumulativedistkm","straightness","straightness","msdkm",
             "tac","durationdays","durationdays","durationdays")
best_models_OK <- cbind.data.frame(dep_var,best_models)
## row names as variable names
kk <- best_models_OK %>% rownames_to_column("indep_var")
## reordering the table
best_models_OK <- kk[c("dep_var","indep_var","Chisq", "Df","Pr(>Chisq)")]
write.csv2(best_models_OK, "best_models_p_value_results.csv")
