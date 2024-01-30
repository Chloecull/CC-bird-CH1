# Cox Proportional Hazards Modelling broken down by site 
library(dplyr)
library(tidyverse)
library(coxme)
library(survival)

# Read data
cox24 <- read.csv("input/nestcox.csv")

# Splitting data based on site
split(cox24, f = cox24$Site)
sites <- split(cox24, f = list(cox24$Site))
sites
bdl <- cox24 %>% filter(Site == "BDL")
tech <- cox24 %>% filter(Site == "TECH")
stny <- cox24 %>% filter(Site == "STNY")

# BDL ONLY 
bdlALL <- coxme(Surv(DaysActive, Fate) ~ SmallStems_Ha + NestHeight 
                + CanopyCover + Coverage + Dist_toTrailLog + DOY + (1|Species), data = bdl)
bdlALL
bdlVEG <- coxme(Surv(DaysActive, Fate) ~ SmallStems_Ha + NestHeight 
                + CanopyCover + Coverage + (1|Species), data = bdl)
bdlVEG
bdlDIST <- coxme(Surv(DaysActive, Fate) ~ Dist_toTrailLog
                 + (1|Species), data = bdl)
bdlDIST
bdlSEAS <- coxme(Surv(DaysActive, Fate) ~ DOY+ (1|Species), data = bdl)
bdlSEAS
bdlNULL <- coxme(Surv(DaysActive, Fate) ~ 1 + (1|Species), data =bdl)
bdlNULL

# AIC test BDL
AIC(bdlALL, bdlVEG, bdlDIST, bdlSEAS, bdlNULL)


# STNY ONLY
stnyALL <- coxme(Surv(DaysActive, Fate) ~ SmallStems_Ha + NestHeight 
                 + CanopyCover + Coverage + Dist_toTrailLog + DOY + (1|Species), data = stny)
stnyALL
stnyVEG <- coxme(Surv(DaysActive, Fate) ~ SmallStems_Ha + NestHeight 
                 + CanopyCover + Coverage + (1|Species), data = stny)
stnyVEG
stnyDIST <- coxme(Surv(DaysActive, Fate) ~ Dist_toTrailLog
                  + (1|Species), data = stny)
stnyDIST
stnySEAS <- coxme(Surv(DaysActive, Fate) ~ DOY+ (1|Species), data = stny)
stnySEAS
stnyNULL <- coxme(Surv(DaysActive, Fate) ~ 1 + (1|Species), data =stny)
stnyNULL

# AIC STNY
AIC(stnyALL, stnyVEG, stnyDIST, stnySEAS, stnyNULL)


# TECH ONLY
techALL <- coxme(Surv(DaysActive, Fate) ~ SmallStems_Ha + NestHeight 
                 + CanopyCover + Coverage + Dist_toTrailLog + DOY + (1|Species), data = tech)
techALL
techVEG <- coxme(Surv(DaysActive, Fate) ~ SmallStems_Ha + NestHeight 
                 + CanopyCover + Coverage + (1|Species), data = tech)
techVEG
techDIST <- coxme(Surv(DaysActive, Fate) ~ Dist_toTrailLog
                  + (1|Species), data = tech)
techDIST
techSEAS <- coxme(Surv(DaysActive, Fate) ~ DOY+ (1|Species), data =tech)
techSEAS
techNULL <- coxme(Surv(DaysActive, Fate) ~ 1 + (1|Species), data =tech)
techNULL

# AIC for TECH
AIC(techALL, techVEG, techDIST, techSEAS, techNULL)




