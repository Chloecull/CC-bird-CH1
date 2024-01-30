# Cox Proportional Hazards Code
# For all species combined together across all sites

library(coxme)
library(survival)
library(dplyr)

# Read data
cox24 <- read.csv("input/nestcox.csv")

# Build set of candidate models

# Vegetation Model
mod.nestsurvVEG <- coxme(Surv(DaysActive, Fate) ~ SmallStems_Ha + NestHeight 
                         + Coverage + (1|Species) + (1|Site), data = cox24)

# Human Model (no human activity indices YET)
mod.nestsurvDIST <- coxme(Surv(DaysActive, Fate) ~ Dist_toTrailLog + (1|Species) 
                          + (1|Site), data = cox24)

# Initiation Date (as a proxy for seasonality)
mod.nestsurvSEAS <- coxme(Surv(DaysActive, Fate) ~ DOY+ (1|Species) 
                          + (1|Site), data = cox24)

# Null model
mod.nestsurvNULL <- coxme(Surv(DaysActive, Fate) ~ 1 + (1|Species) + (1|Site), data = cox24)

# Global model with all variables of interest 
mod.nestsurvALL <- coxme(Surv(DaysActive, Fate) ~ SmallStems_Ha + NestHeight 
                         + Coverage + Dist_toTrailLog + DOY + (1|Species) + 
                           (1|Site), data = cox24)

# Summary of models
summary(mod.nestsurvALL)
summary(mod.nestsurvDIST)
summary(mod.nestsurvSEAS)
summary(mod.nestsurvVEG)
summary(mod.nestsurvNULL)

# Model AIC comparison
AIC(mod.nestsurvALL, mod.nestsurvDIST, mod.nestsurvSEAS, mod.nestsurvVEG, mod.nestsurvNULL)

# Testing PH assumptions 
test.phALL <- cox.zph(mod.nestsurvALL)
test.phDIST <- cox.zph(mod.nestsurvDIST)
test.phSEAS <- cox.zph(mod.nestsurvSEAS)
test.phVEG <- cox.zph(mod.nestsurvVEG)


test.phALL
test.phDIST
test.phSEAS
test.phVEG


# Are there other assumptions to check ??

