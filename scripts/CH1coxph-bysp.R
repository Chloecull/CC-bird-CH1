# Cox Proportional Hazards Modelling broken down by sp
library(dplyr)
library(tidyverse)
library(coxme)
library(survival)

# Read data
cox24 <- read.csv("input/nestcox.csv")

# Splitting data based on species
split(cox24, f = cox24$Species)
species <- split(cox24, f = list(cox24$Species))
species
yewa <- cox24 %>% filter(Species == "YEWA")
amro <- cox24 %>% filter(Species == "AMRO")
noca <- cox24 %>% filter(Species == "NOCA")
grca <- cox24 %>% filter(Species == "GRCA")


# Building Models and Ranking for Yellow Warblers 
yewaALL <- coxme(Surv(DaysActive, Fate) ~ SmallStems_Ha + NestHeight 
                 + CanopyCover + Coverage + Dist_toTrailLog + DOY + (1|Site), data = yewa)
yewaALL
yewaVEG <- coxme(Surv(DaysActive, Fate) ~ SmallStems_Ha + NestHeight 
                 + CanopyCover + Coverage + (1|Site), data = yewa)
yewaVEG
yewaDIST <- coxme(Surv(DaysActive, Fate) ~ Dist_toTrailLog
                  + (1|Site), data = yewa)
yewaDIST
yewaSEAS <- coxme(Surv(DaysActive, Fate) ~ DOY+ (1|Site), data = yewa)
yewaSEAS
yewaNULL <- coxme(Surv(DaysActive, Fate) ~ 1 + (1|Site), data = yewa)
yewaNULL


# Testing AIC for Yellow Warblers
AIC(yewaALL, yewaVEG, yewaDIST, yewaSEAS, yewaNULL)


# Building Models for Robins
amroALL <- coxme(Surv(DaysActive, Fate) ~ SmallStems_Ha + NestHeight 
                 + CanopyCover + Coverage + Dist_toTrailLog + DOY + (1|Site), data = amro)
amroALL
amroVEG <- coxme(Surv(DaysActive, Fate) ~ SmallStems_Ha + NestHeight 
                 + CanopyCover + Coverage + (1|Site), data = amro)
amroVEG
amroDIST <- coxme(Surv(DaysActive, Fate) ~ Dist_toTrailLog
                  + (1|Site), data = amro)
amroDIST
amroSEAS <- coxme(Surv(DaysActive, Fate) ~ DOY+ (1|Site), data = amro)
amroSEAS
amroNULL <- coxme(Surv(DaysActive, Fate) ~ 1 + (1|Site), data = amro)
amroNULL

# testing AIC for amro
AIC(amroALL, amroVEG, amroDIST, amroSEAS, amroNULL)


# Building models for cardinals
nocaALL <- coxme(Surv(DaysActive, Fate) ~ SmallStems_Ha + NestHeight 
                 + CanopyCover + Coverage + Dist_toTrailLog + DOY + (1|Site), data = noca)
nocaALL
nocaVEG <- coxme(Surv(DaysActive, Fate) ~ SmallStems_Ha + NestHeight 
                 + CanopyCover + Coverage + (1|Site), data = noca)
nocaVEG
nocaDIST <- coxme(Surv(DaysActive, Fate) ~ Dist_toTrailLog
                  + (1|Site), data = noca)
nocaDIST
nocaSEAS <- coxme(Surv(DaysActive, Fate) ~ DOY+ (1|Site), data = noca)
nocaSEAS
nocaNULL <- coxme(Surv(DaysActive, Fate) ~ 1 + (1|Site), data = noca)
nocaNULL

# testing AIC for noca
AIC(nocaALL, nocaVEG, nocaDIST,nocaSEAS, nocaNULL)


# Building models for Gray Catbirds
grcaALL <- coxme(Surv(DaysActive, Fate) ~ SmallStems_Ha + NestHeight 
                 + CanopyCover + Coverage + Dist_toTrailLog + DOY + (1|Site), data = grca)
grcaALL
grcaVEG <- coxme(Surv(DaysActive, Fate) ~ SmallStems_Ha + NestHeight 
                 + CanopyCover + Coverage + (1|Site), data = grca)
grcaVEG
grcaDIST <- coxme(Surv(DaysActive, Fate) ~ Dist_toTrailLog
                  + (1|Site), data = grca)
grcaDIST
grcaSEAS <- coxme(Surv(DaysActive, Fate) ~ DOY+ (1|Site), data = grca)
grcaSEAS
grcaNULL <- coxme(Surv(DaysActive, Fate) ~ 1 + (1|Site), data = grca)
grcaNULL

# testing AIC for grca
AIC(grcaALL, grcaVEG, grcaDIST,grcaSEAS, grcaNULL)








