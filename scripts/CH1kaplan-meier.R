# Kaplan-Meier Curves 
library(ggsurvfit)
library(ggplot2)
library(magrittr)
library(dplyr)
library(tidyverse)


# Read Data
cox24 <- read.csv("input/nestcox.csv")

# Build model
km.model <- survfit(Surv(DaysActive, Fate) ~ 1, data = cox24)

# Look at summary
summary(km.model)

# Plot
plot(km.model, conf.int=T, xlab = "Time (days)", ylab= "Survival Probability", main = "KM-Model", las=1)

# Plot each of the different species on the same graph
allbase <- survfit(Surv(DaysActive, Fate) ~ Species, data= cox24)
plot(allbase, conf.int=F,xlab = "Time (days)", ylab= "Survival Probability", main = 
       "Nest Survival by Species", lwd=3, col=c("darkseagreen", "cornflowerblue", "deeppink4", "darkgoldenrod2"))
legend(18, 0.95, legend=c("American Robin", "Gray Catbird", "Northern Cardinal", 
        "Yellow Warbler"), lty=1, lwd=2, col=c("darkseagreen", "cornflowerblue", "deeppink4", "darkgoldenrod2"), cex=0.6)
