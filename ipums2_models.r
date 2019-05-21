# SETUP & IMPORT ====

# detach all non-base packages
lapply(paste('package:',names(sessionInfo()$otherPkgs),sep=""),
       detach, character.only=TRUE, unload=TRUE)

# clear environment
rm(list = ls())


library(AER)
library(plm)
library(lfe)
library(stargazer)
library(tidyverse)

cnty <- read_csv("330_county_panel_08-16_subset.csv")

# MODELS ====

m1 <- lm(
  data = cnty, log(to.vep) ~ log(hdi)
)

m2 <- lm(
  data = cnty, log(to.vep) ~ log(hdi) + 
    blackprop + hispprop + aapiprop + 
    aianprop + multiprop + otherprop +
    uninsprop + ur.6mo + gini
)

m3 <- plm(
  data = cnty, log(to.vep) ~ log(hdi) +
    blackprop + hispprop + aapiprop + 
    aianprop + multiprop + otherprop +
    uninsprop + ur.6mo + gini,
  effect = "time",
  model = "within",
  index = c("year")
)



stargazer(m1, m2, m3, type = "text")
