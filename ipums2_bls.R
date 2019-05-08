# SETUP & IMPORT ====

# detach all non-base packages
lapply(paste('package:',names(sessionInfo()$otherPkgs),sep=""),
       detach, character.only=TRUE, unload=TRUE)

library(haven)
library(ipumsr)
library(blsAPI)
library(blscrapeR)
library(psych)
library(AER)
library(plm)
library(lfe)
library(tidyverse)


ids <- search_ids(keyword = c("Unemployment", "Rate", "County"))


cnty08 <- get_bls_county()
df <- get_bls_county("May 2017")


guilford <- bls_api("LAUCN200950000000003", startyear = 2007)
