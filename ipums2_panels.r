# SETUP & IMPORT ====

# deatch all non-base packages
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

# load data from ipums pulls
load("ipums2out.Rdata")
load("ipums2_recode.Rdata")

# STATE PROPORTIONS ====

# First add state abbreviations
st <- state.abb
st <- append(st, "DC", after = 7)
stname <- state.name
stname <- append(stname, "District of Columbia", after = 7)
states <- tibble(st, "state" = stname)
state <- merge(state, states, by = "state")
state <- state[, c(2,1,40,3:39)]
rm(states)

# create main demographics proportions
# new working df, saving state$ as backup
stpan <- state %>% 
  mutate(
    maleprop = malepop / poptotal,
    femaleprop = femalepop / poptotal,
    whiteprop = whitepop / poptotal,
    nonwhprop = 1 - whiteprop,
    blackprop = blackpop / poptotal,
    hispprop = hisppop / poptotal,
    aapiprop = aapipop / poptotal,
    aianprop = aianpop / poptotal,
    multiprop = multipop / poptotal,
    otherprop = otherpop / poptotal,
    oldprop = pop60o / poptotal,
    uninsprop = hcnone / poptotal,
    pvtinsprop = hcpvt / poptotal,
    metroprop = popmetro / poptotal
  )




