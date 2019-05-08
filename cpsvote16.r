# SETUP & IMPORT ====

# deatch all non-base packages
lapply(paste('package:',names(sessionInfo()$otherPkgs),sep=""),
       detach, character.only=TRUE, unload=TRUE)

library(haven)
library(ipumsr)
library(tidyverse)


# import & back up
ddi <- read_ipums_ddi("sources/ipums/cps_00001.xml")
voters <- read_ipums_micro(ddi)
voters_bu <- voters

# check out
str(voters)
View(voters[sample(1:nrow(voters), 10) , ])


# subet & lowercase
voters <- voters %>% 
  select(
    YEAR, COUNTY, STATECENSUS, WTFINL, VOSUPPWT, AGE, SEX, RACE, HISPAN,
    EMPSTAT, LABFORCE, CLASSWKR, WKSTAT, EDUC, SCHLCOLL, VOTED, VOREG,
    SERIAL, PERNUM
  ) %>% 
  `colnames<-`(tolower(colnames(.)))

View(voters[sample(1:nrow(voters), 10) , ])



# APPLY FACTOR LABELS ====

factors <- voters %>% 
  mutate(
    fips = lbl_clean(county) %>% as_factor(),
    state = lbl_clean(statecensus) %>% as_factor(),
    age = lbl_clean(age) %>% as_factor(),
    sex = lbl_clean(sex) %>% as_factor(),
    race = lbl_clean(race) %>% as_factor(),
    hispan = lbl_clean(hispan) %>% as_factor(),
    empstat = lbl_clean(empstat) %>% as_factor(),
    labforce = lbl_clean(labforce) %>% as_factor(),
    wkstat = lbl_clean(wkstat) %>% as_factor(),
    educ = lbl_clean(educ) %>% as_factor(),
    schlcoll = lbl_clean(schlcoll) %>% as_factor,
    voted = lbl_clean(voted) %>% as_factor(),
    voreg = lbl_clean(voreg) %>% as_factor()
  )
