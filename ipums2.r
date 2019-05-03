# SETUP & IMPORT ====

# deatch all non-base packages
lapply(paste('package:',names(sessionInfo()$otherPkgs),sep=""),
       detach, character.only=TRUE, unload=TRUE)

library(haven)
library(ipumsr)
library(vtable)
library(tidyverse)


# import & back up
ddi <- read_ipums_ddi("sources/ipums/usa_00013.xml")
data <- read_ipums_micro(ddi)
data_bu <- data

# check out
str(data)
View(data[sample(1:nrow(data) , 10, replace = F), ])


# subsset only used variables (& rename to lowercase)
data <- data %>% 
  `colnames<-`(tolower(colnames(.))) %>% 
  mutate(
    state = statefip
  ) %>% 
  select(
    year, serial, statefip, countyfip, state, metro,
    hhwt, pernum, perwt, gq, gqtype,
    age, sex, race, hispan, citizen, school, educd, 
    incearn, empstat, hcovany, hcovpub
  )

View(data[sample(1:nrow(data) , 10, replace = F), ])



# FACTOR EXPLORATION ====

facs <- data %>% 
  mutate(
    statefip = as.numeric(statefip),
    countyfip = as.numeric(countyfip),
    state = lbl_clean(state) %>% as_factor(),
    metro = lbl_clean(metro) %>% as_factor(),
    gq = lbl_clean(gq) %>% as_factor(),
    gqtype = lbl_clean(gqtype) %>% as_factor(),
    age = lbl_clean(age) %>% as_factor(),
    sex = lbl_clean(sex) %>% as_factor(),
    race = lbl_clean(race) %>% as_factor(),
    hispan = lbl_clean(hispan) %>% as_factor(),
    citizen = lbl_clean(citizen) %>% as_factor(),
    school = lbl_clean(school) %>% as_factor(),
    educd = lbl_clean(educd) %>% as_factor(),
    incearn = lbl_clean(incearn) %>% as_factor(),
    empstat = lbl_clean(empstat) %>% as_factor(),
    hcovany = lbl_clean(hcovany) %>% as_factor(),
    hcovpub = lbl_clean(hcovpub) %>% as_factor()
  )

View(facs[sample(1:nrow(facs) , 10, replace = F), ])


summary(facs$metro)
# no missings, but meanings not necessariy clear

summary(facs$gq)
summary(facs$gqtype)
# count on institutional category matches on each
# code either as binary for institutional, exlude from vep

summary(facs$age)
# one coded category @ 90, but prob means 90 -- use as numeric

summary(facs$sex)
# no missings

summary.factor(facs$race)
# none missing
# combine Chinese, Japanese, Other Asian or Pacific Islander -> aapi
# combine Two major races, Three or more major races -> multi

summary(facs$hispan)
# none missing. make binary & subtract 

summary(facs$citizen)
# none truly missing. "N/A" = born in US

summary(facs$school)
# 299790 total missing, filter for school age & not institutionalized
facs %>%
  mutate(age = as.numeric(age)) %>% 
  filter(age >= 3 & age <= 24, gq != "Group quarters--Institutions") %>% 
  select(school) %>%
  summary.factor()
# still missing 102245 total in the "school age" 3-24 range for HDI calculation
facs %>%
  mutate(age = as.numeric(age)) %>% 
  filter(age >= 4 & age <= 24, gq != "Group quarters--Institutions") %>% 
  select(school) %>%
  summary.factor()
# none missing 4-24, so missings 3-24 must be 3yo's not in school
# impute: "N/A" -> "No, not in school"


summary(facs$educd)
# same 299970 missing as school
facs %>%
  mutate(age = as.numeric(age)) %>% 
  filter(age >= 25) %>% 
  select(educd) %>% 
  summary.factor()
# none missing for over 25s.

summary.factor(facs$incearn)
levels(facs$incearn)
# only special codes are 0 = no earnings & & 1 = $1 or break even
# just convert to numeric

summary(facs$empstat)
# 1768053 missing. many prob < 16, institutionalized, etc
facs %>% 
  mutate(age = as.numeric(age)) %>% 
  filter(age >= 16, gq != "Group quarters--Institutions") %>% 
  select(empstat) %>% 
  summary()
# 119855 missing, check bu school status
facs %>% 
  mutate(age = as.numeric(age)) %>% 
  filter(
    age >= 16,
    gq != "Group quarters--Institutions",
    school != "Yes, in school"
  ) %>% 
  select(empstat) %>% 
  summary()
# 2342 missing, check possible retirees, other group quarters (group homes?)
facs %>% 
  mutate(age = as.numeric(age)) %>% 
  filter(
    age >= 16,
    age <= 65,
    gq != "Group quarters--Institutions",
    gq != "Other group quarters",
    school != "Yes, in school"
  ) %>% 
  select(empstat) %>% 
  summary()
# 2334 missing. impute "Not in labor force"


summary(facs$hcovany)
# none missing

summary(facs$hcovpub)
# none missing





dsfsfd


# RECODES ====