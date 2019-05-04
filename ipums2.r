# SETUP & IMPORT ====

# detach all non-base packages
lapply(paste('package:',names(sessionInfo()$otherPkgs),sep=""),
       detach, character.only=TRUE, unload=TRUE)

library(haven)
library(ipumsr)
library(tidyverse)


# import & back up
ddi <- read_ipums_ddi("sources/ipums/usa_00014.xml")
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
    incearn, inctot, empstat, hcovany, hcovpub
  )

View(data[sample(1:nrow(data) , 10, replace = F), ])



# FACTORIZE ====

facs <- data %>% 
  mutate(
    statefip = as.integer(statefip),
    countyfip = as.integer(countyfip),
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
    inctot = lbl_clean(inctot) %>% as_factor(),
    empstat = lbl_clean(empstat) %>% as_factor(),
    hcovany = lbl_clean(hcovany) %>% as_factor(),
    hcovpub = lbl_clean(hcovpub) %>% as_factor()
  )

View(facs[sample(1:nrow(facs) , 10, replace = F), ])



# INVESTIGATE FACTORS ====

summary(facs$metro)
# no missings, but meanings not necessariy clear

summary(facs$gq)
summary(facs$gqtype)
# count on institutional category matches on each
# code either as binary for institutional, exlude from vep

summary(facs$age)
# one coded category @ 90, but prob means 90 -- use as integer

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
  mutate(age = as.integer(age)) %>% 
  filter(age >= 3 & age <= 24, gq != "Group quarters--Institutions") %>% 
  select(school) %>%
  summary.factor()
# still missing 102245 total in the "school age" 3-24 range for HDI calculation
facs %>%
  mutate(age = as.integer(age)) %>% 
  filter(age >= 4 & age <= 24, gq != "Group quarters--Institutions") %>% 
  select(school) %>%
  summary.factor()
# none missing 4-24, so missings 3-24 must be 3yo's not in school
# impute: "N/A" -> "No, not in school"


summary(facs$educd)
# same 299970 missing as school
facs %>%
  mutate(age = as.integer(age)) %>% 
  filter(age >= 25) %>% 
  select(educd) %>% 
  summary.factor()
# none missing for over 25s.

# vectors of each category for recoding
levels(facs$educd)
vnohs <- levels(facs$educd)[1:16]
vhsplus <- levels(facs$educd)[17:21]
vbacc <- levels(facs$educd)[22]
vgrad <- levels(facs$educd)[23:25]




# check TOTAL income special codes:
# 0 = no earnings & & 1 = $1 or break even
length(which(facs$incearn == "No earnings"))  # 4477222
length(which(facs$incearn == 
               "$1 or break even (2000, 2005-2007 ACS and PRCS)"))  # 914
sum(which(is.na(facs$incearn)))
# since these are 0 & 1 (& 1's are few), can just convert to numeric


# check TOTAL income special codes:
# -009995 = -$9,900 (1980)
# -000001 = Net loss (1950)
# 0000000 = None
# 0000001 = $1 or break even (2000, 2005-onward ACS and PRCS)
# 9999999 = N/A

length(which(facs$inctot == "-$9,900 (1980)")) # none
length(which(facs$inctot == "Net loss (1950)")) # 25: INVESTIGATE
length(which(facs$inctot == "None"))# 956629
length(which(facs$inctot == 
               "$1 or break even (2000, 2005-onward ACS and PRCS)")) # 1429
length(which(facs$inctot == "N/A")) # 1646447: INVESTIGATE


facs %>% 
  filter(inctot == "Net loss (1950)") %>% 
  View()
# all had -1 in incearn, which is the numeric for this code
# convert to numeric

facs %>% 
  mutate(age = as.integer(age)) %>% 
  filter(
    inctot == "N/A",
    age < 16
  ) %>% 
  nrow()
# all N/A's are under 16 -> recode to proper NA
# (< 16 will be excluded in median calculations anyway)

  

summary(facs$empstat)
# 1768053 missing. many prob < 16, institutionalized, etc
facs %>% 
  mutate(age = as.integer(age)) %>% 
  filter(age >= 16, gq != "Group quarters--Institutions") %>% 
  select(empstat) %>% 
  summary()
# 119855 missing, check bu school status
facs %>% 
  mutate(age = as.integer(age)) %>% 
  filter(
    age >= 16,
    gq != "Group quarters--Institutions",
    school != "Yes, in school"
  ) %>% 
  select(empstat) %>% 
  summary()
# 2342 missing, check possible retirees, other group quarters (group homes?)
facs %>% 
  mutate(age = as.integer(age)) %>% 
  filter(
    age >= 16,
    age <= 65,
    gq != "Group quarters--Institutions",
    gq != "Other group quarters",
    school != "Yes, in school"
  ) %>% 
  select(empstat) %>% 
  summary()
# 2334 missing. expand age range to 18-70
facs %>% 
  mutate(age = as.integer(age)) %>% 
  filter(
    age >= 18,
    age <= 70,
    gq != "Group quarters--Institutions",
    gq != "Other group quarters",
    school != "Yes, in school"
  ) %>% 
  select(empstat) %>% 
  summary()
# none missing.
# impute "Not in labor force" for all NA's. All are either
  # under 18
  # over 70
  # in school
  # institutionalized
  # in nursing homes, group homes, etc.
# all reasonably could/would have gotten filled/coded as N/A


summary(facs$hcovany)
# none missing

summary(facs$hcovpub)
# none missing







# RECODE & REPLACE ====

recode <- data %>% 
  mutate(
    # plain numeric state (& county if possible) FIPS
    statefip = as.integer(statefip),
    countyfip = as.integer(countyfip),
    # factor (string) state names
    state = lbl_clean(state) %>% as_factor(),
    # convert metro to 3-part ubranicity
    urban = lbl_clean(metro) %>% as_factor() %>% 
      fct_collapse(
        rural = c(
          "Metropolitan status indeterminable (mixed)",
          "Not in metropolitan area"
        ),
        suburban = c(
          "In metropolitan area: Not in central/principal city",
          "In metropolitan area: Central/principal city status indeterminable (mixed)"
        ),
        urban = "In metropolitan area: In central/principal city"
      ),
    # 3-part group quarters/institutionalized status
    inst = lbl_clean(gq) %>% as_factor() %>% 
      fct_collapse(
        inst = "Group quarters--Institutions",
        ogq = "Other group quarters",
        hh = c(
          "Households under 1970 definition",
          "Additional households under 1990 definition",
          "Additional households under 2000 definition"
        )
      ),
    # simple numeric age
    age = as.integer(age),
    # simple binary factor sex, converting to lowercase
    sex = lbl_clean(sex) %>% as_factor() %>% 
      fct_recode(
        male = "Male",
        female = "Female"
      ),
    # race excluding hispanic
    race = lbl_clean(race) %>% as_factor() %>% 
      fct_collapse(
        white = "White",
        black = "Black/African American/Negro",
        aian = "American Indian or Alaska Native",
        aapi = c( "Chinese", "Japanese", "Other Asian or Pacific Islander" ),
        multi = c( "Two major races", "Three or more major races" ),
        other = "Other race, nec"
      ),
    # hispanic ethnicity binary status
    hispan = lbl_clean(hispan) %>% as_factor() %>% 
      fct_collapse(
        y = c( "Mexican", "Puerto Rican", "Cuban", "Other" ),
        n = "Not Hispanic"
      ),
    # replace hispanic ethnicity in primary race variable (perserving original)
    race2 =
      if_else(
        hispan == "y", "hisp", as.character(race)
      ) %>% 
      as_factor(),
    # binary school enrollemnt status
    # "N/A" imputed as not enrolled from above
    enroll = lbl_clean(school) %>% as_factor() %>% 
      fct_collapse(
        y = "Yes, in school",
        n = c( "No, not in school", "N/A" )
      ),
    # 4-way attainment, from vectors coded in "INVESTIGATE FACTORS" section 
    attain = lbl_clean(educd) %>% as_factor() %>% 
      fct_collapse(
        nohs = vnohs,
        hsplus = vhsplus,
        bacc = vbacc,
        grad = vgrad
      ),
    # basic numeric earnings
    earn = as.integer(incearn),
    # numeric total income, converting
    # labelled "N/A" (9999999) to logical NA
    income = 
      if_else(
        inctot == 9999999, NA_integer_, as.integer(inctot)
      ),
    # employment status, with N/A -> nilf, from investigatioon above
    emp = lbl_clean(empstat) %>% as_factor() %>% 
      fct_collapse(
        emp = "Employed",
        unemp = "Unemployed",
        nilf = c("N/A", "Not in labor force")
      ),
    # convert  health coverage variables to factors
    hcovany = lbl_clean(hcovany) %>% as_factor(),
    hcovpub = lbl_clean(hcovpub) %>% as_factor(),
    # create 3-way health coverage factor
    hcov =
      if_else(
        hcovany == "No health insurance coverage",
        "none",
        if_else(
          hcovpub == "With public health insurance coverage",
          "public",
          "private"
        )
      ) %>% as_factor()
  ) %>% 
  # select & order appropriately
  select(year, serial, statefip, countyfip, state, hhwt, pernum, perwt,
         age, sex, race, hispan, race2, enroll, attain, earn, income,
         emp, hcov, citizen, urban)



View(recode[sample(1:nrow(recode), 10),])


kjljasdflij




