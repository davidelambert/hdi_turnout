# SETUP & IMPORT ====

# detach all non-base packages
lapply(paste('package:',names(sessionInfo()$otherPkgs),sep=""),
       detach, character.only=TRUE, unload=TRUE)

library(ipumsr)
library(tidyverse)


# import & back up
ddi <- read_ipums_ddi("sources/ipums/usa_00015.xml")

start <- proc.time()
data <- read_ipums_micro(ddi)
end <- proc.time()

end - start # 43.35s on battery (NOT on batt saver)
print(object.size(data), units = "auto") # 2.3 Gb


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
    year, serial, statefip, countyfip, state,
    hhwt, pernum, perwt, gq, gqtype,
    age, sex, race, hispan, citizen, school, educd, 
    incearn, inctot, hcovany, hcovpub
  )

View(data[sample(1:nrow(data) , 10, replace = F), ])
print(object.size(data), units = "auto") # 1.6 Gb




# FACTORIZE ====
    
# convert labelled class to factors
start = proc.time()
facs <- data %>% 
  mutate(
    statefip = as.integer(statefip),
    countyfip = as.integer(countyfip),
    state = lbl_clean(state) %>% as_factor(),
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
    hcovany = lbl_clean(hcovany) %>% as_factor(),
    hcovpub = lbl_clean(hcovpub) %>% as_factor()
  )
end = proc.time()

end - start # 69.49s on battery

print(object.size(facs), units = "auto") # 1.5 Gb

View(facs[sample(1:nrow(facs) , 10, replace = F), ])


# INVESTIGATE FACTORS ====

summary(facs$gq)
summary(facs$gqtype)
# Institutional & non-instituitional group quarters counts match.
# code as: Household, Instituional, Other GQ

summary(facs$age)
# ONE categorical code @ 90, but category only applies to pre-1990 topcoding,
# so irrelevant here.
# CODE AS CONTINUOUS (use integer to save a little memory)

summary(facs$sex)
# no missings

summary.factor(facs$race)
# none missing
# combine Chinese, Japanese, Other Asian or Pacific Islander -> aapi
# combine Two major races, Three or more major races -> multi

summary(facs$hispan)
# none missing. code as logical hispanic/not
# & subtract from race, creating "hispaic of any race"

summary(facs$citizen)
# none truly missing. "N/A" = born in US
# code as logical citizen/noncitizen

summary(facs$school)
# 482364 total missing, filter for school age & not institutionalized
facs %>%
  mutate(age = as.integer(age)) %>% 
  filter(age >= 3 & age <= 24, gq != "Group quarters--Institutions") %>% 
  select(school) %>%
  summary.factor()
# still missing 164869 total in the "school age" 3-24 range for HDI calculation
facs %>%
  mutate(age = as.integer(age)) %>% 
  filter(age >= 4 & age <= 24, gq != "Group quarters--Institutions") %>% 
  select(school) %>%
  summary.factor()
# none missing 4-24, so missings 3-24 must be 3yo's not in school
# impute: "N/A" -> "No, not in school"

summary(facs$educd)
# same 482364 missing as school.
# prob identical subset of young/institutionalized.
# test for missing if over 25, since this is needed for HDI calculation,
# which test for education level 25+:
facs %>%
  mutate(age = as.integer(age)) %>% 
  filter(age >= 25) %>% 
  select(educd) %>% 
  summary.factor()
# none missing for over 25s.
# impute: N/A = less than hs.
# (will be ignored in education index calculations since all are <25)

# check personal earnings special codes:
# 0 = no earnings & & 1 = $1 or break even
length(which(facs$incearn == "No earnings"))  # 7701322
length(which(facs$incearn == 
               "$1 or break even (2000, 2005-2007 ACS and PRCS)"))  # none
sum(which(is.na(facs$incearn))) # none
# since these are 0 & 1 (& 1's are few), can just convert to numeric


# check TOTAL income special codes:
# -009995 = -$9,900 (1980)
# -000001 = Net loss (1950)
# 0000000 = None
# 0000001 = $1 or break even (2000, 2005-onward ACS and PRCS)
# 9999999 = N/A

length(which(facs$inctot == "-$9,900 (1980)")) # none
length(which(facs$inctot == "Net loss (1950)")) # none
length(which(facs$inctot == "None")) # 1708490 - already coded as 0
length(which(facs$inctot == 
               "$1 or break even (2000, 2005-onward ACS and PRCS)")) # none
length(which(facs$inctot == "N/A")) # 2708868: INVESTIGATE


facs %>% 
  mutate(age = as.integer(age)) %>% 
  filter(
    inctot == "N/A",
    age < 16
  ) %>% 
  nrow()
# == 2708868, so all "N/A" are under 16 & not in working age pop.
# recode to proper NA
# (< 16 will be excluded in median calculations anyway)

summary(facs$hcovany)
# none missing

summary(facs$hcovpub)
# none missing






# RECODES ====

# vectors of each category for recoding
levels(facs$educd)
vnohs <- levels(facs$educd)[1:16]
vhsplus <- levels(facs$educd)[17:21]
vbacc <- levels(facs$educd)[22]
vgrad <- levels(facs$educd)[23:25]

# main recoding
start = proc.time()
recode <- data %>% 
  mutate(
    # plain numeric state (& county if possible) FIPS
    statefip = as.integer(statefip),
    countyfip = as.integer(countyfip),
    # factor (string) state names
    state = lbl_clean(state) %>% as_factor(),
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
    # binary noncitizen status
    noncit = lbl_clean(citizen) %>% as_factor() %>% 
      fct_collapse(
        y = "Not a citizen",
        n = c("N/A", "Born abroad of American parents", "Naturalized citizen")
      ),
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
  select(state, year, serial, statefip, countyfip, hhwt, pernum, perwt,
         age, sex, race, hispan, race2, enroll, attain, earn, income,
         hcov, inst, noncit)
end = proc.time()

end - start # 40.97s on AC

print(object.size(recode), units = "auto") # 1.4 Gb


View(recode[sample(1:nrow(recode), 10),])




# COUNTY SUMMARIES ====

start = proc.time()
county16 <- recode %>%
  # remove additional attributes from IPUMS/haven labelling
  zap_ipums_attributes() %>% 
  # get full 5-digit fips for grouping
  mutate(
    statefip = str_pad(statefip, width = 2, side = "left", pad = "0"),
    countyfip = str_pad(countyfip, width = 3, side = "left", pad = "0"),
    fips = paste0(statefip, countyfip)
  ) %>% 
  filter(countyfip != "000") %>% 
  # grouping
  group_by(fips) %>% 
  # county-level summaries
  summarise(
    # population & age
    poptotal = sum(perwt),
    pop16o = sum(perwt[age >= 16]),
    popu18 = sum(perwt[age < 18]),
    pop25o = sum(perwt[age >= 25]),
    pop324 = sum(perwt[age >= 3 & age <= 24]),
    pop60o = sum(perwt[age >= 60]),
    noncit = sum(perwt[noncit == "y"]),
    noncit18o = sum(perwt[noncit == "y" & age >= 18]),
    #  quarters type
    household = sum(perwt[inst == "hh"]),
    institution = sum(perwt[inst == "inst"]),
    othergq = sum(perwt[inst == "ogq"]),
    # sex
    male = sum(perwt[sex == "male"]),
    female = sum(perwt[sex == "female"]),
    # race/ethnicity
    white = sum(perwt[race2 == "white"]),
    black = sum(perwt[race2 == "black"]),
    hisp = sum(perwt[race2 == "hisp"]),
    aapi = sum(perwt[race2 == "aapi"]),
    aian = sum(perwt[race2 == "aian"]),
    multi = sum(perwt[race2 == "multi"]),
    other = sum(perwt[race == "other"]),
    # education
    enroll = sum(perwt[enroll == "y" & age >= 3 & age <= 24]),
    nohs = sum(perwt[attain == "nohs"]),
    hsplus = sum(perwt[attain == "hsplus"]),
    bacc = sum(perwt[attain == "bacc"]),
    grad = sum(perwt[attain == "grad"]),
    # health coverage
    hcnone = sum(perwt[hcov == "none"]),
    hcpub = sum(perwt[hcov == "public"]),
    hcpvt = sum(perwt[hcov == "private"])
  )
end = proc.time()


end - start


# SHIT! Only 430 counties, since this is in the microdata FROM EACH YEAR
# of the 2012-2016 ACS.
# Fuck biscuits!

# Solutions:
# Try to "crosswalk" PUMA to counties, aggregating when necessary?
# go back to Census API pulls?
