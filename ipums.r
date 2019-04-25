# SETUP & IMPORT ====

# deatch all non-base packages
lapply(paste('package:',names(sessionInfo()$otherPkgs),sep=""),
       detach, character.only=TRUE, unload=TRUE)

library(haven)
library(ipumsr)
library(tidyverse)


# import & back up
ddi <- read_ipums_ddi("sources/ipums/usa_00004.xml")
data10 <- read_ipums_micro(ddi)
data10_bu <- data10

# check out
str(data10)
View(data10[sample(1:3061692, 12, replace = F), ])


# drop over/under detatiled columns
data10 <- data10 %>% 
  select(STATEFIP, YEAR, PERWT, SEX, AGE, RACE, HISPAN,
         CITIZEN, SCHOOL, EDUCD, INCEARN) %>% 
  rename(fips = STATEFIP,
         year = YEAR,
         perwt = PERWT,
         sex = SEX,
         age = AGE,
         race = RACE,
         hisp = HISPAN,
         cit = CITIZEN,
         school = SCHOOL,
         educ = EDUCD,
         income = INCEARN)


# VALUE LABEL EXPLORATION (SKIP) ====

# # which have value labels
# data10 %>% select_if(is.labelled)
# 
# 
# # apply labels to create factors
# factors <- data10 %>% 
#   mutate(
#     state = as_factor(lbl_clean(fips)), # state name as factor
#     fips = as.numeric(fips), # plain numeric fips code
#     agefct = as_factor(lbl_collapse(age, ~.lbl)), # get coded values (others NA)
#     sex = as_factor(lbl_clean(sex)), # generic factor
#     race = as_factor(lbl_clean(race)), # generic factor
#     hisp = as_factor(lbl_clean(hisp)), # generic factor
#     cit = as_factor(lbl_clean(cit)), # generic factor
#     school = as_factor(lbl_clean(school)), # generic factor
#     educ = as_factor(lbl_clean(educ)), # generic factor
#     incfct = as_factor(lbl_collapse(income, ~.lbl)) # get coded (others NA)
#   )
# 
# # find test labels for special codes in continuous numeric variables
# levels(factors$agefct) # probably ok to just leave as-is, convert to numeric
# levels(factors$incfct)
# 
# 
# # other facotr levels
# levels(factors$state) # Ok
# levels(factors$sex) # Ok
# levels(factors$race) # combine Chi, Japn, & Other As/PI;  2+ & 3+
# levels(factors$hisp) # make binary?
# levels(factors$school) # make binary, w/ N/A == NA
#   summary(factors$school, na.rm = F)
# levels(factors$educ)
#   summary(factors$educ, na.rm = F)
#   # 1 [N/A] == NA
#   # 2-16 == less than hs
#   # 17-21 == hs grad, some coll, assoc.
#   # 22 == bachelor's
#   # 23-25 = grad/professional
  


# RECODE & REPLACE ====

recode <- data10 %>% 
  mutate(
    state = as_factor(lbl_clean(fips)), # state name as factor
    fips = as.numeric(fips), # plain numeric fips code
    sex = as_factor(lbl_clean(sex)), # generic factor
    age = as.numeric(age),
    race = lbl_relabel(
      race,
      lbl(4, "Asian or Pacific Islander") ~.val %in% 4:6,
      lbl(7, "Other race") ~.val == 7,
      lbl(8, "More than one race") ~.val %in% 8:9
    ) %>% lbl_clean() %>% as_factor(),
    hisp = lbl_relabel(
      hisp,
      lbl(0, "No") ~.val == 0,
      lbl(1, "Yes") ~.val %in% 1:4
    ) %>% lbl_clean() %>% as_factor(),
    race2 = ifelse(hisp == "Yes",
                   "Hispanic of any race", 
                   as.character(race)) %>% 
      as_factor(),
    cit = lbl_relabel(
      cit,
      lbl(1, "Yes") ~.val %in% 0:2,
      lbl(5, "No") ~.val %in% 3:5
    ) %>% lbl_clean() %>% as_factor(),
    cit = ifelse(cit == "Yes", 1, 0),
    school = lbl_na_if(school, ~.lbl == "N/A") %>% 
      lbl_clean() %>% as_factor(),
    school = ifelse(school == "Yes, in school", 1, 0),
    educ = lbl_relabel(
      educ,
      lbl(0, "N/A") ~.val %in% 0:1 | .val == 999,
      lbl(2, "Less than HS") ~.val %in% 2:61,
      lbl(62, "HS/GED/Assoc/Some") ~.val %in% 62:100,
      lbl(101, "Bacc/etc.") ~.val %in% 101:113,
      lbl(114, "Mast/Doc/Prof") ~.val %in% 114:116
    ),
    educ = lbl_na_if(educ, ~.lbl == "N/A") %>% 
      lbl_clean() %>% as_factor(),
    income = lbl_na_if(income, ~.lbl == "N/A") %>%
      lbl_clean() %>% as.numeric()
  )

# reorder
recode <- recode[, c(1, 12, 2:6, 13, 7:11)]

# sample -- looks good
View(recode[sample(1:3061692, 12, replace = F), ])




# state groups ====

state <- recode %>% 
  zap_ipums_attributes() %>% 
  group_by(state) %>% 
  mutate(
    poptotal = sum(perwt),
    popu18 = sum(perwt[age < 18]),
    pop25o = sum(perwt[age >= 25]),
    pop324 = sum(perwt[age >= 3 & age <= 24]),
    malepop = sum(perwt[sex == "Male"]),
    femalepop = sum(perwt[sex == "Female"]),
    whitepop = sum(perwt[race2 == "White"]),
    blackpop = sum(perwt[race2 == "Black/African American/Negro"]),
    hisppop = sum(perwt[race2 == "Hispanic of any race"]),
    asianpop = sum(perwt[race2 == "Asian or Pacific Islander"]),
    amindpop = sum(perwt[race2 == "American Indian or Alaska Native"]),
    multipop = sum(perwt[race2 == "More than one race"]),
    otherpop = sum(perwt[race2 == "Other race"]),
    noncit = sum(perwt[cit == 0]),
    enroll = sum(perwt[school == 1 & age >= 3 & age <= 24]),
    nohs = sum(perwt[educ == "Less than HS" & age >= 25]),
    hs = sum(perwt[educ == "HS/GED/Assoc/Some" & age >= 25]),
    bacc = sum(perwt[educ == "Bacc/etc." & age >= 25]),
    grad = sum(perwt[educ == "Mast/Doc/Prof" & age >= 25])
  ) %>% 
  summarise(
    fips = mean(fips),
    year = mean(year),
    poptotal = mean(poptotal),
    malepop = mean(malepop),
    femalepop = mean(femalepop),
    popu18 = mean(popu18),
    pop25o = mean(pop25o),
    pop324 = mean(pop324),
    whitepop = mean(whitepop),
    blackpop = mean(blackpop),
    hisppop = mean(hisppop),
    asianpop = mean(asianpop),
    amindpop = mean(amindpop),
    multipop = mean(multipop),
    otherpop = mean(otherpop),
    noncit = mean(noncit),
    enroll = mean(enroll),
    nohs = mean(nohs),
    hs = mean(hs),
    bacc = mean(bacc),
    grad = mean(grad)
  )




# process intensive medians/means ====

# Also, for some reason, subsetting by age wasn't working in the main pipe
# chain, especially in median() and mean(), so have to do them separately.

# median income, converted to 2016 dollars (June-June, BLS CPI Inflation Calc.)
incmed <- recode %>% 
  group_by(fips) %>%
  filter(age > 15 & income > 1) %>% 
  mutate(
    incmed = median(
      rep(
        income, times = perwt
      ),
      na.rm = T
    ),
    incmed = incmed * 1.1058
  ) %>% 
  summarise(
    incmed = mean(incmed)
  )

# mean income
incmean <- recode %>% 
  group_by(fips) %>% 
  filter(age > 15 & income > 1) %>% 
  mutate(
    incmean = mean(
      rep(
        income, times = perwt
      ),
      na.rm = TRUE
    ),
    incmean = incmean * 1.1058
  ) %>% 
  summarise(
    incmean = mean(incmean)
  )


# per-capita income
incpc <- recode %>% 
  group_by(fips) %>% 
  mutate(
    incpc = sum(
      rep(
        income, times = perwt
      ),
      na.rm = T
    ) / sum(perwt),
    incpc =  incpc * 1.1058
  ) %>% 
  summarise(
    incpc = mean(incpc)
  )


# median age
medage <- recode %>% 
  group_by(fips) %>% 
  mutate(
    medage = median(rep(age, times = perwt))
  ) %>% 
  summarise(
    medage = mean(medage)
  )


# mean age
meanage <- recode %>% 
  group_by(fips) %>% 
  mutate(
    meanage = mean(rep(age, times = perwt))
  ) %>% 
  summarise(
    meanage = mean(meanage)
  )



# merge
state <- 
Reduce(
  function(x, y) merge(x, y, by = "fips", all = TRUE),
  list(state, incmed, incmean, incpc, medage, meanage)
)




# clean up & write out ====

rm(list = setdiff(ls(), "state"))

save(state, file = "ipums10.Rdata")
write_csv(state, "ipums10.csv")
