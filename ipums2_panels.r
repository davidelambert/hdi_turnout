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
# load("ipums2_recode.Rdata")
# lets just do counties for now
rm(state)



# COUNTY PROPORTIONS ====


ctpan <- county %>% 
  ungroup %>% 
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
  ) %>% 
  arrange(state, county, year)











# AGE-ADJ DEATH RATE ====

# YEARLY county-level life expectancy estimates are
# not available to my knowledge. Still following the
# basic premise of SSRC's American HDI, I'm substituting
# AGE-ADJUSTED MORTALITY from CDC WONDER. These are
# obviously NOT the same, but they appear to be close
# to inverse, as shown here:
# https://www.cdc.gov/nchs/data-visualization/mortality-trends/
# Also, the inverse age-adjusted relationship seems to
# correct for different age distributions, as described:
# http://freerangestats.info/blog/2018/05/31/life-expectancy.
# Will need to to use reciprocal, possibly scaled up by 1,000,
# so that increases relate to increases in HDI.


# get list of included couty fips codes
ctlist <- ctpan %>% 
  ungroup() %>% 
  filter(year == 2008) %>% 
  select(fips)
ctlist <- unique(ctlist$fips)


# read in data
death <- 
  read_delim(
    "sources/Compressed Mortality, 1999-2016.txt", 
    delim = "\t",
    skip = 1,
    col_names = c("notes", "county", "fips", "year", "yr", "deaths",
                  "population", "crude", "aadr")
  )


# filter only the 331 counties in the set
death <- death %>% 
  filter(fips %in% ctlist)


# drop "(Unreliable)" flags, get RECIPROCAL age-adj death rate, subset to merge
death <- death %>%  
  mutate(
    aadr = gsub("[^0-9.]", "", aadr) %>% as.numeric(),
    death = 1/aadr * 1000
  ) %>% 
  select(year, fips, death)


# join to main dataset
ctpan <- ctpan %>% 
  left_join(death)


rm(death)

# HEALTH INDEX ====

# check out max's & min's
summary(subset(ctpan, year == 2008, select = death))
# 0.89 - 1.91
summary(subset(ctpan, year == 2012, select = death))
# 0.94 - 2.05
summary(subset(ctpan, year == 2016, select = death))
# 0.93 - 2.3

deathmin <- 0.5
deathmax <- 2.5

ctpan <- ctpan %>% 
  mutate(
    health_index = ((death - deathmin) / (deathmax - deathmin)) * 10
  )

# descriptives
# meadian are close to 5, so obv > 10
summary(ctpan$health_index)
summary(subset(ctpan, year == 2008, select = health_index))
summary(subset(ctpan, year == 2012, select = health_index))
summary(subset(ctpan, year == 2016, select = health_index))

# visualize
# all have a few upper outliers, but not many
ctpan %>% ggplot() +
  geom_boxplot(aes(y = health_index)) +
  coord_flip() +
  facet_wrap(~year, ncol = 1) +
  labs(title = "Health Index", y = "") +
  theme_minimal() +
  theme(
    axis.text.y = element_blank(),
    plot.title = element_text(hjust = .05),
    strip.text = element_text(hjust = .05),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank()
  )


# go with it!



# ATTAINMENT INDEX ====

# gather attainment "score"
ctpan <- ctpan %>% 
  mutate(
    hsprop = (hsplus + bacc + grad) / pop25o,
    baccprop = (bacc + grad) / pop25o,
    gradprop = grad / pop25o,
    attain_score = hsprop + baccprop + gradprop
  )


# check for utility of SSRA's goalposts
summary(subset(ctpan, year == 2008, select = attain_score))
summary(subset(ctpan, year == 2012, select = attain_score))
summary(subset(ctpan, year == 2016, select = attain_score))
# SSRA goalposts should be ok

attmin <- 0.5
attmax <- 2

ctpan <- ctpan %>% 
  mutate(
   attain_index = ((attain_score - attmin) / (attmax - attmin)) * 10 
  )


# median a little high, but go with it
summary(subset(ctpan, year == 2008, select = attain_index))
summary(subset(ctpan, year == 2012, select = attain_index))
summary(subset(ctpan, year == 2016, select = attain_index))



# visualize
# all have a few upper outliers, but not many
ctpan %>% ggplot() +
  geom_boxplot(aes(y = attain_index)) +
  coord_flip() +
  facet_wrap(~year, ncol = 1) +
  labs(title = "Attainment Index", y = "") +
  theme_minimal() +
  theme(
    axis.text.y = element_blank(),
    plot.title = element_text(hjust = .05),
    strip.text = element_text(hjust = .05),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank()
  )




# ENROLLMENT INDEX ====

ctpan <- ctpan %>% 
  mutate(enrollprop = enroll / pop324)


# check out
summary(subset(ctpan, year == 2008, select = enrollprop))
summary(subset(ctpan, year == 2012, select = enrollprop))
summary(subset(ctpan, year == 2016, select = enrollprop))
# 0.6-0.95 should be ok

enrollmin <- 0.6
enrollmax <- 0.95


ctpan <- ctpan %>% 
  mutate(
    enroll_index = ( (enrollprop - enrollmin) / (enrollmax - enrollmin) ) * 10
  )


summary(subset(ctpan, year == 2008, select = enroll_index))
summary(subset(ctpan, year == 2012, select = enroll_index))
summary(subset(ctpan, year == 2016, select = enroll_index))
# looks ok


# visualize
ctpan %>% ggplot() +
  geom_boxplot(aes(y = enroll_index)) +
  coord_flip() +
  facet_wrap(~year, ncol = 1) +
  labs(title = "Enrollment Index", y = "") +
  theme_minimal() +
  theme(
    axis.text.y = element_blank(),
    plot.title = element_text(hjust = .05),
    strip.text = element_text(hjust = .05),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank()
  )



# OVERALL EDUCATION INDEX ====

ctpan <- ctpan %>% 
  mutate(ed_index = ((2/3) * attain_index) + ((1/3) * enroll_index))

summary(subset(ctpan, year == 2008, select = ed_index))
summary(subset(ctpan, year == 2012, select = ed_index))
summary(subset(ctpan, year == 2016, select = ed_index))
# a little higher than 5, but probably OK


# visualize
ctpan %>% ggplot() +
  geom_boxplot(aes(y = ed_index)) +
  coord_flip() +
  facet_wrap(~year, ncol = 1) +
  labs(title = "Education Index", y = "") +
  theme_minimal() +
  theme(
    axis.text.y = element_blank(),
    plot.title = element_text(hjust = .05),
    strip.text = element_text(hjust = .05),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank()
  )


sdfg


