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
  theme_minimal() +
  theme(
    axis.text.y = element_blank()
  )


# go with it!

sdfg

