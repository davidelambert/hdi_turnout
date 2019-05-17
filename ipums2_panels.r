# SETUP & IMPORT ====

# deatch all non-base packages
lapply(paste('package:',names(sessionInfo()$otherPkgs),sep=""),
       detach, character.only=TRUE, unload=TRUE)

library(haven)
library(ipumsr)
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


# get list of FIPS codes for the 331 counties for which we have 3 full periods
ctlist <- unique(ctpan$fips)


# read in age-adjusted mortality data & filter only the 331 county subset
death <- 
  read_delim(
    "sources/Compressed Mortality, 1999-2016.txt", 
    delim = "\t",
    skip = 1,
    col_names = c("notes", "county", "fips", "year", "yr", "deaths",
                  "population", "crude", "aadr")
  ) %>% 
  filter(fips %in% ctlist)


# convert chracter age-adj death rate to numeric.
# generate reciprocal AADR, so that higher rates are numerically smaller
# to match the "bigger is better" meaning of education & income indices
# (also scale the reciprocal by 10,000 to make more legible)
death <- death %>%  
  mutate(
    aadr = as.numeric(aadr),
    death = 1/aadr * 10000
  ) %>% 
  select(year, fips, death)


# join to main dataset
ctpan <- ctpan %>% 
  left_join(death)

rm(death)






# HEALTH INDEX ====

# check out max's & min's
summary(subset(ctpan, year == 2008, select = death))
summary(subset(ctpan, year == 2012, select = death))
summary(subset(ctpan, year == 2016, select = death))
# overall min 8.9, overall max 23.0
# want to set goalposts so that medians of INDEX are close-ish to 5

deathmin <- 5
deathmax <- 22

ctpan <- ctpan %>% 
  mutate(
    health_index = ((death - deathmin) / (deathmax - deathmin)) * 10
  )

# descriptives
# medians are close to 5 both pooled and in individual years
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
  geom_hline(yintercept = 5, color = "orange", size = 1.3) +
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
  geom_hline(yintercept = 5, color = "orange", size = 1.3) +
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
  geom_hline(yintercept = 5, color = "orange", size = 1.3) +
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
  geom_hline(yintercept = 5, color = "orange", size = 1.3) +
  scale_y_continuous(breaks = c(2,3,4,5,6,7,8,9,10,11)) +
  theme_minimal() +
  theme(
    axis.text.y = element_blank(),
    plot.title = element_text(hjust = .05),
    strip.text = element_text(hjust = .05),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank()
  )

# the medians are quite a bit higher than 5. But the overall ModHDI, espically
# since it uses a geometric mean, will probably still be OK.


# INCOME INDEX ====

# remember these are already converted to 2016 $s
summary(subset(ctpan, year == 2008, select = incmed))
summary(subset(ctpan, year == 2012, select = incmed))
summary(subset(ctpan, year == 2016, select = incmed))
# SSRA goalposts should be ok


incmin <- 16009
incmax <- 67730


ctpan <- ctpan %>% 
  mutate(
    inc_index = ( (log(incmed) - log(incmin) ) / 
                  (log(incmax) - log(incmin) ) * 10
    )
  )


summary(subset(ctpan, year == 2008, select = inc_index))
summary(subset(ctpan, year == 2012, select = inc_index))
summary(subset(ctpan, year == 2016, select = inc_index))


# visualize
ctpan %>% ggplot() +
  geom_boxplot(aes(y = inc_index)) +
  coord_flip() +
  facet_wrap(~year, ncol = 1) +
  labs(title = "Income Index", y = "") +
  geom_hline(yintercept = 5, color = "orange", size = 1.3) +
  theme_minimal() +
  theme(
    axis.text.y = element_blank(),
    plot.title = element_text(hjust = .05),
    strip.text = element_text(hjust = .05),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank()
  )


# looks good!!



# HDI ====

ctpan <- ctpan %>% 
  mutate(hdi = (health_index * ed_index * inc_index) ^ (1/3) )


summary(subset(ctpan, year == 2008, select = hdi))
summary(subset(ctpan, year == 2012, select = hdi))
summary(subset(ctpan, year == 2016, select = hdi))


# boxplots
ctpan %>% ggplot() +
  geom_boxplot(aes(y = hdi)) +
  coord_flip() +
  facet_wrap(~year, ncol = 1) +
  labs(title = "Modified HDI", y = "") +
  geom_hline(yintercept = 5, color = "orange", size = 1.3) +
  theme_minimal() +
  theme(
    axis.text.y = element_blank(),
    plot.title = element_text(hjust = .05),
    strip.text = element_text(hjust = .05),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.grid.minor.x = element_blank()
  )

# Looks good


# ADD POOLING FOR HDI VIZ ====

# pooled mean hdi for each county
pool <- ctpan %>% 
  select(state, year, fips, hdi) %>% 
  group_by(state, fips) %>% 
  summarise_all(mean) %>%
  mutate(year = "Pooled") %>% 
  ungroup() %>% 
  select(year, state, fips, hdi)


# spot check first & last rows
summary(subset(ctpan, fips == "01003", select = hdi))
summary(subset(ctpan, fips == "55117", select = hdi))
# looks good


# subset main df
ctsub <- ctpan %>% 
  select(year, state, fips, hdi) %>% 
  mutate(year = as.character(year))


# row-bind
pool <- pool %>% 
  bind_rows(ctsub) %>% 
  arrange(state, fips, year)

rm(ctsub)



# HDI VIZ ====

# boxplots
pool %>% ggplot() +
  geom_boxplot(aes(y = hdi)) +
  coord_flip() +
  facet_wrap(~year, ncol = 1) +
  labs(title = "Modified HDI", y = "") +
  geom_hline(yintercept = 5, color = "orange", size = 1.3) +
  theme_minimal() +
  theme(
    axis.text.y = element_blank(),
    plot.title = element_text(hjust = .05),
    strip.text = element_text(hjust = .05),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank()
  )


# generate normal distribution based on pooled distribution
normpool <- rnorm(
  1324,
  mean = mean(pool$hdi[pool$year == "Pooled"]),
  sd = sd(pool$hdi[pool$year == "Pooled"])
)



# histograms
pool %>% 
  ggplot() +
  geom_histogram(bins = 33, mapping = aes(x = hdi, y = ..density..),
                 fill = "grey70", color = "white") +
  stat_density(aes(x = hdi, color = "Observed Density"),
               geom = "line", size = 1.2) +
  stat_density(aes(x = normpool, color = "Simulated Normal"), 
               geom = "line", size = 1.2) +
  scale_color_manual(values = c("seagreen", "orange"), name = "") +
  labs( title = "Modified HDI Distribution Density", x = "", y = "" )+
  facet_wrap(~year) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = .05),
    strip.text = element_text(hjust = .05),
    legend.position = "bottom"
    
  )





# AVERAGE UNEMPLOYMENT ====

# Calculate average unemployment over the
# May-Oct period preceeding each election

# read in data from BLS scrape (in cps16.r)
ur_orig <- read_csv("ur_07-16_331counties_monthly.csv")

# cleaning
ur <- ur_orig %>% 
  # create standardized date format YYYY-MM-DD & convert to Date type
  mutate(
    date = 
      paste(
        year,
        str_sub(period, start = 2, end = 3),
        "01",
        sep = "-"
      ) %>% as.Date()
  ) %>% 
  # rename unemployment rate something intuitive
  rename(urate = value) %>% 
  # filter only the presidential years & May-Oct
  filter(
    year %in% c("2008", "2012", "2016"),
    period %in% 
      c(
        paste0(
          "M",
          str_pad(
            as.character(seq(05,10,1)),
            2, 
            pad = "0"
          )
        )
      )
  ) %>% 
  # group by county & year
  group_by(year, fips) %>% 
  # summarise to mean 6-month unemployemnt by county by election year
  summarize(ur6mo = mean(urate)) %>% 
  arrange(fips, year)
  


sdfg


