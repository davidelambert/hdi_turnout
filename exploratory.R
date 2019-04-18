library(sf)
library(tigris)
library(tidycensus)
library(leaflet)
library(Hmisc)
library(psych)
library(stringr)
library(plm)
library(lmtest)
library(clubSandwich)
library(lfe)
library(tidyverse)


# get all versions
load("acs_.Rdata")
load("fuckak_.Rdata")

# EXPLORATORY =====

describe(fuckak$hdi)


# histogram w/ eqivalent normal & boxplot
# close to normal, w/ a few outliers
hdinorm <- rnorm(3108, mean = mean(fuckak$hdi), sd = sd(fuckak$hdi))

ggplot(data = fuckak, mapping = aes(x = hdi)) +
  geom_histogram(bins = 51, mapping = aes(y = ..density..),
                 fill = "grey70", color = "white") +
  geom_density(color = "orange", size = 1.2) +
  geom_density(mapping = aes(x = hdinorm),
               color = "seagreen", size = 1.2) +
  geom_abline(slope = 0, intercept = 0, size = 1, color = "grey80") +
  theme_minimal()

ggplot() +
  geom_boxplot(data = fuckak, mapping = aes(y = hdi)) + 
  coord_flip() + theme_minimal()






# weakish in acs
cor(acs$gini, acs$turnout)
# much strionger in fuckak
cor(fuckak$gini, fuckak$turnout)
# very weak in ACS
cor(acs$hdi, acs$turnout)
# STRONG (by my standards! in fuckak
cor(fuckak$hdi, fuckak$turnout)

# moderate, negative - investigate?
cor(acs$hdi, acs$gini)
# about the same
cor(fuckak$hdi, fuckak$gini)

fuckak %>% ggplot(aes(x = hdi, y = gini)) +
  geom_point(shape = 16,
             alpha = .2,
             size = 3,
             stroke = 0,
             color = "grey70") +
  geom_smooth(se = FALSE, color = "orange", size = 1.2, method = "loess") +
  geom_smooth(se = FALSE, color = "seagreen", size = 1.2, method ="lm") +
  theme_minimal()





# clean hdi outliers ===========
  # retrieve outliers
  hdiout <- boxplot(fuckak$hdi, plot = FALSE)$out
  
  # trim outliers
  hditrim <- fuckak[-which(fuckak$hdi %in% hdiout), ]
  
  # correlations
  cor(fuckak$hdi, fuckak$turnout)
  cor(hditrim$hdi, hditrim$turnout)
  # actually ***slightly*** weaker, but no bigs
  
  # scatterplots
  fuckak %>% ggplot(mapping = aes(x = hdi, y = turnout)) +
    geom_point(shape = 16,
               alpha = .2,
               size = 4,
               stroke = 0,
               color = "grey70") +
    geom_smooth(se = FALSE, color = "orange", size = 1.5, method = "loess") +
    geom_smooth(se = FALSE, color = "seagreen", size = 1.5, method ="lm") +
    scale_x_continuous(breaks = c(0, 2.5, 5.0, 7.5, 10),
                       minor_breaks = c(1.25, 3.75, 6.25, 8.75),
                       limits = c(0,10)) +
    theme_minimal() +
    labs(title = "HDI & 2016 Turnout by County",
         subtitle = "Excluding Alaska and Hawaii",
         caption = "Sources: American Community Survey 2016 5-year Estimates, Institute for Health Metrics & Evaluation, MIT Election Data Lab.
                    HDI modified from Social Science Research Council's American HDI methodology.",
         x = "HDI", y = "Turnout")

  
  hditrim %>% ggplot(mapping = aes(x = hdi, y = turnout)) +
    geom_point(shape = 16,
               alpha = .2,
               size = 4,
               stroke = 0,
               color = "grey70") +
    geom_smooth(se = FALSE, color = "orange", size = 1.5, method = "loess") +
    geom_smooth(se = FALSE, color = "seagreen", size = 1.5, method ="lm") +
    scale_x_continuous(breaks = c(0, 2.5, 5.0, 7.5, 10),
                       minor_breaks = c(1.25, 3.75, 6.25, 8.75),
                       limits = c(0,10)) +
    theme_minimal() +
    labs(title = "HDI & 2016 Turnout by County",
         subtitle = "Excluding Alaska, Hawaii, and HDI outliers",
         caption = "Sources: American Community Survey 2016 5-year Estimates, Institute for Health Metrics & Evaluation, MIT Election Data Lab.
                    HDI modified from Social Science Research Council's American HDI methodology.",
         x = "HDI", y = "Turnout")
  
  
  
  

  
# basic models ====

# simple OLS
m1.0 <- lm(turnout ~ hdi, data = fuckak)
m2.0 <- lm(turnout ~ hdi, data = hditrim)
summary(m1.0)
summary(m2.0)


# add median age
m1.1 <- lm(turnout ~ hdi + medage, data = fuckak)
m2.1 <- lm(turnout ~ hdi + medage, data = hditrim)
summary(m1.1)
summary(m2.1)



# 2012 Turnout ====  

# prior_vars <- c(# Total Pop, Under 18, Non-Citizen 18+
#                 "B01001_001", "B09001_001", "B16008_046")


# pull from census API
# pop12 <- get_acs(geography = "county", variables = prior_vars, 
#                  geometry = FALSE, year = 2012,
#                  survey = "acs5", output = "wide")


# write raw data pull
# write.csv(pop12, file = "pop12_pull.csv")


# ALTERNATIVELY: load from saved pull
pop12 <- read_csv("pop12_pull.csv")
pop12_bu <- pop12

pop12 <- pop12 %>% 
  rename(county_full = NAME,
         poptotal12 = B01001_001E,
         popu1812 = B09001_001E,
         noncit12 = B16008_046E) %>% 
  mutate(fips = as.numeric(GEOID),
         vap12 = poptotal12 - popu1812 - noncit12) %>% 
  select(GEOID, fips, county_full, poptotal12, popu1812, noncit12, vap12) %>% 
  separate(county_full, sep = ", ", into = c("county_only", "state_only"),
           remove = FALSE, extra = "warn", fill = "warn") %>% 
  filter(state_only != "Alaska" & 
          state_only != "Hawaii" & 
          state_only != "Puerto Rico")

# load vote totals from MIT
load("sources/countypres_2000-2016.RData")

# select 2012 (also only need 1 party)
votes12 <- x %>% 
  filter(year == 2012 & party == "democrat") %>% 
  mutate(fips = as.numeric(FIPS)) %>% 
  select(fips, totalvotes) %>% 
  rename(totalvotes12 = totalvotes)

# join & create turnout
pop12 <- pop12 %>% 
  left_join(votes12, by = "fips")  %>% 
  mutate(turnout12 = totalvotes12 / vap12)
  
# check out
describe(pop12$turnout12)
# some are > 1, like 2016
View(pop12[which(pop12$turnout12 > 1), ])
# probably also due to ACS mis-estimates in fairly low-pop counties
# topcode at 99%, like 2012
pop12$turnout12[pop12$turnout12 > 1] <- .99
describe(pop12$turnout12)
# NOTE: leave McPherson Co, NE at 100%, plausible for 291 voters.....

# just fips & turnout for joining
to12 <- pop12 %>%
  select(fips, turnout12)

# join to main datasets (untrimmed, trimmed)
fuckak <- left_join(fuckak, to12, by = "fips")
hditrim <- left_join(hditrim, to12, by = "fips")


# clean up
rm(list = c("pop12", "pop12_bu", "to12", "votes12", "x"))








# Prior turnout models ====
m1.2 <- lm(turnout ~ hdi + turnout12, data = fuckak)
m2.2 <- lm(turnout ~ hdi + turnout12, data = hditrim)
m1.3 <- lm(turnout ~ hdi + medage + turnout12, data = fuckak)
m2.3 <- lm(turnout ~ hdi + medage + turnout12, data = hditrim)
summary(m1.2)
summary(m2.2)
summary(m1.3)
summary(m2.3)



# race/ethnicity ====

# race_vars <- c("B01001_001", "B03002_001", "B03002_002", "B03002_003",
#                "B03002_004", "B03002_005", "B03002_006", "B03002_007",
#                "B03002_008", "B03002_009", "B03002_010", "B03002_011",
#                "B03002_012")

# acs pull - alternatively load race_pull.csv, created in testrace.R, as below
# race <- get_acs(geography = "county", variables = race_vars, 
#                geometry = FALSE, year = 2016,
#                survey = "acs5", output = "wide")

# from previously pulled data - use ONLY this or the acs pull above
race <- read_csv("race_pull.csv")
race_bu <- race

race <- race %>%
  rename(county_state = NAME,
         total = B01001_001E,
         totalcheck = B03002_001E,
         nothilat = B03002_002E,
         white = B03002_003E,
         black = B03002_004E,
         amind = B03002_005E,
         asian = B03002_006E,
         hawpi = B03002_007E,
         other = B03002_008E,
         multi = B03002_009E,
         multi_other = B03002_010E,
         multi_noother = B03002_011E,
         hilat = B03002_012E
  ) %>% 
  mutate(fips = as.numeric(GEOID),
         white_prop = white / total,
         black_prop = black / total,
         amind_prop = amind / total,
         asian_prop = asian / total,
         hawpi_prop = hawpi / total,
         other_prop = other / total,
         multi_prop = multi / total,
         hilat_prop = hilat / total
  ) %>% 
  separate(county_state, sep = ", ", into = c("county", "state"),
           remove = FALSE, extra = "warn", fill = "warn") %>% 
  select(GEOID, fips, state, county, county_state, total, totalcheck, nothilat,
         white, white_prop, black, black_prop, hilat, hilat_prop,
         asian, asian_prop, amind, amind_prop, hawpi, hawpi_prop,
         other, other_prop, multi, multi_prop, multi_other, multi_noother
         ) 


# do we want to exclude/combine anything?

  # some counties have substantial Native American Pops - KEEP!
  length(which(race$amind_prop > .1 & race$state != "Alaska"))
  View(race[which(race$amind_prop > .1 & race$state != "Alaska"),])
  
  # only a few counties outside HI & AK have higher than 2%
  # including some possible miscodes - Childress county TX????
  # may combine with "other" ? or "asian"?
  View(race[which(race$hawpi_prop > .01 & 
                  race$state != "Hawaii" &
                  race$state != "Alaska"),])
  
  # several counties over 10%, some higher than 20%,
  # including populous counties -- KEEP as is!!!!!
  View(race[which(race$asian_prop > .1 & 
                  race$state != "Hawaii" &
                  race$state != "Alaska"),])
  
  # 73% of counties have multiracial proportions over 1%
  # 3% have proportions over 5%
  # KEEP as-is for now.
  # MAY want to subtract those including "other"? (probably not)
  View(race[which(race$multi_prop > .01 & 
                  race$state != "Hawaii" &
                  race$state != "Alaska"),])
  length(which(race$multi_prop > .01)) / nrow(race)
  length(which(race$multi_prop > .02)) / nrow(race)
  length(which(race$multi_prop > .03)) / nrow(race)
  length(which(race$multi_prop > .04)) / nrow(race)
  length(which(race$multi_prop > .05)) / nrow(race)
  
  
  
  # only a few with > even 1/2 percent.
  View(race[which(race$other_prop > .01 & 
                  race$state != "Hawaii" &
                  race$state != "Alaska"),])
  View(race[which(race$other_prop > .005 & 
                    race$state != "Hawaii" &
                    race$state != "Alaska"),])
  
  
# VERDICT:
# combine "multi", "other" and "hawaiin/pacific islander into "other"
# also create "non-white" category
race2 <- race %>% 
  filter(state != "Alaska" &
         state != "Hawaii" &
         state != "Puerto Rico") %>% 
  mutate(other = other + multi + hawpi,
         other_prop = other / total,
         nonwh_prop = black_prop + hilat_prop + asian_prop +
                      amind_prop + other_prop,
         total = white_prop + black_prop + hilat_prop +
                 asian_prop + amind_prop + other_prop) %>% 
  select(fips, white_prop, black_prop,  hilat_prop,
         asian_prop, amind_prop, other_prop, nonwh_prop)

# verify that proportions for each county sum to 1:
which((race2$white_prop + race2$black_prop + race2$hilat_prop +
       race2$asian_prop + race2$amind_prop + race2$other_prop) < .999999)

# also check white + nonwhite == 1
which((race2$white_prop + race2$nonwh_prop < .99999))


# joins
fuckak <- left_join(fuckak, race2, by = "fips")
hditrim <- left_join(hditrim, race2, by = "fips")



# clean up
rm(list = c("race", "race_bu", "race2"))



# race models ====

# moderate negative correlation:
cor(fuckak$nonwh_prop, fuckak$hdi)

# Models WITHOUT prior turnout, but including nonwhite proportion of population
# rationale: negative correlation of nonwhite pop & hdi above
m1.4 <- lm(turnout ~ hdi + medage + nonwh_prop, data = fuckak)
m2.4 <- lm(turnout ~ hdi + medage + nonwh_prop, data = hditrim)

# NOTE: these models are almost definitely a mistake!!!!
# should cluster std. errors at state level to account for state election law
# rather than using lagged county turnout....
m1.5 <- lm(turnout ~ hdi + medage + nonwh_prop + turnout12, data = fuckak)
m2.5 <- lm(turnout ~ hdi + medage + nonwh_prop + turnout12, data = hditrim)

summary(m1.4)
summary(m2.4)
summary(m1.5)
summary(m2.5)


# clustering ====

# compute stata-style degress of freedom
G <- length(unique(fuckak$state))
N <- length(fuckak$state)
dfa <- (G/(G-1)) * (N - 1) / m1.1$df.residual

# create clustering variance-covariance matrix
state_cluster <- dfa * vcovHC(m1.1, type = "HC0", cluster = "group", adjust = T)


# Cluster hdi + medage models  
  # stata-style clustering
  coeftest(m1.1, vcov. = state_cluster)
  # unmodified clustering - VERY close SE's, t-stat's, etc.
  # in below will just use this method
  coeftest(m1.1, vcov. = vcovHC(m1.1, type = "HC0", cluster = "group"))
  # clustering via the popular lfe::felm
  # largest/most conservative SEs, but still significant
  m1.1.cl <- felm(turnout ~ hdi + medage | 0 | 0 | state, data = fuckak)
  summary(m1.1.cl)
  # regular standard errors
  sumamry(m1.1)
  
  # Clustering increases standard errors, as expeected
  # but still strongly significant
  
  

  #repeat for prior turnout models
  m1.2.cl <- felm(turnout ~ hdi + turnout12 | 0 | 0 | state, data = fuckak)
  m1.2.fe <- felm(turnout ~ hdi + turnout12 | state, data = fuckak)
  m1.2.both <- felm(turnout ~ hdi + turnout12 | state | 0 | state,
                    data = fuckak)
  coeftest(m1.1, vcov. = vcovHC(m1.2, type = "HC0", cluster = "group"))
  summary(m1.2.cl)
  summary(m1.2.fe)
  summary(m1.2.both)
  summary(m1.2)
  
  
  # repeat for median age + race models
  m1.4.cl <- felm(turnout ~ hdi + medage + nonwh_prop | 0 | 0 | state,
                  data = fuckak)
  m1.4.fe <- felm(turnout ~ hdi + medage + nonwh_prop | state, data = fuckak)
  m1.4.both <- felm(turnout ~ hdi + medage + nonwh_prop | state | 0 | state,
                    data = fuckak)
  coeftest(m1.1, vcov. = vcovHC(m1.4, type = "HC0", cluster = "group"))
  summary(m1.4.cl)
  summary(m1.4.fe)
  summary(m1.4.both)
  summary(m1.4)
 

# SPATIAL REGRESSION MODELS ====
library(spatialreg)
library(spdep)
nb <- poly2nb(fuckak)
lw <- nb2listw(nb, zero.policy = TRUE)
s1.0 <- lagsarlm(turnout ~ hdi, data = fuckak, lw, zero.policy = TRUE)
s1.1 <- lagsarlm(turnout ~ hdi + medage + white_prop,
                 data = fuckak, lw, zero.policy = TRUE)
summary(s1.0)
summary(s1.1)

# SAVE THESE RESULTS AS R OBJECTS CUZ THIS SHIT TAKES FOR GODDAMN EVER!!!
save(s1.0, file = "spatial_simple.Rdata")
save(s1.1, file = "spatial_multi.Rdata")

moran.mc()

  