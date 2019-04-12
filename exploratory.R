library(sf)
library(tigris)
library(tidycensus)
library(leaflet)
library(Hmisc)
library(psych)
library(stringr)
library(tidyverse)
library(plm)
library(clubSandwich)
options(tigris_class = "sf")
options(tigris_use_cache = TRUE)


# get all versions
load("acs_.Rdata")
load("fuckak_.Rdata")

# EXPLORATORY =====

hdistats <- describe(fuckak$hdi)
hdistats # whoa! zero's




# histogram w/ eqivalent normal & boxplot
# close to normal, w/ a few outliers
hdinorm <- rnorm(3108, mean = hdistats$mean, sd = hdistats$sd)

ggplot(data = fuckak, mapping = aes(x = hdi)) +
  geom_histogram(bins = 51, mapping = aes(y = ..density..),
                 fill = "navyblue", color = "white") +
  geom_density(color = "orange", size = 1.2) +
  geom_density(mapping = aes(x = hdinorm),
               color = "hotpink", size = 1.2) +
  geom_abline(slope = 0, intercept = 0, size = 1, color = "grey80") +
  theme_minimal()

ggplot() +
  geom_boxplot(data = acs, mapping = aes(y = hdi)) + 
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

fuckak %>% ggplot(mapping = aes(x = hdi, y = gini)) +
  geom_point(shape = 16,
             alpha = .2,
             size = 4,
             stroke = 0,
             color = "navyblue") +
  geom_smooth(se = FALSE, color = "orange", size = 1.2) +
  geom_smooth(se = FALSE, color = "hotpink", size = 1.2, method ="lm") +
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
               color = "navyblue") +
    geom_smooth(se = FALSE, color = "orange", size = 1.5, method = "loess") +
    geom_smooth(se = FALSE, color = "hotpink", size = 1.5, method ="lm") +
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
               color = "navyblue") +
    geom_smooth(se = FALSE, color = "orange", size = 1.5, method = "loess") +
    geom_smooth(se = FALSE, color = "hotpink", size = 1.5, method ="lm") +
    theme_minimal() +
    labs(title = "HDI & 2016 Turnout by County",
         subtitle = "Excluding Alaska, Hawaii, and HDI outliers",
         caption = "Sources: American Community Survey 2016 5-year Estimates, Institute for Health Metrics & Evaluation, MIT Election Data Lab.
                    HDI modified from Social Science Research Council's American HDI methodology.",
         x = "HDI", y = "Turnout")
  
  
  
  

  
# MODELS ====

# simple OLS
m1.0 <- lm(turnout ~ hdi, data = fuckak)
summary(m1.0)
m2.0 <- lm(turnout ~ hdi, data = hditrim)
summary(m2.0)


# multiple
m1.1 <- lm(turnout ~ hdi + medage, data = fuckak)
summary(m1.1)
m2.1 <- lm(turnout ~ hdi + medage, data = hditrim)
summary(m2.1)





# 2012 Turnout ====  

prior_vars <- c(# Total Pop, Under 18, Non-Citizen 18+
                "B01001_001", "B09001_001", "B16008_046")


# pull from census API
pop12 <- get_acs(geography = "county", variables = prior_vars, 
                 geometry = FALSE, year = 2012,
                 survey = "acs5", output = "wide")
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
to_fuckups <- which(pop12$turnout12 > 1)
View(pop12[to_fuckups,])
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








# Prior turnout models ====
m1.2 <- lm(turnout ~ hdi + turnout12, data = fuckak)
summary(m1.2)
m2.2 <- lm(turnout ~ hdi + turnout12, data = hditrim)
summary(m2.2)


# cluster at state
coef_test(m1.1, vcov = "CR2", cluster = fuckak$state, )

View(fuckak[which(is.na(fuckak$state)),])
