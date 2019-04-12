# PULL 2016 ACS RACE & ETHNICITY ESTIMATES FROM CENSUS API ====

library(psych)
library(tidycensus)
library(tidyverse)


#              Total,        Total (race), NOT Hisp/Lat, White
race_vars <- c("B01001_001", "B03002_001", "B03002_002", "B03002_003",
#              Black,        AmInd/AKNat,  Asian,        Hawaiian/Pac Isl.    
               "B03002_004", "B03002_005", "B03002_006", "B03002_007",
#              Other,        multi,         multi&other, multiNOother    
               "B03002_008", "B03002_009", "B03002_010", "B03002_011",
#              Hispanic/Latino of any race (total)
               "B03002_012")

race_vars <- c("B01001_001", "B03002_001", "B03002_002", "B03002_003",
               "B03002_004", "B03002_005", "B03002_006", "B03002_007",
               "B03002_008", "B03002_009", "B03002_010", "B03002_011",
               "B03002_012")


race <- get_acs(geography = "county", variables = race_vars, 
               geometry = FALSE, year = 2016,
               survey = "acs5", output = "wide")

race_bu <- race

# write out raw data pull - only use if editing the vars above!
write.csv(race, file = "race_pull.csv")


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
  separate(county_state, sep = ", ", into = c("county_only", "state_only"),
           remove = FALSE, extra = "warn", fill = "warn") %>% 
  select(GEOID, fips, state_only, county_only, county_state,
         total, totalcheck, nothilat,
         white, white_prop, black, black_prop, hilat, hilat_prop,
         asian, asian_prop, amind, amind_prop, hawpi, hawpi_prop,
         other, other_prop, multi, multi_prop, multi_other, multi_noother
         )






# EXPLORING ====


describe(race$white_prop)
# average white proportion is 75%, w/ left skew & some @ 0, 1?

View(race[which(race$white_prop == 1 | race$white_prop == 0),])
# all white is 1 county in  Montana, all nonwhite is 1 in Puerto Rico

race %>% ggplot() +
  geom_histogram(aes(x = white_prop), bins = 101)

race %>% ggplot() +
  geom_histogram(aes(x = white_prop^2), bins = 101)

sample <- race[sample(1:nrow(race), 1000, replace = FALSE),]
sample %>% ggplot() +
  geom_histogram(aes(x = white_prop),
                 bins = 101)
