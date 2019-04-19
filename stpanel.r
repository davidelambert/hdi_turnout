library(psych)
library(AER)
library(plm)
library(lfe)
library(tidycensus)
library(tidyverse)

# possible differences b/w avaialability in each year, but probably not
# for anything I'm interested in
vars16 <- load_variables(2016, "acs1", cache = T)
vars12 <- load_variables(2012, "acs1", cache = T)


# NOTE: 5-yr ESTIMATES OVERLAP, CONSIDERED INAPPROPRIATE FOR COMPARISON!
# SEE HERE:
# https://www.census.gov/programs-surveys/acs/guidance/estimates.html
# ARCHIVED:
# https://web.archive.org/web/20190418181537/https://www.census.gov/programs-surveys/acs/guidance/estimates.html
# AND HERE:
# https://www.census.gov/programs-surveys/acs/guidance/comparing-acs-data.html
# ARCHIVED:
# https://web.archive.org/web/20190418181630/https://www.census.gov/programs-surveys/acs/guidance/comparing-acs-data.html


# SINGLE-YEAR ACS PULL ====

stpan_vars <- c(# Total Pop, Under 18, Non-Citizen 18+, & Median Age,
                "B01001_001", "B09001_001", "B16008_046", "B01002_001",
                # Gini, median earnings, per capita income
                "B19083_001", "B20017_001", "B19301_001",
                # Attainment
                "B15003_017", "B15003_018", "B15003_019", "B15003_020",
                "B15003_021", "B15003_022", "B15003_023", "B15003_024",
                "B15003_025",
                # Pop over 3, Enrollment 
                "B14001_001","B14001_002",
                # Population under 25
                "B01001_003", "B01001_004", "B01001_005", "B01001_006", 
                "B01001_007", "B01001_008", "B01001_009", "B01001_010", 
                "B01001_027", "B01001_028", "B01001_029", "B01001_030", 
                "B01001_031", "B01001_032", "B01001_033", "B01001_034",
                # Race counts
                "B03002_001", "B03002_002", "B03002_003", "B03002_004",
                "B03002_005", "B03002_006", "B03002_007", "B03002_008",
                "B03002_009", "B03002_010", "B03002_011", "B03002_012")


# years to send to mapping function
years <- lst(2012, 2016)

# ALSO: many other variables, like mortality, etc, are not available at the
# Congressional District level, so reverting back to state level.

stpan <-
  map_dfr(
    years,
    ~ get_acs(
      geography = "state",
      variables = stpan_vars,
      year = .x,
      survey = "acs1",
      output = "wide"
    ),
    .id = "year"
  )

stpan_bu <- stpan

write_csv(stpan_bu, "stpan_pull.csv")


# CLEAN UP ====

# list to use in select()
keep <- c("GEOID", "year", "d16", "NAME", "poptotal", "noncit", "popu18",
          "popu25", "pop3o", "pop324", "vapacs", "medage", "incmed",
          "incpc", "gini", "hs", "ged", "some1", "some2", "assoc", "bacc",
          "mast", "prof", "phd", "enroll", "pop_check", "nothilat", "white",
          "black", "amind", "asian", "hawpi", "other", "multi", "hilat",
          "white_prop", "black_prop", "amind_prop", "asian_prop", "hawpi_prop",
          "other_prop", "multi_prop", "hilat_prop", "nonwh_prop")


stpan <- stpan %>%
  rename(
    poptotal = B01001_001E,
    popu18 = B09001_001E,
    noncit = B16008_046E,
    pop3o = B14001_001E,
    medage = B01002_001E,
    gini = B19083_001E,
    incmed = B20017_001E,
    incpc = B19301_001E,
    hs = B15003_017E,
    ged = B15003_018E,
    some1 = B15003_019E,
    some2 = B15003_020E,
    assoc = B15003_021E,
    bacc = B15003_022E,
    mast = B15003_023E,
    prof = B15003_024E,
    phd = B15003_025E,
    enroll = B14001_002E,
    pop_check = B03002_001E,
    nothilat = B03002_002E,
    white = B03002_003E,
    black = B03002_004E,
    amind = B03002_005E,
    asian = B03002_006E,
    hawpi = B03002_007E,
    other = B03002_008E,
    multi = B03002_009E,
    hilat = B03002_012E
  ) %>% 
  mutate(
    d16 = ifelse(stpan$year == "2016", 1, 0),
    popu25 = B01001_003E + B01001_004E + B01001_005E + B01001_006E + 
      B01001_007E + B01001_008E + B01001_009E + B01001_010E + 
      B01001_027E + B01001_028E + B01001_029E + B01001_030E + 
      B01001_031E + B01001_032E + B01001_033E + B01001_034E,
    pop324 = popu25 - (poptotal - pop3o),
    vapacs = poptotal - popu18 - noncit,
    white_prop = white / pop_check,
    black_prop = black / pop_check,
    amind_prop = amind / pop_check,
    asian_prop = asian / pop_check,
    hawpi_prop = hawpi / pop_check,
    other_prop = other / pop_check,
    multi_prop = multi / pop_check,
    hilat_prop = hilat / pop_check,
    nonwh_prop = 1 - white_prop
  ) %>% 
  select(keep) %>% 
  filter(NAME != "Puerto Rico")


# sort into panels
stpan <- arrange(stpan, GEOID, year)


# backup
stpan_bu2 <- stpan



# write out the clean version
write_csv(stpan, "stpan.csv")



# HEALTH INDEX ====





View(stpan[sample(1:102, 10, replace = F), c(1:3, 44:45)])

