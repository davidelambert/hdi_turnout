# SETUP & PULL ====

# detach all non-base packages
lapply(paste('package:',names(sessionInfo()$otherPkgs),sep=""),
       detach, character.only=TRUE, unload=TRUE)


library(psych)
library(AER)
library(plm)
library(lfe)
library(tidycensus)
library(tidyverse)

# possible differences b/w avaialability in each year, but probably not
# for anything I'm interested in
# vars16 <- load_variables(2016, "acs1", cache = T)






# ACS PULL ====

# variables to pull
vars <- c(# Total Pop, Under 18, Non-Citizen 18+, & Group Quarters,
                "B01001_001", "B09001_001", "B16008_046", "B26001_001",
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
                # Population 60 and over
                "B01001_018", "B01001_019", "B01001_020", "B01001_021", 
                "B01001_022", "B01001_023", "B01001_024", "B01001_025", 
                "B01001_042", "B01001_043", "B01001_044", "B01001_045", 
                "B01001_046", "B01001_047", "B01001_048", "B01001_049", 
                # Race counts
                "B03002_001", "B03002_002", "B03002_003", "B03002_004",
                "B03002_005", "B03002_006", "B03002_007", "B03002_008",
                "B03002_009", "B03002_010", "B03002_011", "B03002_012")


# years to send to mapping function
years <- lst(2012, 2016)

# get ACS 1-year for counties w/ pop >65K
cntpan <-
  map_dfr(
    years,
    ~ get_acs(
      geography = "county",
      variables = vars,
      year = .x,
      survey = "acs1",
      output = "wide"
    ),
    .id = "year"
  )

cntpan_bu <- cntpan

write_csv(cntpan, "cntpan.csv")


# APPARENTLY THERE'S NO FUCKING USEABLE DATA FOR 2008!!!!!!!
# # have to use 5-year 2008 b/c 1-year not available,
# # doesn't overlap w/ 1-year 2012, so should be ok
# cnt08 <- 
#   get_acs(
#     geography = "county",
#     variables = "B01001_001",
#     year = 2008,
#     survey = "acs1",
#     output = "wide"
#   )
# 




# IMPORT IF NOT PULLING ====
# cntpan <- read_csv("cntpan_pull.csv")
# cntpan_bu <- cntpan


# CLEAN UP ====

# list to use in select()
keep <- c("year", "GEOID", "cntst", "fips", "stfips", "cntfips",
          "poptotal", "noncit", "groupqt",
          "popu18", "popu25", "pop3o", "pop324", "vapacs", "incmed",
          "incpc", "gini", "hs", "ged", "some1", "some2", "assoc", "bacc",
          "mast", "prof", "phd", "enroll", "pop_check", "nothilat", "white",
          "black", "amind", "asian", "hawpi", "other", "multi", "hilat",
          "white_prop", "black_prop", "amind_prop", "asian_prop", "hawpi_prop",
          "other_prop", "multi_prop", "hilat_prop", "nonwh_prop")


cntpan <- cntpan %>%
  rename(
    cntst = NAME,
    poptotal = B01001_001E,
    popu18 = B09001_001E,
    noncit = B16008_046E,
    pop3o = B14001_001E,
    groupqt = B26001_001E,
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
    popu25 = B01001_003E + B01001_004E + B01001_005E + B01001_006E + 
      B01001_007E + B01001_008E + B01001_009E + B01001_010E + 
      B01001_027E + B01001_028E + B01001_029E + B01001_030E + 
      B01001_031E + B01001_032E + B01001_033E + B01001_034E,
    pop324 = popu25 - (poptotal - pop3o),
    vapacs = poptotal - popu18 - noncit - groupqt,
    white_prop = white / pop_check,
    black_prop = black / pop_check,
    amind_prop = amind / pop_check,
    asian_prop = asian / pop_check,
    hawpi_prop = hawpi / pop_check,
    other_prop = other / pop_check,
    multi_prop = multi / pop_check,
    hilat_prop = hilat / pop_check,
    nonwh_prop = 1 - white_prop,
    fips = as.character(GEOID),
    stfips = substr(fips, 1, 2),
    cntfips = substr(fips, 3, 5),
  ) %>% 
  select(keep)


# select only complete cases -- drops to 569!!!
cntpan <- cntpan[complete.cases(cntpan),]


# complete case counties from 2012 & 2016
cnt12 <- cntpan %>% 
  filter(year == 2012) %>% 
  select(fips) %>% 
  unique() %>% 
  as_vector()
cnt16 <- cntpan %>% 
  filter(year == 2016) %>% 
  select(fips) %>% 
  unique() %>% 
  as_vector()

# counties in both for balanced panel
both <- intersect(cnt12, cnt16)
# only 176 counties!!!



library(blscrapeR) # for county fips codes
cntfips <- county_fips

test <- 
  inner_join(
    complete, cntfips,
    by = c(
      "stfips" = "fips_state",
      "cntfips" = "fips_county"
    )
  )








# add state abbreviations
st <- state.abb
# which(st == "DE")
st <- append(st, "DC", after = 7)
state <- state.name
# which(state == "Delaware")
state <- append(state, "District of Columbia", after = 7)
states <- tibble(state, st)


# join
cntpan <- merge(cntpan, states, by = "state")
# rearrange
cntpan <- cntpan[, c(2:5,1,46,6:45)]
# sort into panels
cntpan <- arrange(cntpan, GEOID, year)


# backup again
cntpan_bu2 <- cntpan


# write out the clean version
write_csv(cntpan, "cntpan_cleaned.csv")



# LIFE EXPENCTANCY ====

# Source: United States Mortality Database, by UC Berkeley Demography,
# https://usa.mortality.org/
# Citation:
# *United States Mortality DataBase.* University of California, Berkeley (USA).
# Available at usa.mortality.org (data downloaded on Apr 20, 2019).

# Create vector of file names
mortvect <- c()
for (i in st) {
  mortvect <- c(mortvect, paste0("sources/mort/", i, "_bltper_5x1.csv"))
}


# loop over files & get 2012/2016 life expectancy
le <- data.frame()

for (i in 1:length(mortvect)) {
  tmp <- read_csv(mortvect[i]) %>%
    filter(Year == 2012 & Age == 0 |
             Year == 2014 & Age == 0 |
             Year == 2016 & Age == 0) %>% 
    select(PopName, Year, ex) %>% 
    rename(st = PopName,
           year = Year,
           lexp = ex)
  le <- rbind(le, tmp)
}


# join
cntpan <- left_join(cntpan, le, by = c("st" = "st", "year" = "year"))

# Spot Check
View(cntpan[sample(1:153, 10, replace = F), c(1:5, 47)])

# clean up
rm(list = c("tmp", "le"))

# HEALTH INDEX ====

# get observed Max & Min for each year
  # 2012: min = 75.06, max = 81.42
  describe(subset(cntpan, year == "2012", select = lexp))
  # 2012: min = 74.89, max = 81.62
  describe(subset(cntpan, year == "2014", select = lexp))
  # 2012: min = 74.72, max = 81.74
  describe(subset(cntpan, year == "2016", select = lexp))
  
# based on above, SSRA goalposts of 66-90 should be OK
lemin <- 66
lemax <- 90

# construct index
cntpan <- cntpan %>% 
  mutate(health_index = ((lexp - lemin) / (lemax - lemin)) * 10)

# check out
describe(subset(cntpan, year == "2012", select = health_index))
describe(subset(cntpan, year == "2014", select = health_index))
describe(subset(cntpan, year == "2016", select = health_index))
# medians close enough to 5. Lowish ranges. Go with it.



# ATTAINMENT INDEX ====

# bin all >= HS into HS, Bacc, higher, (bacc is already binned)
# then generate population proportions FOR POP 25 & OVER,
# then aggregate attainment score
cntpan <- cntpan %>%
  mutate(hsplus = hs+ged+some1+some2+assoc+bacc+mast+prof+phd,
         baccplus = bacc+mast+prof+phd,
         mastplus = mast+prof+phd,
         hsprop = hsplus / (poptotal - popu25),
         baccprop = baccplus / (poptotal - popu25),
         gradprop = mastplus / (poptotal - popu25),
         attain_score = hsprop + baccprop + gradprop)

# SSRA goalposts of 0.5, 2 should be ok
describe(subset(cntpan, year == "2012", select = attain_score))
describe(subset(cntpan, year == "2014", select = attain_score))
describe(subset(cntpan, year == "2016", select = attain_score))

attmin <- 0.5
attmax <- 2

# create index
cntpan <- cntpan %>% 
  mutate(attain_index = ((attain_score - attmin) / (attmax - attmin)) * 10)

# check out
describe(subset(cntpan, year == "2012", select = attain_index))
describe(subset(cntpan, year == "2014", select = attain_index))
describe(subset(cntpan, year == "2016", select = attain_index))
# medians close to 5




# ENROLLMENT INDEX ====

cntpan <- cntpan %>% 
  mutate(enroll_prop = enroll / pop324)

describe(subset(cntpan, year == "2012", select = enroll_prop))
describe(subset(cntpan, year == "2014", select = enroll_prop))
describe(subset(cntpan, year == "2016", select = enroll_prop))
# Goalposts of .6-.95 should be ok

enrollmin <- .60
enrollmax <- .95

cntpan <- cntpan %>% 
  mutate(enroll_index = ((enroll_prop - enrollmin) / 
                           (enrollmax - enrollmin)) * 10)

describe(subset(cntpan, year == "2012", select = enroll_index))
describe(subset(cntpan, year == "2014", select = enroll_index))
describe(subset(cntpan, year == "2016", select = enroll_index))
# medians are high, but perhaps they'll get moderated in the overall
# education index? Go with it for now


# OVERALL EDUCATION INDEX ====

cntpan <- cntpan %>% 
  mutate(ed_index = ((2/3) * attain_index) + ((1/3) * enroll_index))

describe(subset(cntpan, year == "2012", select = ed_index))
describe(subset(cntpan, year == "2014", select = ed_index))
describe(subset(cntpan, year == "2016", select = ed_index))
# medians are at 6 - redo with higher bottom goalpost?
# for now, leave it. I think using the GEOMETRIC mean, rather
# than SSRA's arithmetic mean, will prevent undue influence
# of any single index, via:
# https://en.wikipedia.org/wiki/Geometric_mean#Applications,
# with specific reference to HDI


# INCOME INDEX ====

# first, convert 2012 & 2014 dollars to 2016,
# Using BLS CPI Inflation calculator, June-June

  # # test
  # test <- cntpan
  # test$incmed[test$year == "2012"] <- 
  #   test$incmed[test$year == "2012"] * 1.0503
  # sample <- sample(1:102, 10, replace = F)
  # test[c(sample), "incmed"]
  # cntpan[c(sample), "incmed"]
  # # works!
  # rm(test)

# inflation adjustment for real
cntpan$incmed[cntpan$year == "2012"] <- 
  cntpan$incmed[cntpan$year == "2012"] * 1.0503
cntpan$incpc[cntpan$year == "2012"] <- 
  cntpan$incpc[cntpan$year == "2012"] * 1.0503
cntpan$incmed[cntpan$year == "2014"] <- 
  cntpan$incmed[cntpan$year == "2014"] * 1.0112
cntpan$incpc[cntpan$year == "2014"] <- 
  cntpan$incpc[cntpan$year == "2014"] * 1.0112

# check out median income
describe(subset(cntpan, year == "2012", select = incmed))
describe(subset(cntpan, year == "2014", select = incmed))
describe(subset(cntpan, year == "2016", select = incmed))
# max goalpost of $67,730 seems a little high, but was set at $55K
# in 2005 $, so maybe recessional effects still at play?
# lower goalpost of $16009 is probably ok, though.


# check out per capita income
describe(subset(cntpan, year == "2012", select = incpc))
describe(subset(cntpan, year == "2014", select = incpc))
describe(subset(cntpan, year == "2016", select = incpc))
# per capita's look pretty similar to medians
# just go with median & present goalposts for now


# construct index
incmin <- 16009
incmax <- 67730

cntpan <- cntpan %>% 
  mutate(inc_index = (log(incmed) - log(incmin)) / 
           (log(incmax) - log(incmin)) * 10)

describe(subset(cntpan, year == "2012", select = inc_index))
describe(subset(cntpan, year == "2014", select = inc_index))
describe(subset(cntpan, year == "2016", select = inc_index))
# median close enough to 5 -- roll with it.



# HDI ====

# create index
cntpan <- cntpan %>% 
  mutate(hdi = (health_index * ed_index * inc_index) ^ (1/3) )

# descriptive stats per year
describe(subset(cntpan, year == "2012", select = hdi))
describe(subset(cntpan, year == "2014", select = hdi))
describe(subset(cntpan, year == "2016", select = hdi))
# median a little above 5, where it should be

# create simulated normals
mean12 <- describe(subset(cntpan, year == "2012", select = hdi))$mean
sd12 <- describe(subset(cntpan, year == "2012", select = hdi))$sd
mean12 <- describe(subset(cntpan, year == "2014", select = hdi))$mean
sd14 <- describe(subset(cntpan, year == "2014", select = hdi))$sd
mean14 <- describe(subset(cntpan, year == "2016", select = hdi))$mean
sd16 <- describe(subset(cntpan, year == "2016", select = hdi))$sd
meanpool <- describe(cntpan$hdi)$mean
sdpool <- describe(cntpan$hdi)$sd
norm12 <- rnorm(51, mean = mean12, sd = sd12)
norm14 <- rnorm(51, mean = mean14, sd = sd14)
norm16 <- rnorm(51, mean = mean12, sd = sd16)
normpool <- rnorm(153, mean = meanpool, sd = sdpool)

# 2012 histogram
cntpan %>% 
  filter(year == "2012") %>% 
  ggplot(mapping = aes(x = hdi)) +
    geom_histogram(bins = 11, mapping = aes(y = ..density..),
                   fill = "grey70", color = "white") +
    geom_density(color = "orange", size = 1.2) +
    geom_density(mapping = aes(x = norm12),
                 color = "seagreen", size = 1.2) +
    geom_abline(slope = 0, intercept = 0, size = 1, color = "grey80") +
    theme_minimal()
# close enough!

# 2014 histogram
cntpan %>% 
  filter(year == "2014") %>% 
  ggplot(mapping = aes(x = hdi)) +
    geom_histogram(bins = 11, mapping = aes(y = ..density..),
                   fill = "grey70", color = "white") +
    geom_density(color = "orange", size = 1.2) +
    geom_density(mapping = aes(x = norm14),
                 color = "seagreen", size = 1.2) +
    geom_abline(slope = 0, intercept = 0, size = 1, color = "grey80") +
    theme_minimal()
# close enough!

# 2016 histogram
cntpan %>% 
  filter(year == "2016") %>% 
  ggplot(mapping = aes(x = hdi)) +
    geom_histogram(bins = 11, mapping = aes(y = ..density..),
                   fill = "grey70", color = "white") +
    geom_density(color = "orange", size = 1.2) +
    geom_density(mapping = aes(x = norm16),
                 color = "seagreen", size = 1.2) +
    geom_abline(slope = 0, intercept = 0, size = 1.2, color = "grey70") +
    theme_minimal()
# close enough!

# pooled histogram
cntpan %>% 
  ggplot(mapping = aes(x = hdi)) +
  geom_histogram(bins = 11, mapping = aes(y = ..density..),
                 fill = "grey70", color = "white") +
  geom_density(color = "orange", size = 1.2) +
  geom_density(mapping = aes(x = normpool),
               color = "seagreen", size = 1.2) +
  geom_abline(slope = 0, intercept = 0, size = 1.2, color = "grey70") +
  theme_minimal()
# good!


# TURNOUT ====

# From Michael McDonald's US ELections Project
# http://www.electproject.org/

# import 2012
to12 <-
  read_csv(
    "sources/2012 November General Election v2.0 - Turnout Rates.csv",
    skip = 3,
    col_names = c("state", "veptotal", "tovep", "tovap",
                  "totalvotes", "presvotes", "veppop", "vappop",
                  "noncitpct", "prisonpop", "probatpop", "parolepop",
                  "inelpop", "overseas", "st")
  ) %>% 
  mutate(
    year = "2012",
    tovep = as.numeric(str_sub(tovep, 1, 4)) / 100,
    tovap = as.numeric(str_sub(tovap, 1, 4)) / 100
  ) %>% 
  select("year", "st", "presvotes", "tovep", "tovap")

# import 2014
to14 <-
  read_csv(
    "sources/2014 November General Election - Turnout Rates.csv",
    skip = 3,
    col_names = c("state", "veptotal", "tovep", "tovap",
                  "totalvotes", "presvotes", "veppop", "vappop",
                  "noncitpct", "prisonpop", "probatpop", "parolepop",
                  "inelpop", "overseas", "st")
  ) %>% 
  mutate(
    year = "2014",
    tovep = as.numeric(str_sub(tovep, 1, 4)) / 100,
    tovap = as.numeric(str_sub(tovap, 1, 4)) / 100
  ) %>% 
  select("year", "st", "presvotes", "tovep", "tovap")

# import 2016
to16 <-
  read_csv(
    "sources/2016 November General Election - Turnout Rates.csv",
    skip = 3,
    col_names = c("state", "site", "status", "veptotal", "tovep", "tovap",
                  "totalvotes", "presvotes", "veppop", "vappop",
                  "noncitpct", "prisonpop", "probatpop", "parolepop",
                  "inelpop", "overseas", "st")
  ) %>% 
  mutate(
    year = "2016",
    tovep = as.numeric(str_sub(tovep, 1, 4)) / 100,
    tovap = as.numeric(str_sub(tovap, 1, 4)) / 100
  ) %>% 
  select("year", "st", "presvotes", "tovep", "tovap")


# stack each year, then sort into panels
turnout <- rbind(to12, to14, to16)
turnout <- arrange(turnout, st, year)


# join to main dataset\
turnout$year <- as.numeric(turnout$year)
cntpan <- left_join(cntpan, turnout, by = c("st" = "st", "year" = "year"))


# calculate turnout w/ ACS vap (pop-under18-noncit over 18)
cntpan <- cntpan %>% 
  mutate(toacs = presvotes / vapacs)

# cleanup
rm(list = c("to12", "to14", "to16", "turnout"))


# WRITE OUT ====

rm(list=setdiff(ls(), "cntpan"))

write_csv(cntpan, "cntpan.csv")

save(cntpan, file = "cntpan.Rdata")
