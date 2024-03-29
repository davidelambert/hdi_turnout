library(sf)
library(tigris)
library(tidycensus)
library(leaflet)
library(Hmisc)
library(psych)
library(stringr)
library(tidyverse)
options(tigris_class = "sf")
options(tigris_use_cache = TRUE)


# PULL COUNTY-LEVEL ACS VARS ====

# all available variables for ACS 5-year 20212-2016
vars16 <- load_variables(2016, "acs5", cache = T)

# variables
acs_vars <- c(# Total Pop, Under 18, Non-Citizen 18+, & Median Age,
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
              "B01001_031", "B01001_032", "B01001_033", "B01001_034")


# pull from census API
acs <- get_acs(geography = "county", variables = acs_vars, 
               geometry = TRUE, year = 2016,
               survey = "acs5", output = "wide")

# backup for fucking up
acs_bu <- acs

# write out as csv in case census API breaks
# leave commented unless updating pull
# write.csv(acs, file = "acs_pull.csv")


# descriptive names and drop MOEs
acs <- acs %>% 
  rename(poptotal = B01001_001E,
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
         county_full = NAME) %>% 
  mutate(popu25 = B01001_003E + B01001_004E + B01001_005E + B01001_006E + 
                  B01001_007E + B01001_008E + B01001_009E + B01001_010E + 
                  B01001_027E + B01001_028E + B01001_029E + B01001_030E + 
                  B01001_031E + B01001_032E + B01001_033E + B01001_034E) %>%
  mutate(pop324 = popu25 - (poptotal - pop3o)) %>% 
  select(GEOID, county_full, poptotal, noncit, popu18, popu25, pop3o, pop324,
         medage, incmed, incpc, gini, hs, ged, some1, some2, assoc,
         bacc, mast, prof, phd, enroll, geometry) %>%
  separate(county_full, sep = ", ", into = c("county_only", "state_only"),
           remove = FALSE, extra = "warn", fill = "warn")


# glimpse(acs)

# MATCH TO FIPS CODES =====

# get state abbreviations
statedc <- state.abb
# append dc alphabetically before delaware
which(statedc == "DE") # DE has index [8], so add before [7]
statedc <- append(statedc, "DC", after = 7)

# get fips codes
data("fips_codes")
# glimpse(fips_codes)


# backup county and state_name columns
# MUST use for match....
fips_codes$county_only_fips <- fips_codes$county
fips_codes$state_only_fips <- fips_codes$state_name


# cat state and column
fips_codes <- within(fips_codes, county_full <- paste(county, state_name,
                                                      sep = ", "))

    # Try Join -- doesn't work
    # join1 <- inner_join(acs, fips_codes, by = "county_full")
    
    
    # try double join -- FUCK YEAH THIS SHIT WORKS!
    # (except for the 3 mislabelled counties)
    # join2 <- inner_join(acs, fips_codes,
    #                    by = c("county_only" = "county_only_fips",
    #                           "state_only" = "state_only_fips"))

# rename 3 bad counties as established in section OOPS
acs$county_only[acs$county_only == "Do?a Ana County"] <- "Dona Ana County"
acs$county_only[acs$county_only == "LaSalle Parish"] <- "La Salle Parish"
acs$county_only[acs$county_only == "Petersburg Borough"] <- "Petersburg Census Area"


    # redo as above w/ renames -- FUCK THE FUCK YEAH!!!!!!!!
    # some inexact matches, but the observations match up!!!!!
    # join3 <- inner_join(acs, fips_codes,
    #                     by = c("county_only" = "county_only_fips",
    #                            "state_only" = "state_only_fips"))


acs <- inner_join(acs, fips_codes,
                    by = c("county_only" = "county_only_fips",
                           "state_only" = "state_only_fips"))


# rename & clean up
acs <- acs %>%
  mutate(fips = as.numeric(GEOID)) %>% 
  select(GEOID, fips, state, state_name, state_code, 
         county, county_code, county_full.x,
         poptotal, noncit, popu18, popu25, pop3o, pop324, medage,
         incmed, incpc, gini, hs, ged, some1, some2, assoc,
         bacc, mast, prof, phd, enroll, geometry) %>% 
  filter(state %in% statedc) %>% 
  rename(county_state = county_full.x)

# glimpse(acs)

# clear experiments
rm(fips_codes)


# OOPS =====

  # THIS WHOLE SECTION WAS SILLY B/C SOME STATES HAVE COUNTIES W/ THE SAME
  # NAME!!!!! WILL HAVE TO JOIN THE FIPS CODES DATA SET BY Xxxx County, State
  # NAMING CONVENTION

  # ALSO NOTE THAT THERE ARE THREE COUNTIES MIS-TYPED IN THE ACS PULL,
  # SO NEED TO RECOPY THE 3 RENAMING LINES
    
      # # the county names don't quite match the fips_codes data frame, so have to get
      # # creative w/ a semi & anti, to be matched later
      #   
      #   # semi - these are the good matches
      #   acs_semi <- semi_join(acs, fips_codes, by = "county")
      #   # drop the geometry column
      #   acs_semi <- acs_semi[, -12]
      #     
      #     
      #   # anti - these are the mismatches in acs
      #   acs_anti <- anti_join(acs, fips_codes, by = "county")
      #   # drop geometry
      #   acs_anti <- acs_anti[, -12]
      #   
      #   # these are the mismatches in fips_codes
      #   # discrepancy is b/c Bedford City, VA and Wade Hampton, AK are no longer used
      #   acs_anti2 <- fips_codes %>% 
      #     anti_join(acs, by = "county") %>% 
      #     filter(state %in% statedc)
      #   
      #   ## rename the wrong ones in acs
      #   acs$county[acs$county == "Do?a Ana County"] <- "Dona Ana County"
      #   acs$county[acs$county == "LaSalle Parish"] <- "La Salle Parish"
      #   acs$county[acs$county == "Petersburg Borough"] <- "Petersburg Census Area"
      #   
      #   
      #   ## semi join properply - right number of rows now
      #   acs_semi2 <- semi_join(acs, fips_codes, by = "county")
      #   acs_semi2 <- acs_semi2[, -12]
      #   
      #   
      #   ## now try inner
      #   acs_join <- inner_join(acs_semi2, fips_codes, by = "county")
  


# MERGE IN LIFE EXPECTANCY =====
# SOURCE: Institute for Health Metrics and Evaluation
#         http://www.healthdata.org/us-health/data-download

# import
le <- read_csv("sources/IHME_county_data_EDIT.csv")


# county names don't match perfectly, so alpha sort by county, state
# for each & match by row.

acs <- arrange(acs, state_name, county)
le <- arrange(le, State, County)

# unweighted average of male & female life expectancy
# spot checked against individual datasets, & works
# check before removing if need verification
acs <- acs %>%
  mutate(le = (le$male2010 + le$female2010) / 2)

rm(le)



# # MORTALITY - NO LONGER NEEDED! ====================
# # MERGE CDC CRUDE MORTALITY SCORES AS AN ALTERNATIVE TO LIFE EXPECTANCY
# # Source: CDC Wonder Compressed Mortality 1999-2016, selecting 2016 only
# # https://wonder.cdc.gov/
# 
# 
# 
# # import
# mort <- read_delim("sources/CDC_crude_mortality_2016.txt", delim = "\t",
#                    col_types = "lcdddd")
# 
# # rename to fips
# mort$fips <- mort$`County Code`
# 
# # rename `Crude Rate`
# mort$mort <- mort$`Crude Rate`
# 
# # subset so don't need to clean up
# mort <- mort[, c("fips", "mort")]
# 
# # merge
# acs <- left_join(acs, mort, by = "fips")
# 
# # check - looks ok, slight difference but mort has more rows than acs
# sum(is.na(acs$mort))
# sum(is.na(mort$mort))
# 
# 
# rm(mort)
 
 
 
 
 
 
 
# 2016 VOTE TOTALS =====

# compiled by Tony McGovern, craped from townhall.com results
# https://github.com/tonmcg/US_County_Level_Election_Results_08-16
# 3 missing counties to be filled from MIT data retrieved from
# https://electionlab.mit.edu/data &
# https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/VOQCHQ



# import
votes16 <- read_csv("sources/2016_US_County_Level_Presidential_Results.csv")
# Oglala Lakota County, SD has mislabelled FIPS
which(votes16$county_name == "Oglala County")
votes16[2412,11] <- 46102


# get just FIPS & total votes (only need 1 line, so filter by democrat)
votes16 <- votes16 %>% 
  select(combined_fips, total_votes) %>% 
  rename(fips = combined_fips) 

    # test merge
    # test1 <- inner_join(acs, votes16, by = "fips") # lose 3 obs
    # test2 <- anti_join(acs, votes16, by = "fips")  # these are the three lost
    # test3 <- left_join(acs, votes16, by = "fips")
    # which(is.na(test3$total_votes))
    # View(test3[c(82, 549), ])  # expected via Test 2 & Oglala Lakota correction


# Merge
acs <- left_join(acs, votes16, by = "fips")


# find the missings
which(is.na(acs$total_votes))

# Kusilvak Census Area, AK correction, imputed from:
# https://rrhelections.com/index.php/2018/02/02/alaska-results-by-county-equivalent-1960-2016/
# Archived on Wayback Machine:
# https://web.archive.org/web/20190404123624/https://rrhelections.com/index.php/2018/02/02/alaska-results-by-county-equivalent-1960-2016/
acs[82, 30] <- 2228


# Kawalao County, HI correction, based unsourced on (but only 20 votes)
# https://en.wikipedia.org/wiki/2016_United_States_presidential_election_in_Hawaii
# Archived on Wayback Machine
# https://web.archive.org/web/20190404125721/https://en.wikipedia.org/wiki/2016_United_States_presidential_election_in_Hawaii
acs[549, 30] <- 20


which(is.na(acs$total_votes))  # All Good!


# CREATE TURNOUT VARIABLE =====

acs <- acs %>% 
  mutate(vap = poptotal - popu18 - noncit,
         turnout = total_votes / vap)

which(is.na(acs$turnout))  # all good!



# investigate
describe(acs$turnout)
summary(acs$turnout)
# several values over 100! W, as they say, TF?
to_fuckups <- which(acs$turnout > 100)
View(acs[c(to_fuckups),])
# all in Alaska? AK & HI have been consistent problems!
# maybe jsut go w/  lower 48 for analysis.
# just say AK & HI have been consistent outliers???



# clean up 
rm("votes16")

# CREATE HDI =====

# Methodology based on Social Science Research Council's
# "American HDI". See PDF under resources tab at
# http://www.measureofamerica.org/10yearsandcounting/
# EXCEPT: Per-capita income used instead of median earnings



# HEALTH INDEX
  
  # Goalposts from SSRC's 2016 Methodology
  lemax <- 90
  lemin <- 66
  
  # create index
  acs <- acs %>%
    mutate(health_index = ((le - lemin) / (lemax - lemin)) * 10)
  
  # check out
  describe(acs$health_index)
  

  
  
  
  
  
  
  
# EDUCATION INDEX
  
  # Attainment index
  
    # bin all >= HS into HS, Bacc, higher, (bacc is already binned)
    # then generate population proportions FOR POP 25 & OVER,
    # then aggregate attainment score
    acs <- acs %>%
      mutate(hsplus = hs+ged+some1+some2+assoc+bacc+mast+prof+phd,
             baccplus = bacc+mast+prof+phd,
             mastplus = mast+prof+phd,
             hsprop = hsplus / (poptotal - popu25),
             baccprop = baccplus / (poptotal - popu25),
             gradprop = mastplus / (poptotal - popu25),
             attain_score = hsprop + baccprop + gradprop)
    
    describe(acs$attain_score)
    
    # Goalposts from SSRC 2016 report
    attmax <- 2
    attmin <- 0.5
    
    # create attainment index
    acs <- acs %>%
      mutate(attain_index = ((attain_score - attmin) / (attmax - attmin)) * 10)
    
    
    # check out
    describe(acs$attain_index)
    
    
    
  # Enrollment index
  
    # generate proportion
    acs <- acs %>% 
      mutate(enroll_prop = enroll / pop324)
    
    describe(acs$enroll_prop)
    
    # Attempt to establish goalposts since SSRA's methodology is opaque. 
    # Failed to replicate SSRA Goalposts.
    # Establishing Slightly Arbitrary Goalposts to to avoid 0 scores
    # # need to get goalposts from 2001-2005 5 yr ACS b/c
    # # SSRC methodology is opaque
    # enroll10_vars <- c("B01001_001", "B14001_001", "B14001_002",
    #                    "B01001_003", "B01001_004", "B01001_005", "B01001_006", 
    #                    "B01001_007", "B01001_008", "B01001_009", "B01001_010", 
    #                    "B01001_027", "B01001_028", "B01001_029", "B01001_030", 
    #                    "B01001_031", "B01001_032", "B01001_033", "B01001_034")
    # 
    # # pull from census API
    # enroll10 <- get_acs(geography = "county", variables = enroll10_vars, 
    #                     geometry = FALSE, year = 2010,
    #                     survey = "acs5", output = "wide")
    # 
    # # create proportion & select
    # enroll10 <- enroll10 %>%
    #   rename(poptotal_10 = B01001_001E,
    #          enroll_10 = B14001_002E,
    #          pop3o_10 = B14001_001E) %>% 
    #   mutate(popu25_10 = B01001_003E + B01001_004E + B01001_005E +
    #                      B01001_006E + B01001_007E + B01001_008E +
    #                      B01001_009E + B01001_010E + B01001_027E +
    #                      B01001_028E + B01001_029E + B01001_030E + 
    #                      B01001_031E + B01001_032E + B01001_033E +
    #                      B01001_034E,
    #          
    #          pop324_10 = popu25_10 - (poptotal_10 - pop3o_10),
    #          enroll_prop_10 = enroll_10 / pop324_10
    #          )
    # 
    # describe(enroll10$enroll_prop_10) # weird, got some Infinity?
    # which(enroll10$enroll_prop_10 == Inf)
    # View(enroll10[340,])  # The Hawaii leper colony. replace w/ NA
    # enroll10[340, "enroll_prop_10"] <- NA
    # 
    # describe(enroll10$enroll_prop_10)
    # describe(acs$enroll_prop)
    # # minimum is actuall higher in 2010!
    # # use somewhat arbitrary min & min that will avoid 0's
    
    
    # SET goalposts, avoiding 0 scores, using closest possible to
    # SSRC variables. Max is good, but setting min to 0.4 to be below
    # The 0.45 observed minimum
    enrollmax <- .95
    enrollmin <- .4
    
    # create enrollment index
    acs <- acs %>% 
      mutate(enroll_index = ((enroll_prop - enrollmin) / 
                               (enrollmax - enrollmin)) * 10)
    
    # check out
    describe(acs$enroll_index)
    
    # topcode at 10, following SSRA methods
    acs$enroll_index[acs$enroll_index > 10] <- 10
    
    describe(acs$enroll_index)
    
  
  # Overall Education Index
    acs <- acs %>% 
      mutate(ed_index = ((2/3) * attain_index) + ((1/3) * enroll_index) )
 
    
    describe(acs$ed_index)
    
    
    
    
# Income index
    
  # check out median income
  describe(acs$incmed)
  # $5223 seems EXTREMELY LOW! -- check out
  acs$county_state[acs$incmed == min(acs$incmed, na.rm = T)]
  # Lexington City, VA Can't possibly be right:
    # ONLY city limits, hiking, tourism, etc......
  
  # use per-capita income instead?
  describe(acs$incpc)
  # 9286 still low, but seems more reasonable
  acs$county_state[acs$incpc == min(acs$incpc)]
  # Oglala Lakota, SD -- makes more sense!
  # Also no NA's -- use this!
  
  
  # SET max & min, lowering min to avoid 0's
  incmax <- max(acs$incpc)
  incmin <- 9000
  
  # create index
  acs <- acs %>% 
    mutate(inc_index = (log(incpc) - log(incmin)) / 
                       (log(incmax) - log(incmin)) * 10 )
  
  
  # check out
  describe(acs$inc_index)
  
  
  
  
  
# OVERALL MODIFIED HDI
  
  acs <- acs %>%
    mutate(hdi = (health_index * ed_index * inc_index) ^ (1/3) )
  
 
  describe(acs$hdi)
  ggplot(acs) + geom_histogram(aes(x = hdi), bins = 51)
  # looks good!!
   

# Write Out =========
  
# remove all but acs SF dataframe/shapefile
rm(list=setdiff(ls(), "acs"))

# write Rdata
save(acs, file = "acs_.Rdata")



# remove geometry (shapefile) to write CSV
st_geometry(acs) <- NULL

# csv version w/ no geometry
write.csv(acs, file ="acs_.csv")

  
