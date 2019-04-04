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

# check out what's what
vars16 <- load_variables(2016, "acs5", cache = T)

# variables
acs_vars <- c("B01003_001", "B09001_001", "B19083_001", "B19001_001", 
              "B19313_001", "B01002_001", "B15003_017", "B15003_018",
              "B15003_019", "B15003_020", "B15003_021", "B15003_022", 
              "B15003_023", "B15003_024", "B15003_025", "B14001_002")


# pull from census API
acs <- get_acs(geography = "county", variables = acs_vars, 
               geometry = TRUE, year = 2016,
               survey = "acs5", output = "wide", shift_geo = TRUE)

# backup for fucking up
acs_bu <- acs

# write out as csv in case census API breaks
# leave commented unless updating pull
# write.csv(acs, file = "acs_pull.csv")


# descriptive names and drop MOEs
acs <- acs %>% 
  rename(poptotal = B01003_001E,
         popund18 = B09001_001E,
         gini = B19083_001E,
         hhinc = B19001_001E,
         aginc = B19313_001E,
         medage = B01002_001E,
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
  select(GEOID, county_full, poptotal, popund18, gini, hhinc, aginc,
         medage, hs, ged, some1, some2, assoc, bacc, mast, prof,
         phd, enroll, geometry) %>%
  separate(county_full, sep = ", ", into = c("county_only", "state_only"),
           remove = FALSE, extra = "warn", fill = "warn")




# MATCH TO FIPS CODES =====

# get state abbreviations
statedc <- state.abb
# append dc alphabetically before delaware
which(statedc == "DE") # DE has index [8], so add before [7]
statedc <- append(statedc, "DC", after = 7)

# get fips codes
data("fips_codes")
glimpse(fips_codes)


# backup county and state_name columns
fips_codes$county_only_fips <- fips_codes$county
fips_codes$state_only_fips <- fips_codes$state_name


# cat state and column
fips_codes <- within(fips_codes, county_full <- paste(county, state.name,
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
join3 <- inner_join(acs, fips_codes,
                    by = c("county_only" = "county_only_fips",
                           "state_only" = "state_only_fips"))


# rename & clean up
acs <- join3 %>% 
  select(GEOID, state, state_name, state_code, 
         county, county_code, county_full.x,
         poptotal, popund18, gini, hhinc, aginc,
         medage, hs, ged, some1, some2, assoc, bacc, 
         mast, prof, phd, enroll, geometry) %>% 
  filter(state %in% statedc) %>% 
  rename(county_state = county_full.x) %>%
  mutate(fips = as.numeric(GEOID))


# clear experiments
rm(list = c("join1", "join2", "join3", "fips_codes"))


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
le <- read_csv("IHME_county_data_EDIT.csv")


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




# MERGE CDC CRUDE MORTALITY SCORES AS AN ALTERNATIVE TO LIFE EXPECTANCY
# Source: CDC Wonder Compressed Mortality 1999-2016, selecting 2016 only
# https://wonder.cdc.gov/



# import
mort <- read_delim("CDC_crude_mortality_2016.txt", delim = "\t",
                   col_types = "lcdddd")

# rename to fips
mort$fips <- mort$`County Code`

# rename `Crude Rate`
mort$mort <- mort$`Crude Rate`

# subset so don't need to clean up
mort <- mort[, c("fips", "mort")]

# merge
acs <- left_join(acs, mort, by = "fips")

# check - looks ok, slight difference but mort has more rows than acs
sum(is.na(acs$mort))
sum(is.na(mort$mort))


rm(mort)



# 2016 VOTE TOTALS =====

# compiled by Tony McGovern, craped from townhall.com results
# https://github.com/tonmcg/US_County_Level_Election_Results_08-16
# 3 missing counties to be filled from MIT data retrieved from
# https://electionlab.mit.edu/data &
# https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/VOQCHQ



# import
votes16 <- read_csv("2016_US_County_Level_Presidential_Results.csv")
# Oglala Lakota County, SD has mislabelled FIPS
which(votes16$county_name == "Oglala County")
votes16[2412,11] <- 46102


# get just FIPS & total votes (only need 1 line, so filter by democrat)
votes16 <- votes16 %>% 
  select(combined_fips, total_votes) %>% 
  rename(fips = combined_fips) 

# test merge
test1 <- inner_join(acs, votes16, by = "fips") # lose 3 obs
test2 <- anti_join(acs, votes16, by = "fips")  # these are the three lost
test3 <- left_join(acs, votes16, by = "fips")
which(is.na(test3$total_votes))
View(test3[c(82, 549), ])  # expected via Test 2 & Oglala Lakota correction


# Merge
acs <- left_join(acs, votes16, by = "fips")

# Kusilvak Census Area, AK correction, imputed from:
# https://rrhelections.com/index.php/2018/02/02/alaska-results-by-county-equivalent-1960-2016/
# Archived on Wayback Machine:
# https://web.archive.org/web/20190404123624/https://rrhelections.com/index.php/2018/02/02/alaska-results-by-county-equivalent-1960-2016/
acs[82, 27] <- 2228


# Kawalao County, HI correction, based unsourced on (but only 20 votes)
# https://en.wikipedia.org/wiki/2016_United_States_presidential_election_in_Hawaii
# Archived on Wayback Machine
# https://web.archive.org/web/20190404125721/https://en.wikipedia.org/wiki/2016_United_States_presidential_election_in_Hawaii
acs[549, 27] <- 20


# All Good!
which(is.na(acs$total_votes))


# CREATE TURNOUT VARIABLE =====

acs <- acs %>% 
  mutate(vap = poptotal - popund18,
         turnout = total_votes / vap)

which(is.na(acs$turnout))  # all good!


# clean up 
rm(list = c("test1", "test2", "test3", "votes16"))

# CREATE HDI =====

# Methodology based on Social Science Research Council's
# "American HDI". See PDF under resources tab at
# http://www.measureofamerica.org/10yearsandcounting/
# EXCEPT: geometric mean of 3 sub-indices, following UN standard HDI metodology
# EXCEPT: Per-capita income used instead of median earnings



# HEALTH INDEX
  
  # Get max's & min's
  lemax <- max(acs$le)
  lemin <- min(acs$le)
  
  # create index
  acs <- acs %>%
    mutate(health_index = ((le - lemin) / (lemax - lemin)) * 10)
  

  
  
  
  
  
  
  
# EDUCATION INDEX
  
  # Attainment index
  
    # bin all >= HS into HS, Bacc, higher, (bacc is already binned)
    # then generate population proportions,
    # then aggregate attainment score
    acs <- acs %>%
      mutate(hsplus = hs + ged + some1 + some2 + assoc,
             grad = mast + prof + phd,
             hsplus_prop = hsplus / poptotal,
             bacc_prop = bacc / poptotal,
             grad_prop = grad / poptotal,
             attain_score = hsplus_prop + bacc_prop + grad_prop)
    
    # get max's & min's
    attmax <- max(acs$attain_score)
    attmin <- min(acs$attain_score)
    
    # create attainment index
    acs <- acs %>%
      mutate(attain_index = ((attain_score - attmin) / (attmax - attmin)) * 10)
    
    
    
  # Enrollment index
    
    # generate proportion
    acs <- acs %>% 
      mutate(enroll_prop = enroll / poptotal)
    
    # get max & min
    enrollmax <- max(acs$enroll_prop)
    enrollmin <- min(acs$enroll_prop)
    
    # create enrollment index
    acs <- acs %>% 
      mutate(enroll_index = ((enroll_prop - enrollmin) / 
                               (enrollmax - enrollmin)) * 10)
    
  
  # Overall Education Index
    acs <- acs %>% 
      mutate(ed_index = ((2/3) * attain_index) + ((1/3) * enroll_index) )
 
    
    
    
    
    
# Income index
    
  # generate per-capita income
  acs <- acs %>%
    mutate(incpc = aginc / poptotal)
  
  # get max & min
  incmax <- max(acs$incpc)
  incmin <- min(acs$incpc)
  
  # create index
  acs <- acs %>% 
    mutate(inc_index = (log(incpc) - log(incmin)) / 
                       (log(incmax) - log(incmin)) * 10 )
  
  
  
  
  
# OVERALL MODIFIED HDI
  
  acs <- acs %>%
    mutate(hdi = (health_index * ed_index * inc_index) ^ (1/3) )
  
  

  
  
# EXPLORATORY =====

hdistats <- describe(acs$hdi)
hdistats # whoa! zero's

which(acs$hdi == 0)
View(acs[c(2413, 3019), ])  # Ok these are poor/ unhealthy AF
which(acs$health_index == 0)  # McDowell, WV
which(acs$inc_index == 0)  # Oglala Lakota, SD



# histogram w/ eqivalent normal & boxplot
# close to normal, w/ a few outliers
hdinorm <- rnorm(3142, mean = hdistats$mean, sd = hdistats$sd)

ggplot(data = acs, mapping = aes(x = hdi)) +
  geom_histogram(bins = 51, mapping = aes(y = ..density..),
                 fill = "navyblue") +
  geom_density(color = "hotpink", size = 0.8) +
  geom_density(mapping = aes(x = hdinorm),
               color = "orange", size = 0.8) +
  geom_abline(slope = 0, intercept = 0, size = 1, color = "grey80") +
  theme_minimal()

ggplot() +
  geom_boxplot(data = acs, mapping = aes(y = hdi)) + 
  coord_flip() + theme_minimal()

# weak
cor(acs$gini, acs$turnout)
# weaker
cor(acs$hdi, acs$turnout)

# moderate, negative - investigate?
cor(acs$hdi, acs$gini)

acs %>% ggplot(mapping = aes(x = hdi, y = gini)) +
  geom_point(shape = 19, alpha = 0.1) +
  geom_smooth(method = "lm") +
  theme_minimal()
  
  
# nope!  
cor(acs$hhinc, acs$turnout) 

# weak, but better & expected
cor(acs$incpc, acs$turnout)
  




  
# GRAPHICS ====
  
# gini  map
ginimap <- acs %>% 
  ggplot() +
  geom_sf(aes(fill = gini, color = gini)) +
  scale_fill_viridis_c(name = "Gini") + 
  scale_color_viridis_c(name = "Gini") +
  labs(
    title = "Gini Coefficient by County, 2016", 
    caption = "Data: 2016 ACS 5-year Estimates, US Census Bureau"
    ) +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        legend.position = c(0.75, 0.1),
        legend.background = element_blank(),
        legend.direction = "horizontal",
        legend.title = element_blank(),
        legend.key.width = unit(2, "line"),
        legend.text = element_text(vjust = -.1)
        )  
ginimap



# HDI  map
hdimap <- acs %>% 
  ggplot() +
  geom_sf(aes(fill = hdi, color = hdi)) +
  scale_fill_viridis_c(name = "HDI", option = "plasma") + 
  scale_color_viridis_c(name = "HDI", option = "plasma") +
  labs(
    title = "Modified HDI by County, 2016", 
    caption = "Data: 2016 ACS 5-year Estimates, US Census Bureau"
    ) +
  borders("state") +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        legend.position = c(0.75, 0.1),
        legend.background = element_blank(),
        legend.direction = "horizontal",
        legend.title = element_blank(),
        legend.key.width = unit(2, "line"),
        legend.text = element_text(vjust = -.1)
        )  
hdimap




# Interactive
# LOOKS UGLY AS B/C OF RESCALED AK/HI!!!!!
# KEEP CODE TO USE IN ANOTHER VERSION W/O RESCALING (OR DROPPING)
color_pal <- colorQuantile(palette = "viridis", domain = acs$hdi, n = 20)
acs %>% 
  st_transform(crs = "+init=epsg:4326") %>%
  leaflet(width = "100%") %>%
  addProviderTiles(provider = "CartoDB.Positron") %>% 
  addPolygons(
    popup = ~paste0(county, " County", "<br>",
                    "Gini Coefficient: ", prettyNum(round(gini, 2)), "<br>",
                    "HDI: ", prettyNum(hdi, big.mark = ",")),
    stroke = FALSE,
    smoothFactor = 0,
    fillOpacity = 0.7,
    color = ~color_pal(hdi)
  )

# add with pipe if legend
  # addLegend(
  #   position = "bottomleft",
  #   pal = color_pal,
  #   values = ~turnout16,
  #   title = "2016 Turnout",
  #   opacity = 1
  # )
