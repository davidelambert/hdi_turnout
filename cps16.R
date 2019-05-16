# SETUP ====

# detach all non-base packages
lapply(paste('package:',names(sessionInfo()$otherPkgs),sep=""),
       detach, character.only=TRUE, unload=TRUE)

library(haven)
library(ipumsr)
library(blscrapeR)
library(tidyverse)


# # IMPORT =====
# 
# # import & back up
# ddi <- read_ipums_ddi("sources/ipums/cps_00002.xml")
# data <- read_ipums_micro(ddi)
# data_bu <- data
# 
# 
# 
# # check  out
# str(data)
# View(data[sample(1:nrow(data) , 10, replace = F), ])
# object.size(data) %>% print(units = "auto", standard = "legacy")
# # 58 Mb in memory
# 
# 
# 
# # subset 10/2016 ====
# 
# # subset employment vars & attach factor labels
# oct16 <- data %>% 
#   `colnames<-`(tolower(colnames(.))) %>% 
#   filter(year == 2016, month == 5) %>% 
#   select(year, month, statefip, county, wtfinl,
#          popstat, empstat, labforce, wkstat) %>% 
#   mutate(
#     month = as.numeric(month),
#     state = lbl_clean(statefip) %>% as_factor(),
#     statefip = as.character(statefip) %>% str_pad(2, pad = "0"),
#     county = as.character(county) %>% str_pad(5, pad = "0"),
#     popstat = lbl_clean(popstat) %>% as_factor(),
#     empstat = lbl_clean(popstat) %>% as_factor(),
#     labforce = lbl_clean(labforce) %>% as_factor(),
#     wkstat = lbl_clean(wkstat) %>% as_factor()
#   ) %>% 
#   arrange(statefip, county)
# 
# 
# 
# # COUNTY LEVEL INVESTIGATION ====
# 
# # how many have non-identifiable county?
# length(which(oct16$county == "00000"))
# nrow(oct16) - length(which(oct16$county == "00000"))
# # would lose 77,238 obs, leaving only 51,894
# length(unique(oct16$county[oct16$county != "00000"]))
# # those would be cover only 280 counties
# 
# 
# # check against those 331 counties in IPUMS ACS panel....
# cps <- unique(oct16$county[oct16$county != "00000"])
# acs <- read_csv("ipums2county.csv") %>% 
#   filter(year == 2016) %>% 
#   select(fips)
# acs <- acs$fips
# both <- intersect(cps, acs)
# # only 189 counties! Boo!
# # could still be useful for state-level though.
# 
# 
# 

# 331 COUNTY BLS SCRAPE, 07-16 ====

# get vector of 331 county FIPS codes
acs <- read_csv("ipums2county.csv") %>% 
  filter(year == 2016) %>% 
  select(fips)
acs <- acs$fips

# BLS LAUS codes are  FIPS w/ affixes
# for Unemployment Rate:
laus <- paste0("LAUCN", acs, "0000000003")

# split into chunks of 50 to meet api limit
chunks_331 <- split(laus, ceiling(seq_along(laus)/50))

# define function to pull each chunk
ur_331 <- function(x) {
  temp <- 
    bls_api(
      x,
      startyear = 2007,
      endyear = 2016,
    )
}


# apply the ur funtion to each list object (each chunk)
# returns a list of tibbles
ur_331_output <- lapply(chunks_331, ur_331)

# unpack list of tibbles, rowinding each
# also extract fips & delete footnotes
ur0716 <- bind_rows(ur_331_output) %>% 
  mutate(
    fips = str_sub(seriesID, start = 6, end = 10)
  ) %>% 
  select(fips, year, period, periodName, value, seriesID)




# write this out
write_csv(ur0716, "ur_07-16_331counties_monthly.csv")
  







# ALL COUNTIES 15-16 ====

# get list of all counties & FIPS codes from blscrapeR package
fips <- blscrapeR::county_fips %>% 
  mutate(allfips = paste0(fips_state, fips_county))

# extract just the codes
fips <- fips$allfips

# add affixes for BLS unemployment rate series
fips <- paste0("LAUCN", fips, "0000000003")


# split into chunks of 50
chunks <- split(fips, ceiling(seq_along(fips)/50))

# first <- chunks[1:3]

# create function to submit a chunk to the BLS API
ur <- function(x) {
  temp <- 
    bls_api(
      x,
      startyear = 2015,
      endyear = 2016,
    )
}


# apply the ur funtion to each list object (each chunk)
# returns a list of tibbles
ur_output <- lapply(chunks, ur)

# unpack list of tibbles, rowinding each
# also extract fips & delete footnotes
ur1516 <- bind_rows(ur_output) %>% 
  mutate(
    fips = str_sub(seriesID, start = 6, end = 10)
  ) %>% 
  select(fips, year, period, periodName, value, seriesID)

# save that shit
write_csv(ur1516, "ur_15-16_allcounties_monthly.csv")



# Average change in unemployment over May-October

months <- c("May", "June", "July", "August", "September", "October")

avg0810 <- ur2 %>% 
  filter(year == 2016, periodName %in% months) %>% 
  select(year, fips, periodName, value) %>% 
  spread(periodName, value) %>% 
  mutate(
    d0910 = ( log(October) - log(September) ) * 100,
    d0809 = ( log(September) - log(August) ) * 100,
    d0708 = ( log(August) - log(July) ) * 100,
    d0607 = ( log(July) - log(June) ) * 100,
    d0506 = ( log(June) - log(May) ) * 100,
    davg = round(((d0910+d0809+d0708+d0607+d0506) / 5), 3)
  )




iynk5tvnyk