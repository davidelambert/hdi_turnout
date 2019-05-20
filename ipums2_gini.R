# Caluclate gini coefficients for each of the 330 counties in ipumns 2 panel.

# detach all non-base packages
lapply(paste('package:',names(sessionInfo()$otherPkgs),sep=""),
       detach, character.only=TRUE, unload=TRUE)

library(ipumsr)
library(tidyverse)

# load raw but recoded data from ipums2.R (from ipums usa extract #14)
load("ipums2_recode.Rdata")
View(recode[sample(1:nrow(recode), 10), ])


# filter out under 16, instituionalized, negative/0 income
# approximating Civilian Non-Institutionalized Working Age Population
cniwap <- recode %>% filter(age >= 16, income > 0, inst != "inst")


# grab list of fips codes from 330-county panel
cntlist <- read_csv("330_county_panel_08-16_subset.csv")
cntlist <- unique(cntlist$fips)


# get full 5-digit fips as character, filter for 330 counties,
# subset only fips, person weight, income
cniwap <- cniwap %>% 
  zap_ipums_attributes() %>% 
  mutate(
    statefip = as.character(statefip),
    statefip = str_pad(statefip, 2, pad = "0"),
    countyfip = as.character(countyfip),
    countyfip = str_pad(countyfip, 3, pad = "0"),
    fips = paste0(statefip, countyfip)
  ) %>% 
  filter(fips %in% cntlist) %>% 
  select(fips, year, income, perwt) %>% 
  arrange(fips, year)


  
# define function to calculate a single county (x)
gini.pctile <- function(x) {
  # filter county
  g <- cniwap %>% filter(fips == x)
  # repeat each obervation by weight to represent entire county population
  g <- g[rep(rownames(g), times = g$perwt), 1:3]
  g <- g %>% 
    group_by(fips, year) %>%
    mutate(
      # calculate percentiles for each income observation
      pctile = ntile(income, 100),
      inc.total = sum(as.numeric(income))
    ) %>% 
    group_by(fips, year, pctile) %>% 
    summarise(
      # total county income by year
      inc.total = mean(inc.total),
      # total income for each percentile by year
      inc.pctile = sum(as.numeric(income))
    ) %>% 
    mutate(
      # percentile's share of total income
      inc.share = inc.pctile / inc.total,
      # proportion richer than each percentile (1-pctile)
      richer = 1 - pctile/100,
      # "area" of trapezoid for each percentile
      score = inc.share * (.01 + (2 * richer))
    ) %>% 
    # remove percentile group to summarize per year
    group_by(fips, year) %>% 
    # final gini calculation
    summarise(gini = 1 - sum(score))
  
}


start <- proc.time()
gini <- lapply(cntlist, gini.pctile)
proc.time() - start

gini <- bind_rows(gini)



# canned gini
library(acid)
ginican <- cniwap %>% 
  group_by(fips, year) %>% 
  summarise(
    ginican = weighted.gini(income, w = perwt)$Gini[1]
  )


# join calulated & canned
gini <- left_join(gini, ginican)


# check differences
gini$diff <- gini$gini - gini$ginican

# visualize difference:
densityplot(gini$diff)
# all basically 0, but slightly negative,
# so percentile method *slightly* overstates gini.



# write out
gini <- gini[1:4]
write_csv(gini, "gini.csv")

