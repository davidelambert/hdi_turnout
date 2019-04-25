# SETUP & IMPORT ====

# deatch all non-base packages
lapply(paste('package:',names(sessionInfo()$otherPkgs),sep=""),
       detach, character.only=TRUE, unload=TRUE)

library(psych)
library(AER)
library(plm)
library(lfe)
library(tidyverse)

# build panel
load("ipums08.Rdata")
panel <- state
load("ipums10.Rdata")
panel <- rbind(panel, state)
load("ipums12.Rdata")
panel <- rbind(panel, state)
load("ipums14.Rdata")
panel <- rbind(panel, state)
load("ipums16.Rdata")
panel <- rbind(panel, state)
panel <- arrange(panel, state, year)
panel_bu <- panel
rm("states")



# ADD HELPER VARIABLES ====

# year dummies
panel <- panel %>% 
  mutate(
    d08 = ifelse(year == 2008, 1, 0),
    d10 = ifelse(year == 2010, 1, 0),
    d12 = ifelse(year == 2012, 1, 0),
    d14 = ifelse(year == 2014, 1, 0),
    d16 = ifelse(year == 2016, 1, 0),
  )


# add state abbreviations
st <- state.abb
# which(st == "DE")
st <- append(st, "DC", after = 7)
state <- state.name
# which(state == "Delaware")
state <- append(state, "District of Columbia", after = 7)
states <- tibble(state, st)
panel <- merge(panel, states, by = "state")



# rearrange
panel <- panel[, c(2,1,33,3,28:32,4:27)]

rm("states")



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
    filter(
      Year == 2008 & Age == 0 |
      Year == 2010 & Age == 0 |
      Year == 2012 & Age == 0 |
      Year == 2014 & Age == 0 |
      Year == 2016 & Age == 0
    ) %>% 
    select(PopName, Year, ex) %>% 
    rename(st = PopName,
           year = Year,
           lexp = ex)
  le <- rbind(le, tmp)
}


# join
panel <- left_join(panel, le, by = c("st" = "st", "year" = "year"))

# Spot Check
View(panel[sample(1:nrow(panel), 10, replace = F), c(1:4, 34)])

# clean up
rm(list = c("tmp", "le"))





# HEALTH INDEX ====

# get observed Max & Min for each year
  describe(subset(panel, year == "2008", select = lexp))
  describe(subset(panel, year == "2010", select = lexp))
  describe(subset(panel, year == "2012", select = lexp))
  describe(subset(panel, year == "2014", select = lexp))
  describe(subset(panel, year == "2016", select = lexp))
  
# based on above, SSRA goalposts of 66-90 should be OK
lemin <- 66
lemax <- 90

# construct index
panel <- panel %>% 
  mutate(health_index = ((lexp - lemin) / (lemax - lemin)) * 10)

# check out
describe(subset(panel, year == "2008", select = health_index))
describe(subset(panel, year == "2012", select = health_index))
describe(subset(panel, year == "2012", select = health_index))
describe(subset(panel, year == "2014", select = health_index))
describe(subset(panel, year == "2016", select = health_index))
# medians close enough to 5. Lowish ranges. Go with it.




