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
rm("state")



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
panel <- panel[, c(2,1,35,3,30:34,4:29)]


# clean up
rm("states")



# sex & race/ethnicity proportions to examine as covariates
panel <- panel %>% 
  mutate(
    maleprop = malepop / poptotal,
    femaleprop = femalepop / poptotal,
    whiteprop = whitepop / poptotal,
    nonwhprop = 1 - whiteprop,
    blackprop = blackpop / poptotal,
    hispprop = hisppop / poptotal,
    asianprop = asianpop / poptotal,
    amindprop = amindpop / poptotal,
    multiprop = multipop / poptotal,
    otherprop = otherpop / poptotal
  )
  


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
View(panel[sample(1:nrow(panel), 10, replace = F), c(1:4, 46)])

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




# ATTAINMENT INDEX ====

# generate proportions for pop 25 & over
panel <- panel %>% 
  mutate(
    hsplus = hs + bacc + grad,
    baccplus = bacc + grad,
    hsprop = hsplus / pop25o,
    baccprop = baccplus / pop25o,
    gradprop = grad / pop25o,
    attain_score = hsprop + baccprop + gradprop
  )

# SSRA goalposts of 0.5, 2 should be ok
describe(subset(panel, year == 2008, select = attain_score))
describe(subset(panel, year == 2010, select = attain_score))
describe(subset(panel, year == 2012, select = attain_score))
describe(subset(panel, year == 2014, select = attain_score))
describe(subset(panel, year == 2016, select = attain_score))

attmin <- 0.5
attmax <- 2

# create index
panel <- panel %>% 
  mutate(attain_index = ((attain_score - attmin) / (attmax - attmin)) * 10)

describe(subset(panel, year == 2008, select = attain_index))
describe(subset(panel, year == 2010, select = attain_index))
describe(subset(panel, year == 2012, select = attain_index))
describe(subset(panel, year == 2014, select = attain_index))
describe(subset(panel, year == 2016, select = attain_index))
# looks good!



# ENROLLMENT INDEX ====

panel <- panel %>% 
  mutate(enrollprop = enroll / pop324)

describe(subset(panel, year == 2008, select = enrollprop))
describe(subset(panel, year == 2010, select = enrollprop))
describe(subset(panel, year == 2012, select = enrollprop))
describe(subset(panel, year == 2014, select = enrollprop))
describe(subset(panel, year == 2016, select = enrollprop))
# SSRA's .60/.95 goalpsts should be ok

enrollmin <- .60
enrollmax <- .95

panel <- panel %>% 
  mutate(
    enroll_index = ( (enrollprop - enrollmin) / (enrollmax - enrollmin) ) * 10
  )

describe(subset(panel, year == 2008, select = enroll_index))
describe(subset(panel, year == 2010, select = enroll_index))
describe(subset(panel, year == 2012, select = enroll_index))
describe(subset(panel, year == 2014, select = enroll_index))
describe(subset(panel, year == 2016, select = enroll_index))
# ok





# OVERALL EDUCATION INDEX ====

panel <- panel %>% 
  mutate(ed_index = ((2/3) * attain_index) + ((1/3) * enroll_index))

describe(subset(panel, year == 2008, select = ed_index))
describe(subset(panel, year == 2010, select = ed_index))
describe(subset(panel, year == 2012, select = ed_index))
describe(subset(panel, year == 2014, select = ed_index))
describe(subset(panel, year == 2016, select = ed_index))
# looks good!




# INCOME INDEX ====
  # 
  # # Median income
  # describe((subset(panel, year == 2008, select = incmed)))
  # describe((subset(panel, year == 2010, select = incmed)))
  # describe((subset(panel, year == 2012, select = incmed)))
  # describe((subset(panel, year == 2014, select = incmed)))
  # describe((subset(panel, year == 2016, select = incmed)))
  # 
  # 
  # # Mean income
  # describe((subset(panel, year == 2008, select = incmean)))
  # describe((subset(panel, year == 2010, select = incmean)))
  # describe((subset(panel, year == 2012, select = incmean)))
  # describe((subset(panel, year == 2014, select = incmean)))
  # describe((subset(panel, year == 2016, select = incmean)))
  # 
  # 
  # # Per capita income
  # describe((subset(panel, year == 2008, select = incpc)))
  # describe((subset(panel, year == 2010, select = incpc)))
  # describe((subset(panel, year == 2012, select = incpc)))
  # describe((subset(panel, year == 2014, select = incpc)))
  # describe((subset(panel, year == 2016, select = incpc)))


# The above indicate median income w/ SRRA 2016$ goalposts should be OK
incmin <- 16009
incmax <- 67730

panel <- panel %>% 
  mutate(
    inc_index = (log(incmed) - log(incmin)) / (log(incmax) - log(incmin)) * 10
  )


describe(subset(panel, year == 2008, select = inc_index))
describe(subset(panel, year == 2010, select = inc_index))
describe(subset(panel, year == 2012, select = inc_index))
describe(subset(panel, year == 2014, select = inc_index))
describe(subset(panel, year == 2016, select = inc_index))






# HDI ====
panel <- panel %>% 
  mutate(hdi = (health_index * ed_index * inc_index) ^ (1/3) )


describe(subset(panel, year == 2008, select = hdi))
describe(subset(panel, year == 2010, select = hdi))
describe(subset(panel, year == 2012, select = hdi))
describe(subset(panel, year == 2014, select = hdi))
describe(subset(panel, year == 2016, select = hdi))
# looks good!



# pooled histogram & boxplot
meanpool <- describe(panel$hdi)$mean
sdpool <- describe(panel$hdi)$sd
normpool <- rnorm(255, mean = meanpool, sd = sdpool)

panel %>% 
  ggplot(mapping = aes(x = hdi)) +
  geom_histogram(bins = 11, mapping = aes(y = ..density..),
                 fill = "grey70", color = "white") +
  geom_density(color = "orange", size = 1.2) +
  geom_density(mapping = aes(x = normpool),
               color = "seagreen", size = 1.2) +
  geom_abline(slope = 0, intercept = 0, size = 1.2, color = "grey70") +
  theme_minimal()

panel %>% 
  ggplot(aes(y = hdi)) + 
  geom_boxplot() + coord_flip() + theme_minimal()




# Year-Facetted Histograms & boxplots
panel %>% 
  ggplot(mapping = aes(x = hdi)) +
  geom_histogram(bins = 11, mapping = aes(y = ..density..),
                 fill = "grey70", color = "white") +
  geom_density(color = "orange", size = 1.2) +
  geom_density(mapping = aes(x = normpool),
               color = "seagreen", size = 1.2) +
  geom_abline(slope = 0, intercept = 0, size = 1.2, color = "grey70") +
  theme_minimal() +
  facet_wrap(~year)

panel %>% 
  ggplot(aes(y = hdi)) + 
  geom_boxplot() + coord_flip() + theme_minimal() + 
  facet_wrap(~year, ncol = 1)



# TURNOUT ====

# import 2008
to08 <- 
  read_csv(
    "sources/2008 November General Election - Turnout Rates.csv",
    skip = 3,
    col_names = c("state", "veptotal", "tovep", "tovap",
                  "totalvotes", "presvotes", "veppop", "vappop",
                  "noncitpct", "prisonpop", "probatpop", "parolepop",
                  "inelpop", "overseas", "st")
  ) %>% 
  mutate(
    year = "2008",
    tovep = as.numeric(str_sub(tovep, 1, 4)) / 100,
    tovap = as.numeric(str_sub(tovap, 1, 4)) / 100
  ) %>% 
  select("year", "st", "presvotes", "tovep", "tovap")
  
# Import 2010
to10 <- 
  read_csv(
    "sources/2010 November General Election - Turnout Rates.csv",
    skip = 3,
    col_names = c("state", "veptotal", "tovep", "tovap",
                  "totalvotes", "presvotes", "veppop", "vappop",
                  "noncitpct", "prisonpop", "probatpop", "parolepop",
                  "inelpop", "overseas", "st")
  ) %>% 
  mutate(
    year = "2010",
    tovep = as.numeric(str_sub(tovep, 1, 4)) / 100,
    tovap = as.numeric(str_sub(tovap, 1, 4)) / 100
  ) %>% 
  select("year", "st", "presvotes", "tovep", "tovap")
  
# Import 2012
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
turnout <- rbind(to08, to10, to12, to14, to16)
turnout <- arrange(turnout, st, year)


# join to main dataset
turnout$year <- as.numeric(turnout$year)
panel <- left_join(panel, turnout, by = c("st" = "st", "year" = "year"))


# add turnout based on ACS totalpop - under 18 - noncitizen 18+
panel <- panel %>% 
  mutate(
    toacs = presvotes / (poptotal - popu18 - noncit18o)
  )





# WRITE OUT ====

rm(list = setdiff(ls(), "panel"))

save(panel, file = "bigpanel.Rdata")
write_csv(panel, "bigpanel.csv")
