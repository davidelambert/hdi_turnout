## need at least dplyr, stringr, tidyr, ggplot2. Might as well install the whole thing.
require("tidyverse")


##### 2016 Turnout
## Read in 2016 turnout data
## Prior manual manipulation in Excel:
    ## "State" vairable name moved 1 row down
    ## percentage column conversted to "General" number format
turnout16 <- read.csv("2016 November General Election - Turnout Rates.csv", skip = 1)
## lowercase variable names so my pinkies don't get tired hitting Shift
turnout16 <- `colnames<-`(turnout16, str_to_lower(colnames(turnout16))) %>% 
    ## drop the national total row
    filter(state != "United States") %>% 
    ## keep only the state, imputed eligible turnout percentage,
    ## and voting age turnout percentage variables
    select(state, vep.highest.office, vap.highest.office) %>% 
    ## rename vep/vap columns to reference 2016
    ## (dinstinguish from 2012 in final data frame)
    `colnames<-`(c("state", "vep16", "vap16"))
    




##### 2012 Turnout
## everything same as above
turnout12 <- read.csv("2012 November General Election v2.0 - Turnout Rates.csv", skip = 1)
turnout12 <- `colnames<-`(turnout12, str_to_lower(colnames(turnout12))) %>% 
    filter(state != "United States") %>% 
    select(state, vep.highest.office, vap.highest.office) %>% 
    `colnames<-`(c("state", "vep12", "vap12"))





##### Gini coefficient
## import from ACS
gini <- read.csv("ACS_16_5YR_B19083_with_ann.csv", skip = 1)
## lowercase
gini <- `colnames<-`(gini, str_to_lower(colnames(gini))) %>% 
    ## keep only relevant columns
    select(c("geography", "estimate..gini.index")) %>% 
    ## rename columns
    `colnames<-`(c("state", "gini"))






##### Median personal earnings
## import
earn.pers <- read.csv("ACS_16_5YR_S2001_with_ann.csv", skip = 1)
## lowercase
earn.pers <- `colnames<-`(earn.pers, str_to_lower(colnames(earn.pers))) %>% 
    ## keep only relevant columns
    select(c("geography", "total..estimate..median.earnings..dollars.")) %>% 
    ## rename columns
    `colnames<-`(c("state", "earn.pers")) %>% 
    ## create variable in measured in thousands:
    mutate(earn.pers.1k = earn.pers / 1000)





##### Median household earnings
## import
earn.hh <- read.csv("ACS_16_5YR_S1901_with_ann.csv", skip = 1)
## lowercase
earn.hh <- `colnames<-`(earn.hh, str_to_lower(colnames(earn.hh))) %>% 
    ## keep only relevant columns
    select(c("geography", "households..estimate..total")) %>% 
    ## rename columns
    `colnames<-`(c("state", "earn.hh")) %>% 
    ## create variable measured in thousands
    mutate(earn.hh.1k = earn.hh / 1000)






##### over60 than 60 & median age
## import
age <- read.csv("ACS_16_5YR_S0101_with_ann.csv", skip = 1)
## lowercase
age <- `colnames<-`(age, str_to_lower(colnames(age))) %>% 
    ## keep only relevant columns
    select(c("geography", 
             "total..estimate..selected.age.categories...60.years.and.over",
             "total..estimate..summary.indicators...median.age..years.")) %>% 
    ##rename columns
    `colnames<-`(c("state", "over60", "median.age")) %>% 
    ## convert percent to proportion
    mutate(over60 = over60 / 100)






##### education proxied by bachelor's degreee or higher
## import
prop.ba <- read.csv("ACS_16_5YR_S1501_with_ann.csv", skip = 1)
## lowercase
prop.ba <- `colnames<-`(prop.ba, str_to_lower(colnames(prop.ba))) %>% 
    ## keep only relevant columns
    select(c("geography",
             "percent..estimate..population.25.years.and.over...bachelor.s.degree",
             "percent..estimate..percent.bachelor.s.degree.or.higher")) %>% 
    ## rename columns
    `colnames<-`(c("state", "ba.over25", "ba.total")) %>% 
    ## convert percentages to proportions
    mutate(ba.over25 = ba.over25 / 100,
           ba.total = ba.total / 100)





##### proportion white
## import
prop.white <- read.csv("ACS_16_5YR_B02001_with_ann.csv", skip = 1)
##lowercase
prop.white <- `colnames<-`(prop.white, str_to_lower(colnames(prop.white))) %>% 
    ## keep only relevant columns
    select(c("geography",
             "estimate..total.",
             "estimate..total....white.alone")) %>% 
    ## rename columns
    `colnames<-`(c("state", "pop.total", "pop.white")) %>% 
    ## generate proportion white from population totals
    mutate(prop.white = pop.white / pop.total) %>% 
    ## drop absolute population columns
    select(c("state", "prop.white"))





##### Merge
turnout <- turnout16 %>% 
    merge(gini, by = "state", sort = FALSE) %>% 
    merge(earn.pers, by = "state", sort = FALSE) %>% 
    merge(earn.hh, by = "state", sort = FALSE) %>% 
    merge(age, by = "state", sort = FALSE) %>% 
    merge(prop.ba, by = "state", sort = FALSE) %>% 
    merge(prop.white, by = "state", sort = FALSE) %>%  
    merge(turnout12, by = "state", sort = FALSE)
    

            



##### cleanup and export
## drop the imported files
rm(list = "age", "earn.hh", "earn.pers", "gini", "prop.ba",
   "prop.white", "turnout12", "turnout16")
## export the slim datatset
write.csv(turnout, file = "turnout.csv")
