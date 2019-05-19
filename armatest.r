## AR MODEL EXPERIMENTS


## USING:
## Hanck, A., Arnold, M., Gerber, A., & Schmelzer, M. Introduction to
##   Econometrics with R. Chapter 14. 2018-12-19.

# detach all non-base packages
lapply(paste('package:',names(sessionInfo()$otherPkgs),sep=""),
       detach, character.only=TRUE, unload=TRUE)


library(AER)
library(tidyverse)


## 14.1 =====

# import AER's CA school dataset
data("CASchools")
cas <- CASchools
rm(CASchools)



# student-teacher ratio
cas$str <- cas$students / cas$teachers

# unweighted average standardized test score
cas$scores <- (cas$read + cas$math) / 2

# naive OLS model
mod <- lm(scores ~ str, data = cas)
summary(mod)

predict(mod, newdata = data.frame(str = 20:25))






## 14.2  ====

library(quantmod)
library(readxl)

# import dataset w/ better names
usmacro <-
  read_xlsx(
    "sources/us_macro_quarterly.xlsx",
    col_types = c("text", rep("numeric", 9)),
    col_names = c("date", "gdpc96", "japan_ip", "pcectpi", "gs10",
                  "gs1", "tb3ms", "unrate", "exusuk", "cpiaucsl"),
    skip = 1
  )

# convert character date column to year-quarter format
usmacro$date <- as.yearqtr(usmacro$date, format = "%Y:0%q")


# grab simple bivariate time series as "extensible time-series" object
gdp <- xts(usmacro$gdpc96, usmacro$date)

# annualized GDP growth rate as same
gdp_growth <- xts(400 * log(gdp/stats::lag(gdp)))


# plot quarterly GDP
rownames_to_column(as.data.frame(gdp)) %>% 
  `colnames<-`(c("quarter", "gdp")) %>% 
  mutate(quarter = as.yearqtr(quarter, format = "%Y Q%q")) %>% 
  ggplot(aes(x = quarter, y = log(gdp))) +
  geom_smooth(method = "lm", se = F, color = "firebrick") +
  geom_line(color = "steelblue", size = 1.3) +
  scale_x_continuous(breaks = c(seq(1960, 2010, 10))) +
  labs(
    title = "U.S. Quarterly Real GDP",
    y = "Log GDP",
    x = "Date"
  ) +
  theme_minimal()


# plot quarterly GDP Growth Rate
rownames_to_column(as.data.frame(gdp_growth)) %>% 
  `colnames<-`(c("quarter", "growth")) %>% 
  mutate(quarter = as.yearqtr(quarter, format = "%Y Q%q")) %>% 
  ggplot(aes(x = quarter, y = growth)) +
  geom_line(color = "steelblue", size = 1.3) +
  scale_x_continuous(breaks = c(seq(1960, 2010, 10))) +
  geom_smooth(method = "lm", se = F, color = "firebrick") +
  labs(
    title = "U.S. Quarterly Real GDP Growth Rates",
    y = "Growth Rate",
    x = "Date"
  ) +
  theme_minimal()



# first 4 autocorrelations of growth rate
acf(na.omit(gdp_growth), plot = F)
acf(na.omit(gdp_growth))
# first 2 quarters lagged show stat-sig autocorrelation



# 14.3 =======

# susbset the GDP Growth data (don't know why, it's just in the book)
gdpgrsub <- gdp_growth["1962::2012"]

# estimate AR(1) model
ar.ols(gdpgrsub, order.max = 1, demean = F, intercept = T)

rownames_to_column(as.data.frame(gdpgrsub)) %>% 
  `colnames<-`(c("quarter", "growth")) %>% 
  mutate(quarter = as.yearqtr(quarter, format = "%Y Q%q")) %>% 
  lm(growth ~ quarter, data = .)


