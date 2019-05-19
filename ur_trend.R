# SETUP & IMPORT ====

# deatch all non-base packages
lapply(paste('package:',names(sessionInfo()$otherPkgs),sep=""),
       detach, character.only=TRUE, unload=TRUE)


library(AER)
library(tidyverse)
library(lubridate)


# Import the raw 2015-16 monthly unemployment data
ur1516 <- read_csv("ur_15-16_allcounties_monthly.csv")

# Create standard date format # subset Nov 15 - Oct 16
urate <- ur1516 %>% 
  mutate(
    date = 
      # combine year & month into YYYY-MM-DD format
      paste(
        # year is already 4 digits
        year,
        # take off the "M" prefix from period, extracting 2-digit month
        str_sub(period, start = 2, end = 3),
        # supply generic 1st day of each month
        "01",
        # put a "-" in between everything
        sep = "-"
      # convert string into standard date format
      ) %>% as.Date()
  ) %>%
  # rename unemployment rate something intuitive
  rename(urate = value) %>% 
  # keep just what we need
  select(fips, date, urate) %>% 
  # select just Oct-Oct
  filter(date >= "2015-11-01", date <= "2016-10-01") %>% 
  # make sure we're grouped by county
  group_by(fips) %>% 
  # sort by ascening date w/in each county group
  arrange(fips, date)

# get state & county names to attach
county_fips <- blscrapeR::county_fips %>% 
  mutate(fips = paste0(fips_state, fips_county)) %>% 
  filter(state != "PR") %>% 
  select(fips, state, county)

# attach state & county names (& rearrange)
urate <- urate %>% 
  left_join(county_fips) %>% 
  select(fips, date, state, county, urate)

# get state Division & Region for facetting
states <- 
  tibble(
    state = state.abb,
    state_name = state.name,
    division = state.division,
    region = state.region
  )

# add DC,
states <- rbind(states,
                c("DC", "District of Columbia", "South Atlantic", "South"))



# join to urate & reorder
urate <- left_join(urate, states, by = "state")
urate <- urate[, c(1:3, 6, 4, 7:8, 5)]


# exploratory plots ====

# combined time series plots by STATE
urate %>% ggplot(aes(x = date, y = urate)) +
  geom_line(aes(group = fips), color = "gray30", alpha = 0.3) +
  geom_smooth(method = "loess", se = FALSE) +
  facet_wrap(~state, ncol = 6) +
  labs(
    title = "Monthly Unemployment Rate",
    subtitle = "Nov. 2015 - Oct. 2016, by State & County",
    y = "Unemployment Rate",
    x = "Month"
  ) +
  scale_x_date(
    date_breaks = "3 months",
    date_labels = "%b%y"
  ) +
  theme_minimal()


# combined time series plots by DIVISION
urate %>% ggplot(aes(x = date, y = urate)) +
  geom_line(aes(group = fips), color = "gray30", alpha = 0.3) +
  geom_smooth(method = "loess", se = FALSE) +
  facet_wrap(~division, ncol = 3) +
  labs(
    title = "Monthly Unemployment Rate",
    subtitle = "Nov. 2015 - Oct. 2016, by Division & County",
    y = "Unemployment Rate",
    x = "Month"
  ) +
  scale_x_date(
    date_breaks = "3 months",
    date_labels = "%b%y"
  ) +
  theme_minimal()


# combined time series plots by REGION
urate %>% ggplot(aes(x = date, y = urate)) +
  geom_line(aes(group = fips), color = "gray30", alpha = 0.3) +
  geom_smooth(method = "loess", se = FALSE) +
  facet_wrap(~region, ncol = 2) +
  labs(
    title = "Monthly Unemployment Rate",
    subtitle = "Nov. 2015 - Oct. 2016, by Region & County",
    y = "Unemployment Rate",
    x = "Month"
  ) +
  scale_x_date(
    date_breaks = "3 months",
    date_labels = "%b%y"
  ) +
  theme_minimal()


# All of the above plots look fairly smooth, with no major aggregate trends
# apparent, especially at the state level, which is most electorally important.
# Alaska is all fucked up, but I'll probably drop it in the final analysis,
# as before for other annoying ALASKA PROBLEMS.

# Verdict: get 12- and 6- month county averages.





# 12-/6-month county averages ====

# 12mo (ur16 for 2016 election Urate variables )
ur16 <- urate %>% 
  group_by(fips) %>% 
  summarise(ur12mo = mean(urate)) %>% 
  left_join(county_fips, by = "fips") %>% 
  left_join(states, by = "state") %>% 
  select(fips, state, state_name, county, division, region, ur12mo)


# 6mo
ur6mo <- urate %>% 
  filter(date >= "2016-05-01") %>% 
  group_by(fips) %>% 
  summarise(ur6mo = mean(urate))

# put them together
ur16 <- left_join(ur16, ur6mo)



# write out ====
write_csv(ur16, "county_avg_urate_2016.csv")



## TREND TESTS =====

# import the 331-county panel Urates
ur_orig <- read_csv("ur_07-16_331counties_monthly.csv")

# get just Nov 07 - Oct 08 for testing purposes
ur0708 <- ur_orig %>% 
  mutate(
    date = 
      paste(
        year,
        str_sub(period, start = 2, end = 3),
        "01",
        sep = "-"
      ) %>% as.Date()
  ) %>% 
  rename(urate = value) %>% 
  filter(date >= "2007-11-01" & date <= "2008-10-01") %>% 
  select(fips, date, urate)


# get state & county names to attach
county_fips <- blscrapeR::county_fips %>% 
  mutate(fips = paste0(fips_state, fips_county)) %>% 
  filter(state != "PR") %>% 
  select(fips, state, county)


# get state Division & Region for facetting
states <- 
  tibble(
    state = state.abb,
    state_name = state.name,
    division = state.division,
    region = state.region
  )

# add DC,
states <- rbind(states,
                c("DC", "District of Columbia", "South Atlantic", "South"))
  

# join
ur0708 <- ur0708 %>% 
  left_join(county_fips, by = "fips") %>% 
  left_join(states, by = "state") %>% 
  select(fips, date, state, state_name, county, division, region, urate) %>% 
  arrange(fips, date)


# get just Guilford for testing & numeric dates
guilford <- filter(ur0708, fips == "37081") %>% 
  mutate(period = as.numeric(date))



# linear model for comparison
guilford.ols <- lm(urate ~ period, data = guilford)
summary(guilford.ols)

# save fitted values to data frame
guilford$ols.fit <- fitted(guilford.ols)
# save residuals
guilford$ols.err <- residuals(guilford.ols)

# redsidual plot
guilford %>%
  ggplot(aes(x = date, y = ols.err)) +
  geom_point(size = 4, color = "seagreen") +
  geom_smooth(method = "loess", size = 1.3, color = "seagreen", se = F) +
  geom_hline(yintercept = 0, size = 1.3, color = "orange") +
  labs(
    title = "Urate Residuals Plot",
    subtitle = "Guilford County, NC",
    x = "Month",
    y = "Error"
  ) +
  scale_x_date(
    date_breaks = "1 month",
    date_labels = "%b %y"
  ) +
  theme_minimal() +
  theme(
    panel.grid.minor.x = element_blank()
  )
# sinusoid shape indicative of correlated errors


# Generate lagged errors
guilford$ols.lagerr <- lag(guilford$ols.err)

# auxiliary regression on lagged errors
guilford.aux <- lm(ols.err ~ ols.lagerr, data = guilford)
summary(guilford.aux)
# debateable significance @ t=1.54, but fairly strong estimate @ .484
# assume some autocorrelation


# look at autocorrelation plot/
acf(guilford$urate, guilford$period)
acf(guilford$urate, guilford$period, plot = F)
# this shows first lag is StatSig autocorrelated, so roll w/ that


# more standardized: Durbin-Watson test
# Ho: no autocorrelation (rho == 0)
# Ha: autocorrelation (rho != 0)
durbinWatsonTest(guilford.ols)
# reject Ho / accept Ha: rho != 0


# store the rho estimate on auxiliary regression:
rho <- coefficients(guilford.aux)[2]


# lag unemployment rate
guilford$urate.lag <- lag(guilford$urate)
# get mean urate
urate.avg <- mean(guilford$urate)
# get rho-transformed urate
guilford$urate.rho <- urate.avg - rho * guilford$urate.lag

# lag date (month)
guilford$period.lag <- lag(guilford$period)
# get rho-transformed date???
guilford$period.rho <- guilford$period - rho * guilford$period.lag

# rho-transformed (Cochrane-Corcutt) model
guilford.rho <- lm(urate.rho ~ period.rho, data = guilford)
summary(guilford.rho)
summary(guilford.ols)

# Packaged Cochrane-Orcutt & Prais-Winsten models
library(orcutt)
library(prais)
guilford.orc <- cochrane.orcutt(guilford.ols)
guilford.pra <- prais_winsten(urate ~ period, data = guilford)
summary(guilford.orc)
summary(guilford.pra)


# dynamic (lagged dv)
guilford.ldv <- lm(urate ~ period + urate.lag, data = guilford)
summary(guilford.ldv)



# look at them all together
# (stargazer dones't accept the prais-winsten output)
library(stargazer)

stargazer(
  guilford.ols, guilford.rho, guilford.orc, guilford.ldv,
  type = "text",
  dep.var.caption = "",
  dep.var.labels = c("OLS", "Rho-Trans", "C-O", "Dynamic"),
  model.names = F
)



asdfasdfgdssa
