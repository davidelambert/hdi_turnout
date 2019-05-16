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
