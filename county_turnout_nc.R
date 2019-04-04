library(sf)
library(leaflet)
library(tigris)
library(tidycensus)
library(Hmisc)
library(stringr)
library(tidyverse)
options(tigris_class = "sf")
options(tigris_use_cache = TRUE)


# COUNTY-LEVEL ACS DATA ====

# check out what's what
v16 <- load_variables(2016, "acs5", cache = T)

# variables
acs_vars <- c("B19083_001", "B19001_001", "B01002_001", "B16010_041")
# pull from census API
acs <- get_acs(geography = "county", variables = acs_vars, 
                state = "NC", geometry = TRUE, year = 2016, output = "wide")
# descriptive names and drop MOEs
acs <- acs %>% 
  rename(gini = B19083_001E,
         hhinc = B19001_001E,
         medage = B01002_001E,
         educ = B16010_041E,
         county = NAME) %>% 
  select(GEOID, county, gini, hhinc, medage, educ, geometry) %>% 
  mutate(county = str_sub(county, end = -24L))


acs_bu <- acs




# TURNOUT DATA =====

# import
load("countypres_2000-2016.RData")


# votes cast - from MIT Election data science lab, via harvard dataverse
# https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/VOQCHQ
# https://electionlab.mit.edu/data
  
  # 2016
  # filter - only need 1 party b/c only need total votes cast
  votes16 <- x %>% 
    filter(state_po == "NC", year == 2016, party == "democrat") %>% 
    select(county, totalvotes) %>% 
    rename(votes16 = totalvotes)
  
  # 2012
  votes12 <- x %>% 
    filter(state_po == "NC", year == 2012, party == "democrat") %>% 
    select(county, totalvotes) %>% 
    rename(votes12 = totalvotes)



# registrations total from NCSBOE: https://vt.ncsbe.gov/RegStat/
# totals taken from election day

  # 2016
  reg16 <- read_csv("reg16.csv") 
  reg16 <- reg16 %>% 
    select(county, Total) %>% 
    rename(reg16 = Total) %>% 
    mutate(county = tolower(county)) %>% 
    mutate(county = capitalize(county)) %>% 
    slice(1:100)  # two blank rows at end for some reason
  # correct capitalization on Mcdowell & New hanover
  reg16$county[reg16$county == "Mcdowell"] <- "McDowell"
  reg16$county[reg16$county == "New hanover"] <- "New Hanover"
  
    
  # 2012
  reg12 <- read_csv("reg12.csv") 
  reg12 <- reg12 %>% 
    select(county, Total) %>% 
    rename(reg12 = Total) %>% 
    mutate(county = tolower(county)) %>% 
    mutate(county = capitalize(county)) %>% 
    slice(1:100)  # two blank rows at end for some reason
  # correct capitalization on Mcdowell & New hanover
  reg12$county[reg12$county == "Mcdowell"] <- "McDowell"
  reg12$county[reg12$county == "New hanover"] <- "New Hanover"
    

  
# MERGE & CREATE TURNOUT % =====
full <- list(acs, reg16, votes16, reg12, votes12) %>%  # list of things to join
    reduce(full_join, by = "county") %>% 
    mutate(turnout16 = votes16 / reg16 * 100) %>% 
    mutate(turnout12 = votes12 / reg12 * 100)


  
# GRAPHICS ====
  
# gini  map
ginimap <- full %>% 
  ggplot() +
  geom_sf(aes(fill = gini, color = gini)) +
  coord_sf(crs = 6543) +
  scale_fill_viridis_c(name = "Gini") + 
  scale_color_viridis_c(name = "Gini") +
  labs(
    title = "Gini Coefficient by County, 2016", 
    caption = "Data: 2016 ACS 5-year Estimates, US Census Bureau"
    ) +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        legend.position = c(0.25, 0.2),
        legend.background = element_blank(),
        legend.direction = "horizontal",
        legend.title = element_blank(),
        legend.key.width = unit(2, "line"),
        legend.text = element_text(vjust = -.1)
        )  
ginimap


  
## 16 turnout map
turnout16map <- full %>% 
  ggplot() +
  geom_sf(aes(fill = turnout16, color = turnout16)) +
  coord_sf(crs = 6543) +
  scale_fill_viridis_c(name = "Turnout") + 
  scale_color_viridis_c(name = "Turnout") +
  labs(
    title = "Turnout Percentage by County, 2016", 
    caption = "Data: NC State Board of Elections, MIT Election & Data Science Lab"
    ) +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        legend.position = c(0.25, 0.2),
        legend.background = element_blank(),
        legend.direction = "horizontal",
        legend.title = element_blank(),
        legend.key.width = unit(2, "line"),
        legend.text = element_text(vjust = -.1)
        )  
turnout16map

color_pal <- colorQuantile(palette = "viridis", domain = full$turnout16, n = 20)
full %>% 
  leaflet(width = "100%") %>%
  addProviderTiles(provider = "CartoDB.Positron") %>% 
  addPolygons(
    popup = ~paste0(county, " County", "<br>", 
                    "2016 Turnout: ", prettyNum(round(turnout16, 1)), "%<br>",
                    "Gini Coefficient: ", prettyNum(round(gini, 2)), "<br>",
                    "Average HH Income: $", prettyNum(hhinc, big.mark = ",")),
    stroke = FALSE,
    smoothFactor = 0,
    fillOpacity = 0.7,
    color = ~color_pal(turnout16)
  )

# add with pipe if legend
  # addLegend(
  #   position = "bottomleft",
  #   pal = color_pal,
  #   values = ~turnout16,
  #   title = "2016 Turnout",
  #   opacity = 1
  # )
