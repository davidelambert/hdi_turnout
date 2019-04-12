library(sf)
library(tigris)
library(tidycensus)
library(leaflet)
library(Hmisc)
library(psych)
library(stringr)i
library(viridis)
library(tidyverse)
options(tigris_class = "sf")
options(tigris_use_cache = TRUE)


# get all versions
load("acs_.Rdata")
load("fuckak_.Rdata")

# GRAPHICS ====
  
# gini  map
ginimap <- fuckak %>% 
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
hdimap <- fuckak %>% 
  ggplot() +
  geom_sf(aes(fill = hdi, color = "white")) +
  scale_fill_viridis_c(name = "HDI", option = "plasma") + 
  labs(
    title = "Modified HDI by County, 2016", 
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
hdimap




# Interactive ========

color_pal <- colorNumeric(palette = "viridis", domain = fuckak$hdi)

fuckak %>% 
  st_transform(crs = "+proj=longlat +datum=WGS84") %>%
  leaflet(width = "100%") %>%
  addProviderTiles(provider = "CartoDB.Positron") %>% 
  addPolygons(
    popup = ~paste0(county_state, "<br>",
                    "HDI: ", round(hdi, 2), "<br>",
                    "Life Expectancy:", round(le, 2), " years <br>",
                    "Per Capita Income: $", round(incpc, 2), "<br>"
                    ),
    stroke = FALSE,
    smoothFactor = 0,
    fillOpacity = 0.7,
    color = ~color_pal(hdi)
  ) %>% 
  addPolylines(color = "white", weight = .7) # %>% 
  # addLegend(
  #   position = "bottomleft",
  #   pal = color_pal,
  #   values = ~hdi,
  #   title = "Modified HDI",
  #   opacity = 1
  # )
