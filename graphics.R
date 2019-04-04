
# GRAPHICS ====
  
# gini  map
ginimap <- acs %>% 
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
hdimap <- acs %>% 
  ggplot() +
  geom_sf(aes(fill = hdi, color = hdi)) +
  scale_fill_viridis_c(name = "HDI", option = "plasma") + 
  scale_color_viridis_c(name = "HDI", option = "plasma") +
  labs(
    title = "Modified HDI by County, 2016", 
    caption = "Data: 2016 ACS 5-year Estimates, US Census Bureau"
    ) +
  borders("state") +
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




# Interactive
# LOOKS UGLY AS B/C OF RESCALED AK/HI!!!!!
# KEEP CODE TO USE IN ANOTHER VERSION W/O RESCALING (OR DROPPING)
color_pal <- colorQuantile(palette = "viridis", domain = acs$hdi, n = 20)
acs %>% 
  st_transform(crs = "+init=epsg:4326") %>%
  leaflet(width = "100%") %>%
  addProviderTiles(provider = "CartoDB.Positron") %>% 
  addPolygons(
    popup = ~paste0(county, " County", "<br>",
                    "Gini Coefficient: ", prettyNum(round(gini, 2)), "<br>",
                    "HDI: ", prettyNum(hdi, big.mark = ",")),
    stroke = FALSE,
    smoothFactor = 0,
    fillOpacity = 0.7,
    color = ~color_pal(hdi)
  )

# add with pipe if legend
  # addLegend(
  #   position = "bottomleft",
  #   pal = color_pal,
  #   values = ~turnout16,
  #   title = "2016 Turnout",
  #   opacity = 1
  # )