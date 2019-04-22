library(psych)
library(tidyverse)
library(cowplot)
library(GGally)

load("stpan.Rdata")


# BASIC CORRELATIONS ====

# 2012: VEP .42 / VAP .24 / ACS .45
cor12_vep <- 
  cor(
    subset(stpan, year == "2012", select = hdi),
    subset(stpan, year == "2012", select = tovep)
  )[1]

cor12_vap <-   
  cor(
    subset(stpan, year == "2012", select = hdi),
    subset(stpan, year == "2012", select = tovap)
  )[1]

cor12_acs <-   
  cor(
    subset(stpan, year == "2012", select = hdi),
    subset(stpan, year == "2012", select = toacs)
  )[1]
  


# 2016: VEP .44 / VAP .25 / ACS .46 -- SIMILAR to 2012, GOOD!
cor16_vep <- 
  cor(
    subset(stpan, year == "2016", select = hdi),
    subset(stpan, year == "2016", select = tovep)
  )[1]

cor16_vap <-   
  cor(
    subset(stpan, year == "2016", select = hdi),
    subset(stpan, year == "2016", select = tovap)
  )[1]
  
cor16_acs <- 
  cor(
    subset(stpan, year == "2016", select = hdi),
    subset(stpan, year == "2016", select = toacs)
  )[1]

  
# POOLED: VEP .43 / VAP .25 / ACS .46 -- Similar, as expected
corpool_vep <- cor(stpan$hdi, stpan$tovep)[1]
corpool_vap <- cor(stpan$hdi, stpan$tovap)[1]
corpool_acs <- cor(stpan$hdi, stpan$toacs)[1]
  

  
# 2012 SCATTERPLOTS ====

# 2012 VEP
scat12_vep <-   
  stpan %>% 
    filter(year == "2012") %>% 
    ggplot(aes(x = hdi, y = tovep)) +
      geom_point(
        shape = 16,
        alpha = .7,
        size = 3,
        stroke = 0,
        color = "grey70"
      ) +
      geom_smooth(
        se = FALSE,
        color = "orange",
        size = 1.5,
        method = "loess"
      ) +
      geom_smooth(
        se = FALSE,
        color = "seagreen",
        size = 1.5,
        method = "lm"
      ) +
      scale_x_continuous(limits = c(4,7.25)) +
      scale_y_continuous(limits = c(.35, .8)) +
      theme_minimal() +
      labs(
        title = paste0("2012 VEP, r = ", round(cor12_vep, 3)),
        x = "HDI",
        y = "Turnout"
      )

# 2012 VAP
scat12_vap <-   
  stpan %>% 
    filter(year == "2012") %>% 
    ggplot(aes(x = hdi, y = tovap)) +
      geom_point(
        shape = 16,
        alpha = .7,
        size = 3,
        stroke = 0,
        color = "grey70"
      ) +
      geom_smooth(
        se = FALSE,
        color = "orange",
        size = 1.5,
        method = "loess"
      ) +
      geom_smooth(
        se = FALSE,
        color = "seagreen",
        size = 1.5,
        method = "lm"
      ) +
      scale_x_continuous(limits = c(4,7.25)) +
      scale_y_continuous(limits = c(.35, .8)) +
      theme_minimal() +
      labs(
        title = paste0("2012 VAP, r = ", round(cor12_vap, 3)),
        x = "HDI",
        y = "Turnout"
      )

# 2012 ACS VEP
scat12_acs <-   
  stpan %>% 
    filter(year == "2012") %>% 
    ggplot(aes(x = hdi, y = toacs)) +
      geom_point(
        shape = 16,
        alpha = .7,
        size = 3,
        stroke = 0,
        color = "grey70"
      ) +
      geom_smooth(
        se = FALSE,
        color = "orange",
        size = 1.5,
        method = "loess"
      ) +
      geom_smooth(
        se = FALSE,
        color = "seagreen",
        size = 1.5,
        method = "lm"
      ) +
      scale_x_continuous(limits = c(4,7.25)) +
      scale_y_continuous(limits = c(.35, .8)) +
      theme_minimal() +
      labs(
        title = paste0("2012 ACS VEP, r = ", round(cor12_acs, 3)),
        x = "HDI",
        y = "Turnout"
      )



# 2016 SCATTERPLOTS ====


# 2016 VEP
scat16_vep <-   
  stpan %>% 
    filter(year == "2016") %>% 
    ggplot(aes(x = hdi, y = tovep)) +
      geom_point(
        shape = 16,
        alpha = .7,
        size = 3,
        stroke = 0,
        color = "grey30"
      ) +
      geom_smooth(
        se = FALSE,
        color = "orange",
        size = 1.5,
        method = "loess"
      ) +
      geom_smooth(
        se = FALSE,
        color = "seagreen",
        size = 1.5,
        method = "lm"
      ) +
      scale_x_continuous(limits = c(4,7.25)) +
      scale_y_continuous(limits = c(.35, .8)) +
      theme_minimal() +
      labs(
        title = paste0("2016 VEP, r = ", round(cor16_vep, 3)),
        x = "HDI",
        y = "Turnout"
      )

# 2016 VAP
scat16_vap <-   
  stpan %>% 
    filter(year == "2016") %>% 
    ggplot(aes(x = hdi, y = tovap)) +
      geom_point(
        shape = 16,
        alpha = .7,
        size = 3,
        stroke = 0,
        color = "grey30"
      ) +
      geom_smooth(
        se = FALSE,
        color = "orange",
        size = 1.5,
        method = "loess"
      ) +
      geom_smooth(
        se = FALSE,
        color = "seagreen",
        size = 1.5,
        method = "lm"
      ) +
      scale_x_continuous(limits = c(4,7.25)) +
      scale_y_continuous(limits = c(.35, .8)) +
      theme_minimal() +
      labs(
        title = paste0("2016 VAP, r = ", round(cor16_vap, 3)),
        x = "HDI",
        y = "Turnout"
      )

# 2016 ACS VEP
scat16_acs <-   
  stpan %>% 
    filter(year == "2016") %>% 
    ggplot(aes(x = hdi, y = toacs)) +
      geom_point(
        shape = 16,
        alpha = .7,
        size = 3,
        stroke = 0,
        color = "grey30"
      ) +
      geom_smooth(
        se = FALSE,
        color = "orange",
        size = 1.5,
        method = "loess"
      ) +
      geom_smooth(
        se = FALSE,
        color = "seagreen",
        size = 1.5,
        method = "lm"
      ) +
      scale_x_continuous(limits = c(4,7.25)) +
      scale_y_continuous(limits = c(.35, .8)) +
      theme_minimal() +
      labs(
        title = paste0("2016 ACS VEP, r = ", round(cor16_acs, 3)),
        x = "HDI",
        y = "Turnout"
      )




# POOLED SCATTERPLOTS ====

# grey pallets
greys <- c("grey70", "grey30")

# Pooled VEP
pooled_vep <-   
  stpan %>% 
    ggplot(aes(x = hdi, y = tovep, color = year)) +
      geom_point(
        shape = 16,
        alpha = .7,
        size = 3,
        stroke = 0
      ) +
      scale_color_manual(values = greys) +
      geom_smooth(
        se = FALSE,
        color = "orange",
        size = 1.5,
        method = "loess"
      ) +
      geom_smooth(
        se = FALSE,
        color = "seagreen",
        size = 1.5,
        method = "lm"
      ) +
      scale_x_continuous(limits = c(4,7.25)) +
      scale_y_continuous(limits = c(.35, .8)) +
      theme_minimal() +
      theme(
        legend.position = c(.8, .25) ,
        legend.title = element_blank()
      ) +
      labs(
        title = paste0("Pooled VEP, r = ", round(corpool_vep, 3)),
        x = "HDI",
        y = "Turnout"
      )


# Pooled VAP
pooled_vap <-   
  stpan %>% 
    ggplot(aes(x = hdi, y = tovap, color = year)) +
      geom_point(
        shape = 16,
        alpha = .7,
        size = 3,
        stroke = 0
      ) +
      scale_color_manual(values = greys) +
      geom_smooth(
        se = FALSE,
        color = "orange",
        size = 1.5,
        method = "loess"
      ) +
      geom_smooth(
        se = FALSE,
        color = "seagreen",
        size = 1.5,
        method = "lm"
      ) +
      scale_x_continuous(limits = c(4,7.25)) +
      scale_y_continuous(limits = c(.35, .8)) +
      theme_minimal() +
      theme(
        legend.position = c(.8, .25) ,
        legend.title = element_blank()
      ) +
      labs(
        title = paste0("Pooled VAP, r = ", round(corpool_vap, 3)),
        x = "HDI",
        y = "Turnout"
      )
 

# Pooled ACS VEP
pooled_acs <-   
  stpan %>% 
    ggplot(aes(x = hdi, y = toacs, color = year)) +
      geom_point(
        shape = 16,
        alpha = .7,
        size = 3,
        stroke = 0
      ) +
      scale_color_manual(values = greys) +
      geom_smooth(
        se = FALSE,
        color = "orange",
        size = 1.5,
        method = "loess"
      ) +
      geom_smooth(
        se = FALSE,
        color = "seagreen",
        size = 1.5,
        method = "lm"
      ) +
      scale_x_continuous(limits = c(4,7.25)) +
      scale_y_continuous(limits = c(.35, .8)) +
      theme_minimal() +
      theme(
        legend.position = c(.8, .25) ,
        legend.title = element_blank()
      ) +
      labs(
        title = paste0("Pooled ACS VEP, r = ", round(corpool_acs, 3)),
        x = "HDI",
        y = "Turnout"
      )
 


# SCATTERPLOT GRID ====

# via cowplot::

# Lay out the main grid
grid <- plot_grid(scat12_vap, scat12_vep, scat12_acs,
                  scat16_vap, scat16_vep, scat16_acs,
                  pooled_vap, pooled_vep, pooled_acs)


# create the title, then add it
title <- ggdraw() +
  draw_label("HDI* and Voter Turnout by State",
             fontface = "bold", size = 18, x = 0.03, hjust = 0)
titled <- plot_grid(title, grid, ncol = 1, rel_heights = c(0.1, 1))


# create line legend
orline <- ggdraw() +
  draw_line(x = c(0.1,1), y = c(0.5,0.5), color = "orange", size = 1.5)
orcapt <- ggdraw() +
  draw_label("LOESS", size = 12, x = 0, hjust = -.25)
grline <- ggdraw() +
  draw_line(x = c(0.1,1), y = c(0.5,0.5), color = "seagreen", size = 1.5)
grcapt <- ggdraw() +
  draw_label("OLS", size = 12, x = 0, hjust = -.25)
caption <- ggdraw() +
  draw_label(
    "Sources: American Community Survey 1-Year Estimates, U.S. Elections Project, and U.S. Mortality Database.
    * Modified HDI adapted from Social Science Research Council's \"American HDI\" methodology, substituting a geometric mean.",
    size = 9, colour = "grey70", fontface = "italic", hjust = 1, x = .9
  )
legend <- plot_grid(orline, orcapt, grline, grcapt,
                    caption, nrow = 1, hjust = 1,
                    rel_widths = c(.15, .15, .15, .15, 1))

# add line legend to plot
addleg <- plot_grid(titled, legend, ncol = 1, rel_heights = c(1, 0.1))
addleg




# CORRELOGRAM ====

# via GGally::

# subset variables to correlate by year & pooled

cormat12 <- stpan %>% 
  filter(year == "2012") %>% 
  select(
    tovap, tovep, toacs, poptotal, vapacs, hdi, gini, nonwh_prop, black_prop, 
    hilat_prop, amind_prop, asian_prop, hawpi_prop, other_prop, multi_prop, 
    medage, health_index, attain_index, enroll_index, ed_index, inc_index
  )

cormat16 <- stpan %>% 
  filter(year == "2016") %>% 
  select(
    tovap, tovep, toacs, poptotal, vapacs, hdi, gini, nonwh_prop, black_prop, 
    hilat_prop, amind_prop, asian_prop, hawpi_prop, other_prop, multi_prop, 
    medage, health_index, attain_index, enroll_index, ed_index, inc_index
  )

cormatpool <- stpan %>%
  select(
    tovap, tovep, toacs, poptotal, vapacs, hdi, gini, nonwh_prop, black_prop, 
    hilat_prop, amind_prop, asian_prop, hawpi_prop, other_prop, multi_prop, 
    medage, health_index, attain_index, enroll_index, ed_index, inc_index
  )



# correlograms

corgram12 <- ggcorr(cormat12, low = "navyblue", 
                    mid = "white", high = "darkred",
                    label = TRUE, label_round = 2, 
                    hjust = 0.8, layout.exp = 1)

corgram16 <- ggcorr(cormat16, low = "darkorchid4",
                    mid = "white", high = "yellow",
                    label = TRUE, label_round = 2,
                    hjust = 0.8, layout.exp = 1)  

corgrampool <- ggcorr(cormatpool, low = "seagreen",
                    mid = "white", high = "orange",
                    label = TRUE, label_round = 2,
                    hjust = 0.8, layout.exp = 1)  



