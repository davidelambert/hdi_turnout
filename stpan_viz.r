library(tidyverse)
library(grid)
library(gridExtra)




# BASIC CORRELATIONS ====

# 2012: VEP .42 / VAP .24 / ACS .45
  cor(
    subset(stpan, year == "2012", select = hdi),
    subset(stpan, year == "2012", select = tovep)
  )
  
  cor(
    subset(stpan, year == "2012", select = hdi),
    subset(stpan, year == "2012", select = tovap)
  )
  
  cor(
    subset(stpan, year == "2012", select = hdi),
    subset(stpan, year == "2012", select = toacs)
  )
  


# 2016: VEP .44 / VAP .25 / ACS .46 -- SIMILAR to 2012, GOOD!
  cor(
    subset(stpan, year == "2016", select = hdi),
    subset(stpan, year == "2016", select = tovep)
  )
  
  cor(
    subset(stpan, year == "2016", select = hdi),
    subset(stpan, year == "2016", select = tovap)
  )
  
  cor(
    subset(stpan, year == "2016", select = hdi),
    subset(stpan, year == "2016", select = toacs)
  )

  
# POOLED: VEP .43 / VAP .25 / ACS .46 -- Similar, as expected
  cor(stpan$hdi, stpan$tovep)
  cor(stpan$hdi, stpan$tovap)
  cor(stpan$hdi, stpan$toacs)
  

  
# 2012 SCATTERPLOTS ====

# 2012 VEP
scat12_vep <-   
  stpan %>% 
    filter(year == "2012") %>% 
    ggplot(aes(x = hdi, y = tovep)) +
      geom_point(
        shape = 16,
        alpha = .7,
        size = 4,
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
        title = "2012 VEP",
        x = "HDI",
        y = "Turnout"
      )
scat12_vep  

# 2012 VAP
scat12_vap <-   
  stpan %>% 
    filter(year == "2012") %>% 
    ggplot(aes(x = hdi, y = tovap)) +
      geom_point(
        shape = 16,
        alpha = .7,
        size = 4,
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
        title = "2012 VAP",
        x = "HDI",
        y = "Turnout"
      )
scat12_vap  

# 2012 ACS VEP
scat12_acs <-   
  stpan %>% 
    filter(year == "2012") %>% 
    ggplot(aes(x = hdi, y = toacs)) +
      geom_point(
        shape = 16,
        alpha = .7,
        size = 4,
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
        title = "2012 ACS VEP",
        x = "HDI",
        y = "Turnout"
      )
scat12_acs  



# 2016 SCATTERPLOTS ====


# 2016 VEP
scat16_vep <-   
  stpan %>% 
    filter(year == "2016") %>% 
    ggplot(aes(x = hdi, y = tovep)) +
      geom_point(
        shape = 16,
        alpha = .7,
        size = 4,
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
        title = "2016 VEP",
        x = "HDI",
        y = "Turnout"
      )
scat16_vep  

# 2016 VAP
scat16_vap <-   
  stpan %>% 
    filter(year == "2016") %>% 
    ggplot(aes(x = hdi, y = tovap)) +
      geom_point(
        shape = 16,
        alpha = .7,
        size = 4,
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
        title = "2016 VAP",
        x = "HDI",
        y = "Turnout"
      )
scat16_vap  

# 2016 ACS VEP
scat16_acs <-   
  stpan %>% 
    filter(year == "2016") %>% 
    ggplot(aes(x = hdi, y = toacs)) +
      geom_point(
        shape = 16,
        alpha = .7,
        size = 4,
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
        title = "2016 ACS VEP",
        x = "HDI",
        y = "Turnout"
      )
scat16_acs  




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
        size = 4,
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
        title = "Pooled VEP",
        x = "HDI",
        y = "Turnout"
      )
pooled_vep  


# Pooled VAP
pooled_vap <-   
  stpan %>% 
    ggplot(aes(x = hdi, y = tovap, color = year)) +
      geom_point(
        shape = 16,
        alpha = .7,
        size = 4,
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
        title = "Pooled VAP",
        x = "HDI",
        y = "Turnout"
      )
pooled_vap  

# Pooled ACS VEP
pooled_acs <-   
  stpan %>% 
    ggplot(aes(x = hdi, y = toacs, color = year)) +
      geom_point(
        shape = 16,
        alpha = .7,
        size = 4,
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
        title = "Pooled ACS VEP",
        x = "HDI",
        y = "Turnout"
      )
pooled_acs  