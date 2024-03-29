# SETUP & IMPORT ====

# deatch all non-base packages
lapply(paste('package:',names(sessionInfo()$otherPkgs),sep=""),
       detach, character.only=TRUE, unload=TRUE)

# clear environment
rm(list = ls())


# some libraries
library(AER)
library(plm)
library(lfe)
library(GGally)
library(plotly)
library(RColorBrewer)
library(tidyverse)


cnty <- read_csv("330_county_panel_08-16_subset.csv")
pool <- read_csv("330_county_panel_08-16_pooling_appended.csv")



# CORRELOGRAM ====

# define a 3-point, diverging palette
corr.colors <- brewer.pal(3, "PRGn")

# Corellogram
cnty %>% 
  select(to.vap, to.vep, femaleprop, whiteprop, nonwhprop, blackprop,
         hispprop, aapiprop, aianprop, multiprop, otherprop, oldprop,  
         uninsprop, ur.6mo, ur.oct, ur.trend, hdi) %>% 
  ggcorr(low = corr.colors[1], mid = corr.colors[2], high = corr.colors[3],
         label = TRUE, label_round = 2,
         hjust = 0.8, layout.exp = 1)






# HISTOGRAMS ====

# simualte normal distributions based on pooled distribution
set.seed(9832574)

norm.hdi <- rnorm(
  1980,
  mean = mean(pool$hdi[pool$year == "Pooled"]),
  sd = sd(pool$hdi[pool$year == "Pooled"])
)

norm.hdilog <- rnorm(
  1980,
  mean = mean(log(pool$hdi[pool$year == "Pooled"])),
  sd = sd(log(pool$hdi[pool$year == "Pooled"]))
)

norm.vep <- rnorm(
  1980,
  mean = mean(pool$to.vep[pool$year == "Pooled"]),
  sd = sd(pool$to.vep[pool$year == "Pooled"])
)

norm.veplog <- rnorm(
  1980,
  mean = mean(log(pool$to.vep[pool$year == "Pooled"])),
  sd = sd(log(pool$to.vep[pool$year == "Pooled"]))
)

# Define a palette:
density_colors <- brewer.pal(2, "Set2")

# hdi histograms
pool %>% 
  ggplot() +
  geom_histogram(bins = 33, mapping = aes(x = hdi, y = ..density..),
                 fill = "grey70", color = "white") +
  stat_density(aes(x = hdi, color = "Observed Density"),
               geom = "line", size = 1.2) +
  stat_density(aes(x = norm.hdi, color = "Simulated Normal"), 
               geom = "line", size = 1.2) +
  labs(
    title = "HDI Distribution Density",
    x = "",
    y = "" 
  ) +
  facet_wrap(~year) +
  scale_color_manual(name = "Densities", values = density_colors) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    legend.direction = "vertical",
    legend.title = element_blank()
  )

# Minor right skew, even when pooled




# LOG hdi histograms
pool %>% 
  ggplot() +
  geom_histogram(bins = 33, mapping = aes(x = log(hdi), y = ..density..),
                 fill = "grey70", color = "white") +
  stat_density(aes(x = log(hdi), color = "Observed Density"),
               geom = "line", size = 1.2) +
  stat_density(aes(x = norm.hdilog, color = "Simulated Normal"), 
               geom = "line", size = 1.2) +
  labs(
    title = "log(HDI) Distribution Density",
    x = "",
    y = "" 
  ) +
  facet_wrap(~year) +
  scale_color_manual(name = "Densities", values = density_colors) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    legend.direction = "vertical",
    legend.title = element_blank()
  )

# closer to normal






# VEP histograms
pool %>% 
  ggplot() +
  geom_histogram(bins = 33, mapping = aes(x = to.vep, y = ..density..),
                 fill = "grey70", color = "white") +
  stat_density(aes(x = to.vep, color = "Observed Density"),
               geom = "line", size = 1.2) +
  stat_density(aes(x = norm.vep, color = "Simulated Normal"), 
               geom = "line", size = 1.2) +
  labs(
    title = "Turnout (VEP) Distribution Density",
    x = "",
    y = "" 
  )+
  facet_wrap(~year) +
  scale_color_manual(name = "Densities", values = density_colors) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    legend.direction = "vertical",
    legend.title = element_blank()
  )

# Normal in Pooled, 2012, LEFT skewewed in 2008
# Log distsribution will make left skew worse,
# but may be desireable for a log-log elasticity regression...



# LOG VEP histograms
pool %>% 
  ggplot() +
  geom_histogram(bins = 33, mapping = aes(x = log(to.vep), y = ..density..),
                 fill = "grey70", color = "white") +
  stat_density(aes(x = log(to.vep), color = "Observed Density"),
               geom = "line", size = 1.2) +
  stat_density(aes(x = norm.veplog, color = "Simulated Normal"), 
               geom = "line", size = 1.2) +
  labs(
    title = "Log Turnout (VEP) Distribution Density",
    x = "",
    y = "" 
  )+
  facet_wrap(~year) +
  scale_color_manual(values = density_colors) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    legend.direction = "vertical",
    legend.title = element_blank()
  )

# OK, a little worse, but not THAT much....






# SCATTERPLOTS ====

# Define palette
scatter.colors <- brewer.pal(4, "Set2")

# base scatterplot, all models, no CI
pool %>% 
  ggplot(aes(x = hdi, y = to.vep)) +
  geom_point(
    shape = 16,
    alpha = .3,
    size = 2,
    stroke = 0
  ) +
  geom_smooth(
    se = FALSE,
    size = 1.5,
    method = "loess",
    aes(color = "LOESS", fill = "LOESS")
  ) +
  geom_smooth(
    se = FALSE,
    size = 1.5,
    method = "lm",
    aes(color = "OLS", fill = "OLS")
  ) +
  geom_smooth(
    se = FALSE,
    size = 1.5,
    method = "lm",
    formula = y ~ log(x),
    aes(color = "Linear-Log", fill = "Linear-Log")
  ) +
  geom_smooth(
    se = FALSE,
    size = 1.5,
    method = "lm",
    formula = y ~ splines::bs(x, degree = 2),
    aes(color = "Quadratic", fill = "Quadratic")
  ) +
  facet_wrap(~year) +
  scale_color_manual(name = "Model", values = scatter.colors) +
  scale_fill_manual(name = "Model", values = scatter.colors) +
  labs(
    title = "Turnout (VEP) & HDI",
    x = "HDI",
    y = "Turnout"
  ) +
  theme_minimal() 
    




# PLOTLY ====

# linear fits to plot

ols <- lm(to.vep ~ hdi, data = cnty)

loess <- loess(to.vep ~ hdi, data = cnty, span = 50,
               method = "loess", family = "symmetric", degree = 2)
loess.pred <- predict(loess)

cnty$hdi2 <- cnty$hdi^2
quad <- lm(to.vep ~ hdi + hdi2, data = cnty)
quad.pred <- predict(quad)


# a palette
plotly.colors <- brewer.pal(3, "Set2")

cnty %>% 
  mutate(year = as_factor(year)) %>% 
  plot_ly(data = ., x = ~hdi) %>% 
  add_markers(
    y = ~to.vep,
    color = ~year,
    colors = plotly.colors[1:3],
    # Hover text:
    text = ~paste0(county, ", ", state, 
                  "<br>HDI: ", round(hdi, 2), 
                  "<br>Turnout: ", round(to.vep * 100, 1), "%"),
    hoverinfo = 'text'
  ) %>% 
  add_lines(
    y = fitted(ols),
    name = "Pooled OLS",
    line = list(color = "black")
  ) %>% 
  add_lines(
    y = ~quad.pred,
    name = "Pooled Quadratic",
    line = list(color = "red")
  ) %>% 
  add_lines(
    y = ~fitted(loess(to.vep ~ hdi)),
    name = "Pooled LOESS",
    line = list(color = "yellow")
  )
  




sdfg

