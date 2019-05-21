# SETUP & IMPORT ====

# deatch all non-base packages
lapply(paste('package:',names(sessionInfo()$otherPkgs),sep=""),
       detach, character.only=TRUE, unload=TRUE)

# clear environment
rm(list = ls())


# some libraries
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
         uninsprop, ur.6mo, ur.oct, ur.trend, gini, hdi) %>% 
  ggcorr(low = corr.colors[1], mid = corr.colors[2], high = corr.colors[3],
         label = TRUE, label_round = 2,
         hjust = 0.8, layout.exp = 1)






# CORRELORGRAM 2 ====

corr.sub <- cnty %>% 
  select(to.vap, to.vep, femaleprop, whiteprop, nonwhprop, blackprop,
         hispprop, aapiprop, aianprop, multiprop, otherprop, oldprop,  
         uninsprop, ur.6mo, ur.oct, ur.trend, gini, hdi)

corr.mat <- round(cor(corr.sub), 2)

corr.mat[lower.tri(corr.mat)] <- NA

corr.mat <- t(corr.mat)

library(reshape2)

corr.melt <- melt(corr.mat, na.rm = TRUE)

corr.pal <- brewer.pal(7, "PRGn")

corr.full <- corr.melt %>% 
  ggplot(aes(x = Var1, y = Var2, fill = value)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(
    low = corr.pal[1], mid = corr.pal[4], high = corr.pal[7],
    midpoint = 0, limit = c(-1,1), name = "Pearson's r"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)
  ) +
  coord_fixed()
  

ggplotly(corr.full)


hkfdshksdf




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


# Define a palette:
density_colors <- brewer.pal(3, "Set2")


# hdi histograms
hist.hdi <- 
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

ggplotly(hist.hdi)

# Minor right skew, even when pooled




# LOG hdi histograms
hist.loghdi <- 
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

ggplotly(hist.loghdi)

# closer to normal



# VEP histograms
hist.vep <- 
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

ggplotly(hist.vep)

# Normal in Pooled, 2012, LEFT skewewed in 2008
# Log distsribution will make left skew worse,




# SCATTERPLOTS ====

# Define palette
scatter.colors <- brewer.pal(4, "Set2")

# grab squared and log transformations of HDI
pool$hdi2 <- pool$hdi^2
pool$loghdi <- log(pool$hdi)

# base scatterplot, all models, no CI
scatter <- 
pool %>% 
  ggplot(aes(x = hdi, y = to.vep)) +
  geom_point(
    shape = 16,
    alpha = .3,
    size = 2,
    stroke = 0,
    aes(text = paste0(county, ", ", st,
                      "<br>HDI: ", round(hdi, 2),
                      "<br>Turnout: ", round(to.vep * 100, 1), "%"))
  ) +
  geom_smooth(
    aes(color = "LOESS", fill = "LOESS"),
    method = "loess",
  ) +
  geom_smooth(
    aes(color = "OLS", fill = "OLS"),
    method = "lm",
  ) +
  geom_smooth(
    aes(color = "Linear-Log", fill = "Linear-Log"),
    method = "lm",
    formula = y ~ log(x),
  ) +
  geom_smooth(
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


    
ggplotly(scatter, tooltip = "text")



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

