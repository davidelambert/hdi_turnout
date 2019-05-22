# SETUP & IMPORT ====

# detach all non-base packages
lapply(paste('package:',names(sessionInfo()$otherPkgs),sep=""),
       detach, character.only=TRUE, unload=TRUE)

# clear environment
rm(list = ls())


# some libraries
# library(GGally) # only needed for old, unused correlogram
library(plotly)
library(RColorBrewer)
library(viridis)
library(reshape2)
library(tidyverse)


cnty <- read_csv("330_county_panel_08-16_subset.csv")
pool <- read_csv("330_county_panel_08-16_pooling_appended.csv")



# # CORRELOGRAM ====
# 
# # define a 3-point, diverging palette
# corr.colors <- brewer.pal(3, "PRGn")
# 
# # Corellogram
# cnty %>% 
#   select(to.vap, to.vep, femaleprop, whiteprop, nonwhprop, blackprop,
#          hispprop, aapiprop, aianprop, multiprop, otherprop, oldprop,  
#          uninsprop, ur.6mo, ur.oct, ur.trend, gini, hdi) %>% 
#   ggcorr(low = corr.colors[1], mid = corr.colors[2], high = corr.colors[3],
#          label = TRUE, label_round = 2,
#          hjust = 0.8, layout.exp = 1)
# 





# CORRELORGRAM 2 ====

# subset what we want in the matrix
corr.sub <- cnty %>% 
  select(to.vap, to.vep, femaleprop, whiteprop, nonwhprop, blackprop,
         hispprop, aapiprop, aianprop, multiprop, otherprop, oldprop,  
         uninsprop, ur.6mo, ur.oct, ur.trend, gini, hdi)

# grab full, square matrix
corr.mat <- round(cor(corr.sub), 2)

# NA's to upper triangle, will keep only lower
corr.mat[upper.tri(corr.mat)] <- NA

# "melt" matrix into 3col df, w/ each vairable pair & correlation
# (also drop the upper triangle NA's)
corr.melt <- melt(corr.mat, na.rm = TRUE)

# get all 7 colors of canned palette to force darker/saturated endpoints
corr.pal <- brewer.pal(7, "PRGn")

# create the matrix
corr.full <- corr.melt %>% 
  ggplot(aes(x = Var1, y = Var2, fill = value)) +
  geom_tile(
    color = "white",
    aes(text = paste0(Var1, ",<br>", Var2, "<br>r = ", value))
  ) +
  scale_fill_gradient2(
    low = corr.pal[1], mid = corr.pal[4], high = corr.pal[7],
    midpoint = 0, limit = c(-1,1), name = "Pearson's r"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
    axis.text.y = element_text(vjust = -1),
    axis.title = element_blank(),
    panel.grid = element_blank()
    ) 
  

# send to plotly
ggplotly(corr.full, tooltip = "text")


# clean up all corr. prefixes except the final ggplot object corr.full
rm(list = setdiff(ls(pattern = "^corr."), "corr.full"))


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
  scale_color_manual(
    name = "Density",
    values = plasma(2, begin = 0, end = 0.7, alpha = 0.7)
  ) +
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
  scale_color_manual(
    name = "Density",
    values = plasma(2, begin = 0.1, end = 0.8, alpha = 0.7)
  ) +
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
  scale_color_manual(
    name = "Density",
    values = plasma(2, begin = 0.2, end = 0.9, alpha = 0.7)
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    legend.direction = "vertical",
    legend.title = element_blank()
  )

ggplotly(hist.vep)

# Normal in Pooled, 2012, LEFT skewewed in 2008
# Log distsribution will make left skew worse,\
# but could still be usefull for a log-log elasticity model


hist.logvep <- 
  pool %>% 
  ggplot() +
  geom_histogram(bins = 33, mapping = aes(x = log(to.vep), y = ..density..),
                 fill = "grey70", color = "white") +
  stat_density(aes(x = log(to.vep), color = "Observed Density"),
               geom = "line", size = 1.2) +
  stat_density(aes(x = norm.veplog, color = "Simulated Normal"), 
               geom = "line", size = 1.2) +
  labs(
    title = "log(VEP) Distribution Density",
    x = "",
    y = "" 
  )+
  facet_wrap(~year) +
  scale_color_manual(
    name = "Density",
    values = plasma(2, begin = 0.3, end = 1, alpha = 0.7)
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    legend.direction = "vertical",
    legend.title = element_blank()
  )

ggplotly(hist.logvep)




# SCATTERPLOTS ====

# Define palette
scatter.colors <- brewer.pal(5, "Set2")

# base scatterplot, all models, no CI
scatter <- 
pool %>% 
  ggplot(aes(x = hdi, y = veppct)) +
  geom_point(
    shape = 16,
    alpha = .3,
    size = 2,
    stroke = 0,
    aes(text = paste0(county, ", ", st,
                      "<br>HDI: ", round(hdi, 2),
                      "<br>Turnout: ", round(veppct, 1), "%"))
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







# Scatter Unfacetted ====

# some new computed variables
cnty <- cnty %>% 
  mutate(
    hdi2 = hdi ^ 2,
    loghdi = log(hdi),
    veppct = to.vep * 100,
    logvep = log(veppct)
  )

# Models 
loess <- loess(data = cnty, veppct ~ hdi)
lm <- lm(data = cnty, veppct ~ hdi)
quad <- lm(data = cnty, veppct ~ hdi + hdi2)
linlog <- lm(data = cnty, veppct ~ loghdi)
loglog <- lm(data = cnty, logvep ~ loghdi)


# Define palette
unface.colors <- brewer.pal(5, "Set2")

# base scatterplot, all models, no CI
unface <- 
  cnty %>% 
  ggplot() +
  geom_point(
    alpha = .3,
    size = 2,
    aes(
      x = hdi, y = veppct, shape = as_factor(year),
      text = paste0(county, ", ", st,
                    "<br>HDI: ", round(hdi, 2),
                    "<br>Turnout: ", round(veppct, 1), "%")
    )
  ) +
  geom_line(
    data = data.frame(x.loess = as.vector(loess$x),
                      y.loess = loess$fitted),
    aes(x = x.loess, y = y.loess, color = "LOESS"),
    size = 1.2
  ) + 
  geom_line(
    data = data.frame(x.lm = lm$model$hdi,
                      y.lm = lm$fitted.values),
    aes(x = x.lm, y = y.lm, color = "OLS"),
    size = 1.2
  ) +
  geom_line(
    data = data.frame(x.linlog = exp(linlog$model$loghdi),
                      y.linlog = linlog$fitted.values),
    aes(x = x.linlog, y = y.linlog, color = "Linear-Log"),
    size = 1.2
  ) +
  geom_line(
    data = data.frame(x.loglog = exp(loglog$model$loghdi),
                      y.loglog = exp(loglog$fitted.values)),
    aes(x = x.loglog, y = y.loglog, color = "Log-Log"),
        size = 1.2
  ) +
  geom_line(
    data = data.frame(x.quad = quad$model$hdi,
                      y.quad = quad$fitted.values),
    aes(x = x.quad, y = y.quad, color = "Quadratic"),
    size = 1.2
  ) +
  scale_color_manual(values = unface.colors) +
  labs(
    title = "Turnout (VEP) & HDI",
    x = "HDI",
    y = "Turnout"
  ) +
  theme_minimal() 



ggplotly(unface, tooltip = "text")


























## LOG-LOG ====

# Define palette
scatter2.colors <- brewer.pal(5, "Set2")

# base scatterplot, all models, no CI
scatter2 <- 
pool %>% 
  ggplot(aes(x = log(hdi), y = log(to.vep * 100))) +
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
    method = "lm",
    aes(color = "Log-log", fill = "Log-log")
  ) +
  facet_wrap(~year) +
  scale_color_manual(name = "Model", values = scatter2.colors) +
  scale_fill_manual(name = "Model", values = scatter2.colors) +
  labs(
    title = "Turnout (VEP) & HDI",
    x = "Log HDI",
    y = "Log Turnout"
  ) +
  theme_minimal() 


    
ggplotly(scatter2, tooltip = "text")




# # PLOTLY ====
# 
# # linear fits to plot
# 
# ols <- lm(to.vep ~ hdi, data = cnty)
# 
# loess <- loess(to.vep ~ hdi, data = cnty, span = 50,
#                method = "loess", family = "symmetric", degree = 2)
# loess.pred <- predict(loess)
# 
# cnty$hdi2 <- cnty$hdi^2
# quad <- lm(to.vep ~ hdi + hdi2, data = cnty)
# quad.pred <- predict(quad)
# 
# 
# # a palette
# plotly.colors <- brewer.pal(3, "Set2")
# 
# cnty %>% 
#   mutate(year = as_factor(year)) %>% 
#   plot_ly(data = ., x = ~hdi) %>% 
#   add_markers(
#     y = ~to.vep,
#     color = ~year,
#     colors = plotly.colors[1:3],
#     # Hover text:
#     text = ~paste0(county, ", ", state, 
#                   "<br>HDI: ", round(hdi, 2), 
#                   "<br>Turnout: ", round(to.vep * 100, 1), "%"),
#     hoverinfo = 'text'
#   ) %>% 
#   add_lines(
#     y = fitted(ols),
#     name = "Pooled OLS",
#     line = list(color = "black")
#   ) %>% 
#   add_lines(
#     y = ~quad.pred,
#     name = "Pooled Quadratic",
#     line = list(color = "red")
#   ) %>% 
#   add_lines(
#     y = ~fitted(loess(to.vep ~ hdi)),
#     name = "Pooled LOESS",
#     line = list(color = "yellow")
#   )
#   
# 
# 
# 
# 
# 
# 
# 
# 
# 
# sdfg
# 
