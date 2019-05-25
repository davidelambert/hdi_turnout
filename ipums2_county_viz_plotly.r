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

# get all.6 colors of canned palette to force darker/saturated endpoints
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
set.seed(983264)

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
                 fill = "grey60") +
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
    values = plasma(2, begin = 0, end = 0.6, alpha = 0.6)
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
                 fill = "grey60") +
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
    values = plasma(2, begin = 0.1, end = 0.8, alpha = 0.6)
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
                 fill = "grey60") +
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
    values = plasma(2, begin = 0.2, end = 0.9, alpha = 0.6)
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
                 fill = "grey60") +
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
    values = plasma(2, begin = 0.3, end = 1, alpha = 0.6)
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    legend.direction = "vertical",
    legend.title = element_blank()
  )

ggplotly(hist.logvep)




# scatterplot preprocessing ====

# Ignoring Quadratic, b/c I think it sucks

# computed variables in pool df
cnty <- cnty %>% 
  mutate(
    year = as.character(year),
    year2 = year,
    hdi2 = hdi ^ 2,
    loghdi = log(hdi),
    veppct = to.vep * 100,
    logvep = log(veppct)
  )


# Models
  # 2008 fits
  loess.2008 <- loess(veppct ~ hdi, data = cnty, subset = (year == "2008"))
  ols.2008 <- lm(veppct ~ hdi, data = cnty, subset = (year == "2008"))
  linlog.2008 <- lm(veppct ~ loghdi, data = cnty, subset = (year == "2008"))
  loglog.2008 <- lm(logvep ~ loghdi, data = cnty, subset = (year == "2008"))
  
  # 2012 fits
  loess.2012 <- loess(veppct ~ hdi, data = cnty, subset = (year == "2012"))
  ols.2012 <- lm(veppct ~ hdi, data = cnty, subset = (year == "2012"))
  linlog.2012 <- lm(veppct ~ loghdi, data = cnty, subset = (year == "2012"))
  loglog.2012 <- lm(logvep ~ loghdi, data = cnty, subset = (year == "2012"))
  
  # 2016 fits
  loess.2016 <- loess(veppct ~ hdi, data = cnty, subset = (year == "2016"))
  ols.2016 <- lm(veppct ~ hdi, data = cnty, subset = (year == "2016"))
  linlog.2016 <- lm(veppct ~ loghdi, data = cnty, subset = (year == "2016"))
  loglog.2016 <- lm(logvep ~ loghdi, data = cnty, subset = (year == "2016"))
  
  # pooled fits
  loess.pool <- loess(veppct ~ hdi, data = cnty)
  ols.pool <- lm(veppct ~ hdi, data = cnty)
  linlog.pool <- lm(veppct ~ loghdi, data = cnty)
  loglog.pool <- lm(logvep ~ loghdi, data = cnty)


# palette
p.colors <- brewer.pal(4, "Set2")








# 2008 plot ====
p08 <- 
  plot_ly() %>% 
  # 2008 markers - only visible in 2008 plot
  add_markers(
    data = subset(cnty, year == "2008"),
    x = ~hdi,
    y = ~veppct,
    name = "2008",
    legendgroup = "2008",
    showlegend = F,
    visible = T,
    marker = list(
      size = 10,
      symbol = "circle",
      color = "rgba(64, 64, 64, .4)",
      line = list(color = "rgba(255, 255, 255, 1)", width = 1)
    ),
    text = ~paste0(county, ", ", state,
                   "<br>HDI: ", round(hdi, 2),
                   "<br>Turnout: ", round(to.vep * 100, 1), "%"),
    hoverinfo = 'text'
  ) %>% 
  # 2012 markers - only visible in 2012 plot
  add_markers(
    data = subset(cnty, year == "2012"),
    x = ~hdi,
    y = ~veppct,
    name = "2012",
    legendgroup = "2012",
    showlegend = F,
    visible = F,
    marker = list(
      size = 10,
      symbol = "diamond",
      color = "rgba(64, 64, 64, .4)",
      line = list(color = "rgba(255, 255, 255, 1)", width = 1)
    ),
    text = ~paste0(county, ", ", state,
                   "<br>HDI: ", round(hdi, 2),
                   "<br>Turnout: ", round(to.vep * 100, 1), "%"),
    hoverinfo = 'text'
  ) %>% 
  # 2016 markers - only visible in 2016 plot
  add_markers(
    data = subset(cnty, year == "2016"),
    x = ~hdi,
    y = ~veppct,
    name = "2016",
    legendgroup = "2016",
    showlegend = F,
    visible = F,
    marker = list(
      size = 10,
      symbol = "x",
      color = "rgba(64, 64, 64, .4)",
      line = list(color = "rgba(255, 255, 255, 1)", width = 1)
    ),
    text = ~paste0(county, ", ", state,
                   "<br>HDI: ", round(hdi, 2),
                   "<br>Turnout: ", round(to.vep * 100, 1), "%"),
    hoverinfo = 'text'
  ) %>% 
  add_lines(
    x = ~as.vector(loess.2008$x),
    y = ~loess.2008$fitted,
    name = "LOESS",
    legendgroup = "loess",
    showlegend = F,
    line = list(color = p.colors[1], width = 4),
  ) %>% 
  add_lines(
    x = ~ols.2008$model$hdi,
    y = ~ols.2008$fitted,
    name = "OLS",
    legendgroup = "ols",
    showlegend = F,
    line = list(color = p.colors[2], width = 4),
    visible = "legendonly"
  ) %>% 
  add_lines(
    x = ~exp(linlog.2008$model$loghdi),
    y = ~linlog.2008$fitted,
    name = "Linear-Log",
    legendgroup = "linlog",
    showlegend = F,
    line = list(color = p.colors[3], width = 4),
    visible = "legendonly"
  ) %>% 
  add_lines(
    x = ~exp(loglog.2008$model$loghdi),
    y = ~exp(loglog.2008$fitted),
    name = "Log-Log",
    legendgroup = "loglog",
    showlegend = F,
    line = list(color = p.colors[4], width = 4),
    visible = "legendonly"
  ) %>% 
  layout(
    xaxis = list(range = c(2.5, 9.5)),
    yaxis = list(range = c(30, 90))
  )



# 2012 plot ====
p12 <- 
  plot_ly() %>% 
  # 2008 markers - only visible in 2008 plot
  add_markers(
    data = subset(cnty, year == "2008"),
    x = ~hdi,
    y = ~veppct,
    name = "2008",
    legendgroup = "2008",
    showlegend = F,
    visible = F,
    marker = list(
      size = 10,
      symbol = "circle",
      color = "rgba(64, 64, 64, .4)",
      line = list(color = "rgba(255, 255, 255, 1)", width = 1)
    ),
    text = ~paste0(county, ", ", state,
                   "<br>HDI: ", round(hdi, 2),
                   "<br>Turnout: ", round(to.vep * 100, 1), "%"),
    hoverinfo = 'text'
  ) %>% 
  # 2012 markers - only visible in 2012 plot
  add_markers(
    data = subset(cnty, year == "2012"),
    x = ~hdi,
    y = ~veppct,
    name = "2012",
    legendgroup = "2012",
    showlegend = F,
    visible = T,
    marker = list(
      size = 10,
      symbol = "diamond",
      color = "rgba(64, 64, 64, .4)",
      line = list(color = "rgba(255, 255, 255, 1)", width = 1)
    ),
    text = ~paste0(county, ", ", state,
                   "<br>HDI: ", round(hdi, 2),
                   "<br>Turnout: ", round(to.vep * 100, 1), "%"),
    hoverinfo = 'text'
  ) %>% 
  # 2016 markers - only visible in 2016 plot
  add_markers(
    data = subset(cnty, year == "2016"),
    x = ~hdi,
    y = ~veppct,
    name = "2016",
    legendgroup = "2016",
    showlegend = F,
    visible = F,
    marker = list(
      size = 10,
      symbol = "x",
      color = "rgba(64, 64, 64, .4)",
      line = list(color = "rgba(255, 255, 255, 1)", width = 1)
    ),
    text = ~paste0(county, ", ", state,
                   "<br>HDI: ", round(hdi, 2),
                   "<br>Turnout: ", round(to.vep * 100, 1), "%"),
    hoverinfo = 'text'
  ) %>% 
  add_lines(
    x = ~as.vector(loess.2012$x),
    y = ~loess.2012$fitted,
    name = "LOESS",
    legendgroup = "loess",
    showlegend = F,
    line = list(color = p.colors[1], width = 4),
  ) %>% 
  add_lines(
    x = ~ols.2012$model$hdi,
    y = ~ols.2012$fitted,
    name = "OLS",
    legendgroup = "ols",
    showlegend = F,
    line = list(color = p.colors[2], width = 4),
    visible = "legendonly"
  ) %>% 
  add_lines(
    x = ~exp(linlog.2012$model$loghdi),
    y = ~linlog.2012$fitted,
    name = "Linear-Log",
    legendgroup = "linlog",
    showlegend = F,
    line = list(color = p.colors[3], width = 4),
    visible = "legendonly"
  ) %>% 
  add_lines(
    x = ~exp(loglog.2012$model$loghdi),
    y = ~exp(loglog.2012$fitted),
    name = "Log-Log",
    legendgroup = "loglog",
    showlegend = F,
    line = list(color = p.colors[4], width = 4),
    visible = "legendonly"
  ) %>% 
  layout(
    xaxis = list(range = c(2.5, 9.5)),
    yaxis = list(range = c(30, 90))
  )


# 2016 plot ====
p16 <- 
  plot_ly() %>% 
  # 2008 markers - only visible in 2008 plot
  add_markers(
    data = subset(cnty, year == "2008"),
    x = ~hdi,
    y = ~veppct,
    name = "2008",
    legendgroup = "2008",
    showlegend = F,
    visible = F,
    marker = list(
      size = 10,
      symbol = "circle",
      color = "rgba(64, 64, 64, .4)",
      line = list(color = "rgba(255, 255, 255, 1)", width = 1)
    ),
    text = ~paste0(county, ", ", state,
                   "<br>HDI: ", round(hdi, 2),
                   "<br>Turnout: ", round(to.vep * 100, 1), "%"),
    hoverinfo = 'text'
  ) %>% 
  # 2012 markers - only visible in 2012 plot
  add_markers(
    data = subset(cnty, year == "2012"),
    x = ~hdi,
    y = ~veppct,
    name = "2012",
    legendgroup = "2012",
    showlegend = F,
    visible = F,
    marker = list(
      size = 10,
      symbol = "diamond",
      color = "rgba(64, 64, 64, .4)",
      line = list(color = "rgba(255, 255, 255, 1)", width = 1)
    ),
    text = ~paste0(county, ", ", state,
                   "<br>HDI: ", round(hdi, 2),
                   "<br>Turnout: ", round(to.vep * 100, 1), "%"),
    hoverinfo = 'text'
  ) %>% 
  # 2016 markers - only visible in 2016 plot
  add_markers(
    data = subset(cnty, year == "2016"),
    x = ~hdi,
    y = ~veppct,
    name = "2016",
    legendgroup = "2016",
    showlegend = F,
    visible = T,
    marker = list(
      size = 10,
      symbol = "x",
      color = "rgba(64, 64, 64, .4)",
      line = list(color = "rgba(255, 255, 255, 1)", width = 1)
    ),
    text = ~paste0(county, ", ", state,
                   "<br>HDI: ", round(hdi, 2),
                   "<br>Turnout: ", round(to.vep * 100, 1), "%"),
    hoverinfo = 'text'
  ) %>% 
  add_lines(
    x = ~as.vector(loess.2016$x),
    y = ~loess.2016$fitted,
    name = "LOESS",
    legendgroup = "loess",
    showlegend = F,
    line = list(color = p.colors[1], width = 4),
  ) %>% 
  add_lines(
    x = ~ols.2016$model$hdi,
    y = ~ols.2016$fitted,
    name = "OLS",
    legendgroup = "ols",
    showlegend = F,
    line = list(color = p.colors[2], width = 4),
    visible = "legendonly"
  ) %>% 
  add_lines(
    x = ~exp(linlog.2016$model$loghdi),
    y = ~linlog.2016$fitted,
    name = "Linear-Log",
    legendgroup = "linlog",
    showlegend = F,
    line = list(color = p.colors[3], width = 4),
    visible = "legendonly"
  ) %>% 
  add_lines(
    x = ~exp(loglog.2016$model$loghdi),
    y = ~exp(loglog.2016$fitted),
    name = "Log-Log",
    legendgroup = "loglog",
    showlegend = F,
    line = list(color = p.colors[4], width = 4),
    visible = "legendonly"
  ) %>% 
  layout(
    xaxis = list(range = c(2.5, 9.5)),
    yaxis = list(range = c(30, 90))
  )







# pooled plot ====
pp <- 
  plot_ly() %>% 
  add_markers(
    data = subset(cnty, year == "2008"),
    x = ~hdi,
    y = ~veppct,
    name = "2008",
    legendgroup = "2008",
    showlegend = T,
    visible = T,
    marker = list(
      size = 10,
      symbol = "circle",
      color = "rgba(64, 64, 64, .4)",
      line = list(color = "rgba(255, 255, 255, 1)", width = 1)
    ),
    text = ~paste0(county, ", ", state,
                   "<br>HDI: ", round(hdi, 2),
                   "<br>Turnout: ", round(to.vep * 100, 1), "%"),
    hoverinfo = 'text'
  ) %>% 
  add_markers(
    data = subset(cnty, year == "2012"),
    x = ~hdi,
    y = ~veppct,
    name = "2012",
    legendgroup = "2012",
    showlegend = T,
    visible = T,
    marker = list(
      size = 10,
      symbol = "diamond",
      color = "rgba(64, 64, 64, .4)",
      line = list(color = "rgba(255, 255, 255, 1)", width = 1)
    ),
    text = ~paste0(county, ", ", state,
                   "<br>HDI: ", round(hdi, 2),
                   "<br>Turnout: ", round(to.vep * 100, 1), "%"),
    hoverinfo = 'text'
  ) %>% 
  add_markers(
    data = subset(cnty, year == "2016"),
    x = ~hdi,
    y = ~veppct,
    name = "2016",
    legendgroup = "2016",
    showlegend = T,
    visible = T,
    marker = list(
      size = 10,
      symbol = "x",
      color = "rgba(64, 64, 64, .4)",
      line = list(color = "rgba(255, 255, 255, 1)", width = 1)
    ),
    text = ~paste0(county, ", ", state,
                   "<br>HDI: ", round(hdi, 2),
                   "<br>Turnout: ", round(to.vep * 100, 1), "%"),
    hoverinfo = 'text'
  ) %>% 
  add_lines(
    x = ~as.vector(loess.pool$x),
    y = ~loess.pool$fitted,
    name = "LOESS",
    legendgroup = "loess",
    line = list(color = p.colors[1], width = 4),
  ) %>% 
  add_lines(
    x = ~ols.pool$model$hdi,
    y = ~ols.pool$fitted,
    name = "OLS",
    legendgroup = "ols",
    line = list(color = p.colors[2], width = 4),
    visible = "legendonly"
  ) %>% 
  add_lines(
    x = ~exp(linlog.pool$model$loghdi),
    y = ~linlog.pool$fitted,
    name = "Linear-Log",
    legendgroup = "linlog",
    line = list(color = p.colors[3], width = 4),
    visible = "legendonly"
  ) %>% 
  add_lines(
    x = ~exp(loglog.pool$model$loghdi),
    y = ~exp(loglog.pool$fitted),
    name = "Log-Log",
    legendgroup = "loglog",
    line = list(color = p.colors[4], width = 4),
    visible = "legendonly"
  ) %>% 
  layout(
    xaxis = list(range = c(2.5, 9.5)),
    yaxis = list(range = c(30, 90))
  )


# subplot ====

subplot(p08, p12, p16, pp, nrows = 2) %>% 
  layout(
    xaxis = list(name = "HDI"),
    yaxis = list(name = "Turnout")
  )

