

# CORRELOGRAM ====

library(GGally)

sub %>% 
  select(to.vap, to.vep, femaleprop, whiteprop, nonwhprop, blackprop,
         hispprop, aapiprop, aianprop, multiprop, otherprop, oldprop,  
         uninsprop, ur.6mo, ur.oct, ur.trend, hdi) %>% 
  ggcorr(low = "darkred", mid = "white", high = "darkgreen",
         label = TRUE, label_round = 2,
         hjust = 0.7, layout.exp = 1)






# HISTOGRAMS ====

# simualte normal distribution based on pooled distribution
set.seed(9832574)

norm.hdi <- rnorm(
  1980,
  mean = mean(subpool$hdi[subpool$year == "Pooled"]),
  sd = sd(subpool$hdi[subpool$year == "Pooled"])
)

norm.hdilog <- rnorm(
  1980,
  mean = mean(log(subpool$hdi[subpool$year == "Pooled"])),
  sd = sd(log(subpool$hdi[subpool$year == "Pooled"]))
)

norm.vep <- rnorm(
  1980,
  mean = mean(subpool$to.vep[subpool$year == "Pooled"]),
  sd = sd(subpool$to.vep[subpool$year == "Pooled"])
)

norm.veplog <- rnorm(
  1980,
  mean = mean(log(subpool$to.vep[subpool$year == "Pooled"])),
  sd = sd(log(subpool$to.vep[subpool$year == "Pooled"]))
)

# Define a palette:
library(RColorBrewer)
density_colors <- brewer.pal(2, "Set1")

# hdi histograms
subpool %>% 
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
subpool %>% 
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
subpool %>% 
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
subpool %>% 
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


# SCATTERPLOTS ====

# Define palette
scatter_colors <- brewer.pal(4, "Dark2")

subpool %>% 
  ggplot(aes(x = hdi, y = to.vep)) +
  geom_point(
    shape = 16,
    alpha = .3,
    size = 2,
    stroke = 0
  ) +
  geom_smooth(
    size = 1.5,
    method = "loess",
    aes(color = "LOESS", fill = "LOESS")
  ) +
  geom_smooth(
    size = 1.5,
    method = "lm",
    aes(color = "OLS", fill = "OLS")
  ) +
  geom_smooth(
    size = 1.5,
    method = "lm",
    formula = y ~ log(x),
    aes(color = "Linear-Log", fill = "Linear-Log")
  ) +
  geom_smooth(
    size = 1.5,
    method = "lm",
    formula = y ~ splines::bs(x, degree = 2),
    aes(color = "Quadratic", fill = "Quadratic")
  ) +
  facet_wrap(~year) +
  scale_color_manual(name = "Model", values = scatter_colors) +
  scale_fill_manual(name = "Model", values = scatter_colors) +
  labs(
    title = "Turnout (VEP) & HDI",
    x = "HDI",
    y = "Turnout"
  ) +
  theme_minimal() 
    


sdfg

