library(sf)
library(tigris)
library(tidycensus)
library(leaflet)
library(Hmisc)
library(psych)
library(stringr)
library(tidyverse)
options(tigris_class = "sf")
options(tigris_use_cache = TRUE)


# get all versions
load("acs_.Rdata")
# acs_only <- read_csv("acs_.csv")

# EXPLORATORY =====

hdistats <- describe(acs$hdi)

  # ZEROS have been removed, the below is not needed
  # which(acs$hdi == 0)
  # View(acs[c(2413, 3019), ])  # Ok these are poor/ unhealthy AF
  # which(acs$health_index == 0)  # McDowell, WV
  # which(acs$inc_index == 0)  # Oglala Lakota, SD



# DISTROPOTS =====

# histogram w/ eqivalent normal & boxplot
# close to normal, w/ a few outliers
hdinorm <- rnorm(3142, mean = hdistats$mean, sd = hdistats$sd)

ggplot(data = acs, mapping = aes(x = hdi)) +
  geom_histogram(bins = 51, mapping = aes(y = ..density..),
                 fill = "navyblue", color = "white") +
  geom_density(color = "orange", size = 0.8) +
  geom_density(mapping = aes(x = hdinorm),
               color = "hotpink", size = 0.8) +
  geom_abline(slope = 0, intercept = 0, size = 1, color = "grey80") +
  theme_minimal()

# box
ggplot() +
  geom_boxplot(data = acs, mapping = aes(y = hdi)) + 
  coord_flip() + theme_minimal()


# histogram w/ eqivalent normal 
ginistats <- describe(acs$gini)

gininorm <- rnorm(3142, mean = ginistats$mean, sd = ginistats$sd)

ggplot(data = acs, mapping = aes(x = gini)) +
  geom_histogram(bins = 51, mapping = aes(y = ..density..),
                 fill = "navyblue", color = "white") +
  geom_density(color = "orange", size = 0.8) +
  geom_density(mapping = aes(x = gininorm),
               color = "hotpink", size = 0.8) +
  geom_abline(slope = 0, intercept = 0, size = 1, color = "grey80") +
  theme_minimal()




# histogram w/ eqivalent normal
turnoutstats <- describe(acs$turnout)

turnoutnorm <- rnorm(3142, mean = turnoutstats$mean, sd = turnoutstats$sd)

ggplot(data = acs, mapping = aes(x = turnout)) +
  geom_histogram(bins = 51, mapping = aes(y = ..density..),
                 fill = "navyblue", color = "white") +
  geom_density(color = "orange", size = 0.8) +
  geom_density(mapping = aes(x = turnoutnorm),
               color = "hotpink", size = 0.8) +
  geom_abline(slope = 0, intercept = 0, size = 1, color = "grey80") +
  theme_minimal()

# whoa that's absolutely not normal - not even close
# just check the histogram
ggplot(data = acs, mapping = aes(x = turnout)) +
  geom_histogram(bins = 500, mapping = aes(y = ..density..),
                 fill = "navyblue", color = "white")



# CORRELATIONS & SCATTERS ====



# weak
cor(acs$gini, acs$turnout)
# weaker
cor(acs$hdi, acs$turnout)

# moderate, negative - investigate?
cor(acs$hdi, acs$gini)

acs %>% ggplot(mapping = aes(x = hdi, y = gini)) +
  geom_point(shape = 16,
             alpha = .2,
             size = 4,
             stroke = 0,
             color = "navyblue") +
  geom_smooth(se = FALSE, color = "orange", size = 2) +
  geom_smooth(se = FALSE, color = "hotpink", size = 2, method ="lm") +
  theme_minimal()
  
  

# weak, but better & expected
cor(acs$incpc, acs$turnout)
cor(acs$hdi, acs$turnout)
cor(acs$gini, acs$turnout)
cor(acs$baccprop, acs$turnout)






# clean hdi outliers ===========
  # retrieve outliers
  hdiout <- boxplot(acs$hdi, plot = FALSE)$out
  
  # trim outliers
  hditrim <- acs[-which(acs$hdi %in% hdiout), ]
  
  hditrim %>% ggplot(mapping = aes(x = hdi, y = gini)) +
    geom_point(shape = 16,
               alpha = .2,
               size = 4,
               stroke = 0,
               color = "navyblue") +
    geom_smooth(se = FALSE, color = "orange", size = 2) +
    geom_smooth(se = FALSE, color = "hotpink", size = 2, method ="lm") +
    theme_minimal()

  
  
cor(hditrim$hdi, hditrim$turnout)
cor(acs$hdi, acs$turnout)
cor(hditrim$gini, hditrim$turnout)





  
