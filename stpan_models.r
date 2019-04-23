library(psych)
library(AER)
library(plm)
library(lfe)
library(tidyverse)

load("stpan.Rdata")
stpan$year <- as.numeric(stpan$year)



# BASIC MODEL BUILDING ====

# Equivalency of ols & pooled PLM models
lm(data = stpan, tovep ~ hdi)
pool <- plm(data = stpan, tovep ~ hdi, model = "pooling",
    index = c("state", "year"))
coefficients(pool)



# equivalency of time LSDV OLS & time fixed effect in PLM
lm(data = stpan, tovep ~ hdi + d16)
fetime <- plm(data = stpan, tovep ~ hdi, model = "within",
              effect = "time", index = c("state", "year"))
coefficients(fetime)


# equivalence of state LSDV & state FE in PLM (w/ or w/o "individual" call)
# i.e. "within" assumes "individual", but must call "time" as above
lm(data = stpan, tovep ~ hdi + factor(st) )
plm(data = stpan, tovep ~ hdi, model = "within",
    effect = "individual", index = c("state", "year"))
fest <- plm(data = stpan, tovep ~ hdi, model = "within",
            index = c("state", "year"))
coefficients(fest)





# equivalence of twoway FE & first differences
plm(data = stpan, tovep ~ hdi, model = "within",
    effect = "twoways", index = c("state", "year"))
fd <- plm(data = stpan, tovep ~ hdi, model = "fd")
coefficients(fd)




# random effects
random <- plm(data = stpan, tovep ~ hdi, model = "random",
              index = c("state", "year"))





# TESTS =====

# test for existence of panel (or random) effects:
# Ho: no panel fx, OLS/pooled model is better
plmtest(pool, type = "bp") # reject: use panel model


# hausman tests. Ho: Random is better than fixed
phtest(fest, random)    # Reject: State FE better than RE
phtest(fetime, random)  # Reject: Time FE better than RE
phtest(fd, random)      # Fail to reject: RE better than FD
# results: random only better than FD (which we reject anyway)





# tests for individual/time effects
# Ho: no time/ effects

pFtest(fest, pool)   # reject: yes indiv fx
pFtest(fetime, pool) # failt to reject: no time fx


plmtest(pool, effect = c("individual"), type = "bp")  # reject: yes indiv fx
plmtest(pool, effect = c("time"), type = "bp")  # fail to reject: no time fx
plmtest(fest, effect = c("time"), type = "bp")  # equivalent to above.
plmtest(pool, effect = c("twoways"), type = "bp")
  # fail to reject existence of twoway fx, but that just gives us 
  # the FD model, which we reject as inferior to RE





# cross-sectional dependence tests
pcdtest(fest, test = c("lm"))  # reject (prob b/c low T=2)
pcdtest(fest, test = c("cd"))  # fail to reject: no Xsec dependence




# serial correlation test
pbgtest(fest)  # reject - maybe due to small sample? Use HAC std errors?



# Unit root test (Ho: unit root, Ha: Stationary)

library(tseries)
stpan.set <- pdata.frame(stpan, index = c("state", "year"))
adf.test(stpan.set$tovep, k = 1)
# reject (accept stationarity)




# Breush-Pagan heteroskedacity test. (Ho: homeskedastic errors) 
bptest(tovep ~ hdi + factor(st), data = stpan, studentize = F)
# reject homoskedasticity



# incorporate HC std errors
  coeftest(fest)   # no heterosked.
  coeftest(fest, vcovHC)   # white HC1 std errors
  coeftest(fest, vcovHC(fest, method = "arellano"))  # arellano HC1 se (same)
  coeftest(fest, vcovHC(fest, type = "HC3"))  # HC3 se - still good!



# SUMMARY:
#   Panel effects exist -  prefer to OLS/pooling.
#   Fixed effects preferred to random.
#   Reject the use of time effects.




# EXTENSIONS =====

# FE Multiple regression
extend <- plm(tovep ~ hdi + nonwh_prop, data = stpan,
    model = "within", index = c("state", "year"))
summary(extend)






