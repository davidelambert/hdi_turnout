# GINI EXPERIMENT

# SETUP & IMPORT ====

# detach all non-base packages
lapply(paste('package:',names(sessionInfo()$otherPkgs),sep=""),
       detach, character.only=TRUE, unload=TRUE)

library(haven)
library(ipumsr)
library(blsAPI)
library(blscrapeR)
library(psych)
library(AER)
library(plm)
library(lfe)
library(tidyverse)


load("ipums2_recode.Rdata")

View(recode[sample(1:nrow(recode), 10), ])


# QUINTILES ====

# filter out under 16, instituionalized/group quarters, negative/0 income
# (1 is special code for break even, so excluding)
cniwap <- recode %>% filter(age >= 16, income > 0, inst != "inst")
length(which(cniwap$earn == 0))


# CPI Oct-Oct inflators, from BLS website CPI Inflation Calculator
infl08 <- 1.1162
infl12 <- 1.045


# append quintile breaks & dummies for quintile membership
start <- proc.time()

quintiles <- cniwap %>% 
  zap_ipums_attributes() %>% 
  select(year, state, perwt, income) %>% 
  group_by(state, year) %>% 
  mutate(
    q1 = quantile(rep(income, times = perwt), probs = 0.2),
    q2 = quantile(rep(income, times = perwt), probs = 0.4),
    q3 = quantile(rep(income, times = perwt), probs = 0.6),
    q4 = quantile(rep(income, times = perwt), probs = 0.8),
    q5 = quantile(rep(income, times = perwt), probs = 1),
  ) %>% 
  ungroup() %>% 
  mutate(
    dq1 = if_else(income < q1, 1, 0),
    dq2 = if_else(income >= q1 & income < q2, 1, 0),
    dq3 = if_else(income >= q2 & income < q3, 1, 0),
    dq4 = if_else(income >= q3 & income < q4, 1, 0),
    dq5 = if_else(income > q4, 1, 0)
  )

end <- proc.time() - start
end # ~78sec

View(quintiles[sample(1:nrow(quintiles), 10),])
# this should work


# state/year summaries
start = proc.time()

quintsum <- quintiles %>% 
  group_by(state, year) %>% 
  summarise(
    totinc = sum(income * perwt),
    q1inc = sum(income * perwt * dq1),
    q2inc = sum(income * perwt * dq2),
    q3inc = sum(income * perwt * dq3),
    q4inc = sum(income * perwt * dq4),
    q5inc = sum(income * perwt * dq5),
  ) %>% 
  mutate(
    q1share = q1inc / totinc,
    q2share = q2inc / totinc,
    q3share = q3inc / totinc,
    q4share = q4inc / totinc,
    q5share = q5inc / totinc,
    q1cum = q1share,
    q2cum = q1share + q2share,
    q3cum = q1share + q2share + q3share,
    q4cum = q1share + q2share + q3share + q4share,
    q5cum = q1share + q2share + q3share + q4share + q5share,
    q1area = .5 * .2 * (q1cum),
    q2area = .5 * .2 * (q1cum + q2cum),
    q3area = .5 * .2 * (q2cum + q3cum),
    q4area = .5 * .2 * (q3cum + q4cum),
    q5area = .5 * .2 * (q4cum + q5cum),
    areasum = q1area + q2area + q3area + q4area + q5area,
    gini = (.5 - areasum) / .5
  ) %>% 
  arrange(state, year)

end <- proc.time() - start
end # 5 sec.

quintsum %>%
  select(year, state, gini) %>% 
  spread(state, gini) %>% 
  View()


# close-ISH to FactFinder figure. check against deciles
# & against canned gini functions




# DECILES ====

start <- proc.time()

deciles <- cniwap %>% 
  zap_ipums_attributes() %>% 
  select(year, state, perwt, income) %>% 
  group_by(state, year) %>% 
  mutate(
    d1 = quantile(rep(income, times = perwt), probs = 0.1),
    d2 = quantile(rep(income, times = perwt), probs = 0.2),
    d3 = quantile(rep(income, times = perwt), probs = 0.3),
    d4 = quantile(rep(income, times = perwt), probs = 0.4),
    d5 = quantile(rep(income, times = perwt), probs = 0.5),
    d6 = quantile(rep(income, times = perwt), probs = 0.6),
    d7 = quantile(rep(income, times = perwt), probs = 0.7),
    d8 = quantile(rep(income, times = perwt), probs = 0.8),
    d9 = quantile(rep(income, times = perwt), probs = 0.9),
    d10 = quantile(rep(income, times = perwt), probs = 1)
  ) %>% 
  ungroup() %>% 
  mutate(
    dd1 = if_else(income < d1, 1, 0),
    dd2 = if_else(income >= d1 & income < d2, 1, 0),
    dd3 = if_else(income >= d2 & income < d3, 1, 0),
    dd4 = if_else(income >= d3 & income < d4, 1, 0),
    dd4 = if_else(income >= d4 & income < d5, 1, 0),
    dd5 = if_else(income >= d5 & income < d6, 1, 0),
    dd6 = if_else(income >= d6 & income < d7, 1, 0),
    dd7 = if_else(income >= d7 & income < d8, 1, 0),
    dd8 = if_else(income >= d8 & income < d9, 1, 0),
    dd9 = if_else(income >= d9 & income < d10, 1, 0),
    dd10 = if_else(income > d10, 1, 0)
  )

end <- proc.time() - start
end # ~78sec

View(deciles[sample(1:nrow(deciles), 10),])
# this should work


# state/year summaries
start = proc.time()

decsum <- deciles %>% 
  group_by(state, year) %>% 
  summarise(
    totinc = sum(income * perwt),
    d1inc = sum(income * perwt * dd1),
    d2inc = sum(income * perwt * dd2),
    d3inc = sum(income * perwt * dd3),
    d4inc = sum(income * perwt * dd4),
    d5inc = sum(income * perwt * dd5),
    d6inc = sum(income * perwt * dd6),
    d7inc = sum(income * perwt * dd7),
    d8inc = sum(income * perwt * dd8),
    d9inc = sum(income * perwt * dd9),
    d10inc = sum(income * perwt * dd10)
  ) %>% 
  mutate(
    d1share = d1inc / totinc,
    d2share = d2inc / totinc,
    d3share = d3inc / totinc,
    d4share = d4inc / totinc,
    d5share = d5inc / totinc,
    d6share = d6inc / totinc,
    d7share = d7inc / totinc,
    d8share = d8inc / totinc,
    d9share = d9inc / totinc,
    d10share = d10inc / totinc,
    d1cum = d1share,
    d2cum = d1cum + d2share,
    d3cum = d2cum + d3share,
    d4cum = d3cum + d4share,
    d5cum = d4cum + d5share,
    d6cum = d5cum + d6share,
    d7cum = d6cum + d7share,
    d8cum = d7cum + d8share,
    d9cum = d8cum + d9share,
    d10cum = d9cum + d10share,
    d1area = .5 * .1 * (d1cum),
    d2area = .5 * .1 * (d1cum + d2cum),
    d3area = .5 * .1 * (d2cum + d3cum),
    d4area = .5 * .1 * (d3cum + d4cum),
    d5area = .5 * .1 * (d4cum + d5cum),
    d6area = .5 * .1 * (d5cum + d6cum),
    d7area = .5 * .1 * (d6cum + d7cum),
    d8area = .5 * .1 * (d7cum + d8cum),
    d9area = .5 * .1 * (d8cum + d9cum),
    d10area = .5 * .1 * (d9cum + d10cum),
    areasum = d1area + d2area + d3area + d4area + d5area + 
              d6area + d7area + d8area + d9area + d10area,
    gini = (.5 - areasum) / .5
  ) %>% 
  arrange(state, year)

end <- proc.time() - start
end # 9.5 sec.

decsum %>%
  select(year, state, gini) %>% 
  spread(state, gini) %>% 
  View()


# ????? DOESN'T WORK?????




# DECILES 2 ====

# using a decile FACTOR instead of dummies

# subset only needed columns
# assign a decile rank to each obs (by state by year)
deciles <- cniwap %>% 
  zap_ipums_attributes() %>% 
  select(state, year, perwt, income) %>% 
  group_by(state, year) %>% 
  mutate(
    decile = ntile(income, 10),
  )

View(deciles[sample(1:nrow(deciles), 10),])



decsum <- deciles %>% 
  group_by(state, year) %>% 
  mutate(inctotal = sum(income * perwt)) %>% 
  ungroup %>% 
  group_by(state, year, decile) %>%
  summarise(
    inctotal = mean(inctotal),
    decinc = sum(income * perwt)
  ) %>%
  mutate(
    incshare = decinc / inctotal,
    richer = 1 - decile/10,
    score = incshare * (.1 + 2 * richer)
  )

gini <- decsum %>% 
  group_by(state, year) %>% 
  summarise(scoretot = sum(score)) %>% 
  mutate(gini = 1 - scoretot)



# This appears to mostly work, even though some individual states are off from
# FactFinder (though their methodology is unclear).


# PERCENTILES ====

pctile <- cniwap %>% 
  zap_ipums_attributes() %>% 
  select(state, year, perwt, income) %>% 
  group_by(state, year) %>% 
  mutate(
    pctile = ntile(income, 100),
  )


View(pctile[sample(1:nrow(pctile), 10),])



pctsum <- pctile %>% 
  group_by(state, year) %>% 
  mutate(inctotal = sum(income * perwt)) %>% 
  ungroup %>% 
  group_by(state, year, pctile) %>%
  summarise(
    inctotal = mean(inctotal),
    pctinc = sum(income * perwt)
  ) %>%
  mutate(
    incshare = pctinc / inctotal,
    richer = 1 - pctile/100,
    score = incshare * (.01 + 2 * richer)
  )

ginipct <- pctsum %>% 
  group_by(state, year) %>% 
  summarise(scoretot = sum(score)) %>% 
  mutate(gini = 1 - scoretot)


# canned ginis ====

library(acid)
ginican <- cniwap %>% 
  group_by(state, year) %>% 
  summarise(
    gini = weighted.gini(income, w = perwt)$Gini[1]
  )

weighted.gini(cniwap$income)
x <- weighted.gini(cniwap$income, w = cniwap$perwt)
x$Gini[1]

# nope!

