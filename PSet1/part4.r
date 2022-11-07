# The University of Chicago
# Spring 2022
# ECMA 33620 Intro to Heterogeneous Agent Macroeconomics
# Instructor: Prof. Greg Kaplan

# Problem Set 1
# Part 4
# Chongyu Fang

# Set path
setwd("~/Desktop/ECMA 33620 Heterogeneous Agent Macroeconomics/Problem Sets/Problem Set 1")

# Read data
library(haven)
scf = read_dta("rscfp2019.dta")

# Switch off scientific notation
options(scipen = 999)


### Question 1

library(ggplot2)
fig1 = ggplot(scf, aes(networth, weight = wgt, )) + geom_histogram()
fig1 + ggtitle("Histogram of Networth (Weighted)") + xlab("Networth") + ylab("Count")
fig2 = ggplot(scf, aes(income, weight = wgt, )) + geom_histogram()
fig2 + ggtitle("Histogram of Income (Weighted)") + xlab("Income") + ylab("Count")
fig3 = ggplot(scf, aes(wageinc, weight = wgt, )) + geom_histogram()
fig3 + ggtitle("Histogram of Earnings (Weighted)") + xlab("Earnings") + ylab("Count")


### Question 2

# (a) coefficient of variation
library(Hmisc) # for calculating weighted variance
cv_networth = sqrt(wtd.var(scf$networth, scf$wgt)) / weighted.mean(scf$networth, scf$wgt)
cv_income = sqrt(wtd.var(scf$income, scf$wgt)) / weighted.mean(scf$income, scf$wgt)
cv_wageinc = sqrt(wtd.var(scf$wageinc, scf$wgt)) / weighted.mean(scf$wageinc, scf$wgt)

# (b) variance of logs (exclude zero and negative values)
# variable: networth
positive <- scf$networth > 0
wtd.var(log(scf$networth[positive]), scf$wgt[positive])
# variable: income
positive <- scf$income > 0
wtd.var(log(scf$income[positive]), scf$wgt[positive])
# variable: wageinc
positive <- scf$wageinc > 0
wtd.var(log(scf$wageinc[positive]), scf$wgt[positive])

# (c) gini coefficient
library(dineq) # for calculating weighted gini coefficient
gini_networth = gini.wtd(scf$networth, weights = scf$wgt)
gini_income = gini.wtd(scf$income, weights = scf$wgt)
gini_wageinc = gini.wtd(scf$wageinc, weights = scf$wgt)

# (d) (e) 99-50 ratio and 90-50 ratio
networth_percentile = wtd.quantile(scf$networth, weights = scf$wgt, probs = c(0.5, 0.9, 0.99))
income_percentile = wtd.quantile(scf$income, weights = scf$wgt, probs = c(0.5, 0.9, 0.99))
wageinc_percentile = wtd.quantile(scf$wageinc, weights = scf$wgt, probs = c(0.5, 0.9, 0.99))

# (f) wealth share of top 10%
# variable: networth
top10 <- scf$networth >= networth_percentile[2] # logical condition for top 10%
sum(scf$networth[top10] * scf$wgt[top10]) / sum(scf$networth * scf$wgt) # note: weighted sum
# variable: income
top10 <- scf$income >= income_percentile[2] # logical condition for top 10%
sum(scf$income[top10] * scf$wgt[top10]) / sum(scf$income * scf$wgt)
# variable: wageinc
top10 <- scf$wageinc >= wageinc_percentile[2] # logical condition for top 10%
sum(scf$wageinc[top10] * scf$wgt[top10]) / sum(scf$wageinc * scf$wgt)

# (f) wealth share of top 1%
# variable: networth
top1 <- scf$networth >= networth_percentile[3] # logical condition for top 1%
sum(scf$networth[top1] * scf$wgt[top1]) / sum(scf$networth * scf$wgt) # note: weighted sum
# variable: income
top1 <- scf$income >= income_percentile[3] # logical condition for top 1%
sum(scf$income[top1] * scf$wgt[top1]) / sum(scf$income * scf$wgt)
# variable: wageinc
top1 <- scf$wageinc >= wageinc_percentile[3] # logical condition for top 1%
sum(scf$wageinc[top1] * scf$wgt[top1]) / sum(scf$wageinc * scf$wgt)


### Question 3

# (a) variable: networth
library(dplyr)
networth_mean_byage = scf %>%
  group_by(age) %>%
  summarise(weighted_mean = weighted.mean(networth, wgt)) %>% as.data.frame()

networth_median_byage = scf %>%
  group_by(age) %>%
  summarise(weighted_median = wtd.quantile(networth, weights = wgt, probs = c(0.5))) %>% as.data.frame()

plot(x = networth_mean_byage$age,
     y = networth_mean_byage$weighted_mean,
     main = "Mean Networth - Age",
     xlab = "Age",
     ylab = "Networth",
     xlim = c(10, 100),
     ylim = c(0, 2000000),
     pch = 16,
     col = "red3")

plot(x = networth_median_byage$age,
     y = networth_median_byage$weighted_median,
     main = "Median Networth - Age",
     xlab = "Age",
     ylab = "Networth",
     xlim = c(10, 100),
     ylim = c(0, 2000000),
     pch = 16,
     col = "dodgerblue")

# (b) variable: income
income_mean_byage = scf %>%
  group_by(age) %>%
  summarise(weighted_mean = weighted.mean(income, wgt)) %>% as.data.frame()

income_median_byage = scf %>%
  group_by(age) %>%
  summarise(weighted_median = wtd.quantile(income, weights = wgt, probs = c(0.5))) %>% as.data.frame()

plot(x = income_mean_byage$age,
     y = income_mean_byage$weighted_mean,
     main = "Mean Income - Age",
     xlab = "Age",
     ylab = "Income",
     xlim = c(10, 100),
     ylim = c(0, 200000),
     pch = 16,
     col = "red3")

plot(x = income_median_byage$age,
     y = income_median_byage$weighted_median,
     main = "Median Income - Age",
     xlab = "Age",
     ylab = "Income",
     xlim = c(10, 100),
     ylim = c(0, 200000),
     pch = 16,
     col = "dodgerblue")

# (c) variable: wageinc
wageinc_mean_byage = scf %>%
  group_by(age) %>%
  summarise(weighted_mean = weighted.mean(wageinc, wgt)) %>% as.data.frame()

wageinc_median_byage = scf %>%
  group_by(age) %>%
  summarise(weighted_median = wtd.quantile(wageinc, weights = wgt, probs = c(0.5))) %>% as.data.frame()

plot(x = wageinc_mean_byage$age,
     y = wageinc_mean_byage$weighted_mean,
     main = "Mean Earnings - Age",
     xlab = "Age",
     ylab = "Earnings",
     xlim = c(10, 100),
     ylim = c(0, 150000),
     pch = 16,
     col = "red3")

plot(x = wageinc_median_byage$age,
     y = wageinc_median_byage$weighted_median,
     main = "Median Earnings - Age",
     xlab = "Age",
     ylab = "Earnings",
     xlim = c(10, 100),
     ylim = c(0, 150000),
     pch = 16,
     col = "dodgerblue")

# (d)
agespace = seq(18, 95, 1)
fraction = rep(0, 78)
for (i in agespace){
  target <- scf$networth <= 0 & scf$age == i # logical condition for (networth <= 0) AND (age = i)
  frac = sum(scf$wgt[target]) / sum(scf$wgt)
  fraction[i-17] = frac
}

plot(x = agespace,
     y = fraction,
     main = "Fraction of Households with Zero or Negative Networth - Age",
     xlab = "Age",
     ylab = "Fraction",
     ylim = c(0, 0.006),
     pch = 16,
     col = "darkgreen")
