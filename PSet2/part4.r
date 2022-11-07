# The University of Chicago
# Spring 2022
# ECMA 33620 Intro to Heterogeneous Agent Macroeconomics
# Instructor: Prof. Greg Kaplan

# Problem Set 2
# Part 4
# Chongyu Fang

# Set path
setwd("~/Desktop/ECMA 33620 Heterogeneous Agent Macroeconomics/Problem Sets/Problem Set 2")

# Read data
library(haven)
scf_1989 = read_dta("rscfp1989.dta")
scf_1992 = read_dta("rscfp1992.dta")
scf_1995 = read_dta("rscfp1995.dta")
scf_1998 = read_dta("rscfp1998.dta")
scf_2001 = read_dta("rscfp2001.dta")
scf_2004 = read_dta("rscfp2004.dta")
scf_2007 = read_dta("rscfp2007.dta")
scf_2010 = read_dta("rscfp2010.dta")
scf_2013 = read_dta("rscfp2013.dta")
scf_2016 = read_dta("rscfp2016.dta")
scf_2019 = read_dta("rscfp2019.dta")

# Years
years = c(1989, 1992, 1995, 1998, 2001, 2004, 2007, 2010, 2013, 2016, 2019)

# Switch off scientific notation
options(scipen = 999)

##### (a) coefficient of variation #####
library(Hmisc) # for calculating weighted variance

# networth
coef_var_networth = rep(0,11)
coef_var_networth[1] = sqrt(wtd.var(scf_1989$networth, scf_1989$wgt)) / weighted.mean(scf_1989$networth, scf_1989$wgt)
coef_var_networth[2] = sqrt(wtd.var(scf_1992$networth, scf_1992$wgt)) / weighted.mean(scf_1992$networth, scf_1992$wgt)
coef_var_networth[3] = sqrt(wtd.var(scf_1995$networth, scf_1995$wgt)) / weighted.mean(scf_1995$networth, scf_1995$wgt)
coef_var_networth[4] = sqrt(wtd.var(scf_1998$networth, scf_1998$wgt)) / weighted.mean(scf_1998$networth, scf_1998$wgt)
coef_var_networth[5] = sqrt(wtd.var(scf_2001$networth, scf_2001$wgt)) / weighted.mean(scf_2001$networth, scf_2001$wgt)
coef_var_networth[6] = sqrt(wtd.var(scf_2004$networth, scf_2004$wgt)) / weighted.mean(scf_2004$networth, scf_2004$wgt)
coef_var_networth[7] = sqrt(wtd.var(scf_2007$networth, scf_2007$wgt)) / weighted.mean(scf_2007$networth, scf_2007$wgt)
coef_var_networth[8] = sqrt(wtd.var(scf_2010$networth, scf_2010$wgt)) / weighted.mean(scf_2010$networth, scf_2010$wgt)
coef_var_networth[9] = sqrt(wtd.var(scf_2013$networth, scf_2013$wgt)) / weighted.mean(scf_2013$networth, scf_2013$wgt)
coef_var_networth[10] = sqrt(wtd.var(scf_2016$networth, scf_2016$wgt)) / weighted.mean(scf_2016$networth, scf_2016$wgt)
coef_var_networth[11] = sqrt(wtd.var(scf_2019$networth, scf_2019$wgt)) / weighted.mean(scf_2019$networth, scf_2019$wgt)

# income
coef_var_income = rep(0,11)
coef_var_income[1] = sqrt(wtd.var(scf_1989$income, scf_1989$wgt)) / weighted.mean(scf_1989$income, scf_1989$wgt)
coef_var_income[2] = sqrt(wtd.var(scf_1992$income, scf_1992$wgt)) / weighted.mean(scf_1992$income, scf_1992$wgt)
coef_var_income[3] = sqrt(wtd.var(scf_1995$income, scf_1995$wgt)) / weighted.mean(scf_1995$income, scf_1995$wgt)
coef_var_income[4] = sqrt(wtd.var(scf_1998$income, scf_1998$wgt)) / weighted.mean(scf_1998$income, scf_1998$wgt)
coef_var_income[5] = sqrt(wtd.var(scf_2001$income, scf_2001$wgt)) / weighted.mean(scf_2001$income, scf_2001$wgt)
coef_var_income[6] = sqrt(wtd.var(scf_2004$income, scf_2004$wgt)) / weighted.mean(scf_2004$income, scf_2004$wgt)
coef_var_income[7] = sqrt(wtd.var(scf_2007$income, scf_2007$wgt)) / weighted.mean(scf_2007$income, scf_2007$wgt)
coef_var_income[8] = sqrt(wtd.var(scf_2010$income, scf_2010$wgt)) / weighted.mean(scf_2010$income, scf_2010$wgt)
coef_var_income[9] = sqrt(wtd.var(scf_2013$income, scf_2013$wgt)) / weighted.mean(scf_2013$income, scf_2013$wgt)
coef_var_income[10] = sqrt(wtd.var(scf_2016$income, scf_2016$wgt)) / weighted.mean(scf_2016$income, scf_2016$wgt)
coef_var_income[11] = sqrt(wtd.var(scf_2019$income, scf_2019$wgt)) / weighted.mean(scf_2019$income, scf_2019$wgt)

# wageinc
coef_var_wageinc = rep(0,11)
coef_var_wageinc[1] = sqrt(wtd.var(scf_1989$wageinc, scf_1989$wgt)) / weighted.mean(scf_1989$wageinc, scf_1989$wgt)
coef_var_wageinc[2] = sqrt(wtd.var(scf_1992$wageinc, scf_1992$wgt)) / weighted.mean(scf_1992$wageinc, scf_1992$wgt)
coef_var_wageinc[3] = sqrt(wtd.var(scf_1995$wageinc, scf_1995$wgt)) / weighted.mean(scf_1995$wageinc, scf_1995$wgt)
coef_var_wageinc[4] = sqrt(wtd.var(scf_1998$wageinc, scf_1998$wgt)) / weighted.mean(scf_1998$wageinc, scf_1998$wgt)
coef_var_wageinc[5] = sqrt(wtd.var(scf_2001$wageinc, scf_2001$wgt)) / weighted.mean(scf_2001$wageinc, scf_2001$wgt)
coef_var_wageinc[6] = sqrt(wtd.var(scf_2004$wageinc, scf_2004$wgt)) / weighted.mean(scf_2004$wageinc, scf_2004$wgt)
coef_var_wageinc[7] = sqrt(wtd.var(scf_2007$wageinc, scf_2007$wgt)) / weighted.mean(scf_2007$wageinc, scf_2007$wgt)
coef_var_wageinc[8] = sqrt(wtd.var(scf_2010$wageinc, scf_2010$wgt)) / weighted.mean(scf_2010$wageinc, scf_2010$wgt)
coef_var_wageinc[9] = sqrt(wtd.var(scf_2013$wageinc, scf_2013$wgt)) / weighted.mean(scf_2013$wageinc, scf_2013$wgt)
coef_var_wageinc[10] = sqrt(wtd.var(scf_2016$wageinc, scf_2016$wgt)) / weighted.mean(scf_2016$wageinc, scf_2016$wgt)
coef_var_wageinc[11] = sqrt(wtd.var(scf_2019$wageinc, scf_2019$wgt)) / weighted.mean(scf_2019$wageinc, scf_2019$wgt)

# Plot
plot(x = years,
     y = coef_var_networth, ylab = "Coefficient of Variation", ylim = c(1,8),
     main = "Coefficient of Variation by Years",
     type = "l",
     col = "red3",
     lwd = 3)
lines(x = years,
      y = coef_var_income,
      type = "l",
      col = "dodgerblue",
      lwd = 3)
lines(x = years,
      y = coef_var_wageinc,
      type = "l",
      col = "darkgreen",
      lwd = 3)
legend("topleft",
       c("networth", "income", "wageinc"),
       pch = c("l", "l", "l"),
       col = c("red3", "dodgerblue", "darkgreen"),
       bg = "white")

##### (b) variance of logs (exclude zero and negative values) #####

# networth
var_logs_networth = rep(0,11)
positive_1989 <- scf_1989$networth > 0
var_logs_networth[1] = wtd.var(log(scf_1989$networth[positive_1989]), scf_1989$wgt[positive_1989])
positive_1992 <- scf_1992$networth > 0
var_logs_networth[2] = wtd.var(log(scf_1992$networth[positive_1992]), scf_1992$wgt[positive_1992])
positive_1995 <- scf_1995$networth > 0
var_logs_networth[3] = wtd.var(log(scf_1995$networth[positive_1995]), scf_1995$wgt[positive_1995])
positive_1998 <- scf_1998$networth > 0
var_logs_networth[4] = wtd.var(log(scf_1998$networth[positive_1998]), scf_1998$wgt[positive_1998])
positive_2001 <- scf_2001$networth > 0
var_logs_networth[5] = wtd.var(log(scf_2001$networth[positive_2001]), scf_2001$wgt[positive_2001])
positive_2004 <- scf_2004$networth > 0
var_logs_networth[6] = wtd.var(log(scf_2004$networth[positive_2004]), scf_2004$wgt[positive_2004])
positive_2007 <- scf_2007$networth > 0
var_logs_networth[7] = wtd.var(log(scf_2007$networth[positive_2007]), scf_2007$wgt[positive_2007])
positive_2010 <- scf_2010$networth > 0
var_logs_networth[8] = wtd.var(log(scf_2010$networth[positive_2010]), scf_2010$wgt[positive_2010])
positive_2013 <- scf_2013$networth > 0
var_logs_networth[9] = wtd.var(log(scf_2013$networth[positive_2013]), scf_2013$wgt[positive_2013])
positive_2016 <- scf_2016$networth > 0
var_logs_networth[10] = wtd.var(log(scf_2016$networth[positive_2016]), scf_2016$wgt[positive_2016])
positive_2019 <- scf_2019$networth > 0
var_logs_networth[11] = wtd.var(log(scf_2019$networth[positive_2019]), scf_2019$wgt[positive_2019])

# income
var_logs_income = rep(0,11)
positive_1989 <- scf_1989$income > 0
var_logs_income[1] = wtd.var(log(scf_1989$income[positive_1989]), scf_1989$wgt[positive_1989])
positive_1992 <- scf_1992$income > 0
var_logs_income[2] = wtd.var(log(scf_1992$income[positive_1992]), scf_1992$wgt[positive_1992])
positive_1995 <- scf_1995$income > 0
var_logs_income[3] = wtd.var(log(scf_1995$income[positive_1995]), scf_1995$wgt[positive_1995])
positive_1998 <- scf_1998$income > 0
var_logs_income[4] = wtd.var(log(scf_1998$income[positive_1998]), scf_1998$wgt[positive_1998])
positive_2001 <- scf_2001$income > 0
var_logs_income[5] = wtd.var(log(scf_2001$income[positive_2001]), scf_2001$wgt[positive_2001])
positive_2004 <- scf_2004$income > 0
var_logs_income[6] = wtd.var(log(scf_2004$income[positive_2004]), scf_2004$wgt[positive_2004])
positive_2007 <- scf_2007$income > 0
var_logs_income[7] = wtd.var(log(scf_2007$income[positive_2007]), scf_2007$wgt[positive_2007])
positive_2010 <- scf_2010$income > 0
var_logs_income[8] = wtd.var(log(scf_2010$income[positive_2010]), scf_2010$wgt[positive_2010])
positive_2013 <- scf_2013$income > 0
var_logs_income[9] = wtd.var(log(scf_2013$income[positive_2013]), scf_2013$wgt[positive_2013])
positive_2016 <- scf_2016$income > 0
var_logs_income[10] = wtd.var(log(scf_2016$income[positive_2016]), scf_2016$wgt[positive_2016])
positive_2019 <- scf_2019$income > 0
var_logs_income[11] = wtd.var(log(scf_2019$income[positive_2019]), scf_2019$wgt[positive_2019])

# wageinc
var_logs_wageinc = rep(0,11)
positive_1989 <- scf_1989$wageinc > 0
var_logs_wageinc[1] = wtd.var(log(scf_1989$wageinc[positive_1989]), scf_1989$wgt[positive_1989])
positive_1992 <- scf_1992$wageinc > 0
var_logs_wageinc[2] = wtd.var(log(scf_1992$wageinc[positive_1992]), scf_1992$wgt[positive_1992])
positive_1995 <- scf_1995$wageinc > 0
var_logs_wageinc[3] = wtd.var(log(scf_1995$wageinc[positive_1995]), scf_1995$wgt[positive_1995])
positive_1998 <- scf_1998$wageinc > 0
var_logs_wageinc[4] = wtd.var(log(scf_1998$wageinc[positive_1998]), scf_1998$wgt[positive_1998])
positive_2001 <- scf_2001$wageinc > 0
var_logs_wageinc[5] = wtd.var(log(scf_2001$wageinc[positive_2001]), scf_2001$wgt[positive_2001])
positive_2004 <- scf_2004$wageinc > 0
var_logs_wageinc[6] = wtd.var(log(scf_2004$wageinc[positive_2004]), scf_2004$wgt[positive_2004])
positive_2007 <- scf_2007$wageinc > 0
var_logs_wageinc[7] = wtd.var(log(scf_2007$wageinc[positive_2007]), scf_2007$wgt[positive_2007])
positive_2010 <- scf_2010$wageinc > 0
var_logs_wageinc[8] = wtd.var(log(scf_2010$wageinc[positive_2010]), scf_2010$wgt[positive_2010])
positive_2013 <- scf_2013$wageinc > 0
var_logs_wageinc[9] = wtd.var(log(scf_2013$wageinc[positive_2013]), scf_2013$wgt[positive_2013])
positive_2016 <- scf_2016$wageinc > 0
var_logs_wageinc[10] = wtd.var(log(scf_2016$wageinc[positive_2016]), scf_2016$wgt[positive_2016])
positive_2019 <- scf_2019$wageinc > 0
var_logs_wageinc[11] = wtd.var(log(scf_2019$wageinc[positive_2019]), scf_2019$wgt[positive_2019])

# Plot
plot(x = years,
     y = var_logs_networth, ylab = "Variance of Logs", ylim = c(0,10),
     main = "Variance of Logs by Years",
     type = "l",
     col = "red3",
     lwd = 3)
lines(x = years,
      y = var_logs_income,
      type = "l",
      col = "dodgerblue",
      lwd = 3)
lines(x = years,
      y = var_logs_wageinc,
      type = "l",
      col = "darkgreen",
      lwd = 3)
legend("topleft",
       c("networth", "income", "wageinc"),
       pch = c("l", "l", "l"),
       col = c("red3", "dodgerblue", "darkgreen"),
       bg = "white")

##### (c) gini coefficient #####
library(dineq) # for calculating weighted gini coefficient

# networth
gini_networth = rep(0,11)
gini_networth[1] = gini.wtd(scf_1989$networth, weights = scf_1989$wgt)
gini_networth[2] = gini.wtd(scf_1992$networth, weights = scf_1992$wgt)
gini_networth[3] = gini.wtd(scf_1995$networth, weights = scf_1995$wgt)
gini_networth[4] = gini.wtd(scf_1998$networth, weights = scf_1998$wgt)
gini_networth[5] = gini.wtd(scf_2001$networth, weights = scf_2001$wgt)
gini_networth[6] = gini.wtd(scf_2004$networth, weights = scf_2004$wgt)
gini_networth[7] = gini.wtd(scf_2007$networth, weights = scf_2007$wgt)
gini_networth[8] = gini.wtd(scf_2010$networth, weights = scf_2010$wgt)
gini_networth[9] = gini.wtd(scf_2013$networth, weights = scf_2013$wgt)
gini_networth[10] = gini.wtd(scf_2016$networth, weights = scf_2016$wgt)
gini_networth[11] = gini.wtd(scf_2019$networth, weights = scf_2019$wgt)

# income
gini_income = rep(0,11)
gini_income[1] = gini.wtd(scf_1989$income, weights = scf_1989$wgt)
gini_income[2] = gini.wtd(scf_1992$income, weights = scf_1992$wgt)
gini_income[3] = gini.wtd(scf_1995$income, weights = scf_1995$wgt)
gini_income[4] = gini.wtd(scf_1998$income, weights = scf_1998$wgt)
gini_income[5] = gini.wtd(scf_2001$income, weights = scf_2001$wgt)
gini_income[6] = gini.wtd(scf_2004$income, weights = scf_2004$wgt)
gini_income[7] = gini.wtd(scf_2007$income, weights = scf_2007$wgt)
gini_income[8] = gini.wtd(scf_2010$income, weights = scf_2010$wgt)
gini_income[9] = gini.wtd(scf_2013$income, weights = scf_2013$wgt)
gini_income[10] = gini.wtd(scf_2016$income, weights = scf_2016$wgt)
gini_income[11] = gini.wtd(scf_2019$income, weights = scf_2019$wgt)

# wageinc
gini_wageinc = rep(0,11)
gini_wageinc[1] = gini.wtd(scf_1989$wageinc, weights = scf_1989$wgt)
gini_wageinc[2] = gini.wtd(scf_1992$wageinc, weights = scf_1992$wgt)
gini_wageinc[3] = gini.wtd(scf_1995$wageinc, weights = scf_1995$wgt)
gini_wageinc[4] = gini.wtd(scf_1998$wageinc, weights = scf_1998$wgt)
gini_wageinc[5] = gini.wtd(scf_2001$wageinc, weights = scf_2001$wgt)
gini_wageinc[6] = gini.wtd(scf_2004$wageinc, weights = scf_2004$wgt)
gini_wageinc[7] = gini.wtd(scf_2007$wageinc, weights = scf_2007$wgt)
gini_wageinc[8] = gini.wtd(scf_2010$wageinc, weights = scf_2010$wgt)
gini_wageinc[9] = gini.wtd(scf_2013$wageinc, weights = scf_2013$wgt)
gini_wageinc[10] = gini.wtd(scf_2016$wageinc, weights = scf_2016$wgt)
gini_wageinc[11] = gini.wtd(scf_2019$wageinc, weights = scf_2019$wgt)

# Plot
plot(x = years,
     y = gini_networth, ylab = "Gini Coefficient", ylim = c(0.4,1),
     main = "Gini Coefficient by Years",
     type = "l",
     col = "red3",
     lwd = 3)
lines(x = years,
      y = gini_income,
      type = "l",
      col = "dodgerblue",
      lwd = 3)
lines(x = years,
      y = gini_wageinc,
      type = "l",
      col = "darkgreen",
      lwd = 3)
legend("topleft",
       c("networth", "income", "wageinc"),
       pch = c("l", "l", "l"),
       col = c("red3", "dodgerblue", "darkgreen"),
       bg = "white")

##### (d) 99-50 ratio #####

# networth
ratio_9950_networth = rep(0,11)
ratio_9950_networth[1] = wtd.quantile(scf_1989$networth, weights = scf_1989$wgt, probs = c(0.5, 0.9, 0.99))[3] / wtd.quantile(scf_1989$networth, weights = scf_1989$wgt, probs = c(0.5, 0.9, 0.99))[1]
ratio_9950_networth[2] = wtd.quantile(scf_1992$networth, weights = scf_1992$wgt, probs = c(0.5, 0.9, 0.99))[3] / wtd.quantile(scf_1992$networth, weights = scf_1992$wgt, probs = c(0.5, 0.9, 0.99))[1]
ratio_9950_networth[3] = wtd.quantile(scf_1995$networth, weights = scf_1995$wgt, probs = c(0.5, 0.9, 0.99))[3] / wtd.quantile(scf_1995$networth, weights = scf_1995$wgt, probs = c(0.5, 0.9, 0.99))[1]
ratio_9950_networth[4] = wtd.quantile(scf_1998$networth, weights = scf_1998$wgt, probs = c(0.5, 0.9, 0.99))[3] / wtd.quantile(scf_1998$networth, weights = scf_1998$wgt, probs = c(0.5, 0.9, 0.99))[1]
ratio_9950_networth[5] = wtd.quantile(scf_2001$networth, weights = scf_2001$wgt, probs = c(0.5, 0.9, 0.99))[3] / wtd.quantile(scf_2001$networth, weights = scf_2001$wgt, probs = c(0.5, 0.9, 0.99))[1]
ratio_9950_networth[6] = wtd.quantile(scf_2004$networth, weights = scf_2004$wgt, probs = c(0.5, 0.9, 0.99))[3] / wtd.quantile(scf_2004$networth, weights = scf_2004$wgt, probs = c(0.5, 0.9, 0.99))[1]
ratio_9950_networth[7] = wtd.quantile(scf_2007$networth, weights = scf_2007$wgt, probs = c(0.5, 0.9, 0.99))[3] / wtd.quantile(scf_2007$networth, weights = scf_2007$wgt, probs = c(0.5, 0.9, 0.99))[1]
ratio_9950_networth[8] = wtd.quantile(scf_2010$networth, weights = scf_2010$wgt, probs = c(0.5, 0.9, 0.99))[3] / wtd.quantile(scf_2010$networth, weights = scf_2010$wgt, probs = c(0.5, 0.9, 0.99))[1]
ratio_9950_networth[9] = wtd.quantile(scf_2013$networth, weights = scf_2013$wgt, probs = c(0.5, 0.9, 0.99))[3] / wtd.quantile(scf_2013$networth, weights = scf_2013$wgt, probs = c(0.5, 0.9, 0.99))[1]
ratio_9950_networth[10] = wtd.quantile(scf_2016$networth, weights = scf_2016$wgt, probs = c(0.5, 0.9, 0.99))[3] / wtd.quantile(scf_2016$networth, weights = scf_2016$wgt, probs = c(0.5, 0.9, 0.99))[1]
ratio_9950_networth[11] = wtd.quantile(scf_2019$networth, weights = scf_2019$wgt, probs = c(0.5, 0.9, 0.99))[3] / wtd.quantile(scf_2019$networth, weights = scf_2019$wgt, probs = c(0.5, 0.9, 0.99))[1]

# income
ratio_9950_income = rep(0,11)
ratio_9950_income[1] = wtd.quantile(scf_1989$income, weights = scf_1989$wgt, probs = c(0.5, 0.9, 0.99))[3] / wtd.quantile(scf_1989$income, weights = scf_1989$wgt, probs = c(0.5, 0.9, 0.99))[1]
ratio_9950_income[2] = wtd.quantile(scf_1992$income, weights = scf_1992$wgt, probs = c(0.5, 0.9, 0.99))[3] / wtd.quantile(scf_1992$income, weights = scf_1992$wgt, probs = c(0.5, 0.9, 0.99))[1]
ratio_9950_income[3] = wtd.quantile(scf_1995$income, weights = scf_1995$wgt, probs = c(0.5, 0.9, 0.99))[3] / wtd.quantile(scf_1995$income, weights = scf_1995$wgt, probs = c(0.5, 0.9, 0.99))[1]
ratio_9950_income[4] = wtd.quantile(scf_1998$income, weights = scf_1998$wgt, probs = c(0.5, 0.9, 0.99))[3] / wtd.quantile(scf_1998$income, weights = scf_1998$wgt, probs = c(0.5, 0.9, 0.99))[1]
ratio_9950_income[5] = wtd.quantile(scf_2001$income, weights = scf_2001$wgt, probs = c(0.5, 0.9, 0.99))[3] / wtd.quantile(scf_2001$income, weights = scf_2001$wgt, probs = c(0.5, 0.9, 0.99))[1]
ratio_9950_income[6] = wtd.quantile(scf_2004$income, weights = scf_2004$wgt, probs = c(0.5, 0.9, 0.99))[3] / wtd.quantile(scf_2004$income, weights = scf_2004$wgt, probs = c(0.5, 0.9, 0.99))[1]
ratio_9950_income[7] = wtd.quantile(scf_2007$income, weights = scf_2007$wgt, probs = c(0.5, 0.9, 0.99))[3] / wtd.quantile(scf_2007$income, weights = scf_2007$wgt, probs = c(0.5, 0.9, 0.99))[1]
ratio_9950_income[8] = wtd.quantile(scf_2010$income, weights = scf_2010$wgt, probs = c(0.5, 0.9, 0.99))[3] / wtd.quantile(scf_2010$income, weights = scf_2010$wgt, probs = c(0.5, 0.9, 0.99))[1]
ratio_9950_income[9] = wtd.quantile(scf_2013$income, weights = scf_2013$wgt, probs = c(0.5, 0.9, 0.99))[3] / wtd.quantile(scf_2013$income, weights = scf_2013$wgt, probs = c(0.5, 0.9, 0.99))[1]
ratio_9950_income[10] = wtd.quantile(scf_2016$income, weights = scf_2016$wgt, probs = c(0.5, 0.9, 0.99))[3] / wtd.quantile(scf_2016$income, weights = scf_2016$wgt, probs = c(0.5, 0.9, 0.99))[1]
ratio_9950_income[11] = wtd.quantile(scf_2019$income, weights = scf_2019$wgt, probs = c(0.5, 0.9, 0.99))[3] / wtd.quantile(scf_2019$income, weights = scf_2019$wgt, probs = c(0.5, 0.9, 0.99))[1]

# wageinc
ratio_9950_wageinc = rep(0,11)
ratio_9950_wageinc[1] = wtd.quantile(scf_1989$wageinc, weights = scf_1989$wgt, probs = c(0.5, 0.9, 0.99))[3] / wtd.quantile(scf_1989$wageinc, weights = scf_1989$wgt, probs = c(0.5, 0.9, 0.99))[1]
ratio_9950_wageinc[2] = wtd.quantile(scf_1992$wageinc, weights = scf_1992$wgt, probs = c(0.5, 0.9, 0.99))[3] / wtd.quantile(scf_1992$wageinc, weights = scf_1992$wgt, probs = c(0.5, 0.9, 0.99))[1]
ratio_9950_wageinc[3] = wtd.quantile(scf_1995$wageinc, weights = scf_1995$wgt, probs = c(0.5, 0.9, 0.99))[3] / wtd.quantile(scf_1995$wageinc, weights = scf_1995$wgt, probs = c(0.5, 0.9, 0.99))[1]
ratio_9950_wageinc[4] = wtd.quantile(scf_1998$wageinc, weights = scf_1998$wgt, probs = c(0.5, 0.9, 0.99))[3] / wtd.quantile(scf_1998$wageinc, weights = scf_1998$wgt, probs = c(0.5, 0.9, 0.99))[1]
ratio_9950_wageinc[5] = wtd.quantile(scf_2001$wageinc, weights = scf_2001$wgt, probs = c(0.5, 0.9, 0.99))[3] / wtd.quantile(scf_2001$wageinc, weights = scf_2001$wgt, probs = c(0.5, 0.9, 0.99))[1]
ratio_9950_wageinc[6] = wtd.quantile(scf_2004$wageinc, weights = scf_2004$wgt, probs = c(0.5, 0.9, 0.99))[3] / wtd.quantile(scf_2004$wageinc, weights = scf_2004$wgt, probs = c(0.5, 0.9, 0.99))[1]
ratio_9950_wageinc[7] = wtd.quantile(scf_2007$wageinc, weights = scf_2007$wgt, probs = c(0.5, 0.9, 0.99))[3] / wtd.quantile(scf_2007$wageinc, weights = scf_2007$wgt, probs = c(0.5, 0.9, 0.99))[1]
ratio_9950_wageinc[8] = wtd.quantile(scf_2010$wageinc, weights = scf_2010$wgt, probs = c(0.5, 0.9, 0.99))[3] / wtd.quantile(scf_2010$wageinc, weights = scf_2010$wgt, probs = c(0.5, 0.9, 0.99))[1]
ratio_9950_wageinc[9] = wtd.quantile(scf_2013$wageinc, weights = scf_2013$wgt, probs = c(0.5, 0.9, 0.99))[3] / wtd.quantile(scf_2013$wageinc, weights = scf_2013$wgt, probs = c(0.5, 0.9, 0.99))[1]
ratio_9950_wageinc[10] = wtd.quantile(scf_2016$wageinc, weights = scf_2016$wgt, probs = c(0.5, 0.9, 0.99))[3] / wtd.quantile(scf_2016$wageinc, weights = scf_2016$wgt, probs = c(0.5, 0.9, 0.99))[1]
ratio_9950_wageinc[11] = wtd.quantile(scf_2019$wageinc, weights = scf_2019$wgt, probs = c(0.5, 0.9, 0.99))[3] / wtd.quantile(scf_2019$wageinc, weights = scf_2019$wgt, probs = c(0.5, 0.9, 0.99))[1]

# Plot
plot(x = years,
     y = ratio_9950_networth, ylab = "99-50 Ratio", ylim = c(0,110),
     main = "99-50 Ratio by Years",
     type = "l",
     col = "red3",
     lwd = 3)
lines(x = years,
      y = ratio_9950_income,
      type = "l",
      col = "dodgerblue",
      lwd = 3)
lines(x = years,
      y = ratio_9950_wageinc,
      type = "l",
      col = "darkgreen",
      lwd = 3)
legend("topleft",
       c("networth", "income", "wageinc"),
       pch = c("l", "l", "l"),
       col = c("red3", "dodgerblue", "darkgreen"),
       bg = "white")

##### (e) 90-50 ratio #####

# networth
ratio_9050_networth = rep(0,11)
ratio_9050_networth[1] = wtd.quantile(scf_1989$networth, weights = scf_1989$wgt, probs = c(0.5, 0.9, 0.99))[2] / wtd.quantile(scf_1989$networth, weights = scf_1989$wgt, probs = c(0.5, 0.9, 0.99))[1]
ratio_9050_networth[2] = wtd.quantile(scf_1992$networth, weights = scf_1992$wgt, probs = c(0.5, 0.9, 0.99))[2] / wtd.quantile(scf_1992$networth, weights = scf_1992$wgt, probs = c(0.5, 0.9, 0.99))[1]
ratio_9050_networth[3] = wtd.quantile(scf_1995$networth, weights = scf_1995$wgt, probs = c(0.5, 0.9, 0.99))[2] / wtd.quantile(scf_1995$networth, weights = scf_1995$wgt, probs = c(0.5, 0.9, 0.99))[1]
ratio_9050_networth[4] = wtd.quantile(scf_1998$networth, weights = scf_1998$wgt, probs = c(0.5, 0.9, 0.99))[2] / wtd.quantile(scf_1998$networth, weights = scf_1998$wgt, probs = c(0.5, 0.9, 0.99))[1]
ratio_9050_networth[5] = wtd.quantile(scf_2001$networth, weights = scf_2001$wgt, probs = c(0.5, 0.9, 0.99))[2] / wtd.quantile(scf_2001$networth, weights = scf_2001$wgt, probs = c(0.5, 0.9, 0.99))[1]
ratio_9050_networth[6] = wtd.quantile(scf_2004$networth, weights = scf_2004$wgt, probs = c(0.5, 0.9, 0.99))[2] / wtd.quantile(scf_2004$networth, weights = scf_2004$wgt, probs = c(0.5, 0.9, 0.99))[1]
ratio_9050_networth[7] = wtd.quantile(scf_2007$networth, weights = scf_2007$wgt, probs = c(0.5, 0.9, 0.99))[2] / wtd.quantile(scf_2007$networth, weights = scf_2007$wgt, probs = c(0.5, 0.9, 0.99))[1]
ratio_9050_networth[8] = wtd.quantile(scf_2010$networth, weights = scf_2010$wgt, probs = c(0.5, 0.9, 0.99))[2] / wtd.quantile(scf_2010$networth, weights = scf_2010$wgt, probs = c(0.5, 0.9, 0.99))[1]
ratio_9050_networth[9] = wtd.quantile(scf_2013$networth, weights = scf_2013$wgt, probs = c(0.5, 0.9, 0.99))[2] / wtd.quantile(scf_2013$networth, weights = scf_2013$wgt, probs = c(0.5, 0.9, 0.99))[1]
ratio_9050_networth[10] = wtd.quantile(scf_2016$networth, weights = scf_2016$wgt, probs = c(0.5, 0.9, 0.99))[2] / wtd.quantile(scf_2016$networth, weights = scf_2016$wgt, probs = c(0.5, 0.9, 0.99))[1]
ratio_9050_networth[11] = wtd.quantile(scf_2019$networth, weights = scf_2019$wgt, probs = c(0.5, 0.9, 0.99))[2] / wtd.quantile(scf_2019$networth, weights = scf_2019$wgt, probs = c(0.5, 0.9, 0.99))[1]

# income
ratio_9050_income = rep(0,11)
ratio_9050_income[1] = wtd.quantile(scf_1989$income, weights = scf_1989$wgt, probs = c(0.5, 0.9, 0.99))[2] / wtd.quantile(scf_1989$income, weights = scf_1989$wgt, probs = c(0.5, 0.9, 0.99))[1]
ratio_9050_income[2] = wtd.quantile(scf_1992$income, weights = scf_1992$wgt, probs = c(0.5, 0.9, 0.99))[2] / wtd.quantile(scf_1992$income, weights = scf_1992$wgt, probs = c(0.5, 0.9, 0.99))[1]
ratio_9050_income[3] = wtd.quantile(scf_1995$income, weights = scf_1995$wgt, probs = c(0.5, 0.9, 0.99))[2] / wtd.quantile(scf_1995$income, weights = scf_1995$wgt, probs = c(0.5, 0.9, 0.99))[1]
ratio_9050_income[4] = wtd.quantile(scf_1998$income, weights = scf_1998$wgt, probs = c(0.5, 0.9, 0.99))[2] / wtd.quantile(scf_1998$income, weights = scf_1998$wgt, probs = c(0.5, 0.9, 0.99))[1]
ratio_9050_income[5] = wtd.quantile(scf_2001$income, weights = scf_2001$wgt, probs = c(0.5, 0.9, 0.99))[2] / wtd.quantile(scf_2001$income, weights = scf_2001$wgt, probs = c(0.5, 0.9, 0.99))[1]
ratio_9050_income[6] = wtd.quantile(scf_2004$income, weights = scf_2004$wgt, probs = c(0.5, 0.9, 0.99))[2] / wtd.quantile(scf_2004$income, weights = scf_2004$wgt, probs = c(0.5, 0.9, 0.99))[1]
ratio_9050_income[7] = wtd.quantile(scf_2007$income, weights = scf_2007$wgt, probs = c(0.5, 0.9, 0.99))[2] / wtd.quantile(scf_2007$income, weights = scf_2007$wgt, probs = c(0.5, 0.9, 0.99))[1]
ratio_9050_income[8] = wtd.quantile(scf_2010$income, weights = scf_2010$wgt, probs = c(0.5, 0.9, 0.99))[2] / wtd.quantile(scf_2010$income, weights = scf_2010$wgt, probs = c(0.5, 0.9, 0.99))[1]
ratio_9050_income[9] = wtd.quantile(scf_2013$income, weights = scf_2013$wgt, probs = c(0.5, 0.9, 0.99))[2] / wtd.quantile(scf_2013$income, weights = scf_2013$wgt, probs = c(0.5, 0.9, 0.99))[1]
ratio_9050_income[10] = wtd.quantile(scf_2016$income, weights = scf_2016$wgt, probs = c(0.5, 0.9, 0.99))[2] / wtd.quantile(scf_2016$income, weights = scf_2016$wgt, probs = c(0.5, 0.9, 0.99))[1]
ratio_9050_income[11] = wtd.quantile(scf_2019$income, weights = scf_2019$wgt, probs = c(0.5, 0.9, 0.99))[2] / wtd.quantile(scf_2019$income, weights = scf_2019$wgt, probs = c(0.5, 0.9, 0.99))[1]

# wageinc
ratio_9050_wageinc = rep(0,11)
ratio_9050_wageinc[1] = wtd.quantile(scf_1989$wageinc, weights = scf_1989$wgt, probs = c(0.5, 0.9, 0.99))[2] / wtd.quantile(scf_1989$wageinc, weights = scf_1989$wgt, probs = c(0.5, 0.9, 0.99))[1]
ratio_9050_wageinc[2] = wtd.quantile(scf_1992$wageinc, weights = scf_1992$wgt, probs = c(0.5, 0.9, 0.99))[2] / wtd.quantile(scf_1992$wageinc, weights = scf_1992$wgt, probs = c(0.5, 0.9, 0.99))[1]
ratio_9050_wageinc[3] = wtd.quantile(scf_1995$wageinc, weights = scf_1995$wgt, probs = c(0.5, 0.9, 0.99))[2] / wtd.quantile(scf_1995$wageinc, weights = scf_1995$wgt, probs = c(0.5, 0.9, 0.99))[1]
ratio_9050_wageinc[4] = wtd.quantile(scf_1998$wageinc, weights = scf_1998$wgt, probs = c(0.5, 0.9, 0.99))[2] / wtd.quantile(scf_1998$wageinc, weights = scf_1998$wgt, probs = c(0.5, 0.9, 0.99))[1]
ratio_9050_wageinc[5] = wtd.quantile(scf_2001$wageinc, weights = scf_2001$wgt, probs = c(0.5, 0.9, 0.99))[2] / wtd.quantile(scf_2001$wageinc, weights = scf_2001$wgt, probs = c(0.5, 0.9, 0.99))[1]
ratio_9050_wageinc[6] = wtd.quantile(scf_2004$wageinc, weights = scf_2004$wgt, probs = c(0.5, 0.9, 0.99))[2] / wtd.quantile(scf_2004$wageinc, weights = scf_2004$wgt, probs = c(0.5, 0.9, 0.99))[1]
ratio_9050_wageinc[7] = wtd.quantile(scf_2007$wageinc, weights = scf_2007$wgt, probs = c(0.5, 0.9, 0.99))[2] / wtd.quantile(scf_2007$wageinc, weights = scf_2007$wgt, probs = c(0.5, 0.9, 0.99))[1]
ratio_9050_wageinc[8] = wtd.quantile(scf_2010$wageinc, weights = scf_2010$wgt, probs = c(0.5, 0.9, 0.99))[2] / wtd.quantile(scf_2010$wageinc, weights = scf_2010$wgt, probs = c(0.5, 0.9, 0.99))[1]
ratio_9050_wageinc[9] = wtd.quantile(scf_2013$wageinc, weights = scf_2013$wgt, probs = c(0.5, 0.9, 0.99))[2] / wtd.quantile(scf_2013$wageinc, weights = scf_2013$wgt, probs = c(0.5, 0.9, 0.99))[1]
ratio_9050_wageinc[10] = wtd.quantile(scf_2016$wageinc, weights = scf_2016$wgt, probs = c(0.5, 0.9, 0.99))[2] / wtd.quantile(scf_2016$wageinc, weights = scf_2016$wgt, probs = c(0.5, 0.9, 0.99))[1]
ratio_9050_wageinc[11] = wtd.quantile(scf_2019$wageinc, weights = scf_2019$wgt, probs = c(0.5, 0.9, 0.99))[2] / wtd.quantile(scf_2019$wageinc, weights = scf_2019$wgt, probs = c(0.5, 0.9, 0.99))[1]

# Plot
plot(x = years,
     y = ratio_9050_networth, ylab = "90-50 Ratio", ylim = c(0,15),
     main = "90-50 Ratio by Years",
     type = "l",
     col = "red3",
     lwd = 3)
lines(x = years,
      y = ratio_9050_income,
      type = "l",
      col = "dodgerblue",
      lwd = 3)
lines(x = years,
      y = ratio_9050_wageinc,
      type = "l",
      col = "darkgreen",
      lwd = 3)
legend("topleft",
       c("networth", "income", "wageinc"),
       pch = c("l", "l", "l"),
       col = c("red3", "dodgerblue", "darkgreen"),
       bg = "white")

##### (f) wealth share of top 10% #####

# networth
wealth_share_top_10per_networth = rep(0,11)
top10_1989 <- scf_1989$networth >= wtd.quantile(scf_1989$networth, weights = scf_1989$wgt, probs = c(0.5, 0.9, 0.99))[2]
wealth_share_top_10per_networth[1] = sum(scf_1989$networth[top10_1989] * scf_1989$wgt[top10_1989]) / sum(scf_1989$networth * scf_1989$wgt)
top10_1992 <- scf_1992$networth >= wtd.quantile(scf_1992$networth, weights = scf_1992$wgt, probs = c(0.5, 0.9, 0.99))[2]
wealth_share_top_10per_networth[2] = sum(scf_1992$networth[top10_1992] * scf_1992$wgt[top10_1992]) / sum(scf_1992$networth * scf_1992$wgt)
top10_1995 <- scf_1995$networth >= wtd.quantile(scf_1995$networth, weights = scf_1995$wgt, probs = c(0.5, 0.9, 0.99))[2]
wealth_share_top_10per_networth[3] = sum(scf_1995$networth[top10_1995] * scf_1995$wgt[top10_1995]) / sum(scf_1995$networth * scf_1995$wgt)
top10_1998 <- scf_1998$networth >= wtd.quantile(scf_1998$networth, weights = scf_1998$wgt, probs = c(0.5, 0.9, 0.99))[2]
wealth_share_top_10per_networth[4] = sum(scf_1998$networth[top10_1998] * scf_1998$wgt[top10_1998]) / sum(scf_1998$networth * scf_1998$wgt)
top10_2001 <- scf_2001$networth >= wtd.quantile(scf_2001$networth, weights = scf_2001$wgt, probs = c(0.5, 0.9, 0.99))[2]
wealth_share_top_10per_networth[5] = sum(scf_2001$networth[top10_2001] * scf_2001$wgt[top10_2001]) / sum(scf_2001$networth * scf_2001$wgt)
top10_2004 <- scf_2004$networth >= wtd.quantile(scf_2004$networth, weights = scf_2004$wgt, probs = c(0.5, 0.9, 0.99))[2]
wealth_share_top_10per_networth[6] = sum(scf_2004$networth[top10_2004] * scf_2004$wgt[top10_2004]) / sum(scf_2004$networth * scf_2004$wgt)
top10_2007 <- scf_2007$networth >= wtd.quantile(scf_2007$networth, weights = scf_2007$wgt, probs = c(0.5, 0.9, 0.99))[2]
wealth_share_top_10per_networth[7] = sum(scf_2007$networth[top10_2007] * scf_2007$wgt[top10_2007]) / sum(scf_2007$networth * scf_2007$wgt)
top10_2010 <- scf_2010$networth >= wtd.quantile(scf_2010$networth, weights = scf_2010$wgt, probs = c(0.5, 0.9, 0.99))[2]
wealth_share_top_10per_networth[8] = sum(scf_2010$networth[top10_2010] * scf_2010$wgt[top10_2010]) / sum(scf_2010$networth * scf_2010$wgt)
top10_2013 <- scf_2013$networth >= wtd.quantile(scf_2013$networth, weights = scf_2013$wgt, probs = c(0.5, 0.9, 0.99))[2]
wealth_share_top_10per_networth[9] = sum(scf_2013$networth[top10_2013] * scf_2013$wgt[top10_2013]) / sum(scf_2013$networth * scf_2013$wgt)
top10_2016 <- scf_2016$networth >= wtd.quantile(scf_2016$networth, weights = scf_2016$wgt, probs = c(0.5, 0.9, 0.99))[2]
wealth_share_top_10per_networth[10] = sum(scf_2016$networth[top10_2016] * scf_2016$wgt[top10_2016]) / sum(scf_2016$networth * scf_2016$wgt)
top10_2019 <- scf_2019$networth >= wtd.quantile(scf_2019$networth, weights = scf_2019$wgt, probs = c(0.5, 0.9, 0.99))[2]
wealth_share_top_10per_networth[11] = sum(scf_2019$networth[top10_2019] * scf_2019$wgt[top10_2019]) / sum(scf_2019$networth * scf_2019$wgt)

# income
wealth_share_top_10per_income = rep(0,11)
top10_1989 <- scf_1989$income >= wtd.quantile(scf_1989$income, weights = scf_1989$wgt, probs = c(0.5, 0.9, 0.99))[2]
wealth_share_top_10per_income[1] = sum(scf_1989$income[top10_1989] * scf_1989$wgt[top10_1989]) / sum(scf_1989$income * scf_1989$wgt)
top10_1992 <- scf_1992$income >= wtd.quantile(scf_1992$income, weights = scf_1992$wgt, probs = c(0.5, 0.9, 0.99))[2]
wealth_share_top_10per_income[2] = sum(scf_1992$income[top10_1992] * scf_1992$wgt[top10_1992]) / sum(scf_1992$income * scf_1992$wgt)
top10_1995 <- scf_1995$income >= wtd.quantile(scf_1995$income, weights = scf_1995$wgt, probs = c(0.5, 0.9, 0.99))[2]
wealth_share_top_10per_income[3] = sum(scf_1995$income[top10_1995] * scf_1995$wgt[top10_1995]) / sum(scf_1995$income * scf_1995$wgt)
top10_1998 <- scf_1998$income >= wtd.quantile(scf_1998$income, weights = scf_1998$wgt, probs = c(0.5, 0.9, 0.99))[2]
wealth_share_top_10per_income[4] = sum(scf_1998$income[top10_1998] * scf_1998$wgt[top10_1998]) / sum(scf_1998$income * scf_1998$wgt)
top10_2001 <- scf_2001$income >= wtd.quantile(scf_2001$income, weights = scf_2001$wgt, probs = c(0.5, 0.9, 0.99))[2]
wealth_share_top_10per_income[5] = sum(scf_2001$income[top10_2001] * scf_2001$wgt[top10_2001]) / sum(scf_2001$income * scf_2001$wgt)
top10_2004 <- scf_2004$income >= wtd.quantile(scf_2004$income, weights = scf_2004$wgt, probs = c(0.5, 0.9, 0.99))[2]
wealth_share_top_10per_income[6] = sum(scf_2004$income[top10_2004] * scf_2004$wgt[top10_2004]) / sum(scf_2004$income * scf_2004$wgt)
top10_2007 <- scf_2007$income >= wtd.quantile(scf_2007$income, weights = scf_2007$wgt, probs = c(0.5, 0.9, 0.99))[2]
wealth_share_top_10per_income[7] = sum(scf_2007$income[top10_2007] * scf_2007$wgt[top10_2007]) / sum(scf_2007$income * scf_2007$wgt)
top10_2010 <- scf_2010$income >= wtd.quantile(scf_2010$income, weights = scf_2010$wgt, probs = c(0.5, 0.9, 0.99))[2]
wealth_share_top_10per_income[8] = sum(scf_2010$income[top10_2010] * scf_2010$wgt[top10_2010]) / sum(scf_2010$income * scf_2010$wgt)
top10_2013 <- scf_2013$income >= wtd.quantile(scf_2013$income, weights = scf_2013$wgt, probs = c(0.5, 0.9, 0.99))[2]
wealth_share_top_10per_income[9] = sum(scf_2013$income[top10_2013] * scf_2013$wgt[top10_2013]) / sum(scf_2013$income * scf_2013$wgt)
top10_2016 <- scf_2016$income >= wtd.quantile(scf_2016$income, weights = scf_2016$wgt, probs = c(0.5, 0.9, 0.99))[2]
wealth_share_top_10per_income[10] = sum(scf_2016$income[top10_2016] * scf_2016$wgt[top10_2016]) / sum(scf_2016$income * scf_2016$wgt)
top10_2019 <- scf_2019$income >= wtd.quantile(scf_2019$income, weights = scf_2019$wgt, probs = c(0.5, 0.9, 0.99))[2]
wealth_share_top_10per_income[11] = sum(scf_2019$income[top10_2019] * scf_2019$wgt[top10_2019]) / sum(scf_2019$income * scf_2019$wgt)

# wageinc
wealth_share_top_10per_wageinc = rep(0,11)
top10_1989 <- scf_1989$wageinc >= wtd.quantile(scf_1989$wageinc, weights = scf_1989$wgt, probs = c(0.5, 0.9, 0.99))[2]
wealth_share_top_10per_wageinc[1] = sum(scf_1989$wageinc[top10_1989] * scf_1989$wgt[top10_1989]) / sum(scf_1989$wageinc * scf_1989$wgt)
top10_1992 <- scf_1992$wageinc >= wtd.quantile(scf_1992$wageinc, weights = scf_1992$wgt, probs = c(0.5, 0.9, 0.99))[2]
wealth_share_top_10per_wageinc[2] = sum(scf_1992$wageinc[top10_1992] * scf_1992$wgt[top10_1992]) / sum(scf_1992$wageinc * scf_1992$wgt)
top10_1995 <- scf_1995$wageinc >= wtd.quantile(scf_1995$wageinc, weights = scf_1995$wgt, probs = c(0.5, 0.9, 0.99))[2]
wealth_share_top_10per_wageinc[3] = sum(scf_1995$wageinc[top10_1995] * scf_1995$wgt[top10_1995]) / sum(scf_1995$wageinc * scf_1995$wgt)
top10_1998 <- scf_1998$wageinc >= wtd.quantile(scf_1998$wageinc, weights = scf_1998$wgt, probs = c(0.5, 0.9, 0.99))[2]
wealth_share_top_10per_wageinc[4] = sum(scf_1998$wageinc[top10_1998] * scf_1998$wgt[top10_1998]) / sum(scf_1998$wageinc * scf_1998$wgt)
top10_2001 <- scf_2001$wageinc >= wtd.quantile(scf_2001$wageinc, weights = scf_2001$wgt, probs = c(0.5, 0.9, 0.99))[2]
wealth_share_top_10per_wageinc[5] = sum(scf_2001$wageinc[top10_2001] * scf_2001$wgt[top10_2001]) / sum(scf_2001$wageinc * scf_2001$wgt)
top10_2004 <- scf_2004$wageinc >= wtd.quantile(scf_2004$wageinc, weights = scf_2004$wgt, probs = c(0.5, 0.9, 0.99))[2]
wealth_share_top_10per_wageinc[6] = sum(scf_2004$wageinc[top10_2004] * scf_2004$wgt[top10_2004]) / sum(scf_2004$wageinc * scf_2004$wgt)
top10_2007 <- scf_2007$wageinc >= wtd.quantile(scf_2007$wageinc, weights = scf_2007$wgt, probs = c(0.5, 0.9, 0.99))[2]
wealth_share_top_10per_wageinc[7] = sum(scf_2007$wageinc[top10_2007] * scf_2007$wgt[top10_2007]) / sum(scf_2007$wageinc * scf_2007$wgt)
top10_2010 <- scf_2010$wageinc >= wtd.quantile(scf_2010$wageinc, weights = scf_2010$wgt, probs = c(0.5, 0.9, 0.99))[2]
wealth_share_top_10per_wageinc[8] = sum(scf_2010$wageinc[top10_2010] * scf_2010$wgt[top10_2010]) / sum(scf_2010$wageinc * scf_2010$wgt)
top10_2013 <- scf_2013$wageinc >= wtd.quantile(scf_2013$wageinc, weights = scf_2013$wgt, probs = c(0.5, 0.9, 0.99))[2]
wealth_share_top_10per_wageinc[9] = sum(scf_2013$wageinc[top10_2013] * scf_2013$wgt[top10_2013]) / sum(scf_2013$wageinc * scf_2013$wgt)
top10_2016 <- scf_2016$wageinc >= wtd.quantile(scf_2016$wageinc, weights = scf_2016$wgt, probs = c(0.5, 0.9, 0.99))[2]
wealth_share_top_10per_wageinc[10] = sum(scf_2016$wageinc[top10_2016] * scf_2016$wgt[top10_2016]) / sum(scf_2016$wageinc * scf_2016$wgt)
top10_2019 <- scf_2019$wageinc >= wtd.quantile(scf_2019$wageinc, weights = scf_2019$wgt, probs = c(0.5, 0.9, 0.99))[2]
wealth_share_top_10per_wageinc[11] = sum(scf_2019$wageinc[top10_2019] * scf_2019$wgt[top10_2019]) / sum(scf_2019$wageinc * scf_2019$wgt)

# Plot
plot(x = years,
     y = wealth_share_top_10per_networth, ylab = "Wealth Share of Top 10%", ylim = c(0.1,0.9),
     main = "Wealth Share of Top 10% by Years",
     type = "l",
     col = "red3",
     lwd = 3)
lines(x = years,
      y = wealth_share_top_10per_income,
      type = "l",
      col = "dodgerblue",
      lwd = 3)
lines(x = years,
      y = wealth_share_top_10per_wageinc,
      type = "l",
      col = "darkgreen",
      lwd = 3)
legend("topleft",
       c("networth", "income", "wageinc"),
       pch = c("l", "l", "l"),
       col = c("red3", "dodgerblue", "darkgreen"),
       bg = "white")

##### (g) wealth share of top 1% #####

# networth
wealth_share_top_1per_networth = rep(0,11)
top1_1989 <- scf_1989$networth >= wtd.quantile(scf_1989$networth, weights = scf_1989$wgt, probs = c(0.5, 0.9, 0.99))[3]
wealth_share_top_1per_networth[1] = sum(scf_1989$networth[top1_1989] * scf_1989$wgt[top1_1989]) / sum(scf_1989$networth * scf_1989$wgt)
top1_1992 <- scf_1992$networth >= wtd.quantile(scf_1992$networth, weights = scf_1992$wgt, probs = c(0.5, 0.9, 0.99))[3]
wealth_share_top_1per_networth[2] = sum(scf_1992$networth[top1_1992] * scf_1992$wgt[top1_1992]) / sum(scf_1992$networth * scf_1992$wgt)
top1_1995 <- scf_1995$networth >= wtd.quantile(scf_1995$networth, weights = scf_1995$wgt, probs = c(0.5, 0.9, 0.99))[3]
wealth_share_top_1per_networth[3] = sum(scf_1995$networth[top1_1995] * scf_1995$wgt[top1_1995]) / sum(scf_1995$networth * scf_1995$wgt)
top1_1998 <- scf_1998$networth >= wtd.quantile(scf_1998$networth, weights = scf_1998$wgt, probs = c(0.5, 0.9, 0.99))[3]
wealth_share_top_1per_networth[4] = sum(scf_1998$networth[top1_1998] * scf_1998$wgt[top1_1998]) / sum(scf_1998$networth * scf_1998$wgt)
top1_2001 <- scf_2001$networth >= wtd.quantile(scf_2001$networth, weights = scf_2001$wgt, probs = c(0.5, 0.9, 0.99))[3]
wealth_share_top_1per_networth[5] = sum(scf_2001$networth[top1_2001] * scf_2001$wgt[top1_2001]) / sum(scf_2001$networth * scf_2001$wgt)
top1_2004 <- scf_2004$networth >= wtd.quantile(scf_2004$networth, weights = scf_2004$wgt, probs = c(0.5, 0.9, 0.99))[3]
wealth_share_top_1per_networth[6] = sum(scf_2004$networth[top1_2004] * scf_2004$wgt[top1_2004]) / sum(scf_2004$networth * scf_2004$wgt)
top1_2007 <- scf_2007$networth >= wtd.quantile(scf_2007$networth, weights = scf_2007$wgt, probs = c(0.5, 0.9, 0.99))[3]
wealth_share_top_1per_networth[7] = sum(scf_2007$networth[top1_2007] * scf_2007$wgt[top1_2007]) / sum(scf_2007$networth * scf_2007$wgt)
top1_2010 <- scf_2010$networth >= wtd.quantile(scf_2010$networth, weights = scf_2010$wgt, probs = c(0.5, 0.9, 0.99))[3]
wealth_share_top_1per_networth[8] = sum(scf_2010$networth[top1_2010] * scf_2010$wgt[top1_2010]) / sum(scf_2010$networth * scf_2010$wgt)
top1_2013 <- scf_2013$networth >= wtd.quantile(scf_2013$networth, weights = scf_2013$wgt, probs = c(0.5, 0.9, 0.99))[3]
wealth_share_top_1per_networth[9] = sum(scf_2013$networth[top1_2013] * scf_2013$wgt[top1_2013]) / sum(scf_2013$networth * scf_2013$wgt)
top1_2016 <- scf_2016$networth >= wtd.quantile(scf_2016$networth, weights = scf_2016$wgt, probs = c(0.5, 0.9, 0.99))[3]
wealth_share_top_1per_networth[10] = sum(scf_2016$networth[top1_2016] * scf_2016$wgt[top1_2016]) / sum(scf_2016$networth * scf_2016$wgt)
top1_2019 <- scf_2019$networth >= wtd.quantile(scf_2019$networth, weights = scf_2019$wgt, probs = c(0.5, 0.9, 0.99))[3]
wealth_share_top_1per_networth[11] = sum(scf_2019$networth[top1_2019] * scf_2019$wgt[top1_2019]) / sum(scf_2019$networth * scf_2019$wgt)

# income
wealth_share_top_1per_income = rep(0,11)
top1_1989 <- scf_1989$income >= wtd.quantile(scf_1989$income, weights = scf_1989$wgt, probs = c(0.5, 0.9, 0.99))[3]
wealth_share_top_1per_income[1] = sum(scf_1989$income[top1_1989] * scf_1989$wgt[top1_1989]) / sum(scf_1989$income * scf_1989$wgt)
top1_1992 <- scf_1992$income >= wtd.quantile(scf_1992$income, weights = scf_1992$wgt, probs = c(0.5, 0.9, 0.99))[3]
wealth_share_top_1per_income[2] = sum(scf_1992$income[top1_1992] * scf_1992$wgt[top1_1992]) / sum(scf_1992$income * scf_1992$wgt)
top1_1995 <- scf_1995$income >= wtd.quantile(scf_1995$income, weights = scf_1995$wgt, probs = c(0.5, 0.9, 0.99))[3]
wealth_share_top_1per_income[3] = sum(scf_1995$income[top1_1995] * scf_1995$wgt[top1_1995]) / sum(scf_1995$income * scf_1995$wgt)
top1_1998 <- scf_1998$income >= wtd.quantile(scf_1998$income, weights = scf_1998$wgt, probs = c(0.5, 0.9, 0.99))[3]
wealth_share_top_1per_income[4] = sum(scf_1998$income[top1_1998] * scf_1998$wgt[top1_1998]) / sum(scf_1998$income * scf_1998$wgt)
top1_2001 <- scf_2001$income >= wtd.quantile(scf_2001$income, weights = scf_2001$wgt, probs = c(0.5, 0.9, 0.99))[3]
wealth_share_top_1per_income[5] = sum(scf_2001$income[top1_2001] * scf_2001$wgt[top1_2001]) / sum(scf_2001$income * scf_2001$wgt)
top1_2004 <- scf_2004$income >= wtd.quantile(scf_2004$income, weights = scf_2004$wgt, probs = c(0.5, 0.9, 0.99))[3]
wealth_share_top_1per_income[6] = sum(scf_2004$income[top1_2004] * scf_2004$wgt[top1_2004]) / sum(scf_2004$income * scf_2004$wgt)
top1_2007 <- scf_2007$income >= wtd.quantile(scf_2007$income, weights = scf_2007$wgt, probs = c(0.5, 0.9, 0.99))[3]
wealth_share_top_1per_income[7] = sum(scf_2007$income[top1_2007] * scf_2007$wgt[top1_2007]) / sum(scf_2007$income * scf_2007$wgt)
top1_2010 <- scf_2010$income >= wtd.quantile(scf_2010$income, weights = scf_2010$wgt, probs = c(0.5, 0.9, 0.99))[3]
wealth_share_top_1per_income[8] = sum(scf_2010$income[top1_2010] * scf_2010$wgt[top1_2010]) / sum(scf_2010$income * scf_2010$wgt)
top1_2013 <- scf_2013$income >= wtd.quantile(scf_2013$income, weights = scf_2013$wgt, probs = c(0.5, 0.9, 0.99))[3]
wealth_share_top_1per_income[9] = sum(scf_2013$income[top1_2013] * scf_2013$wgt[top1_2013]) / sum(scf_2013$income * scf_2013$wgt)
top1_2016 <- scf_2016$income >= wtd.quantile(scf_2016$income, weights = scf_2016$wgt, probs = c(0.5, 0.9, 0.99))[3]
wealth_share_top_1per_income[10] = sum(scf_2016$income[top1_2016] * scf_2016$wgt[top1_2016]) / sum(scf_2016$income * scf_2016$wgt)
top1_2019 <- scf_2019$income >= wtd.quantile(scf_2019$income, weights = scf_2019$wgt, probs = c(0.5, 0.9, 0.99))[3]
wealth_share_top_1per_income[11] = sum(scf_2019$income[top1_2019] * scf_2019$wgt[top1_2019]) / sum(scf_2019$income * scf_2019$wgt)

# wageinc
wealth_share_top_1per_wageinc = rep(0,11)
top1_1989 <- scf_1989$wageinc >= wtd.quantile(scf_1989$wageinc, weights = scf_1989$wgt, probs = c(0.5, 0.9, 0.99))[3]
wealth_share_top_1per_wageinc[1] = sum(scf_1989$wageinc[top1_1989] * scf_1989$wgt[top1_1989]) / sum(scf_1989$wageinc * scf_1989$wgt)
top1_1992 <- scf_1992$wageinc >= wtd.quantile(scf_1992$wageinc, weights = scf_1992$wgt, probs = c(0.5, 0.9, 0.99))[3]
wealth_share_top_1per_wageinc[2] = sum(scf_1992$wageinc[top1_1992] * scf_1992$wgt[top1_1992]) / sum(scf_1992$wageinc * scf_1992$wgt)
top1_1995 <- scf_1995$wageinc >= wtd.quantile(scf_1995$wageinc, weights = scf_1995$wgt, probs = c(0.5, 0.9, 0.99))[3]
wealth_share_top_1per_wageinc[3] = sum(scf_1995$wageinc[top1_1995] * scf_1995$wgt[top1_1995]) / sum(scf_1995$wageinc * scf_1995$wgt)
top1_1998 <- scf_1998$wageinc >= wtd.quantile(scf_1998$wageinc, weights = scf_1998$wgt, probs = c(0.5, 0.9, 0.99))[3]
wealth_share_top_1per_wageinc[4] = sum(scf_1998$wageinc[top1_1998] * scf_1998$wgt[top1_1998]) / sum(scf_1998$wageinc * scf_1998$wgt)
top1_2001 <- scf_2001$wageinc >= wtd.quantile(scf_2001$wageinc, weights = scf_2001$wgt, probs = c(0.5, 0.9, 0.99))[3]
wealth_share_top_1per_wageinc[5] = sum(scf_2001$wageinc[top1_2001] * scf_2001$wgt[top1_2001]) / sum(scf_2001$wageinc * scf_2001$wgt)
top1_2004 <- scf_2004$wageinc >= wtd.quantile(scf_2004$wageinc, weights = scf_2004$wgt, probs = c(0.5, 0.9, 0.99))[3]
wealth_share_top_1per_wageinc[6] = sum(scf_2004$wageinc[top1_2004] * scf_2004$wgt[top1_2004]) / sum(scf_2004$wageinc * scf_2004$wgt)
top1_2007 <- scf_2007$wageinc >= wtd.quantile(scf_2007$wageinc, weights = scf_2007$wgt, probs = c(0.5, 0.9, 0.99))[3]
wealth_share_top_1per_wageinc[7] = sum(scf_2007$wageinc[top1_2007] * scf_2007$wgt[top1_2007]) / sum(scf_2007$wageinc * scf_2007$wgt)
top1_2010 <- scf_2010$wageinc >= wtd.quantile(scf_2010$wageinc, weights = scf_2010$wgt, probs = c(0.5, 0.9, 0.99))[3]
wealth_share_top_1per_wageinc[8] = sum(scf_2010$wageinc[top1_2010] * scf_2010$wgt[top1_2010]) / sum(scf_2010$wageinc * scf_2010$wgt)
top1_2013 <- scf_2013$wageinc >= wtd.quantile(scf_2013$wageinc, weights = scf_2013$wgt, probs = c(0.5, 0.9, 0.99))[3]
wealth_share_top_1per_wageinc[9] = sum(scf_2013$wageinc[top1_2013] * scf_2013$wgt[top1_2013]) / sum(scf_2013$wageinc * scf_2013$wgt)
top1_2016 <- scf_2016$wageinc >= wtd.quantile(scf_2016$wageinc, weights = scf_2016$wgt, probs = c(0.5, 0.9, 0.99))[3]
wealth_share_top_1per_wageinc[10] = sum(scf_2016$wageinc[top1_2016] * scf_2016$wgt[top1_2016]) / sum(scf_2016$wageinc * scf_2016$wgt)
top1_2019 <- scf_2019$wageinc >= wtd.quantile(scf_2019$wageinc, weights = scf_2019$wgt, probs = c(0.5, 0.9, 0.99))[3]
wealth_share_top_1per_wageinc[11] = sum(scf_2019$wageinc[top1_2019] * scf_2019$wgt[top1_2019]) / sum(scf_2019$wageinc * scf_2019$wgt)

# Plot
plot(x = years,
     y = wealth_share_top_1per_networth, ylab = "Wealth Share of Top 1%", ylim = c(0,0.5),
     main = "Wealth Share of Top 1% by Years",
     type = "l",
     col = "red3",
     lwd = 3)
lines(x = years,
      y = wealth_share_top_1per_income,
      type = "l",
      col = "dodgerblue",
      lwd = 3)
lines(x = years,
      y = wealth_share_top_1per_wageinc,
      type = "l",
      col = "darkgreen",
      lwd = 3)
legend("topleft",
       c("networth", "income", "wageinc"),
       pch = c("l", "l", "l"),
       col = c("red3", "dodgerblue", "darkgreen"),
       bg = "white")
