# UCLA Logit Regression example
# http://www.ats.ucla.edu/stat/r/dae/logit.htm

# load data -------------------------------------------------------------------

library(aod)
library(ggplot2)
library(Rcpp)

# data downloaded from http://www.ats.ucla.edu/stat/data/binary.csv

grad <- read.csv("data/binary.csv")
head(grad)
#   admit gre  gpa rank
# 1     0 380 3.61    3
# 2     1 660 3.67    3
# 3     1 800 4.00    1

summary(grad)
# `admit` is the binary outcome variable: whether or not someone got into 
# grad school based on 3 predictors: `gre`, `gpa`, and `rank`.
# rank 1 is the highest prestige; rank 4 is the lowest.

#### look at standard deviation across variables
sapply(grad, sd)
#       admit         gre         gpa        rank 
#   0.4660867 115.5165364   0.3805668   0.9444602

#### 2-way contingency table:
#### categorical outcome and predictors
xtabs(~ admit + rank, data = grad)
#      rank
# admit  1  2  3  4
#     0 28 97 93 55
#     1 33 54 28 12

# Methods to consider:
# Logistic Regression
# Probit Regression
# OLS Regression
# Two-group Discriminant Function Analysis
# Hostellings T^2

# Logit Model -----------------------------------------------------------------

# convert 'rank' to factor
grad$rank <- factor(grad$rank)

# model
logit01 <- glm(admit ~ gre + gpa + rank, data = grad, family = "binomial")
summary(logit01)




