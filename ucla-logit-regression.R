# UCLA Logit Regression example
# http://www.ats.ucla.edu/stat/r/dae/logit.htm

# load data -------------------------------------------------------------------

library(aod)
library(ggplot2)
library(Rcpp)
library(broom)

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
logit <- glm(admit ~ gre + gpa + rank, data = grad, family = "binomial")
summary(logit)
tidy(logit)

# calculate confidence intervals

# CIs via log-likelihood
confint(logit)
# CIs with standard errors
confint.default(logit)

# Wald Test
wald.test(b = coef(logit), Sigma = vcov(logit), Terms = 4:6)

# Wald Test for rank = 2 and rank = 3
one <- cbind(0, 0, 0, 1, -1, 0)
wald.test(b = coef(logit), Sigma = vcov(logit), L = one)

# odds ratios
exp(coef(logit))

# odds ratios with 95% CI
exp(cbind(odds = coef(logit), confint(logit)))

# Predictions -----------------------------------------------------------------

# generate new data based on means
newdata01 <- with(grad,
                data.frame(gre = mean(gre), gpa = mean(gpa), rank = factor(1:4)))

# take a look at new data
newdata01

# generate and add predicted values to dataframe
newdata01$rankP <- predict(logit, newdata = newdata01, type = "response")

# look at predicted probabilities including GRE score
newdata02 <- with(grad,
                  data.frame(gre = rep(seq(200, 800, length.out = 100), 4),
                             gpa = mean(gpa), 
                             rank = factor(rep(1:4, each = 100))))

newdata02

# combine predicted values with probabilities, lower and upper limits
newdata03 <- cbind(newdata02, 
                   predict(logit, newdata = newdata02, type = "link", se = T))

newdata03 <- within(newdata03, {
  PredictedProb <- plogis(fit)
  LL <- plogis(fit - (1.96 * se.fit))
  UL <- plogis(fit + (1.96 * se.fit))
})

head(newdata03)
newdata03[32:40, ]

# plot predicted probabilities ~ rank + gre score
pp <- ggplot(newdata03, aes(gre, PredictedProb)) +
  geom_ribbon(aes(ymin = LL, ymax = UL, fill = rank), alpha = 0.25) +
  geom_line(aes(color = rank), size = 1) +
  theme_minimal(base_size = 12, base_family = "GillSans") +
  theme(axis.title.y = element_text(margin = margin(0, 20, 0, 0)),
        axis.title.x = element_text(margin = margin(20, 0, 0, 0)),
        plot.margin = unit(c(1, 1, 1, 1), "cm")) +
  labs(y = "predicted probabilities", x = "GRE score",
       title = "Predicted Probabilities of Graduate School Acceptance ~ GRE score + school rank")

pp
