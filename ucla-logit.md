# UCLA Logit Regression example
===============================

via the [Institute for Digital Research and Education, UCLA](http://www.ats.ucla.edu/stat/r/dae/logit.htm).

This example looks at graduate school admissions: whether or not an applicant is accepted based on GRE score, GPA, and Rank. The outcome variable is binary: admitted or not admitted. 

The required packages are `aod`, `ggplot2`, and `Rcpp`. I've included `broom` for tidy model outputs.

## The Data

```{r}
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
```

**_Notes on variables_**: 

`admit` is the binary outcome variable: whether or not someone got into grad school based on 3 predictors: `gre`, `gpa`, and `rank`. A rank of 1 carries the highest prestige, while a rank of 4 carries the lowest.

What is the standard deviation across variables?

```{R}
sapply(grad, sd)
#       admit         gre         gpa        rank 
#   0.4660867 115.5165364   0.3805668   0.9444602
```

To make sure there are not 0 cells, a two-contingency table is created.

```{r}
xtabs(~ admit + rank, data = grad)
#      rank
# admit  1  2  3  4
#     0 28 97 93 55
#     1 33 54 28 12
```

Analysis methods to consider:

- Logistic Regression
- Probit Regression
- OLS Regression
- Two-group Discriminant Function Analysis
- Hostellings T^2

## Logistic Regression Model

Moving forward with Logistic Regression: before modeling will convert the variable `rank` to a factor - it's an ordinal variable, even though represented by integers.

```{R}
# convert 'rank' to factor
grad$rank <- factor(grad$rank)
```

From here, construct a model using all the variables provided. Gotta love a ready-to-use, groomed dataset.

```{R}
# model
logit01 <- glm(admit ~ gre + gpa + rank, data = grad, family = "binomial")
summary(logit01)
# Deviance Residuals: 
#   Min       1Q   Median       3Q      Max  
# -1.6268  -0.8662  -0.6388   1.1490   2.0790  

# Coefficients:
#   			 Estimate Std. Error z value Pr(>|z|)    
# (Intercept) 	-3.989979   1.139951  -3.500 0.000465 ***
#   gre          0.002264   0.001094   2.070 0.038465 *  
#   gpa          0.804038   0.331819   2.423 0.015388 *  
#   rank2       -0.675443   0.316490  -2.134 0.032829 *  
#   rank3       -1.340204   0.345306  -3.881 0.000104 ***
#   rank4       -1.551464   0.417832  -3.713 0.000205 ***

# (Dispersion parameter for binomial family taken to be 1)

# Null deviance: 499.98  on 399  degrees of freedom
# Residual deviance: 458.52  on 394  degrees of freedom
# AIC: 470.52

# Number of Fisher Scoring iterations: 4
```

Although I've already got the model output above, will use `tidy()` from the `broom` package to get a cleaner look at output. It's generally clear but just to make the translation from base R to tidy output explicit:

- **Pr(>|z|)** is **p.value**
- **z value** is **statistic** and is sometimes called the **Wald z-statistic**
- **Std. Error** is **std.error**
- **Estimate** is **estimate**

What we lose in significance codes we gain in dataframe-ready output clarity: 

```{R}
tidy(logit01)
#          term     estimate   std.error statistic      p.value
# 1 (Intercept) -3.989979073 1.139950936 -3.500132 0.0004650273
# 2         gre  0.002264426 0.001093998  2.069864 0.0384651284
# 3         gpa  0.804037549 0.331819298  2.423119 0.0153878974
# 4       rank2 -0.675442928 0.316489661 -2.134171 0.0328288188
# 5       rank3 -1.340203916 0.345306418 -3.881202 0.0001039415
# 6       rank4 -1.551463677 0.417831633 -3.713131 0.0002047107
```

For every one unit increase in the predictor variables, the coefficients give the change in _log odds_ for the outcome. 

About the _log odds_: they're the _logit_, which is the _link function_ that relates the independent variables to the **binary** outcome variable. Being binary, the outcome variable should essentially follow a **Bernoullli distribution** - which is a special case of the Binomial distribution where _n_ = 1 (a single trial with success/failure, 0/1, admit/not admit).

Looking at our specific model here:

- for every one unit change in GRE score, the log odds of successful admission (1) increases by 0.002. This is derived from the coefficient **estimate**.
- for every one unit increase in `gpa`, the log odds of admission increase by 0.804 - again from the **estimate**
- `rank` is categorical, and the 3 terms output are to be interpreted against the `rank1`. So - if one attended a `rank2` university, the log odds of being admitted to grad school decrease by 0.675.

### Confidence Intervals

Using `confint()`, we can get confidence intervals for the coefficient estimates. These are based on the 'profiled log-likelihood function' (TODO: look this up). 

```{R}
confint(logit01)
# Waiting for profiling to be done...
#                       2.5 %       97.5 %
# (Intercept)   -6.2716202334 -1.792547080
# gre            0.0001375921  0.004435874
# gpa            0.1602959439  1.464142727
# rank2         -1.3008888002 -0.056745722
# rank3         -2.0276713127 -0.670372346
# rank4         -2.4000265384 -0.753542605

# CIs with standard errors
confint.default(logit01)
#                     2.5 %       97.5 %
# (Intercept) -6.2242418514 -1.755716295
# gre          0.0001202298  0.004408622
# gpa          0.1536836760  1.454391423
# rank2       -1.2957512650 -0.055134591
# rank3       -2.0169920597 -0.663415773
# rank4       -2.3703986294 -0.732528724
```














