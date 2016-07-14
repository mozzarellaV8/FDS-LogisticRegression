# Logistic Regression Tutorial
# Regression with binary outcomes
# http://tutorials.iq.harvard.edu/R/Rstatistics/Rstatistics.html#orgheadline27

# lm() is great but only for continuous dependent variables;
# so it has some limitations.
# here we'll take a look at glm() - generalized linear models.

# load data -----------------------------------------------

NH11 <- readRDS("data/NatHealth2011.rds")
labs <- attributes(NH11)$labels

# Using this data, we'll predict (estimate) the probability 
# of being diagnosed with HYPERTENSION based on
# AGE, SEX, SLEEP, and BMI

str(NH11$hypev)
levels(NH11$hypev)
summary(NH11$hypev)

str(NH11$age_p)
summary(NH11$age_p)
#     Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#   18.00   33.00   47.00   48.11   62.00   85.00 

str(NH11$sex)
# 1 Male, 2 Female
summary(NH11$sex)
#   1 Male 2 Female 
#    14811    18203
14811/(14811+18203)
# 0.4486279
# 45% Male, 55% female

str(NH11$sleep)
summary(NH11$sleep) # measured in hours
#    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.
#   3.000   6.000   7.000   7.862   8.000  99.000

table(NH11$sleep) # number of ppl getting over 97 hours
# 97    98    99 
# 25    79   153

str(NH11$bmi)
summary(NH11$bmi)
#    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.
#   11.81   23.57   26.76   29.90   31.31   99.99

# Regression model ----------------------------------------

# convert "" to NA
NH11$hypev <- factor(NH11$hypev, levels = c("2 No", "1 Yes"))
summary(NH11$hypev)

# regression model
hyp_out <- glm(hypev ~ age_p + sex + sleep + bmi,
               data = NH11, family = "binomial")

#### Logistic Regression Coefficients

summary(hyp_out)
# it seems all the coeffiecients are significant:

# Coefficients:
#                  Estimate Std. Error z value Pr(>|z|)    
#   (Intercept) -4.2694660  0.0564947 -75.573  < 2e-16 ***
#   age_p        0.0606993  0.0008227  73.779  < 2e-16 ***
#   sex2 Female -0.1440251  0.0267977  -5.375 7.68e-08 ***
#   sleep       -0.0070358  0.0016397  -4.291 1.78e-05 ***
#   bmi          0.0185717  0.0009511  19.527  < 2e-16 ***

# AIC: 34245

coef(summary(hyp_out))
# (Intercept) -4.269466028 0.0564947294 -75.572820 0.000000e+00
# age_p        0.060699303 0.0008227207  73.778743 0.000000e+00
# sex2 Female -0.144025092 0.0267976605  -5.374540 7.677854e-08
# sleep       -0.007035776 0.0016397197  -4.290841 1.779981e-05
# bmi          0.018571704 0.0009510828  19.526906 6.485172e-85

# _Transform to interpret_
# glm() uses "link functions", so raw coefficients are harder
# to interpret. E.G.
# the age_p coefficient of 0.06 tells us that 
# for every one unit increase in age, the *log odds* of hypertension
# diagnosis increases by 0.06....Not used to thinking in logs.
# So!
## Transform the coefficients to make them easier to interpret.

hyp_out_tab <- coef(summary(hyp_out))
hyp_out_tab[, "Estimate"] <- exp(coef(hyp_out))
hyp_out_tab

#               Estimate   Std. Error    z value     Pr(>|z|)
# (Intercept) 0.01398925 0.0564947294 -75.572820 0.000000e+00
# age_p       1.06257935 0.0008227207  73.778743 0.000000e+00
# sex2 Female 0.86586602 0.0267976605  -5.374540 7.677854e-08
# sleep       0.99298892 0.0016397197  -4.290841 1.779981e-05
# bmi         1.01874523 0.0009510828  19.526906 6.485172e-85

# Now with a value of 1.06 in the age estimate...for every 1 year
# increase in age, the odds of hypertension diagnosis increases by 1.06?

# independent correlation test
cor(NH11$bmi, NH11$sex == "2 Female")
# 0.72
cor(NH11$age_p, NH11$hypev == "1 Yes")

#### Generating Predicted Values 

##   In addition to transforming the log-odds produced by `glm' to odds, we
##   can use the `predict()' function to make direct statements about the
##   predictors in our model. For example, we can ask "How much more likely
##   is a 63 year old female to have hypertension compared to a 33 year old
##   female?".


# create a dataset with predictors set at desired levels
predData <- with(NH11,
                 expand.grid(age_p = c(33, 63),
                             sex = "2 Female",
                             bmi = mean(bmi, na.rm = TRUE),
                             sleep = mean(sleep, na.rm = TRUE)))

# predict hypertension at those levels

cbind(predData, predict(hyp_out, type = "response",
                        se.fit = TRUE, interval = "confidence",
                        newdata = predData))

#   age_p      sex      bmi   sleep       fit      se.fit
# 1    33 2 Female 29.89565 7.86221 0.1289227 0.002849622
# 2    63 2 Female 29.89565 7.86221 0.4776303 0.004816059

# Looking at th e `fit` variable, this shows that a 33 year old female
# has a 13% probability of having beeen diagnosed with hypertension,
# while a 63 year old female has a 48% probability of such.

#### Zelig package for computing+graphing predicted values

library(effects)
plot(allEffects(hyp_out))

