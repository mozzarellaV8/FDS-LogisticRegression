## Exercise: logistic regression
## ───────────────────────────────────

##   Use the NH11 data set that we loaded earlier.

##   1. Use glm to conduct a logistic regression to predict ever worked
##      (everwrk) using age (age_p) and marital status (r_maritl).
##   2. Predict the probability of working for each level of marital
##      status.

##   Note that the data is not perfectly clean and ready to be modeled. You
##   will need to clean up at least some of the variables before fitting
##   the model.


# load data -------------------------------------------------------------------

NH11 <- readRDS("data/NatHealth2011.rds")
labs <- attributes(NH11)$labels
str(NH11)

# clean factors ---------------------------------------------------------------

library(plyr)
library(tidyr)

summary(NH11$age_p)

summary(NH11$everwrk)
NH11$everwrk <- factor(NH11$everwrk, levels = c("2 No", "1 Yes"))
summary(NH11$everwrk)

summary(NH11$r_maritl)
levels(NH11$r_maritl)

# regression model ------------------------------------------------------------

# can we predict if someone has ever worked given their age and marital status? 
# and what is the probability for working for each level of marital status?
everwork.out <- glm(everwrk ~ age_p + r_maritl, data = NH11, family = "binomial")
coef(summary(everwork.out))

# It appears that age_p, Widowed, Divorced, and Never Married are the strongest predictors 
# for `everwrk`. Living with Partner is also strong.  the chances of someone never having 
# worked increase if they are in a marriage and living with their spouse?

#                                                Estimate  Std. Error    z value     Pr(>|z|)
# (Intercept)                                  0.44024757 0.093537691  4.7066328 2.518419e-06
# age_p                                        0.02981220 0.001645433 18.1181481 2.291800e-73
# r_maritl2 Married - spouse not in household -0.04967549 0.217309587 -0.2285932 8.191851e-01
# r_maritl4 Widowed                           -0.68361771 0.084335382 -8.1059419 5.233844e-16
# r_maritl5 Divorced                           0.73011485 0.111680788  6.5375152 6.254929e-11
# r_maritl6 Separated                          0.12809081 0.151366140  0.8462316 3.974236e-01
# r_maritl7 Never married                     -0.34361068 0.069222260 -4.9638756 6.910023e-07
# r_maritl8 Living with partner                0.44358296 0.137769623  3.2197443 1.283050e-03
# r_maritl9 Unknown marital status            -0.39547953 0.492966577 -0.8022441 4.224118e-01

work.out.table <- coef(summary(everwork.out)) # new table for log transformed values.
work.out.table[, "Estimate"] <- exp(coef(everwork.out)) # apply log transform to read coef.
work.out.table

# (Intercept)                                 1.5530917 0.093537691  4.7066328 2.518419e-06
# age_p                                       1.0302610 0.001645433 18.1181481 2.291800e-73
# r_maritl2 Married - spouse not in household 0.9515382 0.217309587 -0.2285932 8.191851e-01
# r_maritl4 Widowed                           0.5047875 0.084335382 -8.1059419 5.233844e-16
# r_maritl5 Divorced                          2.0753189 0.111680788  6.5375152 6.254929e-11
# r_maritl6 Separated                         1.1366562 0.151366140  0.8462316 3.974236e-01
# r_maritl7 Never married                     0.7092050 0.069222260 -4.9638756 6.910023e-07
# r_maritl8 Living with partner               1.5582805 0.137769623  3.2197443 1.283050e-03
# r_maritl9 Unknown marital status            0.6733571 0.492966577 -0.8022441 4.224118e-01

# predict the probability for working at each level of marital status ---------

nh11.wrk.age.mar <- subset(NH11, select = c("everwrk", "age_p", "r_maritl"))
summary(nh11.wrk.age.mar)

# transform everworked into binary and drop levels on marital status.
# droplevels() usually used for subsets - so we don't include levels 
# from the population that aren't within the subset.
NH11 <- transform(NH11, everwrk = factor(everwrk, levels = c("1 Yes", "2 No")),
                  r_maritl = droplevels(r_maritl))

mod.wk.age.mar <- glm(everwrk ~ age_p + r_maritl, data = NH11,
                      family = "binomial")

summary(mod.wk.age.mar)
mod.wk.age.mar_TBL <- coef(summary(mod.wk.age.mar))
mod.wk.age.mar_TBL[, "Estimate"] <- exp(coef(mod.wk.age.mar))
mod.wk.age.mar_TBL

#                                             Estimate  Std. Error    z value   Pr(>|z|)
# (Intercept)                                 0.6438770 0.093537691  -4.7066328 2.518419e-06
# age_p                                       0.9706278 0.001645433 -18.1181481 2.291800e-73
# r_maritl2 Married - spouse not in household 1.0509300 0.217309587   0.2285932 8.191851e-01
# r_maritl4 Widowed                           1.9810316 0.084335382   8.1059419 5.233844e-16
# r_maritl5 Divorced                          0.4818536 0.111680788  -6.5375152 6.254929e-11
# r_maritl6 Separated                         0.8797735 0.151366140  -0.8462316 3.974236e-01
# r_maritl7 Never married                     1.4100296 0.069222260   4.9638756 6.910023e-07
# r_maritl8 Living with partner               0.6417330 0.137769623  -3.2197443 1.283050e-03
# r_maritl9 Unknown marital status            1.4850962 0.492966577   0.8022441 4.224118e-01

mod.wk.age.mar_TBL <- as.data.frame(mod.wk.age.mar_TBL)
mod.wk.age.mar_TBL <- mod.wk.age.mar_TBL[order(sort(mod.wk.age.mar_TBL$Estimate, decreasing = T)),]
mod.wk.age.mar_TBL

# prediction for levels of marital status -------------------------------------

library(effects)
predMar <- data.frame(Effect("r_maritl", mod.wk.age.mar))
plot(allEffects(mod.wk.age.mar))


library(boot)
MSE_10Fold <- cv.glm(NH11, mod.wk.age.mar, K = 10)$delta[1]
