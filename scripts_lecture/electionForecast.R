# Election Forecasting - Logistic Regression
# Foundations unit 7.2-3
# https://www.youtube.com/watch?v=ctJjiuybqdk

# load data -----------------------------------------------

polling <- read.csv("data/lecture/PollingData.csv")
str(polling)

# missing data
table(polling$Year)
#   2004 2008 2012 
#     50   50   45 

# 2012 pollsters felt so confident in 5 states they didn't
# include them in their polls (hence 45 in 2012)

summary(polling)
# Rasmussen and SurveyUSA have a lot of NA's

# multiple imputation -------------------------------------

install.packages("mice")
library(mice)

# doesn't include Republican, State, Year
simple <- as.data.frame(polling, 
                        Rasmussen = polling$Rasmussen,
                        SurveyUSA = polling$SurveyUSA,
                        PropR = polling$PropR,
                        DiffCount = polling$DiffCount)
simple$Republican <- NULL
simple$State <- NULL
simple$Year <- NULL
summary(simple)

# set random seed for reproducible results
set.seed(144)
imputed <- complete(mice(simple))
# iter 5 - 5 rounds of imputation have been run

summary(imputed)
# no more NA's
# now to copy imputed values back into original data frame

polling$Rasmussen <- imputed$Rasmussen
polling$SurveyUSA <- imputed$SurveyUSA
summary(polling)
# voila! no more NA's. But is this a good idea?
# look at Gelman's articles again.

# split into Training and Test sets -----------------------
 
train <- subset(polling, Year == 2004 | Year == 2008)
test <- subset(polling, Year == 2012)

# naive baseline model
table(train$Republican)
#    0  1 
#   47 53

# look at documentation of sign
# for more sophisticated baseline model
sign(20)
sign(-10)
sign(0)

# sophisticated baseline model
table(sign(train$Rasmussen))
# -1  0  1 
# 42  2 56 
# 42 Dem, 56 Rep, 2 inconclusive

table(train$Republican, sign(train$Rasmussen))
#     -1  0  1
#   0 42  1  4
#   1  0  1 52
# confusion matrix

# Multicollinearity Test

cor(train[c("Rasmussen", "SurveyUSA", "PropR", "DiffCount", "Republican")])
#            Rasmussen SurveyUSA     PropR DiffCount Republican
# Rasmussen  1.0000000 0.9034447 0.8392505 0.5214783  0.7960537
# SurveyUSA  0.9034447 1.0000000 0.8868998 0.5737407  0.8270355
# PropR      0.8392505 0.8868998 1.0000000 0.8273785  0.9484204
# DiffCount  0.5214783 0.5737407 0.8273785 1.0000000  0.8092777
# Republican 0.7960537 0.8270355 0.9484204 0.8092777  1.0000000

# high correlation between Rasmussen and SurveyUSA
# might be better to do a single variable model. 
# so which model? PropR is highly correlated to Republican

model01 <- glm(Republican ~ PropR, data = train, family = binomial)
summary(model01)
# Coefficients:
#                   Estimate Std. Error z value Pr(>|z|)    
#       (Intercept)   -6.146      1.977  -3.108 0.001882 ** 
#       PropR         11.390      3.153   3.613 0.000303 ***
# AIC: 19.772

# seems like a reasonable model

# prediction on the training data
predict01 <- predict(model01, type = "response")
table(train$Republican, predict01 >= 0.5)
#     FALSE TRUE
#   0    45    2
#   1     2   51
# this model makes 4 mistakes; same as sophisticated baseline
# to improve: go back to correlation table, and look for
# another variable that has less correlation to 'help out'
# getting another highly correlated one could result in multicollinearity.

model02 <- glm(Republican ~ DiffCount + SurveyUSA, data = train, family = binomial)
predict02 <- predict(model02, type = "response")
table(train$Republican, predict02 >= 0.5)
#     FALSE TRUE
#   0    45    2
#   1     1   52
# one less mistake now

summary(model02)
# Coefficients:
#              Estimate Std. Error z value Pr(>|z|)  
#  (Intercept) -0.70017    0.91857  -0.762   0.4459  
#  DiffCount    0.72115    0.35201   2.049   0.0405 *
#  SurveyUSA    0.16734    0.08445   1.981   0.0475 *
# AIC: 21.718

# less significance on each of the independent variables. 
# we will still use the 2-variable model when making predictions
# on the test set - probably because it made less mistakes.

# Evaluate Models on Test Sets ----------------------------

# set up sohpisticated baseline model
table(test$Republican, sign(test$Rasmussen))
#     -1  0  1
#   0 18  2  4
#   1  0  0 21
# 4 mistakes, 2 inconclusive
# use this to compare our model against.

testpredict <- predict(model02, newdata = test, type = "response")
table(test$Republican, testpredict >= 0.5)
#     FALSE TRUE
#   0    23    1
#   1     0   21
# Only one bad outcome - we don't need ROC and are ok with 0.5 cutoff. 
# Why did the one mistake happen?

# look at the mistake
subset(test, testpredict >= 0.5 & Republican == 0)
#      State Year Rasmussen SurveyUSA DiffCount     PropR Republican
# 24 Florida 2012         2         0         6 0.6666667          0


