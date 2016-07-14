# Framingham Heart Study
# Foundations of Data Science unit 7.2-2
# Logistic Regression

# load data -----------------------------------------------

framingham <- read.csv("data/lecture/framingham.csv")
summary(framingham)
str(framingham)

install.packages("caTools")
library(caTools)

set.seed(1000)
split <- sample.split(framingham$TenYearCHD, SplitRatio = 0.65)
# 65% in training set - with more data can do less in training
# and more in test. Typically 50-80% is desired.

# assign variables to the split sets:
train <- subset(framingham, split == TRUE)
test <- subset(framingham, split == FALSE)

# generalized linear model
# period as independent variable uses all other variables
# in the dataset as independent variables (instead listing all)
framinghamLog <- glm(TenYearCHD ~ ., data = train, family = binomial)
summary(framinghamLog)


# predictions using the framinghamLog model
predictTest <- predict(framinghamLog, type = "response",
                       newdata = test)
# confusion matrix
table(test$TenYearCHD, predictTest > 0.5)

#     FALSE TRUE
#   0  1069    6
#   1   187   11

overallAccuracy <- (1069 + 11)/(1069+6+187+11)
overallAccuracy 
# [1] 0.8483896

# compare to the baseline method/model
baseline <- (1069+6)/(1069+6+187+11)
baseline
# [1] 0.8444619

# Model barely beats the baseline.

install.packages("ROCR")
library(ROCR)

ROCRpred <- prediction(predictTest, test$TenYearCHD)
as.numeric(performance(ROCRpred, "auc")@y.values)
# [1] 0.7421095
# can differentiate between low-risk and high-risk patients well.s
