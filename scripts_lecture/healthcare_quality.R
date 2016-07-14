# Logistic Regression
# Foundations Unit 7.2.1
# Healthcare example

# load data -----------------------------------------------

quality <- read.csv("data/lecture/quality.csv")
str(quality)

# final variable is Poor Care: our outcome variable. 

table(quality$PoorCare)
#  0  1 
# 98 33

# 0 : good care
# 1 : poor care
# 33 received poor care.

# baseline method: predict most frequent outcome.
98/131
# 0.7480916

# baseline model has accuracy of 75%. See if we can do better 
# with an actual model. 

# split the data into training set and test set. 

install.packages("caTools")
library(caTools)

# set seed is to keep random number generator the same
set.seed(88)

# sample.split randomly samples the data and 
# splits into training and test sets
split <- sample.split(quality$PoorCare, SplitRatio = 0.75)
split
# TRUE should be in training set, FALSE should be in test set.

qualityTrain <- subset(quality, split == TRUE)
qualityTest <- subset(quality, split == FALSE)

# Build a Logistic Regression model -----------------------
# Dependent Variable ~ Independent Variable
# Office Visits and Narcotics as independent variables

QualityLog <- glm(PoorCare ~ OfficeVisits + Narcotics, 
                  data = qualityTrain, family = binomial)
summary(QualityLog)

# Coefficients: 
#  Estimate Std. Error z value Pr(>|z|)    
#   (Intercept)  -2.64613    0.52357  -5.054 4.33e-07 ***
#   OfficeVisits  0.08212    0.03055   2.688  0.00718 ** 
#   Narcotics     0.07630    0.03205   2.381  0.01728 *  

# AIC: 95.127

# make prediction -----------------------------------------

# "response" give probabilities
predictTrain <- predict(QualityLog, type = "response")
summary(predictTrain)
#    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.06623 0.11910 0.15970 0.25250 0.26760 0.98460 

# tapply to see if we are predicting higher probabilities
# for the PoorCare cases. 
# i.e.
# compute avg prediction for each TRUE outcome.
tapply(predictTrain, qualityTrain$PoorCare, mean)
#         0         1 
# 0.1894512 0.4392246 

# predicting higher probability for PoorCare cases - good.

# Confusion Matrices w/ Different Thresholds --------------

table(qualityTrain$PoorCare, predictTrain > 0.5)
# returns TRUE is prediction is above 0.5

#   FALSE TRUE
# 0    70    4
# 1    15   10

# above is the confusion matrix.

# compute sensitivity:
10/25 # TP rate: true positives / total positive cases
# [1] 0.4

# specificity:
70/74 # TN rate: true negatives / total negative cases
# [1] 0.9459459

# change the threshold ('t')
table(qualityTrain$PoorCare, predictTrain > 0.7)
#     FALSE TRUE
#   0    73    1
#   1    17    8

# sensitivity (t=0.7)
8/25
# [1] 0.32 ----- lower with higher sensitivity

# specificity (t = 0.7)
73/74
# [1] 0.9864865 ----- much higher with higher sensitivity

table(qualityTrain$PoorCare, predictTrain > 0.2)
#     FALSE TRUE
#   0    54   20
#   1     9   16

# sensitivity (t = 0.2)
16/25
# [1] 0.64 ----- higher with lower t

# specificity (t = 0.2)
54/74
# [1] 0.7297297 ----- lower with lower t

# ROC Curves (Receiver Operator Characteristic) -----------

install.packages("ROCR")
library(ROCR)

ROCRpredict <- prediction(predictTrain, qualityTrain$PoorCare)
ROCRperform <- performance(ROCRpredict, "tpr", "fpr")

par(mar = c(6, 6, 6, 6))
plot(ROCRperform, asp = 1, colorize = TRUE, 
     print.cutoffs.at = seq(0, 1, 0.1),
     text.adj = c(-0.2, 1.7))

# Making Predictions --------------------------------------
# out of sample data

predictTest <- predict(QualityLog, type = "response",
                       newdata = qualityTest)

table(qualityTest$PoorCare, predictTest > 0.3)
#   FALSE TRUE
# 0    19    5
# 1     2    6

