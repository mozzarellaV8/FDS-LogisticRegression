# Linear Discriminant Analysis
# Classification Models
# R in a Nutshell - Chapter 21

# load data -------------------------------------------------------------------

# without library(nutshell):
# library(MASS)
# spambase <- read.csv("data/spambase/spambase-data.txt", header = FALSE)
#
# this data is also available from the UCI machine learning repository
# but requires creating your own word vector of attributes

# or: just get the data from nutshell instead of UCI
# better than creating a wordvector out of all 58 values
library(nutshell)
data(spambase)

names(spambase)
summary(spambase)

# split data ----------------------------------------------------------------

install.packages("sampling")
library(sampling)

# split data 70/30 with $is_spam as stratifying factor
table(spambase$is_spam)
#      0    1 
#   2788 1813 

2788+1813 # 4601 - total observations of `is_spam`
.7*4601 # 3220 - 70% of total observations `is_spam`

1951/3220 # 0.60
1269/3220 # 0.40

1269+1951 # 3220

spambase_strata <- strata(spambase, stratanames = c("is_spam"), 
                          size = c(1269, 1951), method = "srswor")

names(spambase_strata)
# "is_spam" "ID_unit" "Prob"    "Stratum"
summary(spambase_strata)
# "ID_unit" refers to the row numbers in the sample. 
# to create training and test sets, extract observations that match/don't match
# "ID_unit" values in the stratified example.

# training set
spambase.training <- spambase[rownames(spambase) %in% spambase_strata$ID_unit,]

# test set
spambase.validation <- spambase[
  !(rownames(spambase) %in% spambase_strata$ID_unit),]

nrow(spambase.training)
# 3220

nrow(spambase.validation)
# 1381

3220+1381 
# 4601 - total number of observations
# we split the total obs into a 70-30 split, 
# and of the 70 split that into 60-40 for stratification using strata()

# Quadratic Discriminant Analysis ---------------------------------------------

# correlation test across all variables (other than is_spam) first
library(corrplot)
spamcor <- cor(spambase)

spamnum <- spambase
spamnum$capital_run_length_longest <- as.numeric(spamnum$capital_run_length_longest)
spamnum$capital_run_length_total <- as.numeric(spamnum$capital_run_length_total)
spamnum$is_spam <- NULL

spamcor <- cor(spamnum, use = "everything")

png(filename = "spamcor-02.png", width = 1200, height = 1200, units = "px")
corrplot(spamcor, method = "circle", tl.srt = 45, type = c("upper"))
dev.off()

png(filename = "spamcor-03.png", width = 1200, height = 1200, units = "px")
corrplot(spamcor, method = "shade", tl.srt = 45, type = c("upper"))
dev.off()



# Quadratic Discriminant Analysis -------------------------

spambase.training$capital_run_length_longest <- as.numeric(spambase.training$capital_run_length_longest)
spambase.training$capital_run_length_total <- as.numeric(spambase.training$capital_run_length_total)
spambase.training$is_spam <- as.numeric(spambase.training$is_spam)

spam.qda <- qda(formula=is_spam~., data = spambase.training)
# Error in qda.default(x, grouping, ...) : rank deficiency in group 1
# after converting 'is_spam' to numeric from factor:
# Error in qda.default(x, grouping, ...) : rank deficiency in group 2


# Quadratic Discriminant Analysis -------------------------
spambase.training$is_spam <- as.factor(spambase.training$is_spam)

spam.lda <- lda(formula = is_spam ~ . , data = spambase.training)
summary(spam.lda)
#          Length Class  Mode     
# prior     2    -none- numeric  
# counts    2    -none- numeric  
# means   114    -none- numeric  
# scaling  57    -none- numeric  
# lev       2    -none- character
# svd       1    -none- numeric  
# N         1    -none- numeric  
# call      3    -none- call     
# terms     3    terms  call     
# xlevels   0    -none- list  

prediction <- predict(spam.lda, newdata = spambase.training)
table(actual = spambase.training$is_spam, predicted = prediction$class)
#           predicted
# actual       1    2
#     1     1865   86
#     2      258 1011

n <- 1865 + 86 + 258 + 1011          # 3220
accuracy <- (1865+1011)/3220         # 0.893 - 89% not bad
error <- (258 + 86)/n                # 0.1067 - 11% ok
sensitivity <- 1865/(1865+86)        # 0.9559 - 96%
specificity <- 1011 / (1011 + 258)   # 0.796 - 80%
fn_error <- 86/(1865+86)             # 0.044 - 4% false negative error rate
fp_error <- 258/(1011+258)           # 0.203 - 20% false positive error rate

prediction02 <- predict(spam.lda, newdata = spambase.validation)
table(actual = spambase.validation$is_spam, predicted = prediction02$class)
#           predicted
#  actual       1   2
#      0      800  37
#      1      104 440

# Flexible Discriminant Analysis ----------------------------------------------

install.packages("mda")
library(mda)

spam.fda <- fda(formula = is_spam ~ . , data = spambase.training)
summary(spam.fda)
#                   Length Class   Mode   
# percent.explained 1      -none-  numeric
# values            1      -none-  numeric
# means             2      -none-  numeric
# theta.mod         1      -none-  numeric
# dimension         1      -none-  numeric
# prior             2      table   numeric
# fit               5      polyreg list   
# call              3      -none-  call   
# terms             3      terms   call   
# confusion         4      -none-  numeric

# make prediction with spam.fda model on spambase.validation data
fda.spam.pred <- predict(spam.fda, newdata = spambase.validation, type = "class")

# classification matrix
table(actual = spambase.validation$is_spam, 
      predicted = fda.spam.pred)
#            predicted
#   actual       1   2
#       0      800  37
#       1      104 440

# Mixture Discriminant Analysis -----------------------------------------------

spam.mda <- mda(formula = is_spam ~ . , data = spambase.training)
table(actual = spambase.validation$is_spam, 
      predicted = predict(spam.mda, newdata = spambase.validation))

#             predicted
#   actual       1   2
#        0     791  46
#        1      91 453

791+46+91+453 # 1381 total obs
(791+453)/1381 # 0.9007965 - 90% overall accuracy
(46 + 91)/1381 # 0.09920348 - 10% overall error
46/(791+46) # 0.05495818 - 5% false negative rate
91/(453+91) # 0.1672794  - 16% false positive rate


# ROCR

library(ROCR)

performance(prediction02, "acc", "err", "fpr", "fnr")





