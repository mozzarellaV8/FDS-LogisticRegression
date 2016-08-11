# R in a Nutshell - Chapter 21
# Logistic Regression exercise

# load data -------------------------------------------------------------------

install.packages("nutshell")
library(nutshell)

data(field.goals)

# create new dataframe with new variable good (factor good/bad)
field.goals.lr <- transform(field.goals, 
                            good = as.factor(ifelse(play.type == "FG good", "good", "bad")))


# % of good field goals by distance
field.goals.table <- table(field.goals.lr$good, field.goals.lr$yards)
field.goals.table

# plot results as percentages
# colnames(field.goals.table) is the yardage, 
# y argument is formula for percentage calculation

# the baseline model
par(mar = c(8, 8, 8, 8), family = "HersheySans")
plot(colnames(field.goals.table), 
     field.goals.table["good", ] / (field.goals.table["bad", ] + field.goals.table["good", ]),
     xlab = "Distance (Yards)", ylab = "Percent Good",
     main = "Field Goal success % ~ Yardage")

# looks like a slight downhill linear relationship from 20 - 50 yards...
# drops right off at 55 yards to some outliers at 60

field.goals.mdl <- glm(good ~ yards, data = field.goals.lr, 
                       family = "binomial")

# my own look
coef(summary(field.goals.mdl))
fg_tab <- coef(summary(field.goals.mdl)) 
#                  Estimate  Std. Error   z value     Pr(>|z|)
#   (Intercept)  5.17885616 0.416201278 12.443153 1.523684e-35
#   yards       -0.09726075 0.009891874 -9.832389 8.165853e-23

# convert the log odds
fg_tab[, "Estimate"] <- exp(coef(field.goals.mdl))
fg_tab
#                  Estimate  Std. Error   z value     Pr(>|z|)
#   (Intercept) 177.4796874 0.416201278 12.443153 1.523684e-35
#   yards         0.9073194 0.009891874 -9.832389 8.165853e-23

summary(field.goals.mdl)
# AIC: 865.22

#### check fit of the model ---------------------------------------------------

plot(colnames(field.goals.table),
     field.goals.table["good", ] / (field.goals.table["bad", ] + field.goals.table["good", ]),
     xlab = "Distance (Yards)", ylab = "Percent Good",
     main = "Field Goal Conversion Rate ~ Yardage")

# create a function to calculate the probability
# eta is the line equation - eta = intercept + regression coeefficient * y
# second line is the logistic function

fg.prob <- function(y) {
  eta <- 5.178856 + -0.097261 * y;
  1 / (1 + exp(-eta))
}

# drawing plot line with function:
# x = total yards, y = probability/logistic function
lines(15:65, fg.prob(15:65), new = TRUE)

# decent looking fit.





