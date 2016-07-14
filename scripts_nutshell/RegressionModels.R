# R in a Nutshell - Chapter 20
# Regression Models
# lm(), lqs(), and rlm() comparison

# least squares, resistant regression (least median/least trimmed), and
# robust regression.

library(nutshell)
data(shiller)

library(MASS)
library(plyr)

shiller.index <- rename(shiller.index, c("Year" = "year",
                                         "Real.Home.Price.Index" = "index"))

shiller <- shiller.index

hpi.lm <- lm(index ~ year, data = shiller)
hpi.rlm <- rlm(index ~ year, data = shiller)
hpi.lqs <- lqs(index ~ year, data = shiller)

summary(hpi.lm)
summary(hpi.rlm) # slight less t-value
summary(hpi.lqs)

par(mar = c(5, 5, 5, 5))
plot(shiller$index, pch = 19, cex = 0.3)
abline(reg = hpi.lm, lty = 1, col = "black")
abline(reg = hpi.rlm, lty = 2, col = "red")
abline(reg = hpi.lqs, lty = 3, col = "blue")
