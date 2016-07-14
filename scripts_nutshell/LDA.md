# Linear Discriminant Analysis

**R in a Nutshell** - Chapter 21 - Classification Models

Linear Discriminant Analysis (LDA - not to be confused with Latent Dirichlet Allocation)

A statistical technique for finding the the linear combination of features that best separate observations into different classes.

Assumptions:

- data in each class is normally distributed
- there exists a unique covariance matrix for each class

To use linear discriminant analysis in R, use call the function `lda()` from the package `MASS`.

	`lda(formula, data, ..., subset, na.action)`

# Quadratic Discriminant Analysis

Similar to LDA - but QDA looks for a quadratic combination of features that best separates observations into different classes. It's called by the function `qda()` also in the library `MASS`.

	`qda(formula, data, ..., subset, na.action)`

# Spambase Dataset

Spambase was created by Mark Hopkins, Erik Reeber, George Forman, and Jaap Suermondt at Hewlett-Packard Labs. This data has already been cleaned and processed (not a real world example, anymore) - and contains

- 4,601 observations of email messages
- 1,813 of these observations are spam
- 58 different computed attributes

# Quadratic Discriminant Analysis

...didn't work. Came up with this error even after converting variables. 

`Error in qda.default(x, grouping, ...) : rank deficiency in group 1`

# Linear Discriminant Analysis


# Flexible Discriminant Analysis


# Mixture Discriminant Analysis




