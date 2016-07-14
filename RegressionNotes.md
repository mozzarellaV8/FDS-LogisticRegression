# Regression Models

This is an ongoing list of notes taken from the [Harvard Statistical Software Workshop](http://tutorials.iq.harvard.edu/R/Rstatistics/Rstatistics.html) and the book [R in a Nutshell](http://shop.oreilly.com/product/0636920022008.do).

- [Linear Models](#linear-models)
- [Least Sqaures Regression](#least-squares-regression)
- [Assumptions of Least Squares Regression](#assumptions-of-least-squares-regression)
- [Resistant Regression](#resistant-regression)
- [Robust Regression](#robust-regression)
- [Stepwise Variable Selection](#stepwise-variable-selection)
- [Ridge Regression](#lasso-and-least-angle-regression)
- [Lasso and Least Angle Regression](#lasso-and-least-angle-regression)
- [elasticnet](#elasticnet)
- [Principal Component & Partial Least Squares Regression](#principal-component--partial-least-squares-regression)

## Linear Models

*viewing the model*

- `summary()`
- `coef()`

*predicting values using the model*

- `residuals()` - returns vector of residuals
- `fitted()` - returns vector of fitted values

	``` r 
	predict(object, newdata, se.fit = FALSE, SCALE = null, 
	interval = c("none", "confidence", "prediction"),
	level = 0.95, type = c("response", "terms")...)
	```

	- `object` - model returned from fitting function
	- `newdata` - new data source for predictions
	- `na.action` - by default, `predict()` ignores missing `newdata` values

*analyzing the fit*

- `confint(object, parm, level = 0.95)` - `parm` specifies which variables to show confidence intervals of; `level` specifies confidence level.

- `influence(model, do.coef = TRUE,...)` - compute influence of different parameters. also: `influence.measures(model)`
 

### Least Squares Regression 

`lm()`

- the original

- manual tune this model with diagnostics e.g. `summary()` and `lm.influence`

- looks for the coefficients that minimize the residual sum of squares

### Assumptions of Least Squares Regression

	1. Linearity

	2. Full Rank 

	3. Exogenicity of predictor/independent variables - expected value for error term is 0 for all possible values of independent variables.

	4. Homoscedasticity - error term has constant variance and is not correlated with independent variables.

	5. Nonautocorrelation - in sequence of observations, y-values are not correlated with one another.

	6. Exogenously generated data - independent variables are generated independently of the process that generates the _error term_

	7. _Error term_ is normally distributed with standard deviation 𝞂 and mean 0.

### Resistant Regression

`library(MASS)`
`lqs()`

- fitting a model to data with outliers via method = "lms" (least median squares) or "lts" (least trimmed squares)


### Robust Regression

`library(MASS)`
`rlm(formula, data, weights, ... )` 

- method that handles problems with heteroscedasticity and outliers in the data.


### Stepwise Variable Selection 

`step(object, scope, scale = 0, ...)` 
_where object is a model from lm(), glm(), aov()_

- algorithm repeatedly adds/removes variables from the model in an attempt to 'improve' with every step. Akaike Information Criterion (AIC) is the measure for value of variable with this function

### Ridge Regression


`libary(MASS)`
`lm.ridge(formula, data, subset,...)`

- can be useful when suspecting _multicollinearity_ or several highly correlated variables in the data.

- attempts to minimize residual sum of squares (like OLS) but with an additional penalty for coefficient sizes

### Lasso and Least Angle Regression

`library(lars)`
`lars(x, y, type = c("lasso", "lar", "forward.stagewise", "stepwise"),...)`

- reduces the size of coefficients, and thus their final impact on the model. 

- takes sum of absolute values of coefficients (rather than squares) as penalty.

- method `lars` computes enture lasso path at once - from a model with no variables; then lambda values when each variable enters the model; and finally a model with all coefficients present.

### elasticnet

`library(elasticnet)`
`enet(x, y, lambda, max.steps, normalize, intercept, trace, eps)`

- umbrella which `ridge` and `lasso` reside

- takes matrices but not data frames :(

### Principal Component & Partial Least Squares Regression

_01: Principal Component Regression_

`library(pcr)`
`pcr(..., method = pls.options()$pcralg)`
`pslr(..., method = pls.options()$plsralg)`

- model the effects of closely correlated variables that throw off an ordinary least squares model. 

- transforms the independent/predictor variables using principal components analysis, then performs linear regression on the transformed variables.

_02: Partial Least Squares_

- both predictor and response variables are transformed before fitting a linear regression

Both of these functions are aliases to the function 

`mvr(formula, ncomp, data, subset, na.action, ...)`


# Nonlinear Models

## Generalized Linear Models




























