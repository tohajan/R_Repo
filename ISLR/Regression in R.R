#########################################################
#' Simple Linear Regression
#' 
library(MASS) #contains some datasets
library(ISLR) #contains more datasets used in the ISLR book

names(Boston) # a dataset in the MASS library
# ?Boston # provided description about the dataset and its variables
#' 
plot(medv~lstat, Boston) #the ~ notation tells R that the preceding variable is the response, to
#be plotted on the y-axis
#' 
fit1 <- lm(medv~lstat, data = Boston)
fit1 # this gives a brief summary of the fit
summary(fit1) #more detailed summary
#' 
abline(fit1, col = "red") #adds a regression line to the plot
names(fit1) #lists the components of the fit
confint(fit1) # 95% confidence interval for the fit
#' 
predict(fit1, data.frame(lstat=c(5,10,15)), interval="confidence")
#' the above syntax asks R to use the fit to predict the values of medv for the given values of
#' lstat, along the with the corresponding 95% CI
#' 
#' -----------------------------------------------------------------------------
#' -----------------------------------------------------------------------------
#' -----------------------------------------------------------------------------
#' Multiple Linear Regression
fit2 <- lm(medv~lstat+age, Boston) #age is now an additional IV
summary(fit2)
#' NB: age is also significant, but not as strongly as lstat
#' 
fit3 <- lm(medv~., Boston) # all variables except medv are now IVs
summary(fit3)
#' age is no longer significant. This implies that other predictors are included that are
#' correlated with age, and in the presence of those predictors, age is no longer significant.
#' 
par(mfrow = c(2,2))
plot(fit3) # produces various visualizations of the linear model
#'  1. Residuals vs Fitted: this checks for non-linearity in the data. In this example, there is
#' some curvature in red line, indicating the presence of non-linearity in the data
#'  2. Normal Q-Q: checks for non-normality. In this example, the data points do not perfectly
#' align with the dotted line, indicating some presence of non-normality
#'  3. Scale-Location: checks if the variance changes with the fit, which seems to be the case
#' in the current example. This may also be due to data's non-linearity 
#'  4. Residuals vs Leverage:
#' 
fit4 <- update(fit3, ~. -age-indus) #models a new fit based on fit3, but excluding the age and
# indus variables (both were non-significant in the multivariate model)
summary(fit4)
#' 
#' 
#' -----------------------------------------------------------------------------
#' -----------------------------------------------------------------------------
#' -----------------------------------------------------------------------------
#' Nonlinear terms and Interactions
#' 
#' 
#' 
#' 
#' 
#' 
#' 
#' 
#' 
#' 
#' 