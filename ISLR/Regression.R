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
#' Results: every IV left in the model are still statistically significant
#' 
#' -----------------------------------------------------------------------------
#' -----------------------------------------------------------------------------
#' -----------------------------------------------------------------------------
#' Nonlinear terms and Interactions
fit5 <- lm(medv~lstat*age, Boston) # the "*" denoted interaction between variables
summary(fit5)
#' 
fit6 <- lm(medv~lstat+I(lstat^2), Boston) # A quadratic term is now added for lstat: this is 
#' one way to adjust for the non-linearity observed in the scatterplot of medv vs lstat
#' NB: the Identity function is used to protect the quadratic term (to avoid interference from
#' the lm function in an unintended manner???)
summary(fit6)
#' Results: both the linear and quadratic terms are statitically significant
#' 
attach(Boston) #this makes the variable readily available in the workspace without calling on
#' data set name
#' Error message:
#' The following object is masked _by_ .GlobalEnv:
#'      age
#' Why? an object named age (same name as a variable in the Boston data) already exists in the
#' workspace, so the existing object masks the variable age of the Boston dataset.
#' Possible solutions: 
#'    1) always type "Boston::age" when referring the variable age
#'    2) delete the pre-existing "age" object from memory, then reattach the Boston dataset
#'    3) rename one of the two conflicting objects
#' However, the age variable may not need to be referred to for the rest of this lesson; so, 
#' moving on...
#' 
par(mfrow=c(1,1))
plot(medv~lstat) #replots the orginal scatterplot (see the non-linearity)
#' Now include the quadratic fit:
#' Note that the abline() function can no longer be used, since it's only used for a linear fit. And
#' it's certain that the plot is not linear. Use the points() function instead:
points(lstat, fitted(fit6), col="red", pch=20)
#' Recall that fit6 is the quadratic model, fitted earlier
#' 
#' An easier way of fitting polynomials is to use the poly function as follows:
fit7 <- lm(medv~poly(lstat,4)) # the IV here is lstat to the power of 4
#' add this new fit to the plot:
points(lstat, fitted(fit7), col="blue", pch=20)
#' Results: Notice that the new model (blue line) seems to be overfitting the data
#' 
#' More on the "pch" (plotting character) argument:
plot(1:20, 1:20, pch=1:20, cex=2) #shows the different symbols of pch 1-20
#' cex: specifies the size of the plotting character (in this example, "=2" doubles the size
#' of each symbol)
#' 
#' 
#' -----------------------------------------------------------------------------
#' -----------------------------------------------------------------------------
#' -----------------------------------------------------------------------------
#' Qualitative Predictors
fix(Carseats) # the fix function opens up the specified data set in a new window (making the
#' data amenable to editing)
names(Carseats)
summary(Carseats)
#' 
fit_a <- lm(Sales~. + Income:Advertising + Age:Price, Carseats)
#' NB: except Sales, all other variables are IVs. Also, two interaction terms have been added:
#' IncomexAge and AdvertisingxPrice  
summary(fit_a)
#' Results: some IVs are statistically significant, while others are not. The interaction
#' between Income and Advertising is significant but between Income and Age is not
#' 
contrasts(Carseats$ShelveLoc) #shows how the specified qualitative variable will be coded when
#' when put in a linear model
#' Results:
# Good Medium
# Bad       0      0
# Good      1      0
# Medium    0      1
#' 
#' Interpretation: the Shelveloc variable has three levels (Bad, Good, Medium), so two dummy 
#' variables are created (Good, Medium). For observations with an original value of Bad, the 
#' two dummy vars each has a value of 0. For obs with an original value of Good, the former is 
#' 1, the latter is 0. For obs with an original value of Medium, the former is 0, the latter
#' is 1.
#'  
#' 
#' -----------------------------------------------------------------------------
#' -----------------------------------------------------------------------------
#' -----------------------------------------------------------------------------
#' A FEW OTHER THINGS...
#' 
#' Writing R functions
#' Ex: write a function to fit a regression model and make a plot
regplot <- function(x,y){
  fit <- lm(y~x)
  plot(x,y)
  abline(fit, col="red")
}
#' 
attach(Carseats)
#' 
regplot(Price, Sales)
#' 
#' One can go even further...
regplot2 <- function(x,y,...){
  fit <- lm(y~x)
  plot(x,y,...)
  abline(fit, col="green")
}
#' the ellipsis (...) keeps the function open, such that it could take on more argument that
#' were not originally defined when creating the function. See the ffg example:
regplot2(Price, Sales, xlab="Price", ylab="Sales", col="blue", pch=20)
#' 
#' 
#' 
#' 
#' 
#' 
#' 
#' 