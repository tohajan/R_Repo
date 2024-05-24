#' ---
#' title: "Linear Regression"
#' author: "Toheeb"
#' ---
#' 
#' # Linear Regression
#' 
#' ## Association Testing
#' 
#' Example: Using the built-in trees dataset (it includes girth, height, and volume 
#' measurements for 31 black cherry trees).
#' Question: "Can a tree's height be inferred given its girth?"
#' #' Presumably, it’s easier to measure a trees girth (width around) than it is to measure 
#' its height. Thus, here we want to know whether or not height and girth are associated. In 
#' this case, since we’re asking if we can infer height from girth, girth is the independent 
#' variable and height is the dependent variable. In other words, we’re asking "does height 
#' depend on girth?"
#' 
#' First, before carrying out the linear regression to test for association and answer this 
#' question, we have to be sure linear regression is appropriate. We’ll test for linearity and 
#' homoscedasticity.
#' 
#' To do so, we’ll first use ggplot2 to generate a scatterplot of the variables of interest.
library(magrittr) #contains the pipe operator "%>%" [also contained in the dplyr package]
library(ggplot2)
trees %>%
  ggplot() + 
  geom_point(aes(Height, Girth))

#' Results: the relationship looks approximately linear, but to visually make this a little 
#' easier, we’ll add a line of best first to the plot:
trees %>% 
  ggplot(aes(Height, Girth)) +
  geom_point() +
  geom_smooth(method = "lm", se = F)

#' Results: On the graph, the relationship looks approximately linear and the variance 
#' (distance from points to the line) appears constant across the data. Given this, it’s 
#' appropriate to use linear regression for these data.
#' 
#' 
#' -----------------------------------------------------------------------------
#' -----------------------------------------------------------------------------
#' ## Fitting the Model
#' Now, we can run the linear regression. To do so, we’ll use the lm() function to fit the 
#' model. The syntax for this function is lm(dependent_variable ~ independent_variable, 
#' data = dataset).
fit <- lm(Girth ~ Height , data = trees)

#' 
#' -----------------------------------------------------------------------------
#' -----------------------------------------------------------------------------
#' ## Model Diagnostics
#' Some assumptions of linear regression were mentioned above. After fitting a model, it’s 
#' necessary to check if the model satisfies these assumptions. If the model does not (e.g., 
#' if the relationship is nonlinear), then the model cannot be used and interpreted.
#' 
#' In order to assess the model, a number of diagnostic plots can be very helpful. Diagnostic 
#' plots can be generated using the plot() function with the fitted model as an argument.
par(mfrow = c(2, 2))
plot(fit)
#' Results -- four plots are generated: 
#'   1) **Residuals vs Fitted** - checks linear relationship assumption of linear regression. 
#' A linear relationship will demonstrate a horizontal red line here. Deviations from a 
#' horizontal line suggest nonlinearity and that a different approach may be necessary. In the 
#' current example, the red line is approximately horizontal, which is what we’re looking for. 
#' Additionally, look to be sure there is no clear pattern in the points on the plot - they 
#' should be random (as they are in the current example). Clustering of a bunch of points or 
#' trends in this plot would indicate that the data do not have a linear relationship.
#'   
#'   2) **Normal Q-Q** - checks whether or not the residuals (the difference between the 
#' observed and predicted values) from the model are normally distributed. The best fit model 
#' points fall along the dashed line on the plot. Deviation from this line suggests that a 
#' different analytical approach may be more appropriate In the current example, the points do 
#' not fall perfectly along the dotted line, suggesting that the residuals are not normally 
#' distributed.**
#'   A histogram (or density plot) of the residuals can also be used for this portion of 
#'   regression diagnostics. Here, we’re looking for a Normal distribution of the residuals:
sresiduals <- rstandard(fit)
par(mfrow=c(1,1))
hist(sresiduals)
# This operation coudl also be performed using ggplot:
ggplot(fit, aes(fit$residuals)) + 
  geom_histogram(bins = 5)
#' Results: The QQ Plot and the histogram of the residuals will always give the same answer. 
#' Here, with a limited sample size, the residuals are not perfectly Normally distributed; 
#' however, the points do not fall wildly far from the dotted line.
#'   
#'   3) **Scale-Location** - checks the homoscedasticity of the model. A horizontal red line 
#' with points equally spread out indicates a well-fit model. A non-horizontal line or points 
#' that cluster together suggests that non-homoscedastic data. In the current example, there’s 
#' some indication of heteroscedasticity, with points not being equally far from the 
#' regression line across the observations.
#' **PS: when the data are nonlinear or the variances are not homogeneous (i.e., are not 
#' **homoscedastic), the data can be transformed (log, root) before running the linear 
#' regression model.
#'   
#'   4) **Residuals vs Leverage** - helps to identify outlier or extreme values that may 
#'   disproportionately affect the model’s results. Their inclusion or exclusion from the 
#'   analysis may affect the results of the analysis. Note that the top three most extreme 
#'   values are identified with numbers next to the points in all four plots. Generally 
#'   speaking, **standardized residuals greater than 3 or less than -3** are to be considered 
#'   as outliers. In the current example, there are no values in that range (looking at the 
#'   y-axis), suggesting that there are **no extreme outliers** driving the results of the 
#'   analysis.
#' 
#' 
#' -----------------------------------------------------------------------------
#' -----------------------------------------------------------------------------
#' ## Interpreting the Model
#' While the relationship in this example appears to be linear; does not appear to to be 
#' affected by outliers; is approximately homoscedastic; and has residuals that are not 
#' perfectly Normally distributed, but fall close to the line in the QQ plot, ... the model 
#' results can still be interpreted.
summary(fit)
#' The summary() function outputs values of particular interest, among others: 
#'        the beta estimate, 
#'        the standard error (SE), 
#'        and the p-value.
#' 
#' From the beta estimate, (which is positive here), we confirm that the relationship is 
#'    positive (which we could also tell from the scatterplot). We can also interpret this 
#'    beta estimate explicitly. Also known as the beta coefficient, it is how much the 
#'    dependent variable will change given a one unit increase in the independent variable. 
#'    In the case of the trees, a beta estimate of 0.256, indicates that for every inch a 
#'    tree’s girth increases, its height will increase by 0.256 inches. Thus, we not only 
#'    know that there’s a positive relationship between the two variables, but we know by 
#'    precisely how much one variable will change given a single unit increase in the other 
#'    variable. Note that we’re looking at the second row in the output here, where the row 
#'    label is “Height”. This row quantifies the relationship between our two variables. 
#'    The first row quantifies the intercept, or where the line crosses the y-axis.
#' 
#' The standard error and p-value are also included in this output. Error is typically 
#'    something we want to minimize (in life and in statistical analyses), so the smaller the 
#'    error, the more confident we are in the association between these two variables.
#' 
#' The beta estimate and the standard error are then both considered in the calculation of 
#'    the p-value (found in the column Pr[>|t|]). The smaller this value is, the more confident 
#'    we are that this relationship is not due to random chance alone.
#' 
#' 
#' -----------------------------------------------------------------------------
#' -----------------------------------------------------------------------------
#' ## Variance Explained
#' Additionally, the strength of this relationship is summarized using the adjusted 
#'    R-squared metric. This metric explains how much of the variance this regression line 
#'    explains. The more variance explained, the closer this value is to 1. And, the closer 
#'    this value is to 1, the closer the points in your dataset fall to the line of best fit. 
#'    The further they are from the line, the closer this value will be to zero.
#' 
#' As seen in the scatterplot, the data are not exactly on the regression line, so a value of 
#' 0.2445 seems reasonable, suggesting that this model (this regression line) explains 24.45% 
#' of the variance in the data.
#' 
#' 
#' -----------------------------------------------------------------------------
#' -----------------------------------------------------------------------------
#' ## Using broom()
#' Although the summary() output are visually helpful, there's another function that
#'organizes the output into a nice table. The tidy() function (from the broom library)
#' takes the summary output from a statistical model and organize it into a tabular output.
#install.packages("broom")
library(broom)
tidy(fit)

#' 
#' Finally, it’s important to always keep in mind that the interpretation of your inferential 
#' data analysis is incredibly important. When you use linear regression to test for 
#' association, you’re looking at the relationship between the two variables. While girth can 
#' be used to infer a tree’s height, this is just a correlation. It does not mean that an 
#' increase in girth causes the tree to grow more. Associations are correlations. They are not 
#' causal.
#' 
#' For now, however, in response to our question, can we infer a black cherry tree’s height 
#' from its girth, the answer is yes. We would expect, on average, a tree’s height to increase 
#' 0.255 inches for every one inch increase in girth.
#' 
#' 
#' 
#' 
