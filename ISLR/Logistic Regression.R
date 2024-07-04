#' 
require(ISLR) #require() is similar to library() function
#' 
names(Smarket)
summary(Smarket)
?Smarket
#' Make a correlation plot of the data:
pairs(Smarket, col=Smarket$Direction) 
#' The var "Direction" (which has a binary) is set as the color indicator for the plot
#' 
#' 
#' -----------------------------------------------------------------------------
#' -----------------------------------------------------------------------------
#' Logistic Regression
glm_fit <- glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume, data=Smarket, family=binomial)
#' "family=binomial" tells GLM to fit a logistic regression
summary(glm_fit)
#' Results: None of the IVs was statistically significant. But the model can still
#' be used for predictions:
glm_probs <- predict(glm_fit, type="response") #this makes prediction on the training
#' data used to fit the model. The result is a vector of probabilities
glm_probs[1:5] #first 5 items of the fitted probabilities
#' Results: they are all around 50% (not a strong prediction). This is not surprising
#' given the model results (no IV was significant)
#' 
#' More specific predictions (turn the probabilities into classificatiion, with a
#' threshold at 0.5):
glm_pred <- ifelse(glm_probs>0.5, "Up", "Down") #if the predicted probability is >0.5,
# classify the Direction (outcome variable) as "up", otherwise, classify as "down"
attach(Smarket)
table(glm_pred, Direction)
#' Results:
#               Direction
#   glm_pred    Down  Up
#       Down    145   141
#         Up    457   507
#' 
mean(glm_pred==Direction) #calculates the mean classification performance of the model
#' 
#' 
#' 
#' 
#' 
#' 
#' 
#' 
#' 