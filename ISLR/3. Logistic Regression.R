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
#' Results: 0.5216. The performance appears low (slightly better than chance). There
#' may be some overfitting going on.
#' 
#' -----------------------------------------------------------------------------
#' -----------------------------------------------------------------------------
#' Split into training and test sets
dim(Smarket)[1]
#' the data has 1,250 obs 
train <- Year<2005 # this is a logical object containing Year less than 2005

glm_fit2 <- glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume, 
                data=Smarket, family=binomial, subset = train)
#' "subset=train" tells R to fit the model to the data only for observations whose
#' Year values are less than 2005
glm_probs2 <- predict(glm_fit2, newdata = Smarket[!train, ], type="response")
#' Now, we are predicting the response for observations not used in the training (i.e.,
#' where Year values is not less than 2005, i.e., test set)
#' Now, more specifically:
glm_pred2 <- ifelse(glm_probs2>0.5, "Up", "Down")
#' 
#' Create a new object that contains Direction values for the test set
Direction_test <- Smarket$Direction[!train]
#' 
table(glm_pred2, Direction_test) 
#' Results:
#             Direction_test
#   glm_pred2   Down Up
#     Down      77   97
#     Up        34   44
mean(glm_pred2==Direction_test)
#' Results: 0.4802. Even worse than the previous model (null model)!
#' 
#' FIT A SMALLER MODEL:
glm_fit3 <- glm(Direction~Lag1+Lag2, data=Smarket, family=binomial, subset = train)
#' Now, there's only two IVs in the model
#' 
glm_probs3 <- predict(glm_fit3, newdata=Smarket[!train,], type="response")
glm_pred3 <- ifelse(glm_probs3>0.5, "Up", "Down")
table(glm_pred3,Direction_test)
mean(glm_pred3==Direction_test)
#' Results: 0.5595. Higher accuracy than all previous models
#' 
summary(glm_fit3)
#' No IV is significant, still. But the prediction is better on this latest model.
#' 
#' -----------------------------------------------------------------------------
#' -----------------------------------------------------------------------------
#' PERSONAL EXERCISE: USE A RANDOM SPLIT APPROACH
set.seed(5)
rand_sm <- sample(c(TRUE, FALSE), nrow(Smarket), 
                       replace = TRUE, prob = c(0.7,0.3))
# splits the data in a 7:3 ratio
train_sm <- Smarket[rand_sm, ] #training set
test_sm <- Smarket[!rand_sm, ] #test set
#' 
sm_fit <- glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume, 
              data=train_sm, family=binomial)

sm_probs <- predict(sm_fit, newdata=test_sm, type="response")
sm_pred <- ifelse(sm_probs>0.5, "Up", "Down")
table(sm_pred,test_sm$Direction)
mean(sm_pred==test_sm$Direction) 
#' Results: 0.5198. Better than corresponding results (0.4802) for the non-random 
#' split.
summary(sm_fit)
#' Same: No IV is significant
#' 
#' -----------------------------------------------------------------------------
#' Fit a smaller model"
sm_fit2 <- glm(Direction~Lag1+Lag2, data=train_sm, family=binomial)

sm_probs2 <- predict(sm_fit2, newdata=test_sm, type="response")
sm_pred2 <- ifelse(sm_probs2>0.5, "Up", "Down")
table(sm_pred2,test_sm$Direction)
mean(sm_pred2==test_sm$Direction) 
#' Results: 0.53904. Slightly lower than corresponding results (0.5595) for the
#' non-random split
summary(sm_fit2)
#' All IVs still remain non-significant
#' 
#' Conclusion: The model from the random-split data performed no better than the
#' non-random split. In fact, it appears worse for a smaller model (i.e., a model with
#' fewer number of IVs.
#' 