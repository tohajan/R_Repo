#' 
#' LINEAR DISRIMINANT ANALYSIS
#' 
require(ISLR)
require(MASS)
#' 
lda.fit <- lda(Direction~Lag1+Lag2, data = Smarket, subset = Year<2005) 
# NB: this model is run using observations where Year is less than 2005
lda.fit
plot(lda.fit) #plots LDA function separately for the values of the outcome (Direction), 
# i.e., Up and Down
#' 
#' Let's see how well the rule (model) predicts the year 2005
Smarket.2005 <- subset(Smarket,Year==2005) #subsets data containing only obs for Year 2005
lda.pred <- predict(lda.fit, Smarket.2005) #predict using the created model
lda.pred[1:5,]
#' Error message: "incorrect number of dimensions"
#' Check the data type:
class(lda.pred) # it's a list output. So convert to a data type that is responsive to
# indexing (e.g., matrix or data frame):
data.frame(lda.pred)[1:5,]
#' 
table(lda.pred$class, Smarket.2005$Direction) # a tabel of predicted values vs. true
# values
mean(lda.pred$class==Smarket.2005$Direction) #correct classification rate
#' Results: 55.9%
#' 
#' 
#' 