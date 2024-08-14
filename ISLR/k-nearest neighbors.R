#' 
#' K-Nearest Neighbors
#' 
require(ISLR)
#' 
library(class)
?knn #knn is a function in the "class" library with arguments: train, test, etc.
#' Use on the Stock market example
attach(Smarket) #NB: Smarket is a dataset in the ISLR package
#' 
#' Create a matrix using two variables from the dataset
xlag <- cbind(Lag1, Lag2)
train <- Year<2005
#' 
knn.pred <- knn(xlag[train, ], xlag[!train, ], Direction[train], k=1)
#' 1st argument (train) is the training set, containing obs where Year is < 2005
#' 2nd argument (test) is the test set, containing obs where Year is not < 2005
#' The response is Direction
#' k=1 means we want 1 nearest neighbor classification
#' 
table(knn.pred, Direction[!train]) # confusion matrix
mean(knn.pred==Direction[!train]) # correct classification rate
#' Results: 50% (no better than flipping a coin!). What to do next? One could try
#' multiple values of k to see which has a better/optimal performance
#' 
#' 
#' 
#' 
#' 
#' 
#' 
#' 