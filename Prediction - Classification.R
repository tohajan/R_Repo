#' **Predicting a categorical variable (Classification)**
#' 
#' Now we are going to show an example of using the tidymodels packages to perform prediction of a categorical variable.

Again, we will use the iris dataset. However, this time the will predict the identity of the flower species (which is categorical) based on the other variables.

We have already split our data into testing and training sets, so we don’t necessarily need to do that again.

However, we can stratify our split by a particular feature of the data using the strata argument of the initial_split() function.

This is useful to make sure that there is good representation of each species in our testing and training data.
#' 
#' -----------------------------------------------------------------------------
#' -----------------------------------------------------------------------------
#' -----------------------------------------------------------------------------
#' STEP 1: Data Splitting (using 'rsample' package)
#' 
#' 
#' 
#' 
