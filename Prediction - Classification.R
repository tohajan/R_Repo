#' **Predicting a categorical variable (Classification)**
install.packages("tidymodels")
library(tidymodels)
#' Now we are going to show an example of using the tidymodels packages to perform prediction 
#' of a categorical variable.
#' 
#' Again, we will use the iris dataset. However, this time we will predict the identity of the 
#' flower species (which is categorical) based on the other variables.
#' 
#' We have already split our data into testing and training sets, so we don’t necessarily need 
#' to do that again. (PS: See the file, "Prediction - Regression.R")
#' 
#' However, we can stratify our split by a particular feature of the data using the strata 
#' argument of the initial_split() function. 
#' This is useful to make sure that there is good representation of each species in our 
#' testing and training data.
library(rsample) # library for the initial_split() function
set.seed(1234)
initial_split(iris, strata = Species, prop = 2/3)

training_iris <-training(split_iris)
head(training_iris)
library(dplyr) # the dplyr package houses the count() function
count(training_iris, Species)

testing_iris <-testing(split_iris)
head(testing_iris)
count(testing_iris, Species)
#' 
#' Great, indeed we have good representation of all 3 species in both the training and testing 
#' sets.
#' 
#' This time we will also show an example of how to perform what is called cross validation. 
#' This process allows us to get a better estimate about the performance of our model using 
#' just our training data by splitting it into multiple pieces to assess the model fit over and 
#' over. This is helpful for making sure that our model will be generalizable, meaning that it 
#' will work well with a variety of new datasets. Recall that using an independent validation 
#' set is part of what we call out-of-sample testing to get a sense of how our model might 
#' perform with new datasets. Cross validation helps us to get a sense of this using our 
#' training data, so that we can build a better more generalizable model.
#' 
#' By creating subsets of the data, we can test the model performance on each subset which is 
#' also a type of out-of-sample testing, as we are not using the entire training dataset, but 
#' subsets of the data which may have different properties than that of the full training 
#' dataset or each other. For example certain subsets may happen to have unusual values for a 
#' particular predictor that are muted by the larger training dataset. With each round of cross 
#' validation we perform training and testing on subsets of the training data. This gives us 
#' estimates of the out-of-sample performance, where the out-of-sample error or generalization 
#' error indicates how often predictions are incorrect in the smaller testing subsets of the 
#' training data.
#' 
#' Cross validation is also helpful for optimizing what we call hyperparameters. 
#' Hyperparameters are aspects about the model that we need to specify. Often packages will 
#' choose a default value, however it is better to use the training data to see what value 
#' appears to yield the best model performance.
#' For example, the different options at each split in a decision tree is called a node. The 
#' minimum number of data points for a node to be split further when creating a decision tree 
#' model is a hyperparameter.
#' 
#' E.g., consider a model that classifies people based on whether or not they earn less than
#' $40,000...
#' If, in this instance, there were only 3 people who made more than 40,000 and our 
#' hyperparameter for the minimum number of data points to continue creating new branches was 
#' 6, then this side of the tree would stop here.
#' 
#' We will show how to optimize this using cross validation, this process is also called 
#' “tuning”, as we are tuning or adjusting the hyperparameter until we see the best 
#' performance with our training data.
#' 
#' The first thing we need to do to perform this process is split our training data into cross 
#' validation samples.
#' 
#' Technically creating our testing and training sets out of our original training data is 
#' sometimes considered a form of cross validation, called the holdout method.
#' The reason we do this is so we can get a better sense of the accuracy of our model using 
#' data that we did not train on.
#' 
#' However, we can do a better job of optimizing our model for accuracy if we also perform 
#' another type of cross validation on just the newly defined training set that we just 
#' created.
#' 
#' There are many cross validation methods and most can be easily implemented using the 
#' rsample package. Here, we will use a very popular method called either v-fold or k-fold 
#' cross validation.
#' 
#' This method involves essentially performing the holdout method iteratively with the 
#' training data.
#' First, the training set is divided into v (or often called called k) equally sized smaller 
#' pieces. The number of v subsets to use is also a bit arbitrary, although generally 
#' speaking using 10 folds is good practice, but this depends on the variability and size of 
#' your dataset.
#' 
#' We are going to use 4 folds for the sake of expediency and simplicity.
#' 
#' The model will be trained on v-1 subsets of the data iteratively (removing a different v 
#' until all possible v-1 sets have been evaluated), while one fold will be saved to act as a 
#' test set. This will give us a sense of the out-of-sample (meaning not the entire training 
#' sample) performance of the model.
#' 
#' In the case of tuning, multiple values for the hyperparameter are tested to determine what 
#' yields the best model performance.
#' 
#' -----------------------------------------------------------------------------
#' -----------------------------------------------------------------------------
#' -----------------------------------------------------------------------------
#' CREATING CROSS VALIDATION SAMPLES (using rsample)
#' The vfold_cv() function of the rsample package can be used to parse the training data into 
#' folds for v-fold cross validation.
#'    The v argument specifies the number of folds to create.
#'    The repeats argument specifies if any samples should be repeated across folds - default 
#'      is FALSE
#'    The strata argument specifies a variable to stratify samples across folds - just like 
#'      in initial_split().
#'      
#' Again, because these are created at random, we need to use the base set.seed() function in 
#' order to obtain the same results each time.
#' 
#' Remember: only the training data is used to create the cross validation samples.
#' 
set.seed(1234)
vfold_iris <- rsample::vfold_cv(data = training_iris, v = 4)
vfold_iris

pull(vfold_iris, splits)
#' 
#' Now we can see that we have created 4 folds of the data and we can see how many values were 
#' set aside for testing (called assessing for cross validation sets) and training (called 
#' analysis for cross validation sets) within each fold.
#' 
#' First we will just use cross validation to get a better sense of the out-of-sample 
#' performance of our model using just the training data. Then we will show how to modify this 
#' to perform tuning.
#' 
#' If we want to take a look at the cross validation splits we can do so like this:
first_fold <-vfold_iris$splits[[1]]
head(as.data.frame(first_fold, data = "analysis")) # training set of this fold
head(as.data.frame(first_fold, data = "assessment")) # test set of this fold
#' 
#' 
#' 
#' -----------------------------------------------------------------------------
#' -----------------------------------------------------------------------------
#' -----------------------------------------------------------------------------
#' CREATING ANOTHER RECIPE, MODEL, AND WORKFLOW
#' We also need to create a new recipe with different variables assigned to different roles. 
#' This time we want to use Species as the outcome. We can use the . notation to indicate that 
#' we want to use the rest of the variables as predictors. Thus we will create a new 
#' cat_recipe where we are using a categorical variable as the outcome.
library(recipes)
# cat_recipe <- training_iris %>%
#   recipe(Species ~ .)
#' The above approach may have led to an error in the later prediction (see details in the
#' lines following the yardstick() function under the "Model Assessment" section below).
#' 
#' Debugging: recreate the recipe by assigning roles using the update_role function:
cat_recipe <- recipe(training_iris) %>%
  recipes::update_role(Species, new_role = "outcome") %>%
  recipes::update_role(Sepal.Length, new_role = "predictor") %>%
  recipes::update_role(Sepal.Width, new_role = "predictor") %>%
  recipes::update_role(Petal.Length, new_role = "predictor") %>%
  recipes::update_role(Petal.Width, new_role = "predictor")
#' 
#' This time we will also not have any preprocessing steps for simplicity sake, thus our 
#' recipe is actually already finished.
#' 
#' Now our next step is to specify our model. We will be using a Classification And Regression 
#' Tree (CART), which we discussed previously. This method can be used for either 
#' classification or regression (categorical or continuous outcome variables). Thus it is 
#' important that we set the mode for classification. We will use the rpart package as our 
#' engine. To tune using this model we would need to specify it here as well. We will show 
#' that in just a bit.
library(rpart)
cat_model <- parsnip::decision_tree() %>%
  parsnip::set_mode("classification") %>%
  parsnip::set_engine("rpart")
cat_model
#' Great! Now we will make a workflow for this.
iris_cat_wflow <-workflows::workflow() %>%
  workflows::add_recipe(cat_recipe) %>%
  workflows::add_model(cat_model)
iris_cat_wflow
#' 
#' 
#' 
#' -----------------------------------------------------------------------------
#' -----------------------------------------------------------------------------
#' -----------------------------------------------------------------------------
#' ASSESSING MODEL PERFORMANCE WITH CROSS VALIDATION (using tune)
#' First we will demonstrate how we could fit the model using our entire training dataset 
#' like we did previously and use yardstick to check the accuracy this time instead of RMSE.
iris_cat_wflow_fit <- parsnip::fit(iris_cat_wflow, data = training_iris)
iris_cat_wflow_fit
#' 
library(workflows)
wf_fit_cat <- iris_cat_wflow_fit %>% 
  pull_workflow_fit()
wf_fit_cat #same output as "iris_cat_wflow_fit" above
names(wf_fit_cat)
#' The output is a bit different for categorical variables. We can also see variable 
#' importance from the model fit, which shows which variables were most important for 
#' classifying the data values. This lists a score for each variable which shows the decrease 
#' in error when splitting by this variable relative to others.
wf_fit_cat$fit$variable.importance
#' 
#' We can see that Petal.Length (its value, 60.85957, is the highest) was the most important 
#' for predicting Species.
#' 
#' Recall that since we are using a categorical outcome variable, we want to use accuracy to 
#' assess model performance. Thus, we can use the accuracy() function of the yardstick package 
#' instead of the rmse() function to assess the model. We first need to get the predicted 
#' values using the predict() function, as these are not in the fit output.
pred_species<-predict(iris_cat_wflow_fit, new_data = training_iris)

# library(yardstick)
# yardstick::accuracy(training_iris, truth = Species, estimate = pred_species$.pred_class)
#' This code produces the following error message:
#'   Error in `yardstick::accuracy()`:
#'   ! Can't subset columns that don't exist.
#'   ✖ Columns `setosa`, `versicolor`, `virginica`, `virginica`, `virginica`, etc. don't exist.
#' 
#' Debugging: I Googled up the first two lines of the error message. Found a solution on stack 
#'  overflow that suggests the error might be due to how the recipe was prepared. So, I 
#'  recreated the recipe (see details in the "creating recipe" section above)
#'  It didn't work! The error persists. Try something else...???
#' 
#' According to the documentation (https://yardstick.tidymodels.org/reference/accuracy.html), 
#' for the accuracy function, the columns specified by the truth and estimate arguments must
#' be contained in the preceding dataframe argument. In the current example, only the truth
#' argument meets that criteria (i.e., Species is a column in the data), the data has no 
#' column containing the values specified in the estimate argument.
#' 
#' To correct this: I created a new data containing all the columns in training iris and a new
#' column for the predicted values
#' 
pseudo_training <- training_iris
pseudo_training$predicted.species <- pred_species$.pred_class
head(pseudo_training)

yardstick::accuracy(pseudo_training, truth = Species, estimate = predicted.species)
#' It worked!!! Got the same accuracy as the Coursera tutorial = 0.97
#' Interpretation: 97% of the time, the model correctly predicted the right species.
#' We can also see which species were correctly predicted using count function:
count(pseudo_training, Species)

count(pseudo_training, predicted.species)
#' Results: one extra versicolor iris was predicted, and one fewer virginica iris.
#' 
#' To see exactly which rows resulted in incorrect predictions, we can bind the predicted 
#' species to the training data like so. This can be helpful to see if there is something 
#' particular about the incorrectly predicted values that might explain why they are 
#' incorrectly predicted.
#' PS: I have created a similar data above, called pseudo_training
predicted_and_truth <-bind_cols(training_iris, 
                                predicted_species = pull(pred_species, .pred_class))
head(predicted_and_truth)
#' 
#' the exact rows with actual values different from predicted values:
predicted_and_truth[predicted_and_truth$Species != predicted_and_truth$predicted_species, ]
#' 
#' -----------------------------------------------------------------------------
#' 
#' However, to fit the model to our cross validation folds we can use the fit_resamples() 
#' function of the tune package, by specifying our workflow object and the cross validation 
#' fold object we created somewhere above. 
#' library(tune)
set.seed(122)
resample_fit <- tune::fit_resamples(iris_cat_wflow, vfold_iris)
#' 
#' We can now take a look at various performance metrics based on the fit of our cross 
#' validation “resamples”.
#' To do this we will use the collect_metrics function of the tune package. This will show us 
#' the mean of the accuracy estimate of the 4 different cross validation folds.
resample_fit

tune::collect_metrics(resample_fit)
#' The accuracy appears to be 95 percent (a 2% drop from the previous 97%). Often the 
#' performance will be reduced using cross validation.
#' 
#' 
#' -----------------------------------------------------------------------------
#' -----------------------------------------------------------------------------
#' -----------------------------------------------------------------------------
#' TUNING
#' 
#' Let’s see how things change when we now tune a hyperparameter. We want to tune the min_n 
#' argument to tune for the minimum number of data points for each node. The arguments may 
#' vary for the engine that you are using. We need to specify this when we fit the model 
#' using the tune() function like so:
set.seed(122)
library(tune)
cat_model_tune <- parsnip::decision_tree(min_n = tune()) %>%
  parsnip::set_mode("classification") %>%
  parsnip::set_engine("rpart") 
cat_model_tune
#' 
#' Now we can create a new workflow using the categorical recipe and the tuning model:
iris_cat_wflow_tune <-workflows::workflow() %>%
  workflows::add_recipe(cat_recipe) %>%
  workflows::add_model(cat_model_tune)
#' 
#' We can use the tune_grid() function of the tune() package to use the workflow and fit the 
#' vfold_iris cross validation samples of our training data to test out a number of different 
#' values for the min_n argument for our model. The grid() argument specifies how many 
#' different values to try out.
resample_fit2 <-tune::tune_grid(iris_cat_wflow_tune, resamples = vfold_iris, grid = 4)
#' 
#' Again we can use the collect_metrics() function to get the accuracy. Or, we can use the 
#' show_best() function of the tune package to see the min_n values for the top performing 
#' models (those with the highest accuracy).
tune::collect_metrics(resample_fit)

tune::show_best(resample_fit, metric = "accuracy")
#' 
#' 
#' 
#' 