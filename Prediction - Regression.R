#' **Predicting a continuous variable (Regression)**
#' 
#' -----------------------------------------------------------------------------
#' -----------------------------------------------------------------------------
#' -----------------------------------------------------------------------------
#' STEP 1: Data Splitting (using 'rsample' package)
# install.packages("rsample")
library(rsample)
set.seed(1234) #this is important, since the split is performed randomly
split_iris <-initial_split(iris, prop = 2/3) 
# the prop argument specifies what proportion of the dataset to use for training; default is 1/4
#..for testing and 3/4 for training
split_iris
#' 
#' Next, extract the training and testing datasets by using the training() and testing() 
#' functions, also of the rsample package:
training_iris <-training(split_iris)
head(training_iris)

testing_iris <-testing(split_iris)
head(testing_iris)
#' 
#' 
#' -----------------------------------------------------------------------------
#' -----------------------------------------------------------------------------
#' -----------------------------------------------------------------------------
#' STEP 2: Prepare for preprocessing the data (using 'recipes' package)
#' The recipes package is a standardized format for a sequence of steps for preprocessing the 
#' data. This can be very useful because it makes testing out different preprocessing steps or 
#' different algorithms with the same preprocessing very easy and reproducible.
#' 
#' Creating a recipe specifies how a data frame of predictors should be created - it specifies 
#' what variables to be used and the preprocessing steps, but it does not execute these steps 
#' or create the data frame of predictors.
#' 
#' -----------------------------------------------------------------------------
#' Step 2a -- Specify variables with the recipe() function
#' Option 1 -- using formula notation, which looks like this:
#'        outcome(s) ~ predictor(s)
#' 
#' For multiple predictors or a multivariate situation with two outcomes, use a plus sign:
#'        outcome1 + outcome2 ~ predictor1 + predictor2
#'  
#' To include all predictors, use a period like so:
#'        outcome_variable_name ~ .
#' 
#' E.g.: predict Sepal.Length in the training data based on Sepal.Width and Species. Thus, 
#' Sepal.Length is the outcome variable and Sepal.Width and Species are the predictor variables.
# install.packages("recipes")
library(recipes)
first_recipe <- training_iris %>%
  recipe(Sepal.Length ~ Sepal.Width + Species)
first_recipe
#' 
#' Option 2 -- by assigning roles to the variables using the update_role() function.
first_recipe <- recipe(training_iris) %>%
  recipes::update_role(Sepal.Length, new_role = "outcome")  %>%
  recipes::update_role(Sepal.Width, new_role = "predictor") %>%
  recipes::update_role(Species, new_role = "predictor")
first_recipe

#' View the recipe in  more detail using the base summary() function:
summary(first_recipe)
#' 
#' 
#' -----------------------------------------------------------------------------
#' Step 2b -- Specify the preprocessing steps with step*() functions. There are step functions 
#' for a variety of purposes:
#'  1. Imputation– filling in missing values based on the existing data
#'  2. Transformation– changing all values of a variable in the same way, typically to make 
#'        it more normal or easier to interpret
#'  3. Discretization– converting continuous values into discrete or nominal values - binning 
#'        for example to reduce the number of possible levels (However this is generally not 
#'        advisable!)
#'  4. Encoding / Creating Dummy Variables– creating a numeric code for categorical variables 
#'  5. Data type conversions– changing from integer to factor or numeric to date etc.
#'  6. Interaction term addition to the model– modeling for predictors that would influence the 
#'        capacity of each other to predict the outcome
#'  7. Normalization– centering and scaling the data to a similar range of values
#'  8. Dimensionality Reduction/ Signal Extraction– reducing the space of features or 
#'        predictors to a smaller set of variables that capture the variation or signal in the 
#'        original variables (ex. Principal Component Analysis and Independent Component 
#'        Analysis)
#'  9. Filtering – filtering options for removing variables (ex. remove variables that are 
#'        highly correlated to others or remove variables with very little variance and 
#'        therefore likely little predictive capacity)
#'  10. Row operations– performing functions on the values within the rows (ex. rearranging, 
#'        filtering, imputing)
#'  11. Checking functions – Sanity checks to look for missing values, to look at the variable 
#'        classes, etc.
#'  
#' All of the step functions look like step_*() with the * replaced with a name, except for 
#' the check functions which look like check_*().
#' 
#' There are several ways to select what variables to apply steps to:
#'    1. Using tidyselect methods: contains(), matches(), starts_with(), ends_with(), 
#'          everything(), num_range()
#'    2. Using the type: all_nominal(), all_numeric() , has_type()
#'    3. Using the role: all_predictors(), all_outcomes(), has_role()
#'    4. Using the name - use the actual name of the variable/variables of interest
#'    
#' Let’s try adding a preprocessing step to our recipe.
#' We might want to potentially one-hot encode some of our categorical variables so that they 
#' can be used with certain algorithms like a linear regression require numeric predictors.
#' 
#' We can do this with the step_dummy() function and the one_hot = TRUE argument. One-hot 
#' encoding means that we do not simply encode our categorical variables numerically, as our 
#' numeric assignments can be interpreted by algorithms as having a particular rank or order. 
#' Instead, binary variables made of 1s and 0s are used to arbitrarily assign a numeric value 
#' that has no apparent order.
first_recipe <- first_recipe %>%
  step_dummy(Species, one_hot = TRUE)

first_recipe
#' 
#' 
#'
#' -----------------------------------------------------------------------------
#' -----------------------------------------------------------------------------
#' -----------------------------------------------------------------------------
#' STEP 3: Optionally perform the preprocessing to see how it influences the data
#' Optionally one can use the prep() function of the recipes package to update the recipe for 
#' manually performing the preprocessing to see how this influences the data. This step is 
#' however not required when using the workflows package. The preprocessed training data can 
#' then be viewed by using the bake() function with the new_data = NULL argument, while 
#' preprocessed testing data can be viewed using the bake() function and specifying that the 
#' testing data is the new_data.
#' 
#' The prep() function estimates parameters (estimating the required quantities and statistics 
#' required by the steps for the variables) for preprocessing and updates the variables roles, 
#' as sometimes predictors may be removed, this allows the recipe to be ready to use on other 
#' datasets.
#' 
#' It does not necessarily actually execute the preprocessing itself, however we will specify 
#' using the retain argument for it to do this so that we can take a look at the preprocessed 
#' data.
#' 
#' There are some important arguments to know about:
#'    1. training - you must supply a training dataset to estimate parameters for 
#'    preprocessing operations (recipe steps) - this may already be included in your recipe - 
#'    as is the case in the current example
#'    2. fresh - if fresh=TRUE, - will retrain and estimate parameters for any previous steps 
#'    that were already prepped if you add more steps to the recipe
#'    3. verbose - if verbose=TRUE, shows the progress as the steps are evaluated and the size 
#'    of the preprocessed training set
#'    4. retain - if retain=TRUE, then the preprocessed training set will be saved within the 
#'    recipe (as template). This is good if you are likely to add more steps and do not want to 
#'    rerun the prep() on the previous steps. However this can make the recipe size large. 
#'    This is necessary if you want to actually look at the preprocessed data.
#'    
#' Let’s try out the prep() function:
prepped_rec <- prep(first_recipe, verbose = TRUE, retain = TRUE )
prepped_rec
names(prepped_rec)
## [1] "var_info"       "term_info"      "steps"          "template"      
## [5] "retained"       "tr_info"        "orig_lvls"      "last_term_info"
#' 
#' To check out specific outputs of the prep(), use index or $ notation. E.g., the following
#' two syntaxes provide the original variable info (var_info):
prepped_rec[1]
prepped_rec$var_info
#' Other specific outputs are:
#'      the updated variable info after preprocessing (term_info)
#'      the new levels of the variables
#'      the original levels of the variables (orig_lvls)
#'      info about the training dataset size and completeness (tr_info) 
#' 
#' Now we can use bake to see the preprocessed training data. *Note that this used to require 
#' the juice() function.
#' Since we are using our training data we need to specify that we don’t have new_data with 
#' new_data = NULL.
preproc_train <-recipes::bake(prepped_rec, new_data = NULL)
glimpse(preproc_train)
#' 
#' We can see that the Species variable has been replaced by 3 variables representing the 3 
#' different species numerically with zeros and ones.
#' Now we do the same for our testing data using bake(). You generally want to leave your 
#' testing data alone, but it is good to look for issues like the introduction of NA values 
#' if you have complicated preprocessing steps and you want to make sure this performs as 
#' you expect.
baked_test_pm <- recipes::bake(prepped_rec, new_data = testing_iris)
glimpse(baked_test_pm)
#' Great! Now, back to the typical steps.
#' 
#' 
#' 
#' -----------------------------------------------------------------------------
#' -----------------------------------------------------------------------------
#' -----------------------------------------------------------------------------
#' STEP 4: Specify the model (using parsnip)
#' So far we have used the packages rsample to split the data and recipes to assign variable 
#' types, and to specify and prep our preprocessing (as well as to optionally extract the 
#' preprocessed data).
#' 
#' We will now use the parsnip package to specify our model. There are four things we need to 
#' define about our model:
#'    1. The type of model (using specific functions in parsnip like rand_forest(), 
#'      logistic_reg() etc.)
#'    2. The package or engine that we will use to implement the type of model selected (using 
#'      the set_engine() function)
#'    3. The mode of learning - classification or regression (using the set_mode() function)
#'    4. Any arguments necessary for the model/package selected (using the set_args()function 
#'      - for example the mtry = argument for random forest which is the number of variables to 
#'      be used as options for splitting at each tree node)
#'  
#' Let’s walk through these steps one by one. For our case, we are going to start our analysis 
#' with a linear regression but we will demonstrate how we can try different models.
#' The first step is to define what type of model we would like to use.
#' 
#' We want to do a linear regression so we will use the linear_reg() function of the parsnip 
#' package:
# install.packages("parsnip")
library(parsnip)
Lin_reg_model <- parsnip::linear_reg()
Lin_reg_model 
#' 
#' OK. So far, all we have defined is that we want to use a linear regression. Now let’s tell 
#' parsnip more about what we want.
#' We would like to use the ordinary least squares method to fit our linear regression. So we 
#' will tell parsnip that we want to use the lm package to implement our linear regression 
#' (there are many options actually such as rstan, glmnet, keras, and sparklyr).
#' We will do so by using the set_engine() function of the parsnip package:
Lin_reg_model <- 
  Lin_reg_model  %>%
  parsnip::set_engine("lm")

Lin_reg_model
#' 
#' Some packages can do either classification or regression, so it is a good idea to specify 
#' which mode you intend to perform. Here, we aim to predict a continuous variable, thus we 
#' want to perform a regression analysis. You can do this with the set_mode() function of the 
#' parsnip package, by using either set_mode("classification") or set_mode("regression").
Lin_reg_model <- 
  Lin_reg_model %>%
  parsnip::set_engine("lm") %>%
  parsnip::set_mode("regression")

Lin_reg_model
#' 
#' 
#' 
#' -----------------------------------------------------------------------------
#' -----------------------------------------------------------------------------
#' -----------------------------------------------------------------------------
#' STEP 5: Fit the model 
#' We can use the parsnip package with a newer package called workflows to fit our model.
#' 
#' The workflows package allows us to keep track of both our preprocessing steps and our model 
#' specification. It also allows us to implement fancier optimizations in an automated way and 
#' it can also handle post-processing operations.
#' 
#' We begin by creating a workflow using the workflow() function in the workflows package.
#' 
#' Next, we use add_recipe() (our preprocessing specifications) and we add our model with the 
#' add_model() function – both functions from the workflows package.
#' 
#' Note: We do not need to actually prep() our recipe before using workflows - this was just 
#' optional so we could take a look at the preprocessed data!
#' 
# install.packages("workflows")
library(workflows)
iris_reg_wflow <-workflows::workflow() %>%
  workflows::add_recipe(first_recipe) %>%
  workflows::add_model(Lin_reg_model)
iris_reg_wflow 
#' 
#' Ah, nice. Notice how it tells us about both our preprocessing steps and our model 
#' specifications.
#' Next, we “prepare the recipe” (or estimate the parameters) and fit the model to our training 
#' data all at once. Printing the output, we can see the coefficients of the model.
iris_reg_wflow_fit <- parsnip::fit(iris_reg_wflow, data = training_iris)
iris_reg_wflow_fit
#' 
#' 
#' 
#' -----------------------------------------------------------------------------
#' -----------------------------------------------------------------------------
#' -----------------------------------------------------------------------------
#' STEP 6: Assessing the model performance
#' Recall that often for regression analysis we use the RMSE to assess model performance.
#' To get this we first need to get the predicted (also called “fitted”) values.
#' We can get these values using the pull_workflow_fit() function of the workflows package. 
#' These values are in the $fit$fitted.values slot of the output. Alternatively, we can use 
#' the predict() function with the workflow and the training data specified as the new_data.
library(workflows)
wf_fit <- iris_reg_wflow_fit %>% 
  pull_workflow_fit()

head(wf_fit$fit$fitted.values) 
#' 
predict(iris_reg_wflow_fit, new_data = training_iris) 
#' 
#' To get more information about the prediction for each sample, we can use the augment() 
#' function of the broom package. This requires using the preprocessed training data from 
#' bake() (or with previous versions juice()), as well as the predicted values from either of 
#' the two previous methods.
wf_fitted_values <- 
  broom::augment(wf_fit$fit, data = preproc_train) %>% 
  select(Sepal.Length, .fitted:.std.resid)

head(wf_fitted_values)
#' 
#' Nice, now we can see what the original value for Sepal.Length right next to the predicted 
#' .fitted value, as well as standard errors and other metrics for each value.
#' Now we can use the rmse() function of the yardstick package to compare the truth, which is 
#' the Sepal.Length variable, to the predicted or estimate variable which in the previous 
#' output is called .fitted.
install.packages("yardstick")
library(yardstick)
yardstick::rmse(wf_fitted_values, 
                truth = Sepal.Length, 
                estimate = .fitted)
#' 
#' We can see that our RMSE was 0.447. This is fairly low, so our model did pretty well.
#' We can also make a plot to visualize how well we predicted Sepal.Length:
library(ggplot2)
wf_fitted_values %>%
  ggplot(aes(x = Sepal.Length, y = .fitted)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs( x = "True Sepal Length", y = "Predicted Sepal Length")
#' 
#' We can see that overall our model predicted the sepal length fairly well, as the predicted 
#' values are fairly close to the true values. We can also see that the predictions were 
#' similar to the truth for the full range of true sepal length values.
#' 
#' Typically we might modify our preprocessing steps or try a different model until we were 
#' satisfied with the performance on our training data. Assuming we are satisfied, we could 
#' then perform a final assessment of our model using the testing data.
#' 
#' With the workflows package, we can use the splitting information for our original data 
#' split_iris to fit the final model on the full training set and also on the testing data 
#' using the last_fit() function of the tune package. No preprocessing steps are required.
#' 
#' We can do this by using the last_fit() function of the tune package.
install.packages("tune")
library(tune)
overallfit <-iris_reg_wflow %>%
  tune::last_fit(split_iris)

overallfit
#' 
#' We can then use the collect_metrics() function of the tune package to get the RMSE:
collect_metrics(overallfit)
#' With an RMSE of 0.403, we can see that our RMSE is pretty similar for the testing data as 
#' well.
#' 
#' 
#' 
#' 