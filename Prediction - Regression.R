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
#' E.g.: predict Sepal.Length in the training data based on Sepal.Width and the Species. Thus, 
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
#'        classes etc.
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
#' Great! Now back to the typical steps.
#' 
#' 
#' 
#' 
#' 
#' 
#' 