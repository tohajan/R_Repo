#' **PREDICTION - CASE STUDY #1 (COURSERA)**
library(tidymodels)
#' A variety of different sources contribute different types of pollutants to what we call air 
#' pollution.
#'    Gaseous - Carbon Monoxide (CO), Ozone (O3), Nitrogen Oxides(NO, NO2), Sulfur Dioxide (SO2)
#'    Particulate - small liquids and solids suspended in the air (includes lead- can include 
#'      certain types of dust)
#'    Dust - small solids (larger than particulates) that can be suspended in the air for some 
#'      time but eventually settle
#'    Biological - pollen, bacteria, viruses, mold spores
#'    
#' Air pollution particulates are generally described by their size. There are 3 major 
#' categories:
#'   1. Large Coarse Particulate Matter - has diameter of >10 micrometers (10 µm)
#'   2. Coarse Particulate Matter (called PM10-2.5) - has diameter of between 2.5 µm and 10 µm
#'   3. Fine Particulate Matter (called PM2.5) - has diameter of < 2.5 µm
#'   
#' PM10 includes any particulate matter <10 µm (both coarse and fine particulate matter)
#' 
#' Of these different sizes, fine particulate matter air pollution is the most associated with 
#' health risk.
#' 
#' In this case study, we will use fine particulate matter air pollution monitor data from 
#' gravimetric monitors operated by the US Environmental Protection Agency (EPA). Roughly 90% 
#' of these monitors are located within cities. Hence, there is limited data about air 
#' pollution levels of more rural areas.
#' 
#' To get a better sense of the pollution exposures for the individuals living in these areas, 
#' methods like machine learning can be useful to estimate air pollution levels in areas with 
#' little to no monitoring.
#' 
#' We will use data like population density, road density, among other features, to build a 
#' model to predict the known monitored fine particulate air pollution levels. Such a model 
#' can then be used to estimate levels of pollution in places with poor monitoring.
#' 
#' -----------------------------------------------------------------------------
#' -----------------------------------------------------------------------------
#' -----------------------------------------------------------------------------
#' THE DATA
#' There are 48 predictors with values for 876 monitors (observations) - see descriptions in 
#' the attached pdf in the dataset folder. 
#' The data comes from the US Enivornmental Protection Agency (EPA), the National Aeronautics 
#' and Space Administration (NASA), the US Census, and the National Center for Health 
#' Statistics (NCHS).
#' 
#' We have one CSV file that contains both our single outcome variable and all of our features 
#' (or predictor variables). 
#' 
#' Next, we import our data into R now so that we can explore the data further. We will call our 
#' data object pm for particulate matter. We import the data using the read_csv() function 
#' from the readr package.
# install.packages("here")
# library(here)
# pm <- readr::read_csv(here("data","tidy_data","pm25_data.csv"))
#'  ... I need to find out more about the "here" library. For now, I'll just read in the data
#'  from my PC:
pm <- readr::read_csv("G:/My Drive/Programming/Data Science/DATA SETS/pm data/pm25_data.csv")
View(pm)
head(pm)
#' 
#' 
#' -----------------------------------------------------------------------------
#' -----------------------------------------------------------------------------
#' -----------------------------------------------------------------------------
#' DATA EXPLORATION AND WRANGLING
#' First, let’s just get a general sense of our data.
library(dplyr)
pm %>%
  glimpse()
#' 
#' We can see that there are 876 monitors (rows) and that we have 50 total variables (columns) 
#' - one of which is the outcome variable. In this case, the outcome variable is named value.
#' 
#' Notice that some of the variables that we would think of as factors (or categorical data) 
#' are currently of class character as indicated by the <chr> just to the right of the column 
#' names/variable names in the glimpse() output. This means that the variable values are 
#' character strings, such as words or phrases.
#' 
#' The other variables are of class <dbl>, which stands for double precision which indicates 
#' that they are numeric and that they have decimal values. In contrast, one could have integer 
#' values which would not allow for decimal numbers.
#' 
#' Another common data class is factor which is abbreviated like this: <fct>. A factor is 
#' something that has unique levels but there is no appreciable order to the levels. For 
#' example we can have a numeric value that is just an id that we want to be interpreted as 
#' just a unique level and not as the number that it would typically indicate. This would be 
#' useful for several of our variables:
#'    1. the monitor ID (id)
#'    2. the Federal Information Processing Standard number for the county where the monitor 
#'      was located (fips)
#'    3. the zip code tabulation area (zcta)
#' None of the values actually have any real numeric meaning, so we want to make sure that R 
#' does not interpret them as if they do.
#' So let’s convert these variables into factors. We can do this using the across() function of 
#' the dplyr package and the as.factor() base function. The across() function has two main 
#' arguments: (i) the columns you want to operate on and (ii) the function or list of functions 
#' to apply to each column.
library(magrittr)
pm <-pm %>%
  mutate(across(c(id, fips, zcta), as.factor)) 

glimpse(pm)
#' Great! Now we can see that these variables are now factors as indicated by <fct> after the 
#' variable name.
#' 
#' The skim() function of the skimr package is also really helpful for getting a general sense 
#' of your data. By design, it provides summary statistics about variables in the dataset:
library(skimr)
skim(pm)
#' 
#' Notice how there is a column called n_missing about the number of values that are missing. 
#' This is also indicated by the complete_rate variable (or missing/number of observations).
#' In this dataset, it looks like there are no missing data.
#' 
#' Also notice how the function provides separate tables of summary statistics for each data 
#' type: character, factor and numeric.
#' Next, the n_unique column shows us the number of unique values for each of the columns. We 
#' can see that there are 49 states represented in the data.
#' We can see that for many variables there are many low values as the distribution shows two 
#' peaks, one near zero and another with a higher value.
#' This is true for the imp variables (measures of development), the nei variables (measures 
#' of emission sources) and the road density variables.
#' We can also see that the range of some of the variables is very large, in particular the 
#' area and population related variables.
#' Let’s take a look to see which states are included using the distinct() function of the 
#' dplyr package:
pm %>% 
  distinct(state) 
#' 
#' 
#' -----------------------------------------------------------------------------
#' -----------------------------------------------------------------------------
#' -----------------------------------------------------------------------------
#' EVALUATE CORRELATION
#' In prediction analyses, it is also useful to evaluate if any of the variables are 
#' correlated. Why should we care about this?
#' If we are using a linear regression to model our data then we might run into a problem 
#' called multicollinearity which can lead us to misinterpret what is really predictive of 
#' our outcome variable. This phenomenon occurs when the predictor variables actually predict 
#' one another.
#' 
#' Another reason we should look out for correlation is that we don’t want to include 
#' redundant variables. This can add unnecessary noise to our algorithm causing a reduction in 
#' prediction accuracy and it can cause our algorithm to be unnecessarily slower. Finally, it 
#' can also make it difficult to interpret what variables are actually predictive.
#' 
#' Intuitively, we can expect some of our variables to be correlated.
#' 
#' The corrplot package is another option to look at correlation among possible predictors, 
#' and particularly useful if we have many predictors.
#' 
#' First, we calculate the Pearson correlation coefficients between all features pairwise 
#' using the cor() function of the stats package (which is loaded automatically). Then we use 
#' the corrplot::corrplot() function. First we need to select only the numeric variables using 
#' dplyr.
install.packages("corrplot")
library(corrplot)
PM_cor <- cor(pm %>% dplyr::select_if(is.numeric))
corrplot::corrplot(PM_cor, tl.cex = 0.5)
#' 
#' We can see that the development variables (imp) variables are correlated with each other as 
#' we might expect. We also see that the road density variables seem to be correlated with 
#' each other, and the emission variables seem to be correlated with each other.
#' 
#' Also notice that none of the predictors are highly correlated with our outcome variable 
#' (value).
#' Now that we have a sense of what our data are, we can get started with building a machine 
#' learning model to predict air pollution.
#' 
#' 
#' -----------------------------------------------------------------------------
#' -----------------------------------------------------------------------------
#' -----------------------------------------------------------------------------
#' SPLITTING THE DATA
set.seed(1234)
pm_split <- rsample::initial_split(data = pm, prop = 2/3)
pm_split
#' We can see the number of monitors in our training, testing, and original data by typing in 
#' the name of our split object. The result will look like this: <training data sample number, 
#' testing data sample number, original (total) sample number>
#' Importantly the initial_split() function only determines what rows of our pm data frame 
#' should be assigned for training or testing, it does not actually split the data.
#' 
#' To extract the testing and training data we can use the training() and testing() functions 
#' also of the rsample package.
train_pm <- rsample::training(pm_split)
test_pm <- rsample::testing(pm_split)
#' Great!
#' Now let’s make a recipe!
#' 
#' 
#' -----------------------------------------------------------------------------
#' -----------------------------------------------------------------------------
#' -----------------------------------------------------------------------------
#' MAKING A RECIPE
#' Now with our data, we will start by making a recipe for our training data. If you recall, 
#' the continuous outcome variable is value (the average annual gravimetric monitor PM2.5 
#' concentration in ug/m3). Our features (or predictor variables) are all the other variables 
#' except the monitor ID, which is an id variable.
#' The reason not to include the id variable is because this variable includes the county 
#' number and a number designating which particular monitor the values came from (of the 
#' monitors there are in that county). Since this number is arbitrary and the county 
#' information is also given in the data, and the fact that each monitor only has one value in 
#' the value variable, nothing is gained by including this variable and it may instead 
#' introduce noise. However, it is useful to keep this data to take a look at what is 
#' happening later. We will show you what to do in this case in just a bit.
#' 
#' In the simplest case, we might use all predictors like this:
simple_rec <- train_pm %>%
  recipes::recipe(value ~ .)

simple_rec
#' 
#' Now, let’s get back to the id variable. Instead of including it as a predictor variable, 
#' we could also use the update_role() function of the recipes package.
simple_rec <- train_pm %>%
  recipes::recipe(value ~ .) %>%
  recipes::update_role(id, new_role = "id variable")

simple_rec
#' 
#' This link (https://recipes.tidymodels.org/reference/index.html) and this link 
#' (https://cran.r-project.org/web/packages/recipes/recipes.pdf) show the many options for 
#' recipe step functions.
#' 
#' There are several ways to select what variables to apply steps to:
#'    1. Using tidyselect methods: contains(), matches(), starts_with(), ends_with(), 
#'  everything(), num_range()
#'    2. Using the type: all_nominal(), all_numeric() , has_type()
#'    3. Using the role: all_predictors(), all_outcomes(), has_role()
#'    4. Using the name - use the actual name of the variable/variables of interest
#' 
#' Let’s try adding some steps to our recipe.
#' We want to dummy encode our categorical variables so that they are numeric as we plan to 
#' use a linear regression for our model.
#' 
#' We will use the one-hot encoding means that we do not simply encode our categorical 
#' variables numerically, as our numeric assignments can be interpreted by algorithms as 
#' having a particular rank or order. Instead, binary variables made of 1s and 0s are used to 
#' arbitrarily assign a numeric value that has no apparent order.
simple_rec %>%
  recipes::step_dummy(state, county, city, zcta, one_hot = TRUE)
#' 
#' Our fips variable includes a numeric code for state and county - and therefore is 
#' essentially a proxy for county. Since we already have county, we will just use it and keep 
#' the fips ID as another ID variable.
#' 
#' We can remove the fips variable from the predictors using update_role() to make sure that 
#' the role is no longer "predictor".
#' We can make the role anything we want actually, so we will keep it something identifiable.
simple_rec %>%
  recipes::update_role("fips", new_role = "county id")
#' 
#' We also want to remove variables that appear to be redundant and are highly correlated with 
#' others, as we know from our exploratory data analysis that many of our variables are 
#' correlated with one another. We can do this using the step_corr() function.
#' 
#' We don’t want to remove some of our variables, like the CMAQ and aod variables, we can 
#' specify this using the - sign before the names of these variables like so:
simple_rec %>%
  recipes::step_corr(all_predictors(), - CMAQ, - aod)
#' 
#' It is also a good idea to remove variables with near-zero variance, which can be done with 
#' the step_nzv() function.
#' Variables have low variance if all the values are very similar, the values are very sparse, 
#' or if they are highly imbalanced. Again we don’t want to remove our CMAQ and aod variables.
simple_rec %>%
  recipes::step_nzv(all_predictors(), - CMAQ, - aod)
#' 
#' Let’s put all this together now.
#' 
#' Remember: it is important to add the steps to the recipe in an order that makes sense just 
#' like with a cooking recipe.
#' 
#' First, we are going to create numeric values for our categorical variables, then we will 
#' look at correlation and near-zero variance. Again, we do not want to remove the CMAQ and 
#' aod variables, so we can make sure they are kept in the model by excluding them from those 
#' steps. If we specifically wanted to remove a predictor we could use step_rm().
simple_rec <- train_pm %>%
  recipes::recipe(value ~ .) %>%
  recipes::update_role(id, new_role = "id variable") %>%
  recipes::update_role("fips", new_role = "county id") %>%
  recipes::step_dummy(state, county, city, zcta, one_hot = TRUE) %>%
  recipes::step_corr(all_predictors(), - CMAQ, - aod)%>%
  recipes::step_nzv(all_predictors(), - CMAQ, - aod)

simple_rec
#' 
#' Nice! Now let’s check our preprocessing.
#' 
#' 
#' -----------------------------------------------------------------------------
#' -----------------------------------------------------------------------------
#' -----------------------------------------------------------------------------
#' RUNNING PREPROCESSING
#' First we need to use the prep() function of the recipes package to prepare for 
#' preprocessing. However, we will specify that we also want to run and retain the 
#' preprocessing for the training data using the retain = TRUE argument.
library(recipes)
prepped_rec <- prep(simple_rec, verbose = TRUE, retain = TRUE)

names(prepped_rec)
#' 
#' Since we retained our preprocessed training data (i.e. prep(retain=TRUE)), we can take a 
#' look at it by using the bake() function of the recipes package like this (this previously 
#' used the juice() function):
preproc_train <- bake(prepped_rec, new_data = NULL)
glimpse(preproc_train)
#' 
#' Notice that this requires the new_data = NULL argument when we are using the training data.
#' For easy comparison sake - here is our original data:
glimpse(pm)
#' 
#' Notice how we only have 36 variables now instead of 50! Two of these are our ID variables 
#' (fips and the actual monitor ID (id)) and one is our outcome (value). Thus we only have 
#' 33 predictors now. We can also see that we no longer have any categorical variables. 
#' Variables like state are gone and only state_California remains as it was the only state 
#' identity to have nonzero variance. We can also see that there were more monitors listed as 
#' "Not in a city" than any city.
#' 
#' -----------------------------------------------------------------------------
#' Extract preprocessed testing data using bake()
#' According to the tidymodels documentation:
#'    "bake() takes a trained recipe and applies the operations to a data set to create a 
#'    design matrix. For example: it applies the centering to new data sets using these means 
#'    used to create the recipe."
#' 
#' Therefore, if you wanted to look at the preprocessed testing data you would use the bake() 
#' function of the recipes package.
baked_test_pm <- bake(prepped_rec, new_data = test_pm)
glimpse(baked_test_pm)
#' Notice that our city_Not.in.a.city variable seems to be NA values. Why might that be?
#' Ah! Perhaps it is because some of our levels were not previously seen in the training set!
#' 
#' Let’s take a look using the set operations of the dplyr package. We can take a look at 
#' cities that were different between the test and training set.
traincities <- train_pm %>% distinct(city)
testcities <- test_pm %>% distinct(city)
#' #get the number of cities that were different:
dim(dplyr::setdiff(traincities, testcities)) # Results = 381 cities
#get the number of cities that overlapped:
dim(dplyr::intersect(traincities, testcities)) # Results = 55 cities
#' So, there are lots of different cities in our test data that are not in our training data!
#' 
#' So, let go back to our pm dataset and modify the city variable to just be values of in a 
#' city or not in a city using the case_when() function of dplyr. This function allows you to 
#' vectorize multiple if_else() statements.
pm %>%
  mutate(city = case_when(city == "Not in a city" ~ "Not in a city",
                          city != "Not in a city" ~ "In a city"))
#' 
#' Alternatively you could create a custom step function to do this and add this to your 
#' recipe, but that is beyond the scope of this case study.
#' 
#' We will need to repeat all the steps (splitting the data, preprocessing, etc) as the levels 
#' of our variables have now changed.
#' 
#' While we are doing this, we might also have this issue for county. The county variables 
#' appears to get dropped due to either correlation or near zero variance.
#' It is likely due to near zero variance because this is the more granular of these 
#' geographic categorical variables and likely sparse
pm %<>% 
  mutate(city = case_when(city == "Not in a city" ~ "Not in a city",
                          city != "Not in a city" ~ "In a city"))
#' NB: %<>% is known as the compound assignment operator, and it uses the following operation 
#' (mutate() in this example) to permanently change the value of the called object (pm in this 
#' example). Compare with the code just above, which uses %>% (this temporarily changes the 
#' object, not globally)
#' In a way, the latter helps to preview the results of an operation on an object, while still
#' retaining the object's actual value. One can then apply the former to make the actual 
#' /permanent change
#' 
set.seed(1234) # same seed as before
pm_split <-rsample::initial_split(data = pm, prop = 2/3)
pm_split

train_pm <-rsample::training(pm_split)
test_pm <-rsample::testing(pm_split)
#' 
#' Now we will create a new recipe:
novel_rec <-train_pm %>%
  recipe() %>%
  update_role(everything(), new_role = "predictor") %>%
  update_role(value, new_role = "outcome") %>%
  update_role(id, new_role = "id variable") %>%
  update_role("fips", new_role = "county id") %>%
  step_dummy(state, county, city, zcta, one_hot = TRUE) %>%
  step_corr(all_numeric()) %>%
  step_nzv(all_numeric()) 

novel_rec
summary(novel_rec)
#' 
#' 
#' Now we will check the preprocessed data again to see if we still have NA values.
prepped_rec <- prep(novel_rec, verbose = TRUE, retain = TRUE)

names(prepped_rec)
#' 
preproc_train <- bake(prepped_rec, new_data = NULL)
glimpse(preproc_train)
#' 
baked_test_pm <- bake(prepped_rec, new_data = test_pm)
glimpse(baked_test_pm)
#' 
#' Great, now we no longer have NA values!
#' 
#' 
#' -----------------------------------------------------------------------------
#' -----------------------------------------------------------------------------
#' -----------------------------------------------------------------------------
#' SPECIFYING THE MODEL
#' The first step is to define what type of model we would like to use. For our case, we are 
#' going to start our analysis with a linear regression but we will demonstrate how we can try 
#' different models.
PM_model <- parsnip::linear_reg() # PM was used in the name for particulate matter
PM_model
#' 
#' OK. So far, all we have defined is that we want to use a linear regression…Let’s tell 
#' parsnip more about what we want.
#' We would like to use the ordinary least squares method to fit our linear regression. So we 
#' will tell parsnip that we want to use the lm package to implement our linear regression 
#' (there are many options actually such as rstan, glmnet, keras, and sparklyr)
#' 
#' We will do so by using the set_engine() function of the parsnip package.
lm_PM_model <- 
  PM_model  %>%
  parsnip::set_engine("lm")

lm_PM_model
#' 
#' Here, we aim to predict the air pollution. You can do this with the set_mode() function of 
#' the parsnip package, by using either set_mode("classification") or set_mode("regression").
lm_PM_model <- 
  PM_model  %>%
  parsnip::set_engine("lm") %>%
  parsnip::set_mode("regression")

lm_PM_model
#' 
#' Now we will use the workflows package to keep track of both our preprocessing steps and our 
#' model specification. It also allows us to implement fancier optimizations in an automated 
#' way.
#' If you recall novel_rec is the recipe we previously created with the recipes package and 
#' lm_PM_model was created when we specified our model with the parsnip package. Here, we 
#' combine everything together into a workflow.
PM_wflow <-workflows::workflow() %>%
  workflows::add_recipe(novel_rec) %>%
  workflows::add_model(lm_PM_model)
PM_wflow
#' 
#' Ah, nice. Notice how it tells us about both our preprocessing steps and our model 
#' specifications.
#' 
#' Next, we “prepare the recipe” (or estimate the parameters) and fit the model to our 
#' training data all at once. Printing the output, we can see the coefficients of the model.
PM_wflow_fit <- parsnip::fit(PM_wflow, data = train_pm)
PM_wflow_fit
#' 
#' 
#' -----------------------------------------------------------------------------
#' -----------------------------------------------------------------------------
#' -----------------------------------------------------------------------------
#' ASSESSING THE MODEL FIT
#' After we fit our model, we can use the broom package to look at the output from the fitted 
#' model in an easy/tidy way.
#' The tidy() function returns a tidy data frame with coefficients from the model (one row per 
#' coefficient).
#' Many other broom functions currently only work with parsnip objects, not raw workflows 
#' objects. However, we can use the tidy function if we first use the pull_workflow_fit() 
#' function.
wflowoutput <- PM_wflow_fit %>% 
  workflows::pull_workflow_fit() %>% 
  broom::tidy() 
wflowoutput
#' 
#' We have fit our model on our training data, which means we have created a model to predict 
#' values of air pollution based on the predictors that we have included. Yay!
#' 
#' One last thing before we leave this section. We often are interested in getting a sense of 
#' which variables are the most important in our model. We can explore the variable importance 
#' using the vip() function of the vip package. This function creates a bar plot of variable 
#' importance scores for each predictor variable (or feature) in a model. The bar plot is 
#' ordered by importance (highest to smallest).
#' 
#' Notice again that we need to use the pull_workflow_fit() function. 
#' Let’s take a look at the top 10 contributing variables:
# install.packages("vip")
library(vip)

PM_wflow_fit %>% 
  workflows::pull_workflow_fit() %>% 
  vip(num_features = 10)
#' 
#' Results: The state in which the monitor was located (if it was in California or not) and the 
#' CMAQ model appear to be the most important for predicting the air pollution at a given 
#' monitor.
#' 
#' 
#' -----------------------------------------------------------------------------
#' -----------------------------------------------------------------------------
#' -----------------------------------------------------------------------------
#' MODEL PERFORMANCE: GETTING PREDICTED VALUES
#' In this next section, our goal is to assess the overall model performance. The way we do 
#' this is to compare the similarity between the predicted estimates of the outcome variable 
#' produced by the model and the true outcome variable values.
#' 
#' Machine learning (ML) is an optimization problem that tries to minimize the distance between 
#' our predicted outcome and actual outcome using our features (or predictor variables) as 
#' input to a function that we want to estimate.
#' 
#' First, let’s pull out our predicted outcome values from the models we fit (using different 
#' approaches).
wf_fit <- PM_wflow_fit %>% 
  workflows::pull_workflow_fit()

wf_fitted_values <- wf_fit$fit$fitted.values
head(wf_fitted_values)
#' 
#' Alternatively, we can get the fitted values using the augment() function of the broom 
#' package using the output from workflows:
wf_fitted_values <- 
  broom::augment(wf_fit$fit, data = preproc_train) %>% 
  select(value, .fitted:.std.resid)

head(wf_fitted_values)
#' 
#' Finally, we can also use the predict() function. Note that because we use the actual 
#' workflow here, we can (and actually need to) use the raw data instead of the preprocessed 
#' data.
values_pred_train <- 
  predict(PM_wflow_fit, train_pm) %>% 
  bind_cols(train_pm %>% select(value, fips, county, id)) 

values_pred_train
#' 
yardstick::metrics(values_pred_train, truth = value, estimate = .pred)
#' 
#' 
#' -----------------------------------------------------------------------------
#' -----------------------------------------------------------------------------
#' -----------------------------------------------------------------------------
#' VISUALIZING MODEL PERFORMANCE
#' Now, we can compare the predicted outcome values (or fitted values)  to the actual outcome 
#' values that we observed:
library(ggplot2)
wf_fitted_values %>% 
  ggplot(aes(x =  value, y = .fitted)) + 
  geom_point() + 
  xlab("actual outcome values") + 
  ylab("predicted outcome values")
#' 
#' OK, so our range of the predicted outcome values appears to be smaller than the real values. 
#' We could probably do a bit better.
#' 
#' 
#' -----------------------------------------------------------------------------
#' -----------------------------------------------------------------------------
#' -----------------------------------------------------------------------------
#' QUANTIFYING MODEL PERFORMANCE
#' Next, let’s use different distance functions to assess how far off our predicted outcome 
#' and actual outcome values are from each other:
#' There are entire scholarly fields of research dedicated to identifying different distance 
#' metrics for machine learning applications. However, we will focus on root mean squared error
#' (rmse).
#' One way to calculate these metrics within the tidymodels framework is to use the yardstick 
#' package using the metrics() function.
yardstick::metrics(wf_fitted_values, 
                   truth = value, estimate = .fitted)
#' 
# A tibble: 3 × 3
# .metric .estimator .estimate
# <chr>   <chr>          <dbl>
#   1 rmse    standard       1.98 
# 2 rsq     standard       0.392
# 3 mae     standard       1.47 
#' 
#' Here we see the RMSE in addition to the RSQ or the R squared value also known as the 
#' coefficient of determination and the MAE, which stands for the mean absolute error.
#' Alternatively if you only wanted one metric you could use the mae(), rsq(), or rmse() 
#' functions, respectively.
yardstick::rmse(wf_fitted_values, 
                truth = value, estimate = .fitted)
#' 
#' 
#' 
#' -----------------------------------------------------------------------------
#' -----------------------------------------------------------------------------
#' -----------------------------------------------------------------------------
#' ASSESSING MODEL PERFORMANCE ON V-FOLDS (using tune)
#' We also intend to perform cross validation, so we will now split the training data further 
#' using the vfold_cv() function of the rsample package.
#' Again, because these are created at random, we need to use the base set.seed() function in 
#' order to obtain the same results each time. We will create 10 folds.
set.seed(1234)
vfold_pm <- rsample::vfold_cv(data = train_pm, v = 10)
vfold_pm
#' 
pull(vfold_pm, splits)
#' 
#' We can fit the model to our cross validation folds using the fit_resamples() function of 
#' the tune package, by specifying our workflow object and the cross validation fold object we 
#' just created.
set.seed(122)
resample_fit <- tune::fit_resamples(PM_wflow, vfold_pm)
#' 
#' We can now take a look at various performance metrics based on the fit of our cross 
#' validation “resamples”.
#' To do this we will use the collect_metrics() function of the tune package. This will show 
#' us the mean of the accuracy estimate of the 4 different cross validation folds.
resample_fit
#' 
tune::collect_metrics(resample_fit)
#' 
#' # A tibble: 2 × 6
# .metric .estimator  mean     n std_err .config             
# <chr>   <chr>      <dbl> <int>   <dbl> <chr>               
#   1 rmse    standard   2.09     10  0.123  Preprocessor1_Model1
# 2 rsq     standard   0.321    10  0.0357 Preprocessor1_Model1
#' 
#' Recall that the mean accuracy estimate will often be lower than a single accuracy estimate.
#' Here, the mean RSQ is 0.321, a drop from 0.392 of the preceding single model
#' 
#' 
#' 
#' 
#' 
#' 
#' -----------------------------------------------------------------------------
#' -----------------------------------------------------------------------------
#' -----------------------------------------------------------------------------
#' -----------------------------------------------------------------------------
#' -----------------------------------------------------------------------------
#' -----------------------------------------------------------------------------
#' -----------------------------------------------------------------------------
#' -----------------------------------------------------------------------------
#' -----------------------------------------------------------------------------
#' -----------------------------------------------------------------------------
#' -----------------------------------------------------------------------------
#' -----------------------------------------------------------------------------
#' -----------------------------------------------------------------------------
#' -----------------------------------------------------------------------------
#' -----------------------------------------------------------------------------
#' -----------------------------------------------------------------------------
#' -----------------------------------------------------------------------------
#' -----------------------------------------------------------------------------
#' -----------------------------------------------------------------------------
#' -----------------------------------------------------------------------------
#' -----------------------------------------------------------------------------
#' -----------------------------------------------------------------------------
#' -----------------------------------------------------------------------------
#' -----------------------------------------------------------------------------
#' 
#' In the previous sections, we demonstrated how to build a machine learning model 
#' (specifically a linear regression model) to predict air pollution with the tidymodels 
#' framework. In the next few section, we will demonstrate another machine learning model.
#' 
#' -----------------------------------------------------------------------------
#' RANDOM FOREST
#' Now, we are going to predict our outcome variable (air pollution) using a decision tree 
#' method called random forest.
#' 
#' In the case of random forest, multiple decision trees are created - hence the name forest, 
#' and each tree is built using a random subset of the training data (with replacement) - 
#' hence the full name random forest. This random aspect helps to keep the algorithm from 
#' overfitting the data.
#' The mean of the predictions from each of the trees is used in the final output.
#' 
#' In our case, we are going to use the random forest method of the the randomForest package.
#' This package is currently not compatible with categorical variables that have more than 53 
#' levels. See https://cran.r-project.org/web/packages/randomForest/NEWS for the documentation 
#' about when this was updated from 25 levels. 
#' Thus we will remove the zcta and county variables.
#' 
#' Note that the step_novel() function is necessary here for the state variable to get all 
#' cross validation folds to work, because there will be different levels included in each 
#' fold test and training sets. Thus there are new levels for some of the test sets which 
#' would otherwise result in an error.
#' According to the documentation for the recipes package:
#'    "step_novel creates a specification of a recipe step that will assign a previously 
#'    unseen factor level to a new value."
RF_rec <- recipe(train_pm) %>%
  update_role(everything(), new_role = "predictor")%>%
  update_role(value, new_role = "outcome")%>%
  update_role(id, new_role = "id variable") %>%
  update_role("fips", new_role = "county id") %>%
  step_novel("state") %>%
  step_string2factor("state", "county", "city") %>%
  step_rm("county") %>%
  step_rm("zcta") %>%
  step_corr(all_numeric())%>%
  step_nzv(all_numeric())
#' 
#' The rand_forest() function of the parsnip package has three important arguments that act as 
#' an interface for the different possible engines to perform a random forest analysis:
#'    1. mtry - The number of predictor variables (or features) that will be randomly sampled 
#'      at each split when creating the tree models. The default number for regression analyses 
#'      is the number of predictors divided by 3.
#'    2. min_n - The minimum number of data points in a node that are required for the node to 
#'      be split further.
#'    3. trees - the number of trees in the ensemble
#'  
#' We will start by trying an mtry value of 10 and a min_n value of 4. 
#' Now that we have our recipe (RF_rec), let’s specify the model with rand_forest() from 
#' parsnip.
PMtree_model <- 
  parsnip::rand_forest(mtry = 10, min_n = 4)
PMtree_model
#' 
#' Next, we set the engine and mode:
#' 
#' Note that you could also use the ranger or spark packages instead of randomForest. If you 
#' were to use the ranger package to implement the random forest analysis you would need to 
#' specify an importance argument to be able to evaluate predictor importance. The options 
#' are impurity or permutation.
#' 
#' These other packages have different advantages and disadvantages- for example ranger and 
#' spark are not as limiting for the number of categories for categorical variables. For more 
#' information see the documentation of each package.
#' 
#' Note that there are also other R packages for implementing random forest algorithms, but 
#' these three packages (ranger, spark, and randomForest) are currently compatible with 
#' tidymodels.
# install.packages("randomForest")
library(randomForest)
#' 
RF_PM_model <- 
  PMtree_model %>%
  set_engine("randomForest") %>%
  set_mode("regression")

RF_PM_model
#' 
#' Then, we put this all together into a workflow:
RF_wflow <- workflows::workflow() %>%
  workflows::add_recipe(RF_rec) %>%
  workflows::add_model(RF_PM_model)
RF_wflow
#' 
#' Finally, we fit the data to the model:
RF_wflow_fit <- parsnip::fit(RF_wflow, data = train_pm)
RF_wflow_fit
#' 
#' Now, we will look at variable importance:
RF_wflow_fit %>% 
  workflows::pull_workflow_fit() %>% 
  vip(num_features = 10)
#' 
#' Interesting! In the previous model the CMAQ values and the state where the monitor was 
#' located were also the top two most important, however predictors about education levels of 
#' the communities where the monitor was located was among the top most important. Now we see 
#' that population density and proximity to sources of emissions and roads are among the top 
#' ten.
#' 
#' Now let’s take a look at model performance by fitting the data using cross validation:
set.seed(456)
resample_RF_fit <- tune::fit_resamples(RF_wflow, vfold_pm)
tune::collect_metrics(resample_RF_fit)
#' 
#' OK, so the first model had a mean rmse value of 2.09. It looks like the random forest model 
#' had a much lower rmse value of 1.60.
#' 
#' If we tuned our random forest model based on the number of trees or the value for mtry 
#' (which is “The number of predictors that will be randomly sampled at each split when 
#' creating the tree models”), we might get a model with even better performance.
#' 
#' However, our cross validated mean rmse value of 1.60 is quite good because our range of 
#' true outcome values is much larger: (3.024, 22.259).
#' 
#' 
#' -----------------------------------------------------------------------------
#' -----------------------------------------------------------------------------
#' -----------------------------------------------------------------------------
#' MODEL TUNING
#' Hyperparameters are often things that we need to specify about a model. For example, the 
#' number of predictor variables (or features) that will be randomly sampled at each split 
#' when creating the tree models called mtry is a hyperparameter. The default number for 
#' regression analyses is the number of predictors divided by 3. Instead of arbitrarily 
#' specifying this, we can try to determine the best option for model performance by a 
#' process called tuning.
#' 
#' Now let’s try some tuning.
#' Let’s take a closer look at the mtry and min_n hyperparameters in our Random Forest model.
#' 
#' We aren’t exactly sure what values of mtry and min_n achieve good accuracy yet keep our 
#' model generalizable for other data.
#' 
#' This is when our cross validation methods become really handy because now we can test out 
#' different values for each of these hyperparameters to assess what values seem to work best 
#' for model performance on these resamples of our training set data.
#' 
#' Previously we specified our model like so:
RF_PM_model <- 
  parsnip::rand_forest(mtry = 10, min_n = 4) %>%
  set_engine("randomForest") %>%
  set_mode("regression")

RF_PM_model
#' 
#' Now instead of specifying a value for the mtry and min_n arguments, we can use the tune() 
#' function of the tune package like so: mtry = tune(). This indicates that these 
#' hyperparameters are to be tuned.
tune_RF_model <- rand_forest(mtry = tune(), min_n = tune()) %>%
  set_engine("randomForest") %>%
  set_mode("regression")

tune_RF_model
#' 
#' Again we will add this to a workflow, the only difference here is that we are using a 
#' different model specification with tune_RF_model instead of RF_model:
RF_tune_wflow <- workflows::workflow() %>%
  workflows::add_recipe(RF_rec) %>%
  workflows::add_model(tune_RF_model)
RF_tune_wflow
#' 
#' Now we can use the tune_grid() function of the tune package to evaluate different 
#' combinations of values for mtry and min_n using our cross validation samples of our 
#' training set (vfold_pm) to see what combination of values performs best.
#' 
#' To use this function we will specify the workflow using the object argument and the samples 
#' to use using the resamples argument. The grid argument specifies how many possible options 
#' for each argument should be attempted.
#' 
#' By default 10 different values will be attempted for each hyperparameter that is being tuned.
#' We can use the doParallel package to allow us to fit all these models to our cross 
#' validation samples faster. So if you were performing this on a computer with multiple cores 
#' or processors, then different models with different hyperparameter values can be fit to the 
#' cross validation samples simultaneously across different cores or processors.
#' 
#' You can see how many cores you have access to on your system using the detectCores() 
#' function in the parallel package.
library(parallel)
parallel::detectCores()
#' Results: 8
#' 
#' The registerDoParallel() function will use the number for cores specified using the 
#' cores= argument, or it will assign it automatically to one-half of the number of cores 
#' detected by the parallel package.
#' 
#' We need to use set.seed() here because the values chosen for mtry and min_n may vary if we 
#' preform this evaluation again because they are chosen semi-randomly (meaning that they are 
#' within a range of reasonable values but still random).
#' 
#' Note: THIS STEP WILL TAKE SOME TIME.
# install.packages("doParallel")
doParallel::registerDoParallel(cores=2)
set.seed(123)
tune_RF_results <- tune::tune_grid(object = RF_tune_wflow, resamples = vfold_pm, grid = 20)

tune_RF_results
#' 
#' See the tune getting started guide 
#' (https://tune.tidymodels.org/articles/getting_started.html) for more information about 
#' implementing this in tidymodels.
#' 
#' If you wanted more control over this process you could specify the different possible 
#' options for mtry and min_n in the tune_grid() function using the grid_*() functions of the 
#' dials package to create a more specific grid.
#' By default the values for the hyperparameters being tuned are chosen semi-randomly 
#' (meaning that they are within a range of reasonable values but still random).
#' 
#' Now we can use the collect_metrics() function again to take a look at what happened with 
#' our cross validation tests. We can see the different values chosen for mtry and min_n and 
#' the mean rmse and rsq values across the cross validation samples.
#' 
#' 
#' 
#' 
#' 
#' 
#' 
#' 
#' 
#' 
#' 