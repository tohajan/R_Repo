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
#' The data comes from the US Enivornmental Protection Agency (EPA), the National Aeronautics and Space Administration 
#' (NASA), the US Census, and the National Center for Health Statistics (NCHS).
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
#' 
#' 