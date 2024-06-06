#' **PREDICTION - CASE STUDY #1 (COURSERA)**
#' 
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
#' 
#' 
#' 