#' ---
#' title: "Descriptive and Exploratory Analysis"
#' author: "Toheeb"
#' date: "2023-08-27"
#' output: html_document
#' ---
#' 
#' # **Descriptive and Exploratory Analysis (DEA)**
#' 
#' Take a quick glance at the data:
## -----------------------------------------------------------------------------------------------
## load packages
library(tidyverse)
df <- msleep # this data comes from ggplot2
## get a glimpse of your data
glimpse(df)

#' 
#' The glimpse function produces similar output as the str() function:
## -----------------------------------------------------------------------------------------------
str(df)

#' 
#' 
#' Also, since the data is in tibble format, we can gain a lot of information by just viewing the data itself:
## -----------------------------------------------------------------------------------------------
df

#' 
#' The output also provides information about the dimensions of the data object and the name and class of the variables.
#' 
#' 
#' ## Quick Detour -- Short intro to the map function
#' The map function can be used to apply a function to elements of a vector or list and return a list as a result.
## -----------------------------------------------------------------------------------------------
#open the purr library, containing the map function
library(purrr)

#' 
#' ### Vector examples
#' Example 1 (using a function -- the ceiling function is used here):
## -----------------------------------------------------------------------------------------------
x <- c(2.8,5.6,7.2,8.4,1.5, 3.0)
x %>%
  map(ceiling) # return the next higher integer for each element of the vector

#' 
#' Example 2 (using a simple arithmetic):
## -----------------------------------------------------------------------------------------------
num <- c(3,5,8,6,7)
num %>% 
  map(function(x) x^3) #returns the third power of each element

#' 
#' ### List example:
## -----------------------------------------------------------------------------------------------
# list example
ran <- list(c(1, 2, 3), c(4, 5, 6), c(7, 8, NA))
ran %>% 
  map(mean, na.rm = T) # returns the average value of each list

#' 
#' 
#' ## Missing Values
#' Calculate how many NAs there are in each variable:
## -----------------------------------------------------------------------------------------------
# library(purrr) # library already opened in a previous code chunk
df %>% 
        map(is.na)  %>% # identifies the missing values in each variable
        map(sum) # counts the number of missing values identified per variable

#' 
## -----------------------------------------------------------------------------------------------
sum(is.na(df)) # total no. of missing values in the entire data

#' 
#' 
#' Calculate the proportion of missingness for each variable:
## -----------------------------------------------------------------------------------------------
df %>% 
        map(is.na) %>%
        map(sum)%>%
        map(~ . / nrow(df))%>% # number of missing values (identified in the previous line) divided by number of rows in the dataframe
        bind_cols() # returns a df-like results (the list elements are bound/arranged along the column axis --  several columns on a single row )

#' 
#' 
#' We can also visualize missingness in the data (using the vis_miss function in the 'naniar' package):
## -----------------------------------------------------------------------------------------------
#install.packages("naniar")
library(naniar)

vis_miss(df) # visualize missingness

#' 
#' --------------------------------------------------------------------------------------------
#' --------------------------------------------------------------------------------------------
#' --------------------------------------------------------------------------------------------
#' 
#' ## Shape of the data
#' This refers to the data distribution (such as normal/Gaussian, skewed, uniform, etc.)
## -----------------------------------------------------------------------------------------------
#Example 1:
ggplot(df, aes(sleep_total)) +
  geom_density()
# displays the distribution of the sleep_total variable in the sleep data

#' As seen here, the data (of that variable) is somewhat normal but not entirely symmetric.
#' 
## -----------------------------------------------------------------------------------------------
#Example 2 (using the built-in iris dataset):
iris %>% 
  ggplot() +
  geom_density(aes(x=Sepal.Width))

#The previous syntax could also be used:
ggplot(iris, aes(Sepal.Width)) +
  geom_density()

#' As seen, this variable is more normally distributed
#' 
#' 
## -----------------------------------------------------------------------------------------------
#Example 3:
ggplot(df, aes(sleep_rem)) +
  geom_density()
## Warning: Removed 22 rows containing non-finite values (stat_density).

#' The sleep_rem variable (of the sleep dataset) is skewed right (or right-skewed), i.e., the data shifts away from the right, leading to a long right tail. Most of the values are thus at the lower end of the range.
#' 
#' 
#' --------------------------------------------------------------------------------
#' --------------------------------------------------------------------------------
#' --------------------------------------------------------------------------------
#' 
#' ## Identifying Outliers
#' To identify outliers visually, density plots and boxplots can be very helpful.
#' 
#' For example, if we returned to the iris dataset and looked at the distribution of Petal.Length, we would see a bimodal distribution (yet another distribution!). Bimodal distributions can be identified by density plots that have two distinct humps. In these distributions, there are two different modes – this is where the term “bimodal” comes from. In this plot, the curve suggests there are a number of flowers with petal length less than 2 and many with petal length around 5.
#' 
## -----------------------------------------------------------------------------------------------
library(ggplot2)
iris %>%
  ggplot(aes(Petal.Length)) +
  geom_density() #density plot

#' Since the two humps in the plot are about the same height, this shows that it’s not just one or two flowers with much smaller petal lengths, but rather that there are many. Thus, these observations aren’t likely outliers.
#' 
#' To investigate this further, we’ll look at petal length broken down by flower species:
## -----------------------------------------------------------------------------------------------
iris %>%
  ggplot(aes(Species, Petal.Length)) +
  geom_boxplot() #box plot

#' In this boxplot, we see in fact that setosa have a shorter petal length while virginica have the longest. Had we simply removed all the shorter petal length flowers from our dataset, we would have lost information about an entire species!
#' 
#' Boxplots are also helpful because they plot “outlier” samples as points outside the box. By default, boxplots define “outliers” as observations as those that are 1.5 x IQR (interquartile range). The IQR is the distance between the first and third quartiles. This is a mathematical way to determine if a sample may be an outlier. It is visually helpful, but then it’s up to the analyst to determine if an observation should be removed. While the boxplot identifies outliers in the setosa and versicolor species, these values are all within a reasonable distance of the rest of the values, and unless I could determine why this occurred, I would not remove these observations from the dataset.
#' 
#' --------------------------------------------------------------------------------
#' --------------------------------------------------------------------------------
#' --------------------------------------------------------------------------------
#' 
#' ## Evaluating Variables
#' 
#' ### Measures of central tendency.
#' The two most common types are: mean and median
#' 
#' ### Measures of Variability
#' Variance -- this tells you how spread out the values are. If all the values within your variable are exactly the same, that variable’s variance will be zero. The larger your variance, the more spread out your values are. Take the following vector and calculate its variance in R using the var() function:
## -----------------------------------------------------------------------------------------------
## variance of a vector where all values are the same
a <- c(29, 29, 29, 29)
var(a)

## variance of a vector with one very different value
b <-  c(29, 29, 29, 29, 723678)
var(b)

#' The only difference between the two vectors is that the second one has one value that is much larger than “29”. The variance for this vector is thus much higher.
#' 
#' Standard deviation: By definition, the standard deviation is the square root of the variance, thus if we were to calculate the standard deviation in R using the sd() function, we’d see that the sd() function is equal to the square root of the variance:
## -----------------------------------------------------------------------------------------------
## calculate standard deviation:
sd(b)

## this is the same as the square root of the variance:
sqrt(var(b))

#' 
#' ### Summarizing Your Data
#' Often, you’ll want to include tables in your reports summarizing your dataset. These will include the number of observations in your dataset and maybe the mean/median and standard deviation of a few variables. These could be organized into a table.
#' 
#' Alternatively, skimr is a helpful package in this regard:
## -----------------------------------------------------------------------------------------------
# install.packages("skimr")
library(skimr)
skim(df)

#' As seen in the output, the skim() function separately summarizes categorical and continuous variables.  For continuous variables you get information about the mean and median (p50) column. You know what the range of the variable is (p0 is the minimum value, p100 is the maximum value for continuous variables). You also get a measure of variability with the standard deviation (sd). It even quantifies the number of missing values (missing) and shows you the distribution or shape of each variable (hist)! Potential outliers can also be identified from the hist column and the p100 and p0 columns.
#' 
#' If we take a look closer at the bodywt and brianwt variables, we can see that there may be outliers. The maximum value of the bodywt variable looks very different from the mean value.
## -----------------------------------------------------------------------------------------------
dplyr::filter(df, bodywt == 6654)

#' Not surprisingly, it looks like that observation for an elephant.
#' 
#' Taking a deeper look at the histogram we can see that there are two values that are especially different.
## -----------------------------------------------------------------------------------------------
hist(pull(df, bodywt))

#' 
## -----------------------------------------------------------------------------------------------
dplyr::filter(df, bodywt > 2000)

#' Looks like both data points are for elephants.
#' 
#' Therefore, we might consider performing an analysis both with and without the elephant data, to see if it influences the overall result.
#' 
#' --------------------------------------------------------------------------------
#' --------------------------------------------------------------------------------
#' --------------------------------------------------------------------------------
#' 
#' ## Evaluating Relationships
#' Another important aspect of exploratory analysis is looking at relationships between variables.
#' 
#' Again visualizations can be very helpful. We might want to look at the relationships between all of our continuous variables. A good way to do this is to use a visualization of correlation. As a reminder, correlation is a measure of the relationship or interdependence of two variables. In other words, how much do the values of one variable change with the values of another? Correlation can be either positive or negative and it ranges from -1 to 1, with 1 and -1 indicating perfect correlation (1 being positive and -1 being negative) and 0 indicating no correlation. More on this later in associations.
#' 
#' Here are some very useful plots that can be generated using the GGally package and the PerformanceAnalytics package to examine if variables are correlated.
## -----------------------------------------------------------------------------------------------
# install.packages("PerformanceAnalytics")
library(PerformanceAnalytics)
df %>%
dplyr::select_if(is.numeric) %>%
  chart.Correlation()

#' 
## -----------------------------------------------------------------------------------------------
library(GGally)
df %>%
dplyr::select_if(is.numeric) %>%
    ggcorr(label = TRUE)

#' 
## -----------------------------------------------------------------------------------------------
library(GGally)
df %>%
dplyr::select_if(is.numeric) %>%
    ggpairs()

#' We can see from these plots that the awake variable and the sleep_total variable are perfectly correlated. This becomes important for choosing what to include in models when we try to perform prediction or inference analyses.
#' 
#' We may be especially interested in how brain weight (brain_wt) relates to body weight (body_wt). We might assume that these to variables might be related to one another.
#' 
#' Here is a plot of the these two variables including the elephant data:
## -----------------------------------------------------------------------------------------------
library(ggplot2)
ggplot(df, aes(x = bodywt, y = brainwt)) +
  geom_point()

#' Clearly, including the elephant data points makes it hard to look at the other data points, it is possible that these points are driving the positive correlation that we get when we use all the data. Here is a plot of the relationship between these to variables excluding the elephant data and the very low body weight organisms:
## -----------------------------------------------------------------------------------------------
library(ggplot2)

df %>%
  filter(bodywt<2000 & bodywt >1) %>%
  ggplot(aes(x = bodywt, y = brainwt)) +
  geom_point()+
  geom_smooth(method = "lm", se = FALSE) # method argument specifies the smoothing function to be used [Here, the linear model is selected.]
# se argument specifies whether to include a confidence interval around the smooth line [Here, it is set to False]

#' 
## -----------------------------------------------------------------------------------------------
#Pearson's product-moment correlation:
cor.test(pull(df %>% filter(bodywt<2000 & bodywt >1),bodywt),
         pull(df %>%filter(bodywt<2000 & bodywt >1),brainwt))

#' We can see from this plot that in general brainwt is correlated with bodywt. Or in other words, brainwt tends to increase with bodywt.
#' 
#' But it also looks like we have an outlier for our brainwt variable! There is a very high brainwt value that is greater than 1.
#' 
#' We can also see it in our histogram of this variable:
## -----------------------------------------------------------------------------------------------
hist(pull(df, brainwt))

#' Let’s see which organism this is:
## -----------------------------------------------------------------------------------------------
df %>%
  filter(brainwt >=1)

#' It is humans! Let’s see what the plot looks like without humans:
## -----------------------------------------------------------------------------------------------
library(ggplot2)
df %>%
  filter(bodywt<2000 & bodywt >1 & brainwt<1) %>%
  ggplot(aes(x = bodywt, y = brainwt)) +
  geom_point()+
  geom_smooth(method = "lm", se = FALSE)

#' 
## -----------------------------------------------------------------------------------------------
cor.test(pull(df %>% filter(bodywt<2000 & bodywt >1 & brainwt<1),bodywt),
         pull(df %>%filter(bodywt<2000 & bodywt >1 & brainwt<1),brainwt))

#' We can see from these plots that the brainwt variable seems to have a relationship (correlation value = 0.79) with bodywt and it increases with the bodywt variable, however this relationship is less strong when humans are included (correlation value = 0.46). This information would be important to keep in mind when trying to model this data to make inference or predictions about the animals included.
