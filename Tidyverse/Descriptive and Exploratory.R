#' ---
#' title: "Descriptive and Exploratory Analysis"
#' author: "Toheeb"
#' ---
#' 
#' ## SOME APPETIZER -- A brief intro to the map function
#' The map function can be used to apply a function to elements of a vector or list and print 
#' the result in list format.
library(purrr) # map function is contained in the purrr library
#' 
#' ### Vector examples
#' Example 1 (using a function -- the ceiling function is used here):
x <- c(2.8,5.6,7.2,8.4,1.5, 3.0)
x %>%
  map(ceiling) # return the next higher integer for each element of the vector

#' 
#' Example 2 (using a simple arithmetic):
num <- c(3,5,8,6,7)
num %>% 
  map(function(x) x^3) #returns the third power of each element

#' 
#' ### Example 3 (using a List item):
ran <- list(c(1, 2, 3), c(4, 5, 6), c(7, 8, NA))
ran %>% 
  map(mean, na.rm = T) # returns the average value of each list

#' 
## -----------------------------------------------------------------------------
## -----------------------------------------------------------------------------
## -----------------------------------------------------------------------------
## -----------------------------------------------------------------------------
## -----------------------------------------------------------------------------
## -----------------------------------------------------------------------------
#' # **Descriptive and Exploratory Analysis (DEA)**
df <- msleep # the msleep is a built-in dataset 
#' Take a quick glance at the data:
library(tidyverse)
glimpse(df)
#' NB: The glimpse function produces similar output as the str() function:
str(df)
#' Also, since the data is in tibble format, we can gain a lot of information by just viewing 
#' the data itself:
df
#' Like the glimpse and str functions, the output here also provides information about the 
#' dimensions of the data object and the name and class of the variables.
#' 
#' 
#' ## Missing Values
#' Calculate how many NAs there are in each variable:
# library(purrr) # library already opened in a previous code chunk
df %>% 
        map(is.na)  %>% # identifies the missing values in each variable
        map(sum) # counts the number of missing values identified per variable
#' 
sum(is.na(df)) # total no. of missing values in the entire data
#' 
#' Calculate the proportion of missingness for each variable:
df %>% 
        map(is.na) %>%
        map(sum)%>%
        map(~ . / nrow(df))%>% # number of missing values (identified in the previous line) 
  #...divided by number of rows in the df
        bind_cols() # returns a df-like results (a table displaying the proportion of 
  # missingness in each variable/column)
#' 
#' can also visualize missingness in the data (using the vis_miss function in the 'naniar' 
#' package):
#install.packages("naniar")
library(naniar)
vis_miss(df) # charts the missingness proportion on a "bar plot"?? Also shows the % missingness
#...and % non-missingness in th entire data
#' 
#' 
## -----------------------------------------------------------------------------
## -----------------------------------------------------------------------------
#' ## Shape of the data
#' This refers to the data distribution (such as normal/Gaussian, skewed, uniform, etc.)
#Example 1:
ggplot(df, aes(sleep_total)) +
  geom_density() # displays the distribution of the sleep_total variable in the sleep data
#' Results: the variable distribution is somewhat normal but not entirely symmetrical.
#' 
#Example 2 (using the built-in iris dataset):
iris %>% 
  ggplot() +
  geom_density(aes(x=Sepal.Width))
#The previous syntax could equally be used:
ggplot(iris, aes(Sepal.Width)) +
  geom_density()
#' Results: the Sepal.Width variable is more normally distributed
#' 
#Example 3:
ggplot(df, aes(sleep_rem)) +
  geom_density()
## Warning: Removed 22 rows containing non-finite values (stat_density).
#' Results: The sleep_rem variable (of the sleep dataset) is skewed right (or right-skewed), 
#' i.e., the data shifts away from the right, leading to a long right tail. Most of the values 
#' are thus at the lower (or left) end of the range.
#' 
#' 
#' 
## -----------------------------------------------------------------------------
## -----------------------------------------------------------------------------
#' ## Identifying Outliers
#' To identify outliers visually, density plots and boxplots can be very helpful.
#' #' For example, in the iris dataset the distribution of Petal.Length is bimodal (a type of 
#' distribution can be identified by density plots that have two distinct humps). In these 
#' distributions, there are two different modes – this is where the term “bimodal” comes from. 
#' As seen in the plot below, the curve suggests there are many flowers with petal length less 
#' than 2 and many others with petal length around 5.
#' 
# library(ggplot2)
iris %>%
  ggplot(aes(Petal.Length)) +
  geom_density() #density plot
#' Since the two humps in the plot are about the same height, this shows that it’s not just 
#' one or two flowers with much smaller petal lengths, but rather that there are many. Thus, 
#' these observations aren’t likely outliers.
#' 
#' To investigate this further, look at petal length broken down by flower species:
iris %>%
  ggplot(aes(Species, Petal.Length)) +
  geom_boxplot() #box plot
#' Results: in fact, setosa have the shortest petal lengths while virginica have the longest. 
#' Had all shorter petal length flowers been simply removed from the dataset, information would 
#' have been lost about an entire species!
#' 
#' Boxplots are also helpful because they plot “outlier” observations as points outside the box. 
#' By default, boxplots define “outliers” observations as those that are outside a range of 
#' 1.5 x IQR (interquartile range). The IQR is the distance between the first and third 
#' quartiles. This is a mathematical way to determine if a sample may be an outlier. It is 
#' visually helpful, but then it’s up to the analyst to determine if an observation should be 
#' removed. While the boxplot identifies outliers in the setosa and versicolor species, these 
#' values are all within a reasonable distance of the rest of the values, and unless it could 
#' be determined why this occurred, these observations should not be removed.
#' 
#' 
## -----------------------------------------------------------------------------
## -----------------------------------------------------------------------------
#' ## Evaluating Variables
#' 
#' ### Measures of central tendency.
#' The two most common types are: mean and median
#' 
#' ### Measures of Variability
#' Variance -- this tells you how spread out the values are. If all the values of a variable 
#' are exactly the same, that variable’s variance will be zero. The larger the variance, the 
#' more spread out the values are (or vice versa). 
#' 
#' Ex: calculate the variance of the following:
## variance of a vector where all values are the same
a <- c(29, 29, 29, 29)
var(a)
# Results: variance = 0

## variance of a vector with one very different value
b <-  c(29, 29, 29, 29, 723678)
var(b)
# Results: variance = 104733575040

#' NB: The only difference between the two vectors above is that the second one has a value 
#' that is much larger than 29. The variance for this vector is thus much higher.
#' 
#' 
#' Standard deviation: By definition, the standard deviation is the square root of the 
#' variance, thus if we were to calculate the standard deviation in R using the sd() function, 
#' we’d see that the sd() function is equal to the square root of the variance:
## calculate standard deviation:
sd(b)
## this is the same as the square root of the variance:
sqrt(var(b))

#' 
#' 
## -----------------------------------------------------------------------------
## -----------------------------------------------------------------------------
#' ### Summarizing The Data
#' Often, tables are included in reports summarizing the dataset. These will include the 
#' number of observations in the dataset and maybe the mean/median and standard deviation of 
#' a few variables -- all these could be organized into a table.
#' 
#' Alternatively, skimr is a helpful package in this regard:
# install.packages("skimr")
library(skimr)
skim(df)
#' Results: the skim() provides a lot of helpful inormation:
#' -- separately summarizes categorical and continuous variables. 
#' -- For continuous variables the information includes the mean and median (the median is 
#' ...displayed in the column named 'p50', i.e., 50th percentile) column. p0 (zeroth percentile) 
#' ...is the minimum value and p100 (100th percentile) is the maximum value, and both of these 
#' ...values calculate the range. 
#' -- There's also a measure of variability, i.e., the standard deviation (sd). 
#' -- The result also shows the number of missing values per variable 
#' -- the distribution or shape of each variable (as histogram). 
#' -- Potential outliers can also be identified from the hist column and the p100 and p0 columns.
#' 
#' Ex: Take a closer look at the bodywt variables, there may be outliers. (The maximum value 
#' of the variable looks very different from the mean value.)
dplyr::filter(df, bodywt == 6654)
#' Results: Not surprisingly, that observation is for an elephant.
#' 
#' Taking deeper look at the histogram:
hist(pull(df, bodywt))
# Results: about two values/obs are very different from the rest.
dplyr::filter(df, bodywt > 2000)
#' Results: Again, looks like both data points are for elephants.
#' 
#' Therefore, one might consider performing an analysis both with and without the elephant 
#' observations, to see if it influences the overall result.
#' 
#' 
## -----------------------------------------------------------------------------
## -----------------------------------------------------------------------------
#' ## Evaluating Relationships
#' Another important aspect of exploratory analysis is looking at relationships between 
#' variables. Again visualizations can be very helpful here. One might want to look at the 
#' relationships between all the continuous variables. A good way to do this is to use a 
#' visualization of correlation. As a reminder, correlation is a measure of the relationship 
#' or interdependence of two variables. In other words, how much do the values of one variable 
#' change with the values of another? Correlation can be either positive or negative and it 
#' ranges from -1 to 1, with 1 and -1 indicating perfect correlation (1 being positive and -1 
#' being negative) and 0 indicating no correlation. More on this later in associations.
#' 
#' Here are some very useful plots that can be generated using the GGally package and the 
#' PerformanceAnalytics package to examine if variables are correlated.
# install.packages("PerformanceAnalytics")
library(PerformanceAnalytics)
df %>%
dplyr::select_if(is.numeric) %>%
  chart.Correlation()
#' 
library(GGally)
df %>%
  dplyr::select_if(is.numeric) %>%
    ggcorr(label = TRUE)
#' 
# library(GGally)
df %>%
  dplyr::select_if(is.numeric) %>%
    ggpairs()

#' Results: from these plots, the awake variable and the sleep_total variable are perfectly 
#' correlated. This becomes important for choosing what to include in models when running 
#' prediction or inference analyses.
#' 
#' One may be especially interested in how brain weight (brain_wt) relates to body weight 
#' (body_wt). We might assume that these to variables might be related to one another.
#' #' Following is a plot of these two variables including the elephant data:
ggplot(df, aes(x = bodywt, y = brainwt)) +
  geom_point()
#' Results: Clearly, including the elephant data points makes it harder to get a good look at 
#' the other data points. Also, it is possible that these "extreme" observations are driving 
#' the seemingly positive correlation that we get when we use all the data. 
#' 
#' Below is a plot of the relationship between the two variables excluding the elephants and 
#' the very low body weight organisms:
df %>%
  filter(bodywt<2000 & bodywt >1) %>%
  ggplot(aes(x = bodywt, y = brainwt)) +
  geom_point()+
  geom_smooth(method = "lm", se = FALSE) # "method" specifies the smoothing function to be 
# ..used [Here, the linear model is selected.]
# "se" specifies whether to include a confidence interval around the smooth line [Here, it is 
# ..set to False, i.e., no confidence interval]

#' 
#' 
#Pearson's product-moment correlation:
cor.test(pull(df %>% filter(bodywt<2000 & bodywt >1),bodywt),
         pull(df %>%filter(bodywt<2000 & bodywt >1),brainwt))
#' Results: brainwt is correlated with bodywt. Specifically, brainwt tends to increase with 
#' bodywt.
#' 
#' But it also looks like we have an outlier for our brainwt variable! (There is a very high 
#' brainwt value that is greater than 1).
#' 
#' We can also see it in our histogram of this variable:
hist(pull(df, brainwt))

#' Check which organism this is:
df %>%
  filter(brainwt >=1)

#' It is human! Let’s see what the plot looks like without humans:
# library(ggplot2)
df %>%
  filter(bodywt<2000 & bodywt >1 & brainwt<1) %>%
  ggplot(aes(x = bodywt, y = brainwt)) +
  geom_point()+
  geom_smooth(method = "lm", se = FALSE)
#' 
cor.test(pull(df %>% filter(bodywt<2000 & bodywt >1 & brainwt<1),bodywt),
         pull(df %>%filter(bodywt<2000 & bodywt >1 & brainwt<1),brainwt))

#' Results: the brainwt variable is positively related with bodywt #' (correlation = +0.79). 
#' however this relationship is less strong when humans are included (correlation value = 
#' 0.46). This information would be important to keep in mind when trying to model this data 
#' to make inference or predictions about the animals included.
