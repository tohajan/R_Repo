# 
# INFERENCE ANALYSES
#' The infer Package
#' The infer package simplifies inference analyses. Users can quickly calculate a variety of 
#' statistics and perform statistical tests including those that require resampling, permutation, 
#' or simulations using data that is in tidy format.
#' 
#' In fact users can even perform analyses based on specified hypotheses with the hypothesize() 
#' function.
#' 
#' We will perform the same analysis about soda cans that we just did (see the file named 
#' "Beyond Linear Regression.R" in the same R_Practice/Tidyverse path) with this package to 
#' illustrate how to use it.
#' 
#' Recall that we wanted to know if the observed ounces of soda can differs from the expected 
#' mean of 12 oz. Also recall that we had measurements for 100 soda cans (we made up this data). 
#' We had a testable statement or hypothesis that “soda cans typically have 12 ounces, the mean 
#' amount is 12” and we wanted to know if this was true.
#' 
#' This type of hypothesis is called a null hypothesis because it is a statement that expects 
#' no difference or change. The alternative hypothesis is the complement statement. It would be 
#' that the mean is not 12.
#' 
#' OK, so now we will use the infer package to test if our null hypothesis is true.
#' 
#' First, we need to get our data into a tidy format. Thus we will use the as_tibble() function 
#' of the tidyr package.
set.seed(34) 
soda_ounces <- rnorm(100, mean = 12, sd = 0.04)
head(soda_ounces)
# library("tibble")
soda_ounces <-as_tibble(soda_ounces)

head(soda_ounces)
print(soda_ounces, n=25)

#' Now we will use the specify() function of the infer package to indicate that the value 
#' variable is our response variable that will be used in our hypothesis. This is, as you might 
#' expect, more important when we have multiple variables in our data. Then we can specify our 
#' null hypothesis with the hypothesize() function.
#' 
#' There are two options for the null argument of this function: 
#'    1) point - this option should be used when there is one variable in the hypothesis, such 
#' as “the mean of this data x”. 
#'    2) independence - this option should be used when there are two populations, such as 
#' "the means of these two groups identical" or “this variable influences this other variable”.
#' 
#' Then if the point option is used, there are several additional arguments regarding what is 
#' being tested about that one variable. One can test a particular mu for mean, med for median, 
#' sigma for standard deviation, or p for the proportion of successes (for a categorical 
#' variable).
#' 
#' Our hypothesis was “the mean amount of soda ounces is 12” thus we will use the point option 
#' for our null argument and we will specify a mean with the mu argument as 12.
#' 
#' The major benefit of this package, besides allowing the user to think about the statistical 
#' analysis more than the programming required, is that the user can easily implement 
#' iterative methods like resampling.
#' 
#' What do we mean by this? Resampling is a method where a random samples are drawn from the 
#' original data to create a dataset of the same size as the original data (but with some 
#' samples repeated) and this is done repetitively over and over. This process is called 
#' Bootstrapping. This provides more information about the confidence in our estimations from 
#' our sample about the true population that we are trying to investigate, as it gives us more 
#' of a sense of the range of values for statistics like mean and median might vary using 
#' other samples.
#' 
#' To perform resampling, users can use the generate() function with the type argument set to 
#' "bootsrap" and use the rep argument to specify how many bootstrap resamples to generate.
#' 
#' The calculate() function then allows for many different statistics to be calculated 
#' including:
#'   mean, median, sum, sd for standard deviation, 
#'   prop for proportion for categorical variables, count, diff in means, diff in medians,
#'   diff in props, Chisq, F, slope, correlation, t, z, ratio of props, odds ratio...
#' 
#' Finally, the get_confidence_interval(), as you might guess, calculates a confidence interval.
#' 
#' Now we will use these functions on our data.
# install.packages("infer")
library(infer)
set.seed(342)
CI <-soda_ounces %>%
  specify(response = value) %>%
  hypothesize(null = "point", mu = 12) %>%
  generate(rep = 1000, type = "bootstrap") %>%
  calculate(stat = "mean") %>% 
  get_confidence_interval()
CI
#' 
#' We can see that our confidence interval is very similar but slightly different from the 
#' results we obtained using the t.test() function and the lm() function. This is because we 
#' used a different method to calculate the confidence interval based on the bootstrap samples. 
#' Furthermore, the results will vary every time the code is run because the bootstrap samples 
#' are randomly created each time (unless you set a seed with set.seed).
#' 
#' We can also make a visualization of the null distribution of the bootstrap samples using 
#' the visualize() function.
set.seed(342)
bootstrap_means <-soda_ounces %>%
  specify(response = value) %>%
  hypothesize(null = "point", mu = 12) %>%
  generate(rep = 1000, type = "bootstrap") %>%
  calculate(stat = "mean")

bootstrap_means %>%
  visualize()
#' 
#' 
#' 
#' 
#' 
#' 
#' 
#' 