#' ---
#' TITLE: "Data Handling & ggplot"
#' author: "Toheeb"
#' 
#* **PART 1** 
# Reading data
carseats <- read.csv("G:/My Drive/Programming/Data Science/DATA SETS/Carseats.csv")
View(carseats) # displays the data in a separate tab in RStudio

# Creating new data from existing data
age <- carseats$Age
educ <- carseats$Education
age_educ <- cbind(age, educ)
write.table(age_educ, file = "G:/My Drive/Programming/Data Science/R Learning/Data Handling & Graphics/age_educ.csv") 
# the newly created data frame is stored as defined (file location and extension).
#' 
#' 
## -----------------------------------------------------------------------------
#' ## Exploring Data
my_data <- airquality # "airquality" is a built-in data set in R.
View(my_data)

#' Retrieving sections of data
head(my_data) # prints the first 6 elements (rows) of the data
head(my_data, n = 4) # first 4 rows
head(my_data, n = -10) # all but the last 10

tail(my_data)# last 6
tail(my_data, n = 12) # last 12
tail(my_data, n = -20) # all but the first 20

#' some other applicable functions
class(my_data) # prints the class/type of the object (here it is a data frame - df)
length(my_data) # length of the object (i.e., no. of cols in the df)
ncol(my_data) # same as above
nrow(my_data) # no. of rows
names(my_data) # names of columns
rownames(my_data) # index names (i.e., row names)
str(my_data) #' gives a brief overview of the data frame (its dimension, data type 
#..and first few values in each column/variable
dim(my_data) #gives the df's dimension (i.e., no. of rows and columns in the data)
summary(my_data) # gives summary statistics (mean, median, min, etc.) for each 
#..numeric column. The function does not apply to character/categorical variables.

library("dplyr")
glimpse(my_data) 
#' glimpse (a function in the dplyr library) produces outputs similar to the str() 
#..function used above.

#' Converting a categorical var:
my_data$Month <- factor(my_data$Month) 
# converts the Month var to factor type (the var is categorical)

glimpse(my_data) # Note: in the output, Month var is now labelled 'fct' (factor)
levels(my_data$Month) <- list(May = "5", June = "6", July = "7", Aug = "8", 
                              Sept = "9") # labels the values/levels of the Month var
levels(my_data$Month) # lists the categories in the Month column

#' 
#' 
## -----------------------------------------------------------------------------
#' ## Plotting data
plot(my_data$Ozone, my_data$Temp)
idx <- identify(my_data$Ozone, my_data$Temp)
# this syntax allows to click on specific points on the graph.
#' The default labeling is row number. This can be modified as follows:
idx <- identify(my_data$Ozone, my_data$Temp, labels = my_data$Month, plot = TRUE) 
# labels the data points with the corresponding month name.

#' Even better:
idx <- identify(my_data$Ozone, my_data$Temp, 
                labels = paste(as.character(my_data$Day),
                      "-", as.character(my_data$Month)), plot = TRUE)
#' 
#' More operations:
#' To check the frequency of the values of a var/col. For example, the 
#'..variable Month:
xtabs(~Month, my_data) 

#' 
## -----------------------------------------------------------------------------
#' ## Dealing with NA's (missing values) 
#' NA indicates a missing case/obs \
#' NaN = "not a number" (e.g., in cases where a math operation is ran on a non-numeric 
#'..var). Although there already are missing cases in the current data set, one could
#'..throw in more:
my_data[154, ] <- c(NA) # adds another row, with all cells having NA values
my_data[, 7] <- c(NA) # adds a new col, all cells having NA values
View (my_data)

View(is.na(my_data)) # checks for all NA values anywhere in the data set; 
#..prints a table of logical (TRUE/FALSE) values

any(is.na(my_data)) # returns TRUE if at least one missing case exists in the data

all(is.na(my_data)) # checks if all entries in the specified object (i.e., the entire
#..dataset) are missing values

all(is.na(my_data[,7])) # checks if all entries in the specified column are 
#..missing values

# The recently added row and column contain only missing cases, so they can be removed:
my_data <- my_data[-7] # removes the last column (no. 7)
my_data <- my_data[-154, ] # removes the last row (no. 154)

any(is.na(my_data)) # checks again for ANY missing cases
sum(is.na(my_data)) # gives the total no. of missing obs in the entire data set

sum(is.na(my_data$Solar.R)) # gives the no. of missing obs in the specified column

colSums(is.na(my_data)) # shows the distribution of missing cases across all columns

#' The rows of missing observations can be deleted as follows:
clean_data <- na.omit(my_data) # generates a new data with all missing cases removed.
nrow(clean_data)

nrow(my_data) - nrow(clean_data) # gives the difference between the no. of rows in 
#..the original data and the no. of rows in the cleaned data (i.e., no. of missing 
#..obs/rows deleted)

#' Alternatively:
clean_data2 <- my_data[complete.cases(my_data), ] # this code indexes only the rows 
#..with complete cases (no missing values)
nrow(clean_data2)

#' NB: These two approaches greatly reduce sample size. To preserve sample size, remove NA 
#' values only from variables of interest. E.g, if Ozone is not needed for analysis:
clean_data3 <- na.omit(my_data[-1])
ncol(clean_data3) #only 5 cols left (Ozone is removed/ignored)
nrow(clean_data3) #146 rows

nrow(my_data) - nrow(clean_data3) 
# only 7 rows removed (as opposed to 42 rows deleted in the previous approaches)

#' Also, missing values can be removed based on some predefined condition(s):
clean_data4 <- my_data[, colSums(is.na(my_data)) < 10] 
# subsets a new data containing only columns with less than 10 missing cases

#' next, rows with missing cases are removed from the new data:
final_data <- na.omit(clean_data4)
ncol(final_data) # 5 cols
nrow(final_data) #146 rows

#' 
#' 
#' 
## -----------------------------------------------------------------------------
#' ## Combining data
setwd("G:/My Drive/Programming/Data Science/DATA SETS") # changes the working directory
getwd() #retrieves the current working directory
owl_morph <- read.csv("owl.morphometrics.csv", header = TRUE)

summary(owl_morph)
View(owl_morph)
any(is.na(owl_morph)) #prints False
str(owl_morph)

owl_morph$common.name <- as.factor(owl_morph$common.name)
owl_morph$latin <- as.factor(owl_morph$latin)
#' Both columns have now been changed to factor (i.e., categorical variables)

plot(owl_morph$weight.g, owl_morph$wingspan.cm, xlab = "Owl weight (g)", 
     ylab = "Owl wingspan (cm)")

#' 
owl_clutch <- read.csv("owl.clutch.size.csv", header = TRUE)
View(owl_clutch)
any(is.na(owl_clutch))

#' Since the two data sets have the same contents in row 1 (common.name) orderly arranged, 
#' they can be combined using the cbind function:
owl_morph_clutch <- cbind(owl_morph, owl_clutch)


#' NB: The combination syntax above leads to duplication of the "common name row". To avoid this:
owl_morph_clutch <- cbind(owl_morph, owl_clutch[, 2]) # here, only the 2nd col of the second data is used in the combination

names(owl_morph_clutch)[6] <- "clutch.size" # renames the newly added column
View(owl_morph_clutch)

#' plot a graph of clutch size vs wing span
plot(owl_morph_clutch$wingspan.cm, owl_morph_clutch$clutch.size,
     xlab = "Owl wing span (cm)", ylab = "Owl clutch size (??)")

#' 
owl_lspan <- read.csv("owl.lifespan.csv", header = TRUE)
View(owl_lspan)
any(is.na(owl_lspan))
#' 
#' This new data has one NA, and the identifying row (common.name) is not arranged alphabetically. 
#' To combine it with the recently combined data, use the merge() function and specify the key
#' column (i.e., to be used in the combination)
owl_morph_clutch_life <- merge(owl_morph_clutch, owl_lspan, key = "common.name")
View(owl_morph_clutch_life)

#' 
#' 
## -----------------------------------------------------------------------------
## -----------------------------------------------------------------------------
## -----------------------------------------------------------------------------
## -----------------------------------------------------------------------------
## -----------------------------------------------------------------------------
#* **PART 2**
#' 
#' ## The apply() Family of Functions 
#' 1. apply() 
#' It has the general structure: apply(object, margin, function...); the object is 
#' ..usually a matrix or an array. Consider the ffg example:
duckweed_mat <- matrix(c(10, 20, 30, 40, 50, 60, 70, 80, 90, 100, 10, 30, 50, 80, 100, 150, 200,
                         250, 270, 300, 10, 30, 36, 80, 96, 106, 110, 130, 136, 144, 10, 15, 30,
                         50, 70, 86, 95, 100, 105, 190, 10, 40, 50, 65, 78, 96, 107, 120, 144,
                         157, 10, 30, 57, 98, 106, 130, 160, 177, 189, 198),
                       nrow = 10, byrow = FALSE)
rownames(duckweed_mat) <- c("Day1", "Day2", "Day3", "Day4", "Day5", "Day6", "Day7", 
                            "Day8", "Day9", "Day10")
colnames(duckweed_mat) <- c("R1", "R2", "R3", "R4", "R5", "R6")
duckweed_mat
class(duckweed_mat)
#' 
#' E.g., check the maximum number of leaves recorded each day, i,e, per row:
max(duckweed_mat[1,])
max(duckweed_mat[2,])
max(duckweed_mat[3,])
#' ...and so on.
#' Alternatively, a FOR loop could be useful:
for (i in 1:10) {
  row <- duckweed_mat[i, ]
  max <- max(row)
  print(max)
}
#' 
#' All the above is done better with the apply() function:
apply(duckweed_mat, 1, max) # returns the maximum value in each row
#NB: the 2nd argument is the margin -- 1 represents row; 2 implies column
#' 
#' Same function, but along the columns:
apply(duckweed_mat, 2, max) # this prints out the maximum value in each column

#' The apply() function also works on data frames:
duckweed_df <- data.frame(R1 = c(10, 20, 30, 40, 50, 60, 70, 80, 90, 100),
                          R2 = c(10, 30, 50, 80, 100, 150, 200, 250, 270, 300),
                          R3 = c(10, 30, 36, 80, 96, 106, 110, 130, 136, 144),
                          R4 = c(10, 15, 30, 50, 70, 86, 95, 100, 105, 190),
                          R5 = c(10, 40, 50, 65, 78, 96, 107, 120, 144, 157),
                          R6 = c(10, 30, 57, 98, 106, 130, 160, 177, 189, 198))
class(duckweed_df)
duckweed_df

#' calculate the means per row:
rowMeans(duckweed_df)
#' 
#' Using the apply() function:
apply(duckweed_df, 1, mean)
#' 
#' However, unlike matrices and arrays, a data frame can contain non-numeric elements:
duckweed_df$Day <- as.factor(1:10)
(duckweed_df)
#' NB: although the newly added column contains values 1 to 10, these values have 
#' been assigned non-numeric type (factor)
duckweed_df <- duckweed_df[, c(7, 1:6)] # rearranges the data so that the new column comes first
class(duckweed_df$Day) # check that the column is correctly identified as factor 
#' 
#' Now using the apply function again:
apply(duckweed_df, 1, mean)
#' 
#' NB: the above returns an error message ("Warning: argument is not numeric or logical: 
#'returning NA") because each row now has a factor, a data type that is not receptive 
#'..to arithmetic operations.
apply(duckweed_df[, 2:7], 1, mean) # the non-numeric column is now excluded
apply(duckweed_df[, -1], 1, mean) # the column can also be excluded this way
apply(duckweed_df[, -c(1,2,4,6)], 1, mean) # can also exclude several columns

#' 
#' Throw in a missing value:
duckweed_df[6, 5] <- NA
(duckweed_df)
apply(duckweed_df[, -1], 1, mean)
#' 
#' NB: Now that there is a missing value in a row, that row returns a mean of "NA". If 
#'..that specific cell is not of interest, one could instruct R to ignore it and 
#'..compute the mean for only the non-missing values in that row:
apply(duckweed_df[, -1], 1, mean, na.rm = TRUE)
# na.rm ignores the missing value(s) during the operation

#' 
## -----------------------------------------------------------------------------------------------------------------
#' ## Using customized functions
#' The examples thus far have used a built-in R function (i.e., mean). But customized 
#' functions can also be used. For example, one may be interested in calculating, for each plant, 
#' the no. of leaves counted each day as a  proportion of the total no. of leaves (i.e, the 
#' count on the 10th day) for that plant:
prop <- function(x) {
  x / max(x)
}

#' Using the matrix data:
(duckweed_mat)
apply(duckweed_mat, 2, prop)
#' Note that the calculation is per column, hence the margin argument is set to "2".
#' Basically, the apply() function takes each column as a vector and each cell as an element in 
#' the column vector.
#' 
#' 
## -----------------------------------------------------------------------------
## -----------------------------------------------------------------------------
#' 2. lapply()
#' General structure: lapply(object, function, ...)
#' -> the "object" could be vector, list, or data frame.
#' -> lapply() returns only list outputs.
#' Example - Below is an hypothetical data on the clutch sizes of Canada geese under 4 different 
#' diet regimes, with 10 replicates in each diet group:
cago_list <- list(Diet1 = c(2, 5, 4, 5, 3, 5, 4, 4, 4, 5),
                  Diet2 = c(8, 5, 6, 5, 7, 7, 6, 8, 8, 3),
                  Diet3 = c(3, 4, 2, 5, 2, 6, 5, 6, 2, 4),
                  Diet4 = c(2, 2, 3, 2, 5, 2, 4, 3, 5, 7))
class(cago_list)
(cago_list)

#' 
lapply(cago_list, mean)
#' NB: each row is a vector, and the mean is thus returned per vector (i.e., diet).
#' The same data could be stored in a data frame instead:
cago_df <- data.frame(Diet1 = c(2, 5, 4, 5, 3, 5, 4, 4, 4, 5),
                  Diet2 = c(8, 5, 6, 5, 7, 7, 6, 8, 8, 3),
                  Diet3 = c(3, 4, 2, 5, 2, 6, 5, 6, 2, 4),
                  Diet4 = c(2, 2, 3, 2, 5, 2, 4, 3, 5, 7))
lapply(cago_df, mean)
#' Again, using the lapply returns the output in a list format. And there's no margin 
#' ..argument, unlike apply().
#' 
#' A vector example:
random <- c("This", "is", "a", "random", "vector")
#' The no. of letters in each of the words contained in the vector can be determined 
#'..as follows:
lapply(random, nchar)
#' NB -- unlike the lapply() function, the following syntax returns an output in a 
#' ..vector format:
nchar(random)

#' 
#' 
## -----------------------------------------------------------------------------
## -----------------------------------------------------------------------------
#' 3. sapply()
#' It has the same general structure as lapply() and also applies to lists, vectors, 
#' and data frames. However, outputs are returned in a simplified format if possible 
#' (vector, matrix, or list). In addition, as in lapply(), sapply() has no margin 
#' ..argument.
cago_list
sapply(cago_list, mean) # results are in a vector format
#' 
cago_df
sapply(cago_df, mean) # results also in vector format
#' 
random
sapply(random, nchar)

#' Sometimes the output cannot be simplified to a vector or matrix:
sequence <- function(x) {
  seq(nchar(x))
}
#' Reminder: the seq() function returns a sequence from 1 up to and including the 
#' ..specified number. E.g.,: 
seq(3) # this outputs "1 2 3"
seq(nchar("that")) #this outputs "1 2 3 4"

#' Now, apply the function created earlier:
sapply(random, sequence) # the output is displayed as a list.
#' 
#' 
## -----------------------------------------------------------------------------
## -----------------------------------------------------------------------------
#' 4. tapply() 
#' General structure: tapply(x, index, function, ...) \
#' --> It applies to object subsets (vector, column of a data frame, element of a 
#' ..list, etc...)
patient_ID <- 1:30
age <- c(32, 45, 44, 34, 23, 26, 37, 45, 12, 23, 44, 35, 57, 65, 76, 43, 42, 34,
         36, 37, 23, 21, 28, 24, 29, 13, 18, 32, 25, 28)
treatment <- c("a", "c", "c", "b", "b", "b", "c", "b", "c", "a", "a", "a", "a", "a","b",
               "b", "b", "b", "c", "c", "c", "a", "b", "c", "b", "a", "a", "c", "a", "c")
#' 
tapply(age, treatment, mean) # gives the mean age by treatment group

#' Store the data in a data frame:
med_df <- data.frame(patient_ID, treatment, age)
head(med_df) 
#' 
tapply(med_df$age, med_df$treatment, mean)
#' 
#' A list could also work:
med_list <- list(patient_ID = patient_ID, treatment = treatment, age = age)
tapply(med_list$age, med_list$treatment, mean)

#' 
#' 
#' 
## -----------------------------------------------------------------------------
## -----------------------------------------------------------------------------
## -----------------------------------------------------------------------------
## -----------------------------------------------------------------------------
## -----------------------------------------------------------------------------
#' # **PART 3**
#' aggregate() function 
#' --> Similar to tapply() but with wider applications
#' --> General structure: aggregate(formula, data, function, ...) \
#' --> works with list or data frame (other data structures will first have to be 
#' ..converted to either of these two)
#' 
#' Example -- Using the co2_uptake data (a built-in data set in R that contains an 
#' experiment data of CO2 uptake in plants from two different locations):
co2_uptake <- CO2
head(co2_uptake)
tail(co2_uptake)
#' Add a new column to the data:
co2_uptake$height <- c(35.77, 43.95, 38.10, 43.20, 43.02, 39.19, 31.60, 36.88, 41.11, 
                       43.64, 36.82, 33.86, 30.17, 36.92, 36.15, 43.60, 32.35, 43.92, 
                       40.50, 37.46, 33.92, 42.19, 30.20, 35.64, 39.63, 36.39, 42.95, 
                       33.88, 43.75, 41.10, 34.57, 30.21, 37.19, 33.45, 40.93, 32.93, 
                       36.21, 40.74, 32.87, 35.98, 43.57, 39.91, 35.02, 33.20, 37.89, 
                       34.96, 30.99, 40.12, 33.33, 34.48, 38.22, 35.21, 39.60, 40.29, 
                       42.90, 36.09, 38.75, 36.65, 31.51, 39.32, 30.27, 34.21, 40.59, 
                       43.67, 32.10, 30.08, 42.10, 36.60, 43.89, 38.33, 36.99, 32.05, 
                       37.54, 34.51, 33.69, 41.80, 30.91, 39.23, 30.93, 42.73, 34.82, 
                       33.20, 31.57, 43.32)

View(co2_uptake)
#' There are 7 concentration categories (95,175,250,350,500,675,1000) in the data. 
#' To calculate, say, mean uptake for each conc category:
aggregate(uptake~conc, co2_uptake, mean) # tabular output
#' As mentioned earlier, the same operation could be performed using tapply(), but 
#' the output is in a vector format (unlike the aggregate fxn which produced a tabular 
#' output):
tapply(co2_uptake$uptake, co2_uptake$conc, mean) # vector output
#' 
#' Even better, aggregate() can handle multiple subsetting. For example, to calculate 
#' ..the mean uptake by concentration category and treatment group:
aggregate(uptake ~ conc + Treatment, co2_uptake, mean)
#' ...or, mean uptake by conc, treatment, and type:
aggregate(uptake ~ conc + Treatment + Type, co2_uptake, mean)
#' 
#' Operations can also be performed on multiple elements/columns. For example, to 
#' calculate the mean uptake and mean height by conc:
aggregate(cbind(uptake, height) ~ conc, co2_uptake, mean)

#' aggregate() also works on lists:
co2uptake_list <- list(plant = co2_uptake$Plant, type = co2_uptake$Type, 
                       treatment = co2_uptake$Treatment, conc = co2_uptake$conc, 
                       uptake = co2_uptake$uptake, height = co2_uptake$height)

aggregate(cbind(uptake, height) ~ conc, co2uptake_list, mean)
#' NB: the output is again in a table
#' 
#' Other functions can also be applied using aggregate(). For example:
aggregate(uptake ~ conc, co2uptake_list, length) # no. of uptake values per conc group
aggregate(height ~ conc, co2uptake_list, length) # no. of height values per conc group
aggregate(type ~ conc, co2uptake_list, length) # no. of type values per conc group

#' 
#' 
## -----------------------------------------------------------------------------
## -----------------------------------------------------------------------------
#' paste() function
#' This function allows to join multiple elements together as one. For example, in a
#' ..sample of birds, one could assign each individual a label containing their name 
#' ..and ID:
paste("RWBL", 1)
#' Alternatively, the entries can be first stored before calling the paste() function:
species <- "RWBL"
num <- 1
paste(species, num)
#' A "sep" argument can be added to define the separator (default is space):
paste(species, num, sep = "-")

#' When paste() is used on a vector containing several elements, it doesn't work 
#' ..(i.e., individual element in that vector maintains its independence), unless 
#' the "collapse" argument is properly invoked/defined:
id <- c("RWBL", 1)
paste(id) # each element in vector "id" is pasted separately
#' 
paste(id, collapse = " ") # default value for collapse is NULL
# the paste function now works as expected
#' 
paste(id, collapse = "_")
#' NB: the "collapse" argument works like the "sep" argument; so it could take 
#' ..different values like space, underscore, period, etc. The default value is NULL.

#' The paste() function is really powerful when used with several vectors:
species <- c("RWBL", "MODO", "AMRO", "AMCR", "MODO")
num <- 1:5
paste(species, num, sep = "_")
#' If the pasted vectors are of different length, the longer one gets recyled with 
#' ..the shorter one until completion, E.g.:
species <- c("RWBL", "MODO", "AMRO", "AMCR", "MODO")
num <- 1:8
paste(species, num, sep = "_")
#' This "recycling" feature comes in handy during repetitions:
num <- 1:10
paste("Bird", num) # the "Bird" element gets recycled through the num var, thus 
#the output is "Bird 1", "Bird 2", ... "Bird 10".

#' Applying paste() to a df:
df <- data.frame(species, num = 1:5)
paste(df$species, df$num, sep = "_") # as usual, the output is in vector format
#' 
#' The output of the paste() function can be saved in an object and put to further use:
output = paste(df$species, df$num, sep = "_")
df$idnum = output
(df)

#' 
#' 
## -----------------------------------------------------------------------------
## -----------------------------------------------------------------------------
## -----------------------------------------------------------------------------
## -----------------------------------------------------------------------------
## -----------------------------------------------------------------------------
#' # **PART 4**
#' ## Basic Graph Plotting
maxd <- 10
storks <- numeric(maxd) # a vector with 10 unoccupied positions
babies <- numeric(maxd)  # another vector with 10 unoccupied positions
for (i in 1:maxd) {
  set.seed(15)
  storks[i] <- rnorm(1, i * 100, 10) # Recall -- general form is rnorm(n, mean, sd)
  babies[i] <- rnorm(1, i * 10, 10)
}

(storks)
(babies)

plot(storks, babies)
abline(lm(babies ~ storks)) # fits a regression line to the plot

# let's plot a new graph on the existing plot: 
newstorkdata <- c(220, 411, 630, 705, 729, 850)
newbabydata <- c(11, 22, 38, 41, 45, 52)
points(newstorkdata, newbabydata, col = "red", pch = 19) #plots this on the existing 
#graph; "red"  here is the color of the new points/observations
abline(lm(newbabydata ~ newstorkdata), col = "red") # the newly fitted line is red

# add a legend to the graph:
legend(100, 80, c("old data", "new data"), pch = c(1, 19), col = c("black", "red"))
# NB: The first argument for the legend function is "x,y". Here, the value is 100,80, 
#..i.e., the legend should be placed at the point on the graph where x=100 and y=80

# The legend could also be placed by replacing the coordinate argument with a locator 
#..argument:
legend(locator(1), c("old data", "new data"), pch = c(1, 19), col = c("black", "red"))
#' after running the syntax, use a cursor to click on a desired position on the graph
#' for the legend to be placed 
#' 
#' The plot area could also be split as follows:
par(mfrow= c(1, 2)) # splits the plot area into a row with 2 columns
par(mfrow=c(1,1)) # resets the plot area to a single format
#' The par() function can also be used to further define certain attributes of the graph(s), e.g.,:
#' par(las = 1, cex = 1, cex.lab =1.2, cex.axis = 1.1) \
#' >-- las defines the style of axis labels (0=default=parallel, 1=horizontal, ...) \
#' >-- cex magnifies plotting texts and symbols relative to the current size \
#' >-- cex.lab magnifies axis labels relative to the current size \
#' >-- cex.axis magnifies axis annotations relative to the current size
#' 
#' 
## -----------------------------------------------------------------------------
## -----------------------------------------------------------------------------
#' ## Creating box plots using R's ggplot2 package
mydata <- ToothGrowth # ToothGrowth is a built-in data set in R
mydata$dose <- as.factor(mydata$dose) #changes the dose var to factor data type
View(mydata)
# install.packages("tidyverse") 
library("ggplot2")
names(mydata)
#' Now, let's create a box plot:
ggplot(mydata, aes(supp, len)) # creates an empty plot, with variable "supp" on 
#..the x-axis and "len" on the y-axis

ggplot(mydata, aes(supp, len)) + geom_boxplot() # creates a boxplot of "len" vs "supp"

# differentiate columns of box plots by the dose category:
ggplot(mydata, aes(supp, len)) + geom_boxplot() + facet_grid(. ~ dose)
#' Alternatively, this "subsetting" can be done in the aesthetic ("aes") argument:
ggplot(mydata, aes(supp, len, fill = dose)) + geom_boxplot() # same as the last, 
#..but the dose categories are now differentiated by colors.

#' Further, the color can be set such that darker/lighter color indicates an increase 
#' or a decrease in levels of the categorizing variable (e.g., dose in the current 
#' ..example):
my_color <- c("#fff7bc", "#fec44f", "#d95f0e") # yellow, orange, and red colors, respectively
ggplot(mydata, aes(supp, len, fill = dose)) + geom_boxplot() + scale_fill_manual(
                                    name = "Dose (mm)", breaks = c("0.5", "1", "2"),
                                   labels = c("Low 0.5mm", "Medium 1mm", "High 2mm"),
                                   values = my_color)

#' Change the background color to black and white:
ggplot(mydata, aes(supp, len, fill = dose)) + geom_boxplot() + scale_fill_manual(
  name = "Dose (mm)", breaks = c("0.5", "1", "2"),
  labels = c("Low 0.5mm", "Medium 1mm", "High 2mm"),
  values = my_color) + theme_bw() # NB: the previous color was grey-ish

#' And a lot more...
ggplot(mydata, aes(supp, len, fill = dose)) + geom_boxplot() + scale_fill_manual(
  name = "Dose (mm)", breaks = c("0.5", "1", "2"),
  labels = c("Low 0.5mm", "Medium 1mm", "High 2mm"),
  values = my_color) + theme(plot.background = element_blank(),
                             panel.grid.major = element_blank(),
                             panel.grid.minor = element_blank(),
                             panel.background = element_blank(),
                             axis.line = element_line(colour = "black"),
                             axis.title.x = element_text(size = 19, colour = "black"),
                             axis.title.y = element_text(size = 19, colour = "black"),
                             axis.text.x = element_text(size = 12, colour = "black"),
                             axis.text.y = element_text(size = 12, colour = "black"),
                             legend.text = element_text(size = 12),
                             legend.title = element_text(size = 15),
                             legend.background = element_rect(color = "black", 
                                                              size = .5,
                                                              linetype = "solid"),
                             legend.position = c(.60, .85)) + labs(x = "Supplement regimen",
                                                                   y = "Tooth length (mm)")

#' 
#' 
## -----------------------------------------------------------------------------
## -----------------------------------------------------------------------------
#' ## Creating Histograms and density plots using ggplot2 
setwd("G:/My Drive/Programming/Data Science/DATA SETS")
getwd()
mydata <- read.csv("Mammal_lifehistories_v2.csv")
names(mydata)
View(mydata)
head(mydata)
summary(mydata) # gives basic statistics (min, max, mean, median, etc.) of each 
#..numeric column. A similar function but with more detailed output is the describe 
#..function in psych package:
library("psych")
describe(mydata)
ncol(mydata)
nrow(mydata)
str(mydata) # displays basic info of the data (no. of rows & columns, data types, and the first
#...few observations in each column). A similar function is the glimpse()
library("dplyr")
glimpse(mydata)
#' 
#' Some of the cells have "-999" representing NA's (missing cases); remove such obs:
mydata <- mydata[mydata$gestation.mo > 0 & mydata$mass.g > 0 & mydata$litter.size > 0, ]
# Note that this syntax removes missing obs only from variables of interest

m <- ggplot(mydata, aes(x = gestation.mo)) # a histogram uses only one variable, which
#..is thus :defined for the x-axis
m + geom_histogram()
#' Change the width of the bars:
m + geom_histogram(binwidth = 0.4)

#' To create different histograms by, say, order categories (1st column in the current 
#' data):
m + geom_histogram(binwidth = 0.4) + facet_grid(order ~ .) 
# NB: the syntax can be switched for the facet_grid(), i.e., "variable ~ ." [Recall: 
#..the syntax was ". ~ variable" for the boxplots plotted earlier]

#' PS: the result is too tiny to read because there are too many order categories, 
#' some more represented than others.
#' 
#' Check the # of obs in each order category:
xtabs(~ order, mydata) # some orders have only 1 obs, and there are orders with 100+
#' Create a data containing only orders with enough obs (say at least 100 each):
mydata_largeorders <- mydata[mydata$order == "Artiodactyla" | mydata$order == "Carnivora" |
                               mydata$order == "Primates" | mydata$order == "Rodentia", ]

#' Now rerun the operation (plotting a histogram) for the new data:
m2 <- ggplot(mydata_largeorders, aes(x = gestation.mo))
m2 + geom_histogram(binwidth = 0.7) + facet_grid(order ~ .)
#' 
#' 
## -----------------------------------------------------------------------------
## -----------------------------------------------------------------------------
#' ## Creating scatterplots using ggplot2 
# setwd("G:/My Drive/Programming/Data Science/DATA SETS")
co2data <- read.csv("canada.co2.by.sector.csv")
names(co2data)
nrow(co2data)
dim(co2data)
summary(co2data)
View(co2data)
head(co2data)
#' 
#' Remove rows/obs where the sector value is "Total, all sectors"
co2data <- co2data[co2data$SECTOR != "Total, all sectors", ]
#' 
#' Create a scatterplot of output (in kilotonnes; column 4) vs. year:
# install.packages("tidyverse") # package already installed
# library(ggplot2) # library opened already
co2_scatter <- ggplot(co2data, aes(year, log(co2_ann_kilotonnes), colour = 
                                SECTOR)) + geom_point() + stat_smooth(method = "lm")
co2_scatter
#' The scatterplot has some portions cut off because there are too many SECTOR 
#' categories. Try a subset of these categories:
library("dplyr")
co2data_order <- co2data %>% group_by(SECTOR) %>% summarise(co2mean = 
                            mean(co2_ann_kilotonnes)) %>% arrange(desc(co2mean))
#' The above syntax performs the following tasks:
#' first, group the co2data by the variable SECTOR, 
#' next, display the mean co2 output for each sector in a table, 
#' next, arrange the rows by descending order of the mean CO2.
## -------------------------------------------------------------------------------
View(co2data_order)
head(co2data_order)

co2data_order_top10 <- co2data_order[1:10, ] # a new data set containing only the 
#..10 SECTORS with the highest mean co2

top10 <- c(co2data_order_top10$SECTOR) # concatenates the values in the SECTOR 
top10
#..column of the new data
top10[5] # the 5th element in the vector

#' From the original data, sieve out observations in the top 10 sectors into a new data:
co2_top10 <- co2data[co2data$SECTOR == top10[1] | co2data$SECTOR == top10[2] |
                       co2data$SECTOR == top10[3] | co2data$SECTOR == top10[4] |
                       co2data$SECTOR == top10[5] | co2data$SECTOR == top10[6] |
                       co2data$SECTOR == top10[7] | co2data$SECTOR == top10[8] |
                       co2data$SECTOR == top10[9] | co2data$SECTOR == top10[10], ]
nrow(co2_top10)
tail(co2_top10)
View(co2_top10)

#' Now, return to the original task: Creating a scatterplot for the top 10 sectors:
top10_scatter <- ggplot(co2_top10, aes(year, log(co2_ann_kilotonnes), colour = 
                                     as.factor(SECTOR))) + geom_point() + stat_smooth(method = "lm")
(top10_scatter)

top10_scatter + theme_bw() # changes the background theme to black & white

#' Make further changes to the plot:
top10_scatter <- ggplot(co2_top10, aes(year, log(co2_ann_kilotonnes), colour = 
                as.factor(SECTOR))) + geom_point() + stat_smooth(method = "lm") + theme(
                  plot.background = element_blank(),
                  panel.grid.major = element_blank(),
                  panel.grid.minor = element_blank(),
                  panel.background = element_blank(),
                  axis.line = element_line(colour = "black"),
                  axis.title.x = element_text(size = 19, colour = "black"),
                  axis.title.y = element_text(size = 19, colour = "black"),
                  axis.text.x = element_text(size = 12, colour = "black"),
                  axis.text.y = element_text(size = 12, colour = "black"),
                  legend.text = element_text(size = 12),
                  legend.title = element_text(size = 15),
                  legend.background = element_rect(color = "black", 
                                       size = .5,
                                       linetype = "solid")) + labs(x = "Year",
                    y = "Annual CO2 production (Log kilotonnes)") + scale_color_brewer(
                      palette = "Paired", name = "Sector")
top10_scatter

#' 
#' 
## -----------------------------------------------------------------------------
## -----------------------------------------------------------------------------
#' ## Insetting Graphical Panels using ggplot2
# setwd("G:/My Drive/Programming/Data Science/DATA SETS")
mydata <- read.csv("Mammal_lifehistories_v2.csv")
names(mydata)
#' Remove observations with negative values in the variables of interest (the coding for 
#' ...missing cases in the current data):
mydata <- mydata[mydata$gestation.mo > 0 & mydata$mass.g > 0 & mydata$max.life.mo > 0, ]
head(mydata)
#' 
#' Add new columns for the logs of mass and gestation:
mydata$log_mass <- log(mydata$mass.g)
mydata$log_gest <- log(mydata$gestation.mo)
head(mydata)

# library(ggplot2) # library already opened previously
main <- ggplot(mydata, aes(log_mass, log_gest)) + geom_point() + theme_bw()
main
class(main)
#' 
sub <- main + geom_rect(data = mydata[1, ], xmin = 0, ymin = -1, xmax = 12, ymax =2.5,
                        fill = "blue", alpha = 0.5)
#* geom_rect = a geometric rectangle
#* alpha = transparency of the rectangle
#' 
sub$layers <- rev(sub$layers) # reorders the layers so that the graphical panel is placed 
#...inwards/under/below the scatter points (see output of the next code)

main + annotation_custom(ggplotGrob(sub), xmin = 0.01, xmax = 5, ymin = 1.5, 
                         ymax = 3.3) + scale_x_continuous(
                           limits = c(0, 12)) + scale_y_continuous(limits = c(-1, 3.3))
# xmin ... ymax: reads the coordinate of the inset within the main panel
# scale_x_continous: the lower and upper limits of the main panel's x axis
# scale_y_continous: the lower and upper limits of the main panel's y axis

#' >-- The main panel now shows just a portion of the plot covered by the rectangle \
#' >-- the sub panel (showing the blue rectangle and the rest of the entire graph) is inset 
#' at the bottom left of the main panel
#' 
