## -----------------------------------------------------------------------------------------------------------------
carseats <- read.csv("C:/Users/tohaj/Box/Programming/Data Science/DATA SETS/Carseats.csv")
View(carseats) # displays the data in a separate tab in RStudio


## -----------------------------------------------------------------------------------------------------------------
age <- carseats$Age
educ <- carseats$Education
age_educ <- cbind(age, educ)
write.table(age_educ, file = "C:/Users/tohaj/Box/Programming/Data Science/R Learning/Data Handling & Graphics/age_educ.csv") #this syntax stores the newly created data frame in specified format and location.


## -----------------------------------------------------------------------------------------------------------------
my_data <- airquality # "airquality" is a data set readily available in R database.


## -----------------------------------------------------------------------------------------------------------------
head(my_data) # prints the first 6 elements of the data
head(my_data, n = 4) # first 4 rows
head(my_data, n = -10) # all but the last 10


## -----------------------------------------------------------------------------------------------------------------
tail(my_data)# last 6
tail(my_data, n = 12) # last 12
tail(my_data, n = -20) # all but the first 20


## -----------------------------------------------------------------------------------------------------------------
class(my_data) # tells the class/type of the object (here it is a data frame)
length(my_data) # length of the object (i.e., no. of cols in the df)
ncol(my_data) # same as above
nrow(my_data) # no. of rows
names(my_data) # names of columns
rownames(my_data) # index names (rows)
str(my_data) # gives a brief overview of the df, including data type and entries
summary(my_data) # gives summary statistics (mean, median, min, etc.) for each column


## -----------------------------------------------------------------------------------------------------------------
library("dplyr")
glimpse(my_data) # alternative to the str() function


## -----------------------------------------------------------------------------------------------------------------
my_data$Month <- factor(my_data$Month) # converts the Month var to factor type (coz the var is categorical)


## -----------------------------------------------------------------------------------------------------------------
glimpse(my_data)
levels(my_data$Month) <- list(May = "5", June = "6", July = "7", Aug = "8", Sept = "9") # labels the values/levels of the Month var
levels(my_data$Month) # lists the categories in the Month column


## -----------------------------------------------------------------------------------------------------------------
plot(my_data$Ozone, my_data$Temp)
idx <- identify(my_data$Ozone, my_data$Temp)
# this syntax allows to click on specific points on the graph.


## -----------------------------------------------------------------------------------------------------------------
plot(my_data$Ozone, my_data$Temp)
idx <- identify(my_data$Ozone, my_data$Temp, labels = my_data$Month, plot = TRUE) # labels the data points with the corresponding month.


## -----------------------------------------------------------------------------------------------------------------
plot(my_data$Ozone, my_data$Temp)
idx <- identify(my_data$Ozone, my_data$Temp, labels = paste(as.character(my_data$Day),
                      "-", as.character(my_data$Month)), plot = TRUE)


## -----------------------------------------------------------------------------------------------------------------
xtabs(~Month, my_data) 


## -----------------------------------------------------------------------------------------------------------------
my_data[154, ] <- c(NA) # adds another row all with NA values
my_data[, 7] <- c(NA) # adds a new col all having NA values
View (my_data)


## -----------------------------------------------------------------------------------------------------------------
View(is.na(my_data)) # checks for all NA values anywhere in the data set, returns table displaying TRUE/FALSE


## -----------------------------------------------------------------------------------------------------------------
any(is.na(my_data)) # returns TRUE if at least one missing case exists in the data


## -----------------------------------------------------------------------------------------------------------------
all(is.na(my_data)) # checks if all entries in the specified object (i.e., the entire dataset) are missing values


## -----------------------------------------------------------------------------------------------------------------
all(is.na(my_data[,7])) # checks if all entries in the specified column are missing values


## -----------------------------------------------------------------------------------------------------------------
# The recently added row and column contain only missing cases, so they can be removed:
my_data <- my_data[-7] # removes the last column (no. 7)
my_data <- my_data[-154, ] # removes the last row (no. 154)


## -----------------------------------------------------------------------------------------------------------------
any(is.na(my_data)) # rechecks for missing cases
sum(is.na(my_data)) # gives the total no. of missing obs in the entire data set


## -----------------------------------------------------------------------------------------------------------------
sum(is.na(my_data$Solar.R)) # gives the no. of missing obs in a column


## -----------------------------------------------------------------------------------------------------------------
colSums(is.na(my_data)) # shows the distribution of missing cases across all columns


## -----------------------------------------------------------------------------------------------------------------
clean_data <- na.omit(my_data) # generates a new data with all missing cases removed.
nrow(clean_data)


## -----------------------------------------------------------------------------------------------------------------
nrow(my_data)-nrow(clean_data) # gives the difference between the no. of rows in the original data and the # rows in the cleaned data (i.e., no. of missing obs rows deleted)


## -----------------------------------------------------------------------------------------------------------------
clean_data2 <- my_data[complete.cases(my_data), ] # this code indexes only the rows with complete cases (no missing values)
nrow(clean_data2)


## -----------------------------------------------------------------------------------------------------------------
clean_data3 <- na.omit(my_data[-1]) # this removes the NA values from all but col 1 (Ozone)
nrow(clean_data3)


## -----------------------------------------------------------------------------------------------------------------
nrow(my_data)-nrow(clean_data3) # only 7 rows removed (as opposed to 42 rows deleted with the previous approach)


## -----------------------------------------------------------------------------------------------------------------
clean_data4 <- my_data[, colSums(is.na(my_data)) < 10] # a new data containing only columns with less than 10 missing cases


## -----------------------------------------------------------------------------------------------------------------
final_data <- na.omit(clean_data4)
nrow(final_data)


## -----------------------------------------------------------------------------------------------------------------
setwd("C:/Users/tohaj/Box/Programming/Data Science/DATA SETS") # changes the working directory
getwd() #retrieves the current working directory
owl_morph <- read.csv("owl.morphometrics.csv", header = TRUE)


## -----------------------------------------------------------------------------------------------------------------
getwd()
#retrieves the current working directory


## -----------------------------------------------------------------------------------------------------------------
summary(owl_morph)
View(owl_morph)
any(is.na(owl_morph))
str(owl_morph)


## -----------------------------------------------------------------------------------------------------------------
owl_morph$common.name <- as.factor(owl_morph$common.name)
owl_morph$latin <- as.factor(owl_morph$latin)


## -----------------------------------------------------------------------------------------------------------------
plot(owl_morph$weight.g, owl_morph$wingspan.cm, xlab = "Owl weight (g)", ylab = "Owl wingspan (cm)")


## -----------------------------------------------------------------------------------------------------------------
setwd("C:/Users/tohaj/Box/Programming/Data Science/DATA SETS")
owl_clutch <- read.csv("owl.clutch.size.csv", header = TRUE)
View(owl_clutch)
any(is.na(owl_clutch))


## -----------------------------------------------------------------------------------------------------------------
owl_morph_clutch <- cbind(owl_morph, owl_clutch)
View(owl_morph_clutch)


## -----------------------------------------------------------------------------------------------------------------
owl_morph_clutch <- cbind(owl_morph, owl_clutch[, 2]) # only the row 2 of the second data is called upon for the combination


## -----------------------------------------------------------------------------------------------------------------
names(owl_morph_clutch)[6] <- "clutch.size" # renames the newly added column


## -----------------------------------------------------------------------------------------------------------------
plot(owl_morph_clutch$wingspan.cm, owl_morph_clutch$clutch.size,
     xlab = "Owl wing span (cm)", ylab = "Owl clutch size (??)")


## -----------------------------------------------------------------------------------------------------------------
setwd("C:/Users/tohaj/Box/Programming/Data Science/DATA SETS")
owl_lspan <- read.csv("owl.lifespan.csv", header = TRUE)
View(owl_lspan)
any(is.na(owl_lspan))


## -----------------------------------------------------------------------------------------------------------------
owl_morph_clutch_life <- merge(owl_morph_clutch, owl_lspan, key = "common.name")
View(owl_morph_clutch_life)


## -----------------------------------------------------------------------------------------------------------------
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


## -----------------------------------------------------------------------------------------------------------------
max(duckweed_mat[1,])
max(duckweed_mat[2,])
max(duckweed_mat[3,])


## -----------------------------------------------------------------------------------------------------------------
for (i in 1:10) {
  row <- duckweed_mat[i, ]
  max <- max(row)
  print(max)
}


## -----------------------------------------------------------------------------------------------------------------
apply(duckweed_mat, 1, max) # returns the maximum value in each row
#NB: the 2nd argument is the margin -- 1 represents row; 2 implies column


## -----------------------------------------------------------------------------------------------------------------
apply(duckweed_mat, 2, max)
# this prints out the maximum value in each column


## -----------------------------------------------------------------------------------------------------------------
duckweed_df <- data.frame(R1 = c(10, 20, 30, 40, 50, 60, 70, 80, 90, 100),
                          R2 = c(10, 30, 50, 80, 100, 150, 200, 250, 270, 300),
                          R3 = c(10, 30, 36, 80, 96, 106, 110, 130, 136, 144),
                          R4 = c(10, 15, 30, 50, 70, 86, 95, 100, 105, 190),
                          R5 = c(10, 40, 50, 65, 78, 96, 107, 120, 144, 157),
                          R6 = c(10, 30, 57, 98, 106, 130, 160, 177, 189, 198))
class(duckweed_df)


## -----------------------------------------------------------------------------------------------------------------
rowMeans(duckweed_df)


## -----------------------------------------------------------------------------------------------------------------
apply(duckweed_df, 1, mean)


## -----------------------------------------------------------------------------------------------------------------
duckweed_df$Day <- as.factor(1:10)
(duckweed_df)


## -----------------------------------------------------------------------------------------------------------------
duckweed_df <- duckweed_df[, c(7, 1:6)] # rearranges the data so that the new column comes 1st
class(duckweed_df$Day) # check that the column is correctly identified as factor 


## -----------------------------------------------------------------------------------------------------------------
apply(duckweed_df, 1, mean)


## -----------------------------------------------------------------------------------------------------------------
apply(duckweed_df[, 2:7], 1, mean) # the non-numeric column is now excluded
apply(duckweed_df[, -1], 1, mean) # can also be excluded this way
apply(duckweed_df[, -c(1,2,4,6)], 1, mean) # can also exclude several columns


## -----------------------------------------------------------------------------------------------------------------
duckweed_df[6, 5] <- NA
(duckweed_df)
apply(duckweed_df[, -1], 1, mean)


## -----------------------------------------------------------------------------------------------------------------
apply(duckweed_df[, -1], 1, mean, na.rm = TRUE) # na.rm "removes" the missing value(s) from the calculation


## -----------------------------------------------------------------------------------------------------------------
prop <- function(x) {
  x / max(x)
}


## -----------------------------------------------------------------------------------------------------------------
(duckweed_mat)
apply(duckweed_mat, 2, prop)


## -----------------------------------------------------------------------------------------------------------------
cago_list <- list(Diet1 = c(2, 5, 4, 5, 3, 5, 4, 4, 4, 5),
                  Diet2 = c(8, 5, 6, 5, 7, 7, 6, 8, 8, 3),
                  Diet3 = c(3, 4, 2, 5, 2, 6, 5, 6, 2, 4),
                  Diet4 = c(2, 2, 3, 2, 5, 2, 4, 3, 5, 7))
class(cago_list)
(cago_list)


## -----------------------------------------------------------------------------------------------------------------
lapply(cago_list, mean)


## -----------------------------------------------------------------------------------------------------------------
cago_df <- data.frame(Diet1 = c(2, 5, 4, 5, 3, 5, 4, 4, 4, 5),
                  Diet2 = c(8, 5, 6, 5, 7, 7, 6, 8, 8, 3),
                  Diet3 = c(3, 4, 2, 5, 2, 6, 5, 6, 2, 4),
                  Diet4 = c(2, 2, 3, 2, 5, 2, 4, 3, 5, 7))
lapply(cago_df, mean)


## -----------------------------------------------------------------------------------------------------------------
random <- c("This", "is", "a", "random", "vector")


## -----------------------------------------------------------------------------------------------------------------
lapply(random, nchar)


## -----------------------------------------------------------------------------------------------------------------
nchar(random)


## -----------------------------------------------------------------------------------------------------------------
cago_list
sapply(cago_list, mean) # results are in a vector format


## -----------------------------------------------------------------------------------------------------------------
cago_df
sapply(cago_df, mean) # results also in vector format


## -----------------------------------------------------------------------------------------------------------------
random
sapply(random, nchar)


## -----------------------------------------------------------------------------------------------------------------
sequence <- function(x) {
  seq(nchar(x))
}


## -----------------------------------------------------------------------------------------------------------------
seq(3) # this outputs "1 2 3"
seq(nchar("that")) #this outputs "1 2 3 4"


## -----------------------------------------------------------------------------------------------------------------
sapply(random, sequence)


## -----------------------------------------------------------------------------------------------------------------
patient_ID <- 1:30
age <- c(32, 45, 44, 34, 23, 26, 37, 45, 12, 23, 44, 35, 57, 65, 76, 43, 42, 34,
         36, 37, 23, 21, 28, 24, 29, 13, 18, 32, 25, 28)
treatment <- c("a", "c", "c", "b", "b", "b", "c", "b", "c", "a", "a", "a", "a", "a","b",
               "b", "b", "b", "c", "c", "c", "a", "b", "c", "b", "a", "a", "c", "a", "c")


## -----------------------------------------------------------------------------------------------------------------
tapply(age, treatment, mean) # gives the mean age by treatment groups


## -----------------------------------------------------------------------------------------------------------------
med_df <- data.frame(patient_ID, treatment, age)
head(med_df) # Recall: 6 rows by default (same as the tail fxn)


## -----------------------------------------------------------------------------------------------------------------
tapply(med_df$age, med_df$treatment, mean)


## -----------------------------------------------------------------------------------------------------------------
med_list <- list(patient_ID = patient_ID, treatment = treatment, age = age)
tapply(med_list$age, med_list$treatment, mean)


## -----------------------------------------------------------------------------------------------------------------
co2_uptake <- CO2
head(co2_uptake)


## -----------------------------------------------------------------------------------------------------------------
co2_uptake$height <- c(35.77, 43.95, 38.10, 43.20, 43.02, 39.19, 31.60, 36.88, 41.11, 43.64,
                       36.82, 33.86, 30.17, 36.92, 36.15, 43.60, 32.35, 43.92, 40.50, 37.46, 
                       33.92, 42.19, 30.20, 35.64, 39.63, 36.39, 42.95, 33.88, 43.75, 41.10, 
                       34.57, 30.21, 37.19, 33.45, 40.93, 32.93, 36.21, 40.74, 32.87, 35.98,
                       43.57, 39.91, 35.02, 33.20, 37.89, 34.96, 30.99, 40.12, 33.33, 34.48,
                       38.22, 35.21, 39.60, 40.29, 42.90, 36.09, 38.75, 36.65, 31.51, 39.32, 
                       30.27, 34.21, 40.59, 43.67, 32.10, 30.08, 42.10, 36.60, 43.89, 38.33,
                       36.99, 32.05, 37.54, 34.51, 33.69, 41.80, 30.91, 39.23, 30.93, 42.73,
                       34.82, 33.20, 31.57, 43.32)

View(co2_uptake)


## -----------------------------------------------------------------------------------------------------------------
aggregate(uptake~conc, co2_uptake, mean) # tabular output


## -----------------------------------------------------------------------------------------------------------------
tapply(co2_uptake$uptake, co2_uptake$conc, mean) # vector output


## -----------------------------------------------------------------------------------------------------------------
aggregate(uptake ~ conc + Treatment, co2_uptake, mean)


## -----------------------------------------------------------------------------------------------------------------
aggregate(uptake ~ conc + Treatment + Type, co2_uptake, mean)


## -----------------------------------------------------------------------------------------------------------------
aggregate(cbind(uptake, height) ~ conc, co2_uptake, mean)


## -----------------------------------------------------------------------------------------------------------------
co2uptake_list <- list(plant = co2_uptake$Plant, type = co2_uptake$Type, 
                       treatment = co2_uptake$Treatment, conc = co2_uptake$conc, 
                       uptake = co2_uptake$uptake, height = co2_uptake$height)


## -----------------------------------------------------------------------------------------------------------------
aggregate(cbind(uptake, height) ~ conc, co2uptake_list, mean)


## -----------------------------------------------------------------------------------------------------------------
aggregate(uptake ~ conc, co2uptake_list, length) # no. of uptake values recorded for each conc group
aggregate(height ~ conc, co2uptake_list, length) # no. of height values per conc group
aggregate(type ~ conc, co2uptake_list, length) # no. of type values per conc group


## -----------------------------------------------------------------------------------------------------------------
paste("RWBL", 1)


## -----------------------------------------------------------------------------------------------------------------
species <- "RWBL"
num <- 1
paste(species, num)


## -----------------------------------------------------------------------------------------------------------------
paste(species, num, sep = "-")


## -----------------------------------------------------------------------------------------------------------------
id <- c("RWBL", 1)
paste(id) # each element in "id" is pasted separately


## -----------------------------------------------------------------------------------------------------------------
paste(id, collapse = " ") # default value for collapse is NULL
# the paste function now works as expected


## -----------------------------------------------------------------------------------------------------------------
paste(id, collapse = "_")


## -----------------------------------------------------------------------------------------------------------------



## -----------------------------------------------------------------------------------------------------------------
species <- c("RWBL", "MODO", "AMRO", "AMCR", "MODO")
num <- 1:5
paste(species, num, sep = "_")


## -----------------------------------------------------------------------------------------------------------------
species <- c("RWBL", "MODO", "AMRO", "AMCR", "MODO")
num <- 1:8
paste(species, num, sep = "_")


## -----------------------------------------------------------------------------------------------------------------
num <- 1:10
paste("Bird", num) # the "Bird" element gets recycled through the num var, thus printing Bird 1, Bird 2, ... Bird 10.


## -----------------------------------------------------------------------------------------------------------------
df <- data.frame(species, num = 1:5)
paste(df$species, df$num, sep = "_") # as usual, the output is vector


## -----------------------------------------------------------------------------------------------------------------
output = paste(df$species, df$num, sep = "_")
df$idnum = output


## -----------------------------------------------------------------------------------------------------------------
View(df)


## -----------------------------------------------------------------------------------------------------------------
maxd <- 10
storks <- numeric(maxd)
babies <- numeric(maxd)
set.seed(15) # to retain the results of the rnorm functions
for (i in 1:maxd) {
  storks[i] <- rnorm(1, i * 100, 10) # Recall -- general form is rnorm(n, mean, sd)
  babies[i] <- rnorm(1, i * 10, 10)
}


## -----------------------------------------------------------------------------------------------------------------
(storks)
(babies)


## -----------------------------------------------------------------------------------------------------------------
plot(storks, babies)
abline(lm(babies ~ storks)) # fits a regression line to the plot

# let's plot a new graph on the existing plot: 
newstorkdata <- c(220, 411, 630, 705, 729, 850)
newbabydata <- c(11, 22, 38, 41, 45, 52)
points(newstorkdata, newbabydata, col = "red", pch = 19) #plots this on the existing graph; red  here is the color of the new points/observations
abline(lm(newbabydata ~ newstorkdata), col = "red") # red here is the color of the new line

# add a legend to the graph:
legend(100, 90, c("old data", "new data"), pch = c(1, 19), col = c("black", "red"))
# NB: The first argument for the legend function is "x,y". Here, the value is 100,90, i.e., the legend should be placed at the point on the graph where x=100 and y=90


## -----------------------------------------------------------------------------------------------------------------



## -----------------------------------------------------------------------------------------------------------------
par(mfrow= c(1, 2)) # splits the plot area into a row with 2 columns
# Recall that this code does not affect subsequent code chunks. However, in R scripts, the plot area must be reset to return to the original, non-split type, i.e., by running the code: par(mfrow = c(1,1))


## -----------------------------------------------------------------------------------------------------------------
mydata <- ToothGrowth # ToothGrowth is a built-in data set in R
mydata$dose <- as.factor(mydata$dose) #changes the dose var to factor data type
View(mydata)


## -----------------------------------------------------------------------------------------------------------------
# install.packages("tidyverse") 
library("ggplot2")
names(mydata)


## -----------------------------------------------------------------------------------------------------------------
ggplot(mydata, aes(supp, len)) # creates an empty plot 


## -----------------------------------------------------------------------------------------------------------------
ggplot(mydata, aes(supp, len)) + geom_boxplot() # inserts a boxplot into the empty plot created above


## -----------------------------------------------------------------------------------------------------------------
# differentiate columns of box plots by the dose category
ggplot(mydata, aes(supp, len)) + geom_boxplot() + facet_grid(. ~ dose)


## -----------------------------------------------------------------------------------------------------------------
ggplot(mydata, aes(supp, len, fill = dose)) + geom_boxplot() # this creates all box plots on a single graph, each category's plot differentiated by a distinct color.


## -----------------------------------------------------------------------------------------------------------------
my_color <- c("#fff7bc", "#fec44f", "#d95f0e") # yellow, orange, and red colors, respectively
ggplot(mydata, aes(supp, len, fill = dose)) + geom_boxplot() + scale_fill_manual(
                                    name = "Dose (mm)", breaks = c("0.5", "1", "2"),
                                   labels = c("Low 0.5mm", "Medium 1mm", "High 2mm"),
                                   values = my_color)


## -----------------------------------------------------------------------------------------------------------------
ggplot(mydata, aes(supp, len, fill = dose)) + geom_boxplot() + scale_fill_manual(
  name = "Dose (mm)", breaks = c("0.5", "1", "2"),
  labels = c("Low 0.5mm", "Medium 1mm", "High 2mm"),
  values = my_color) + theme_bw() # NB: the previous color was grey-ish


## -----------------------------------------------------------------------------------------------------------------
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


## -----------------------------------------------------------------------------------------------------------------
setwd("C:/Users/tohaj/Box/Programming/Data Science/DATA SETS")
getwd()
mydata <- read.csv("Mammal_lifehistories_v2.csv")
names(mydata)
View(mydata)
head(mydata)
summary(mydata)
ncol(mydata)
nrow(mydata)


## -----------------------------------------------------------------------------------------------------------------
mydata <- mydata[mydata$gestation.mo > 0 & mydata$mass.g > 0 & mydata$litter.size > 0, ]


## -----------------------------------------------------------------------------------------------------------------
m <- ggplot(mydata, aes(x = gestation.mo))
m + geom_histogram()


## -----------------------------------------------------------------------------------------------------------------
m + geom_histogram(binwidth = 0.4)


## -----------------------------------------------------------------------------------------------------------------
m + geom_histogram(binwidth = 0.4) + facet_grid(order ~ .)


## -----------------------------------------------------------------------------------------------------------------
xtabs(~ order, mydata)


## -----------------------------------------------------------------------------------------------------------------
mydata_largeorders <- mydata[mydata$order == "Artiodactyla" | mydata$order == "Carnivora" |
                               mydata$order == "Primates" | mydata$order == "Rodentia", ]


## -----------------------------------------------------------------------------------------------------------------
m2 <- ggplot(mydata_largeorders, aes(x = gestation.mo))
m2 + geom_histogram(binwidth = 0.7) + facet_grid(order ~ .)


## -----------------------------------------------------------------------------------------------------------------
setwd("C:/Users/tohaj/Box/Programming/Data Science/DATA SETS")
co2data <- read.csv("canada.co2.by.sector.csv")
names(co2data)
nrow(co2data); dim(co2data)
summary(co2data)
View(co2data)
head(co2data)



## -----------------------------------------------------------------------------------------------------------------
co2data <- co2data[co2data$SECTOR != "Total, all sectors", ]


## -----------------------------------------------------------------------------------------------------------------
# install.packages("tidyverse") # package already installed
# library(ggplot2) # library opened already
co2_scatter <- ggplot(co2data, aes(year, log(co2_ann_kilotonnes), colour = 
                                     SECTOR)) + geom_point() + stat_smooth(method = "lm")
co2_scatter


## -----------------------------------------------------------------------------------------------------------------
library("dplyr")
co2data_order <- co2data %>% group_by(SECTOR) %>% summarise(co2mean = 
                            mean(co2_ann_kilotonnes)) %>% arrange(desc(co2mean))


## -----------------------------------------------------------------------------------------------------------------
View(co2data_order)
head(co2data_order)


## -----------------------------------------------------------------------------------------------------------------
co2data_order_top10 <- co2data_order[1:10, ] # a new data set containing only the top 10 SECTORS by mean co2 output


## -----------------------------------------------------------------------------------------------------------------
top10 <- c(co2data_order_top10$SECTOR) # concatenates the values of the SECTOR column
top10[5] # the 5th element in the vector


## -----------------------------------------------------------------------------------------------------------------
co2_top10 <- co2data[co2data$SECTOR == top10[1] | co2data$SECTOR == top10[2] |
                       co2data$SECTOR == top10[3] | co2data$SECTOR == top10[4] |
                       co2data$SECTOR == top10[5] | co2data$SECTOR == top10[6] |
                       co2data$SECTOR == top10[7] | co2data$SECTOR == top10[8] |
                       co2data$SECTOR == top10[9] | co2data$SECTOR == top10[10], ]
nrow(co2_top10)
tail(co2_top10)
View(co2_top10)


## -----------------------------------------------------------------------------------------------------------------
top10_scatter <- ggplot(co2_top10, aes(year, log(co2_ann_kilotonnes), colour = 
                                     as.factor(SECTOR))) + geom_point() + stat_smooth(method = "lm")
(top10_scatter)


## -----------------------------------------------------------------------------------------------------------------
top10_scatter + theme_bw() # changes the background theme to black & white


## -----------------------------------------------------------------------------------------------------------------
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


## -----------------------------------------------------------------------------------------------------------------
setwd("C:/Users/tohaj/Box/Programming/Data Science/DATA SETS")
mydata <- read.csv("Mammal_lifehistories_v2.csv")
names(mydata)


## -----------------------------------------------------------------------------------------------------------------
mydata <- mydata[mydata$gestation.mo > 0 & mydata$mass.g > 0 & mydata$max.life.mo > 0, ]
head(mydata)


## -----------------------------------------------------------------------------------------------------------------
mydata$log_mass <- log(mydata$mass.g)
mydata$log_gest <- log(mydata$gestation.mo)
head(mydata)


## -----------------------------------------------------------------------------------------------------------------
# library(ggplot2) # library already opened previously
main <- ggplot(mydata, aes(log_mass, log_gest)) + geom_point() + theme_bw()
main
class(main)


## -----------------------------------------------------------------------------------------------------------------
sub <- main + geom_rect(data = mydata[1, ], xmin = 0, ymin = -1, xmax = 12, ymax =2.5,
                        fill = "blue", alpha = 0.5)
#* geom_rect = a geometric rectangle
#* alpha = transparency of the rectangle


## -----------------------------------------------------------------------------------------------------------------
sub$layers <- rev(sub$layers) # reorders the layers to draw rectangle below the points


## -----------------------------------------------------------------------------------------------------------------
main + annotation_custom(ggplotGrob(sub), xmin = 0.01, xmax = 5, ymin = 1.5, 
                         ymax = 3.3) + scale_x_continuous(
                           limits = c(0, 12)) + scale_y_continuous(limits = c(-1, 3.3))
# xmin ... ymax: reads the coordinate of the inset within the main panel
# scale_x_continous: the lower and upper limits of the main panel's x axis
# scale_y_continous: the lower and upper limits of the main panel's y axis

