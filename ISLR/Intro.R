###################################
#' Simple vectors
#' 
x <- c(2,7,5)
x
y <- seq(from=4, length=3, by=3)
y
x+y
x/y
x^y
x[2]
x[2:3]
x[-2]

#' -----------------------------------------------------------------------------
#' Matrices (2-way arrays)
z <- matrix(seq(1,12), 4, 3)
z
z[3:4, 2:3]
z[, 2:3]
z[, 1] # the output is in a vector format
z[, 1, drop=FALSE] #this retains the matrix format
dim(z)
#' 
#' 
ls() #lists all objects in memory
rm(y) #removes the specified object
ls() #check to confirm the deleted object is no longer in the list
#' 
#' 
#' -----------------------------------------------------------------------------
#' Generating random data, graphics
x <- runif(50) #creates 50 random uniform variables
y <- rnorm(50) #creates 50 random normal variables
plot(x, y) #plot a graph of x vs y
plot(x, y, xlab="Random Uniform", ylab="Random Normal", pch="*", col="blue") #more details
par(mfrow=c(2,1)) #split the plot region
plot(x,y)
hist(y)
#' 
#' 
#' -----------------------------------------------------------------------------
#' Reading in data
Auto <- read.csv("G:/My Drive/Programming/Data Science/DATA SETS/auto.csv")
names(Auto)
dim(Auto)
class(Auto)
summary(Auto)
par(mfrow=c(1,1))
plot(Auto$cylinders, Auto$mpg)
plot(Auto$cyl, Auto$mpg) # note the shortening of the var name ("cyl"). Same output.
attach(Auto) #this created a workspace containing the df, so that variables in the df can be 
#called directly without invoking the df name and dollar sign 
search() #lists the all the workspace available
plot(cylinders, mpg) #now, the variables are used directly, without specifying the df
cylinders <- as.factor(cylinders) #converts the variable to categorical
plot(cylinders, mpg) #the output is now a boxplot
#' 
#' 
#' 
#' 
#' 
#' 
#' 