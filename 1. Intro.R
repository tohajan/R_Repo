#---

#------------------------------ Basic commands in R -------------------------
c(5, 7, 9, 8) #concatenates a series of items/objects
?apply #checks what a function is about (opens a new window about the function)
x <- c(9.0, 5.2, 7.1) # this syntax allows a function (right hand side-RHS) to be 
#saved into an object (LHS; e.g., var name, etc), and thus usable for future operations
length(x) # gives the length of an object
ls() # lists the objects saved thus far in the working directory
rm() # deletes previously saved objects; the objects are specified in the parentheses
rm(list = ls()) # deletes ALL saved objects

# sqrt()
#gives the square root of a number or object specified in the parentheses
# rnorm() # generates a vector of random NORMAL values; sample size is specified in parentheses
# runif() # generates a vector of random UNIFORM values; sample size is specified in parentheses
# NB: by default, regardless of sample size, mean=0 and S.D=1

f <- rnorm(50) # generates a vector of random normal values; sample size is specified
# by default, regardless of sample size, mean=0 and S.D=1
g <- rnorm(50, mean = 50, sd =.1)
# alternatively, the mean and SD can be specified

