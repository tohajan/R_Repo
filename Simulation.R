#' ---
#' title: "Simulation in R"
#' author: "Toheeb"
#' 
#' 
#' # **BASIC POPULATION DYNAMICS**
#' **Difference Equations in R 
#' **DISCRETE-TIME DYNAMICAL MODELS 
#' Difference equations are used for when changes occur in discrete time intervals
#' 
#' ## 1. Exponential Growth (i.e., a population growing indefinitely)
#' ### Method A -- Using difference equation
#' To calculate exponential growth at discrete time intervals, the difference equation (i.e., 
#' the discrete-time exponential equation) is:
#' N_t+1 = N~_ + bN_t - dN_t where N_t+1 = new popn size; N_t = old popn size; b = per capita 
#' ...birth rate; d = per capita death rate)
#'          = N_t + (b-d)N_t
#'          = N_t + (r)N_t 
#'          = (1 + r)N_t 
#'          = λN_t
#' 
#' Example 1: Calculate population size at 10 discrete times, using the formula above.
# Use a FOR loop
generations <- 10
N <- numeric(generations) # a vector with empty spaces for 10 numeric values
N[1] <- 3 # assume the first element in N is 3, i.e., the initial population size
lambda <- 2.1 # assume lambda (growth coefficient??) has a value of 2.1
for (t in 1:(generations - 1)) {
    N[t + 1] <- lambda * N[t] 
}
(N)

plot(0:(generations - 1), N, type = "o", xlab = "Time", ylab = "Size")
# NB: the population is initially at rest (no changes), hence the starting point is zero

#' 
#' Example 2: To calculate population growth for different populations with different starting 
#' sizes, lambdas, and time points, a function is more useful here:
pop_growth <- function(lambda, N0, gen) {
    for (i in 1:(gen - 1)) {
        N0[i + 1] <- lambda * N0[i]
    }
    return(N0)
}

#' 
# if lambda is 2.1 and population size starts at n = 3, what are new population sizes at 9 
#...more instances?
pop_growth(2.1, 3, 10)

#' 
# if lambda is 1.7 and population size starts at n = 5, what are new population sizes at 6 additional discrete times?
pop_growth(1.7, 5, 7)

#' 
#' Throw a graph into the mix:
popgrowth_graph <- function(lambda, N0, gen) {
    for (i in 1:(gen - 1)) {
        N0[i + 1] <- lambda * N0[i]
    }
    new_pop <- N0
    graph <- plot(0:(gen - 1), new_pop, xlab = "Time", ylab = "Size", type = "o")
    return(list(new_pop, graph))
}

popgrowth_graph(2.1, 3, 10)
popgrowth_graph(1.7, 5, 7)

#' 
#' #### PRACTICE QUESTIONS
#' 1. A population of amoeba doubles every 10 minutes. Starting with a sample size of 10, what 
#' will be the population at the end of 90 minutes?
rep <- 10 # 1 starting population, and 9 growth repetitions
pop <- numeric(rep)
pop[1] <- 10
lmb <- 2 # lambda
for (t in 1:(rep - 1)) {
    pop[t + 1] <- pop[t] * lmb
}
pop

#' 
timeInterval <- seq(0,90,10) # a sequence of 0 to 90, spaced at 10-unit intervals
plot(timeInterval, pop, xlab = "Time (in mins)", ylab = "Population size (in units)", type = "o")

#' 2. Write a function to calculate the exponential growth of some unnamed organism, given the 
#' starting pop size, multiplication factor, and number of repetitions.
expo <- function(size, mult_factor, cycles) {
    for (t in 1:(cycles - 1)) {
        size[t + 1] <- size[t] * mult_factor
    }
    graphing <- plot(0:(cycles - 1), size, xlab = "time", ylab = "popn size", type = "o")
    return(list(size, graphing))
}

expo(2, 3, 7)
expo(5, 2, 10)
expo(3, 4, 20)

#' 
#' 
## -----------------------------------------------------------------------------
## -----------------------------------------------------------------------------
#' ### Method B -- Using explicit solution:
#' N_t = (λ ^ t) * N_0 [where N_t = new pop size, t = time interval, N_0 = starting population]
#' 
#' Example: Calculate new pop sizes at 10 time intervals for a pop starting at n=3
lambda <- 2.1
t <- 0:10
N0 <- 3
Nt <- (lambda ^ t) * N0
(Nt)
plot(t, Nt, type = "o", xlab = "Time", ylab = "Popn size")

#' 
#' 
#' 
## -----------------------------------------------------------------------------
## -----------------------------------------------------------------------------
#' ## 2. Logistic Growth (i.e., a geometric growth followed by stability)
#' The Discrete-Time logistic equation (difference equation for logistic growth) is:
#' N_t+1~ = N_t + rN_t (1 - [N_t/K]). \
#' r = per capita birth rate minus per capita death rate; K = carrying capacity
#' NB: This equation simply adds the the last expression (in parenthesis) to the difference 
#' equation for exponential growth. As with exponential growth, log growth difference equation 
#' is solved/applied using simulation (For loops and Functions)
#' 
#' Example:
dlogistic <- function(k, r, n, generations) {
    for (x in 1:(generations - 1)) {
        n[x + 1] <- n[x] + (r * n[x]) * (1 - (n[x] / k))
    }
    new_popns <- n
    output <- plot(0:(generations - 1), new_popns, type = "o", xlab = "time", ylab = "Pop Size")
    return(list(new_popns, output))
}

dlogistic(k = 1000, r = 1.5,  n = 10, generations = 30)

#' 
#' 
#' 
## -----------------------------------------------------------------------------
## -----------------------------------------------------------------------------
## -----------------------------------------------------------------------------
## -----------------------------------------------------------------------------
## -----------------------------------------------------------------------------
## -----------------------------------------------------------------------------
#' # **DETERMINISTIC CHAOS**
#' A population growth dynamic may exhibit:
#' 1-point cycle: same population size repeated continuously
#' 2-point cycle: two populations repeated at certain intervals
#' 3-point cycle
#' ...and so on. 
#' However, a population may also exhibit an irregular, unpredictable pattern of growth. It is 
#' thus said to be in a "chaotic" state. Hence, deterministic CHAOS.
#' 
#' The difference equation here is:
#' X_t+1~ = r * X_t * (1-X~t~) [where: X_t+1 = the new ratio of existing population to maximum 
#'...possible population; X_t = the old ratio of existing population to maximum possible 
#'..population; r is per capita birth rate minus per capita death rate
canlogistic <- function(r, x0, generations) {
    for (i in 1:(generations - 1)) {
        x0[i + 1] <- r * x0[i] * (1 - x0[i])
    }
    pop_ratios <- x0
    return(pop_ratios)
}

generations <- 250
xt_1 <- canlogistic(2, 0.9, 250)
xt_2 <- canlogistic(3.5, 0.9, 250)
xt_3 <- canlogistic(3.9, 0.9, 250)
par(mfrow = c(1, 3))
plot(0:(generations - 1), xt_1, type = "o", xlim = c(200, 250), xlab = "Generations", ylab = "population density", main = "r=2")
plot(0:(generations - 1), xt_2, type = "o", xlim = c(200, 250), xlab = "Generations", ylab = "population density", main = "r=3.5")
plot(0:(generations - 1), xt_3, type = "o", xlim = c(200, 250), xlab = "Generations", ylab = "population density", main = "r=3.9")

#' Note that the graph becomes more "chaotic" as r increases
#' 
#' **Note to self**: The plots didn't display when script is executed in VSCode. Running the 
#' code ".vsc.attach()" fixed the issue.
#' 
#' 
## -----------------------------------------------------------------------------
## -----------------------------------------------------------------------------
#' ## Bifurcation Diagrams
#' A bifurcation diagram can help visualize how different patterns of growth appear in 
#' populations over periods
minr <- 2.5 # minimum r
maxr <- 4.0 # maximum r
inc <- 0.01 # incremental unit
rd <- seq(minr, maxr, inc)

#' In bifurcation diagrams, the interest is usually focused on later generations because 
#'...unpredictable patterns generally appear after the population has attained equilibrium.
ttransients <- 100 # "transient" is a term referring to the early periods (generations) in a 
#...population before it reaches equilibrium. In the current example, transient is set to 100, 
#...i.e., the diagram should ignore the 1st 100 generations of the population
#' 
trange <- 500 # the data will contain the last 400 generations (i.e, after the 1st 100, i.e., it 
#...will display only generations 101-500)
#' 
#create an empty plot:
par(mfrow = c(1, 1)) # This resets the plot (from a previous code that demarcates the plot for 
#...three objects). NB: this is only needed in a basic R environment, not in R Markdown (where 
#...codes are organized in chunks, such that restrictions/definitions in a previous plot 
#...doesn't carry over to a next one)

plot(c(minr, maxr), c(0, 1), type = "n", pch = ".", xlab = "r", 
     ylab = "Population density in the last 400 generations")

# Now insert the plot:
for (r in rd) {
    x <- 0.1
    for (i in 1:ttransients) {
        x <- r * x * (1 - x)
    }
    for (i in 101:trange) {
        x <- r * x * (1 - x)
        points(r, x, pch = ".")
    }
} # recall that rd, ttransients, and trange have been defined previously

#' PS: Notice the different point cycles (1, 2, 4, etc...) in the resulting bifurcation diagram

#' 
#' 
#' 
#' 
#' 
## -----------------------------------------------------------------------------
## -----------------------------------------------------------------------------
## -----------------------------------------------------------------------------
## -----------------------------------------------------------------------------
## -----------------------------------------------------------------------------
## -----------------------------------------------------------------------------
#' # **DIFFERENTIAL EQUATIONS IN R**
#' Differential equations are used for representing population dynamic when the population 
#' ..changes continuously (unlike at discrete time intervals)
#' 
#' ## Representing Basic Dynamics
# install.packages("deSolve")
#' 
#' The ode (ordinary differential equation) function used below is contained in the "deSolve" 
#' ..package. Some of the function's arguments are:
#' y: the state variable(s), y[1] is the 1st state var, y[2] is 2nd, etc.
#' times: sequence of output\
#' func: the set of derivatives; defined as function(times, y, parms)
#' parms: parameters (constant) passed, p[1] is the 1st parameter
#' method: numerical integration algorithm e.g., Runge-Kutta (default is Isoda)
library(deSolve)
#' 
cgrowth <- function(times, y, parms) {
    dN_dT <- p[1] * y[1]
    return(list(dN_dT))
}

#' 
p <- 0.5
y0 <- 2
t <- 0:20
sol <- ode(y = y0, times = t, func = cgrowth, parms = p) # ode = ordinary differential equation
(sol)
#' 
plot(t, sol[, 2], type = "o")

#' 
## -----------------------------------------------------------------------------
## -----------------------------------------------------------------------------
#' ## Continuous time logistic equation
#' The above problem can also be solved using the continuous time logistic equation (i.e., 
#' dN/dT = rN (1 - N/k):
clogistic <- function(times, y, parms) {
    dN_dT <- p[1] * y[1] * (1 - (y[1] / p[2]))
    return(list(dN_dT))
}
#' 
p <- c(0.5, 1000)
y0 <- 2
t <- 0:30
sol2 <- ode(y = y0, times = t, func = clogistic, parms = p)
sol2
plot(t, sol2[, 2], type = "o")
# Note the sigmoidal shape of the curve, which is characteristic of a logistic growth

#' 
#' 
## -----------------------------------------------------------------------------
## -----------------------------------------------------------------------------
#' ## Lotka-Volterra Predation Equations
#' Instantaneous rate of change in the prey population is given as: 
#'        dN/dT = (r * N) - a * P * N
#' Instantaneous rate of change in the predator popn is given as: 
#'        dP/dT = -(b * P) + f * P * N
library(lattice)
r <- 0.5; a <- 0.01; f <- 0.01; b <- 0.2
p <- c(r, a, b, f)
y0 <- c(N = 25, P = 5)
t <- seq(0, 200, 0.1)

#' 
pred_prey <- function(times, y, parms) {
    N <- y[1]
    P <- y[2]
    with(as.list(p), {
        dN_dT <- (r * N) - (a * P * N)
        dP_dT <- -(b * P) + (f * P * N)
        return(list(c(dN_dT, dP_dT)))
    }
    )
}
#' 
output <- ode(y = y0, times = t, func = pred_prey, parms = p)
output[1:10, ] # first 10 observations/rows/elements in the object "output"
par(mfrow = c(2, 1))
matplot(output[, 1], output[, 2:3], type = "o", xlab = "Time??",ylab = "Population Size")

# Or, plot a graph of predator density against prey density
plot(output[, 2], output[, 3], type = "l", xlab = "Prey density", ylab = "Predator density")

#' 
#' 
#' 
## -----------------------------------------------------------------------------
## -----------------------------------------------------------------------------
## -----------------------------------------------------------------------------
## -----------------------------------------------------------------------------
## -----------------------------------------------------------------------------
## -----------------------------------------------------------------------------
#' # **STOCHASTIC MODELS**
#' Stochastic models deal with random variables/numbers. Random numbers can be generated by 
#' ...using any of two procedures:
#' 1. By drawing from existing distributions
#' 2. By drawing from custom-made selections
#' 
#' ## 1. Using Existing Distributions
#' ### A. Uniform Distribution -- the general form of the function is: \
#'        runif(n, min, max)
#' 
runif(15, 1, 10) # generate 15 random numbers of uniform distribution btw 1 and 10
#' 
#' If the min and max are not specified, R sets the default to 0 and 1 respectively:
runif(15) #15 random numbers of uniform distribution btw 0 and 1

#' 
#' ### B. Normal (or Gaussian) Distribution
#' This distribution takes the shape of a bell curve. The general form of its function is:
#'        rnorm(n, mean, sd)
rnorm(10, 50, 100)

#' 
#' As with runif(), the mean and sd has a default value of 0 and 1 respectively:
rnorm(10) # ten random numbers from the normal distribution, with a mean of 0 and sd of 1

#' 
#' ### C. Binomial Distribution
#' This generates the random number of successes of independent trials. The general form of the 
#' ...function is:
#'          rbinom(n, size, prob)
#'          
#' Example: an unbiased coin was tossed 100 times in a row, once a day for a week with a 
#' ...probability that each toss is a success (i.e., heads)
#' 
rbinom(n = 7, size = 100, prob = 0.5) # this outputs the number of successes for each trial(n=7)

#' 
#' 
## -----------------------------------------------------------------------------
## -----------------------------------------------------------------------------
#' ## 2. Using custom-made selections
set.seed(3) # this can also be used with the previous functions -- ensures the same set of 
#...numbers is produced each time
sample(1:10, 5) # from 1 to 10, select 5 random numbers
# NB: this is different from runif(5,1,10), whose output would include decimals

#' Alternatively:
population <- 1:10
sample(population, 5)

#' 
#' NB - the vector does not have to be sequential, i.e., could be any collection of numbers
pop <- c(30, 10, 50, 20, 70, 90, 40, 100, 80, 60)
sample(pop, 5)

#' 
#' It could also be a collection of strings/letters:
popn <- c("A", "B", "C", "D", "E", "F", "G", "H", "I", "J")
sample(popn, 5)

#' 
#' When the sample specified is larger than the pop, R returns an error because the replace 
#' ...argument is defaulted to "False". To avoid the error message, set replace to "TRUE".
sample(popn, 15, replace = TRUE)

#' 
#' Another example:
options <- c("heads", "tails")
sample(options, 10, replace = TRUE) # there are only two elements ("heads"  and "tails"). But 
#...the function asks for 10 drawings. Hence, the replace argument is set to "TRUE."

#' 
#' -------END OF SCRIPT -------
#' 
#' 
