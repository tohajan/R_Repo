#' ---
#' title: "Statistical Modeling"
#' author: "Toheeb"
#' ---
#' 
#' # **Simple Linear Regression**
#' Linear regression:
#' -- i) describes the relationship between variables (including at least 1 continuous); and
#' -- ii) tests for the evidence of a linear relationship of a given slope btw the 2 variables.
#' 
#' The term was coined by Francis Glaton 1889 ("regression to mediocrity"). The overall approach 
#'...was further developed by Pearson and Fisher.
#' 
setwd("G:/My Drive/Programming/Data Science/DATA SETS")
gannet <- read.csv("GannetDataCSV.csv")
View(gannet)
head(gannet)

#' 
par(pch = 19) # sets the plot style
plot(gannet$sqrtpop, gannet$tripduration, xlab = "sqrt colonoy size",
     ylab = "trip duration (h)")
text(gannet$sqrtpop, gannet$tripduration, labels = gannet$colony, cex = 0.6, pos = 4) # this 
#...syntax labels points on the graph
#     labels: defines the labeling rule/indicator for each point on the graph
#*    cex: character size of the label
#*    pos: position of each label relative to the corresponding point
#' 
# fit a linear model to the data
model <- lm(tripduration ~ sqrtpop, data = gannet)
#' 
# fit a regression line to the model
abline(model)
#' 
# obtain descriptors of the model
summary(model)
#' 
# Get the confidence intervals of the model estimates
confint(model)
#' 
#' Can also run ANOVA on the model:
anova(model) #' the p-value is the same as observed in the linear regression model.
#' 
#' 
## -----------------------------------------------------------------------------
## -----------------------------------------------------------------------------
#' ## Evaluating the assumptions of linear regression
#' ### Assumption 1 (homogeneity of variance)
plot(model, which = 1)
#' Results: there isn't evidence of dramatic changes in variance. Hence, assumption 1 
#' (i.e., homogeneity of error variance) holds!
#' 
#' ### Assumption 2 (Linearity)
# obtain a Q-Q plot
plot(model, which = 2)
#' Results: the points are relatively linear. To be sure, one could plot the standard 
#' residuals on a histogram:
sresids <- rstandard(model) #obtain the standard residuals of the model
hist(sresids) #plot the standard residuals on a histogram
#' Results: Hard to tell due to small sample size, but there seems to be no dramatic departure 
#' from normality.
#' 
#' 
#' 
## -----------------------------------------------------------------------------
## -----------------------------------------------------------------------------
#' ## Prediction
#' A linear regression model can also be used for prediction. E.g., predict the tripduration 
#' when the sqrtpop is 100:
predict(model, list(sqrtpop = 100))
#' Results: tripduration = 10.55451. [This can be confirmed on the lin reg graph]
#' 
#' We can also check predictability of the whole data. E.g., assess the 95% prediction 
#'..intervals of the data (i.e., the range under which 95% of y should fall for a given value 
#'..of x)
xv <- seq(0,250,0.1)
yv <- predict(model, list(sqrtpop = xv), int = "prediction", level = 0.95)
matlines(xv, yv, lty = c(1,2,2), col = "red") #highlights the prediction intervals on the graph
# lty (line type): defines the line designs (1=solid, 2=dashed, 3= dotted, etc.)
#' 
#' 
#' 
#' -----------------------------------------------------------------------------
#' -----------------------------------------------------------------------------
#' -----------------------------------------------------------------------------
#' -----------------------------------------------------------------------------
#' 
#' # **Multiple Linear Regression**
#' Ex: Predicting age at death of known statisticians
statisticians <- read.csv("StatisticiansAgesCSV.csv")
View(statisticians)
head(statisticians)
#' 
#' Use a plot to visualize the data: 
plot(statisticians$Birth.Year, statisticians$Age.on.Death, xlab = "Date of Birth",
     ylab = "Age")
text(statisticians$Birth.Year, statisticians$Age.on.Death, 
     labels = statisticians$Statistician, cex = 0.5, pos = 1)
#' 
# fit a linear model to the data (Using Birth.year as IV)
model1 <- lm(Age.on.Death ~ Birth.Year, data = statisticians)
#' 
# run ANOVA on the linear model
anova(model1)
#' Results: p-value=0.3921. DO NOT reject the null hypothesis (NH) that age and birth year 
#' ..are independent of one another (i.e., there is no evidence of a relationship between the 
#' ..two variables).
#' 
#' Repeat the analysis, this time using Death year as IV:
model2 <- lm(statisticians$Age.on.Death ~ statisticians$Death.Year)
anova(model2)
#' Results: p-value=0.03305. REJECT the NH that age and death year are independent (i.e., 
#' ..there is evidence of a linear relationship between both).
#' 
#' How are they related? Check the coefficients:
model2$coeff
#' Results: coefficient=0.1474. 
#' Interpretation: For each unit increase in death year, age on death increases by 0.147 units.
#' 
#' Fit a line representing the model2 equation:
plot(statisticians$Death.Year, statisticians$Age.on.Death, xlab = "Date of Death",
     ylab = "Age")
text(statisticians$Birth.Year, statisticians$Age.on.Death, 
     labels = statisticians$Statistician, cex = 0.5, pos = 1)

abline(coef = c(model2$coeff[1],model2$coeff[2]), col = "red") # equation for model 2

#' 
#' ## Model both IVs (multiple regression - MR)
model3 <- lm(statisticians$Age.on.Death ~ statisticians$Death.Year + 
               statisticians$Birth.Year)
#' 
#' ### Significance of Effects
anova(model3)
#' Results: both predictors are highly significant (p<2.2e-16 each). 
#' Why does birth year become significant? [Recall that it was not significant in the 
#' bi-variable (i.e., single IV) model]
#' >-- Answer: in the current model, death year is held constant meaning that, birth year 
#' ..becomes significant when we control for death year.
#' PS: the default ANOVA approach in a multivariable analysis is called TYPE I SUM OF SQUARES. 
#' That is, the focus is on the last IV in the series; every other IV before/above it is held 
#' constant in the model.*
#' 
#' NB: each IV now has a lower p-value. This is due to multiple IVs in a model. Specifically, 
#' ..having more than one IV (i.e., MR) reduces the error variability, lower than the simple 
#' ..model. This results in a lower p-value.
#' 
# obtain the model coefficient
model3$coef

#' 
#' Because there are multiple IVs, the results can not be visualized in a 2-D plot; instead, use the 
#' ..avPlots function in the car library:
library(car)
avPlots(model3)

#' 
#' -----------------------------------------------------------------------------
#' -----------------------------------------------------------------------------
#' ### THE TYPE III SUM OF SQUARES APPROACH
#' This method simultaneously adjusts for the effects of all other predictors in the model.
# Type III SS uses the 'Anova' function in the 'car' package (loaded earlier)
Anova(model3, type = "III") #Note the capital A in "Anova"
#' Results: each predictor (Death year and Birth year) is highly significant when controlling 
#' ..for the other.
#' 
#' -----------------------------------------------------------------------------
#' -----------------------------------------------------------------------------
#' ## EVALUATING THE ASSUMPTIONS OF THE GENERAL LINEAR MODEL
#' ### Assumption 1: homogeneity of variance
plot(model3, which = 1)
#' Results: there are no apparent significant changes across the error variance. So, 
#' ..assumption 1 seems to hold.
#'  
#' ###  Assumption 2: Normality
plot(model3, which = 2)
#' Results: the Q-Q plot suggests a non-normal data. Assumption 2 may have been violated!
#' To be sure, check the histogram distribution of the standard residuals:
sresids <- rstandard(model3)
hist(sresids)
#' Results: the histogram shows a bivariate shape, rather than the uni-variate appearance that 
#' ..characterizes a normal distribution. So it appears that the assumption is violated. Note, 
#' ..however, that this is a small sample.
#' 
#' 
#' 
#' -----------------------------------------------------------------------------
#' -----------------------------------------------------------------------------
#' -----------------------------------------------------------------------------
#' -----------------------------------------------------------------------------
#' # **Logistic Regression**
#' The logistic regression is a generalized linear model and works for both continuous and 
#' ..categorical predictors.
#' NB: General Linear Model assumes the residuals/errors follow a normal distribution. 
#' Generalized Linear Model, on the other hand, allows residuals to have other distributions 
#' ..from the exponential family of distributions.
#' 
#' ## Hypothesis testing in logistic regression
#' Testing the hypothesis that a coefficient on a predictor variable is different from zero at 
#' ..population level uses the "Wald statistic" (z in R). The Wald statistic is simply the 
#' ..square of the regular t-statistic. If the NH is true, then it has a particular 
#' ..distribution (the chi-square).
#' 
puffinbill <- read.csv("puffinbill.csv")
View(puffinbill)
head(puffinbill)
# three columns:
# bird = bird ID
# sex = bird's sex (male or female)
# curlen = size/length of the bird's bill
#' 
sex <- puffinbill$sex
curlen <- puffinbill$curlen
sexcode <- ifelse(sex == "F", 1, 0) # code sex (if female, recode as 1; otherwise, code as 0)
#' 
# plot a graph
plot(curlen, jitter(sexcode, 0.15), pch = 19,
     xlab = "Bill lenght (mm)", ylab = "Sex (0 - male, 1 - female)")
#' NB: the jitter prevents overlapping (i.e., in cases of individuals of same sex having 
#' ..similar bill length)
#' 
#' According to the the graph, males tend to have longer bills than females.
#' Now let's fit a binary regression (logistic) model to the data:
model <- glm(sexcode~curlen, binomial)
summary(model)
#' Results: we reject the NH that the bill length has no impact on sex (p=0.00075)
#' Next, plot a line of this model on the graph:
xv <- seq(min(curlen), max(curlen), 0.01)
yv <- predict(model, list(curlen = xv), type = "response")
lines(xv, yv, col = "red") # code for the sigmoidal curve - the curve for binary outcomes

#' Alternatively, use the logi.hist.plot() function in "popbio" library:
# install.packages("popbio") #the library hadn't been previously installed
library(popbio)
# Now plot the graph
logi.hist.plot(curlen, sexcode, boxp = FALSE, type = "count", col = "gray", xlabel = "size")

#' 
#' 
#' 
#' -----------------------------------------------------------------------------
#' -----------------------------------------------------------------------------
#' -----------------------------------------------------------------------------
#' -----------------------------------------------------------------------------
#' # **Polynomial Regression**
fluoride <- read.csv("FlourideDMF.csv")
View(fluoride)
head(fluoride)
#' 
#' Assign x and y variables:
y <- fluoride$DMFper100
x <- fluoride$FlouridePPM
#' 
#' Obtain polynomials of x (i.e., the IV):
xsq <- x^2
xcub <- x^3
xquar <- x^4
#' 
#' Create a simple plot of y (i.e., the DV) vs x:
plot(x, y, pch = 19, xlab = "Fluoride concentration in water", ylab = "DMF index")
#' 
#' Fit a simple linear model to the data:
fit1 <- lm(y~x)
#' 
#' Examine the model by calling the anova function:
anova(fit1)
#' Results: x explains significant variability in y (p<0.001).
#' 
#' Fit the model onto the last graph:
abline(lm(y~x), col = "red")

#' 
#' Notice that there is some curvature in the actual data, so the linear model didn't capture 
#' ...the data well. Perhaps a higher-order model would be more appropriate. Try fitting a 
#' ..quadratic term.
# to fit a higher-order model, add the higher-order term to the existing model:
fit2 <- lm(y~x+xsq)
anova(fit2)
#' Results: the simple term (x) still explains significant variability in y (p<0.001), and 
#' the new complex term (x^2^) explains a significant additional variability (p=0.0002). Next, 
#' fit the new model onto the graph:
xv <- seq(min(x), max(x), 0.01) # xv stands for x values
yv <- predict(fit2, list(x = xv, xsq = xv^2)) # yv stands for y values
lines(xv, yv, col = "green") # the quadratic form
#' 
#' Results: the quadratic graph depicts some curvature in the data. Thus, the quadratic model 
#' ..fits the data better than the linear model.
#' 
#' Let's try an even more complicated model (by adding a cubic term):
fit3 <- lm(y~x+xsq+xcub)
anova(fit3)
#' 
#' The linear and quadratic terms each explain significant variability in y. But the cubic 
#' ..term explains no significant additional variability (p=0.442). Next, check to see how 
#' ...all three models are laid out in the graph:
cubic <- predict(fit3, list(x = xv, xsq = xv^2, xcub = xv^3)) 
# xv has been defined earlier; y values are now defined as "cubic"
lines(xv, cubic, col = "black") # the cubic form
#' Results: the cubic term doesn't seem to introduce a new information (i.e., no new 
#' ..contribution different from that of the quadratic). So, the quadratic model seems to be 
#' ..the best fit.
#' 
#' -----------------------------------------------------------------------------
#' -----------------------------------------------------------------------------
#' Check the GLM assumption of the models (quadratic vs linear)
#' Assumption 1 -- Homogeneity of the error variance
plot(fit2, which = 1)
# Results: the error variance is relatively homogeneous. Compare this to that of the linear 
#..model (shown next):
plot(fit1, which = 1) 
# the error variance is not homogeneous. This GLM assumption is therefore violated, making 
# ..the linear model an inappropriate fit for the data (as has been shown in above analyses)
#' 
#' Assumption 2 --- normality
plot(fit2, which = 2)
plot(fit1, which = 2)
#' 
hist(rstandard(fit2))
hist(rstandard(fit1))
#' Results: the quadratic model assumes a more "normal" shape than the linear model, again 
#' ..supporting the conclusion that the former is a better fit.
#' 
#' 
#' 
#' -----------------------------------------------------------------------------
#' -----------------------------------------------------------------------------
#' -----------------------------------------------------------------------------
#' -----------------------------------------------------------------------------
#' -----------------------------------------------------------------------------
#' -----------------------------------------------------------------------------
#' # **One-way ANOVA (Analysis of Variance)**
#' >-- variance: the average squared deviation of all the observations from the population mean \
#' >-- standard deviation: square root of variance
#' 
#' If pairs of samples are repeatedly drawn from the same normally distributed population (or 
#' ..two different normally distributed populations with the same variance), then the ratio 
#' ..s_1^2/s_2^2 will follow an F-distribution.
#' 
#' According to Ronald Fisher, the ANOVA is not a mathematical theorem, but rather a convenient 
#' ..method of arranging the arithmetic. 
#' ANOVA: checks if there is a statistically significant difference between two or more groups 
#' ..by testing the difference between the groups' means.
#' 
#' ANOVA is usually between a continuous DV and ≥1 categorical IV.
#' 
#' Total variation = variation within groups + variation between groups, 
#'    i.e., SS_total = SS_within + SS_between
#' 
#' If there is a treatment effect, then one would expect the between-group variation to be 
#' ..higher than the within-group variation. 
#' 
#' **p-value** \
#' >-- general definition: the probability of getting at least the observed effect if the null 
#' ..hypothesis (NH) were true.
#' >-- ANOVA definition: the probability of getting at least the observed F-ratio if the NH 
#' ..were true.
#' 
#' 
#' ## PRACTICE PROBLEM 
fertData <- read.csv("FertiliserCSV.csv")
head(fertData)
#' Use a stripchart function to visualize the data:
# a stripchart produces a 1-dimensional scatter plot
stripchart(yield ~ as.factor(fertil), vertical = T, pch = 19, data = fertData,
           xlab = "fertilizer type", ylab = "yield in tonnes / ha",
           method = "jitter", jitter = 0.04)
#* vertical: when set to TRUE, plot is drawn vertically (NB: default is False)
#* pch (plot character): defines the plot style (19 = sphere/circle, 17 = triangle, 
#* ..3 = plus, etc.)
#* jitter: applies to overlapping points (???)
#' 
#' From the graph, the yield from fertilizer 1 appears to be generally higher than yield from 
#' ..the other fertilizers. Check whether this is a significant difference.
#' First, fit a linear model to the data:
analysis <- lm(yield ~ as.factor(fertil), data = fertData)
#' 
#' Next, run ANOVA on the model:
anova(analysis)
#' Interpretation of results: assuming the NH were true, the probability of observing an 
#' ...F-value of at least 5.7024 is 0.008594. So we reject the NH that yield was equal across 
#' ..the three fertilizer types.
#' 
#' 
#' -----------------------------------------------------------------------------
#' -----------------------------------------------------------------------------
#' ### CHECKING ANOVA ASSUMPTIONS
#' To confirm the above interpretation, check that the assumptions underlying the ANOVA 
#' ..(as applicable to other members of the general linear model) have not been violated.
#' 
#' #### Assumption 1: Homogeneity of the error variance
plot(analysis, which = 1)
#' Results: as seen on the graph, the variance is relatively homogeneous across each level of 
#' ..the data (Recall: there are 3 fertilizer levels/types). The assumption holds!
#' 
#' #### Assumption 2: Normality of the overall distribution of the residuals
plot(analysis, which = 2) # creates a Q-Q plot 
#' Results: some points on the graph do not align linearly with the rest (i.e., some skewing 
#' ..is present). To visualize this more directly:
sresids <- rstandard(analysis) # obtains the standardized residuals of the model
hist(sresids) # plots a histogram of the standardized residuals
#' Results: the data is positively-skewed (to the right), but not too much to worry about 
#' 
#' 
#' -----------------------------------------------------------------------------
#' -----------------------------------------------------------------------------
#' ### POST-HOC ANALYSIS 
#' From the above analyses, it can be concluded that the fertilizers do not have the same 
#' ..yield. But wherein lies the difference? A post-hoc test addresses this question. An 
#' example of a post-hoc test is Tukey's HSD (honest significant difference) test:
TukeyHSD(aov(analysis))
#' Results: the significant difference lies in fertilizer 2 vs. 1.  Other differences 
#' ..(3vs1 and 3vs2) are not statistically significant.
#' 
#' 
#' -----------------------------------------------------------------------------
#' -----------------------------------------------------------------------------
#' -----------------------------------------------------------------------------
#' -----------------------------------------------------------------------------
#' -----------------------------------------------------------------------------
#' -----------------------------------------------------------------------------
#' # **Randomized Block Designs (a.k.a two-way ANOVA)**
#' Basically, in 2-way ANOVA, there are two categorical predictors [1-way ANOVA has only one 
#' ..predictor which is also categorical] and one continuous response  
#' 
#' ## PRACTICE Example
weight <- c(0.958, 0.971, 0.927, 0.971, 0.986, 1.051, 0.891, 1.010,
            0.925, 0.952, 0.829, 0.955)
experiment <- factor(rep(1:4,3)) 
#NB: the rep function here renders the object as 1234 1234 1234, i.e., the series is repeated
genotype <- rep(c("wild", "red", "hairy"), each = 4) 
#NB: the rep function here repeats each element the specified number of times before the next 
#..element (i.e. wild wild wild wild red red red ...)
#' 
stripchart(weight ~ genotype, vertical = T, pch = 19, xlab = "Genotype", ylab = "Weight",
           method = "jitter", jitter = 0.04)
#' Results: the red genotype generally weigh heavier than the other two.
#' 
#' Now, analyze the differences/relationships. 
#' First, try out a silly approach (a simple 1-way ANOVA) that ignores the experiment variable:
sillyresults <-  lm(weight ~ genotype) 
anova(sillyresults)
#' Results: p-value=0.2348; we can't reject the NH that genotype has no effect on weight 
#' (i.e., there is no evidence that genotype has an effect on weight). But is that actually 
#' true?
#' 
#' Try the more appropriate approach -- a 2-way ANOVA that includes the experiment variable. 
#' Why would this be helpful? Because it would help account for variability that would 
#' otherwise be treated as noise in the 1-way model:
results <- lm(weight ~ genotype+experiment)
anova(results)
#' Results: both IVs each have significant effect on weight. Although because this is a 
#' default Type I SS approach, these results imply that genotype is being controlled for.
#' 
#' Try changing the order in which the IVs are entered into the model:
results2 <- lm(weight ~ experiment+genotype)
anova(results2)
#' Results: same as the previous ordering. Same p-values, F-values. This is because the two 
#' vars are not collinear with one another [the genotypes are equally distributed within each 
#' experiment, i.e., each experiment has exactly one (not less or more) type of the genotypes. 
#' So it's impossible to guess the type of experiment by looking just at the genotype, or vice 
#' versa]. In short, there is complete ORTHOGONALITY!!! So there is really no need to control 
#' for either variable. There is obviously no possibility of confounding! [If there was, one 
#' could run the type III Anova]
#' 
#' -----------------------------------------------------------------------------
#' -----------------------------------------------------------------------------
#' Check the residuals (for the GLM assumptions)
par(mfrow = c(1,2))
plot(results, which = 1)
plot(results, which = 2)
#' Results: Broadly speaking, there seems to be nothing to worry about. Both assumptions appear 
#' to check out. The results are not exactly perfect, but again this is a small sample.
#' 
#' Re-check assumption 2 using a histogram:
sres <- rstandard(results)
hist(sres)
#' 
#' -----------------------------------------------------------------------------
#' -----------------------------------------------------------------------------
#' POST-HOC: 
#' Check exactly where the difference lies in the genotype groups
TukeyHSD(aov(results), "genotype")
#' Results: the actual difference lies in red vs. hairy (p-value=0.023). That is, the primary  
#' source of the variability between genotypes derives from the differences between the red 
#' and the hairy genotype.
#' 
#' Same can be done for the different experiments if desired:
TukeyHSD(aov(results), "experiment")
#' Results: The primary differences lie in the following comparisons: 3 vs. 1 (p-value=0.052); 
#' 3 vs. 2 (p-value=0.009); and 4 vs. 3 (p-value=0.017).
#' 
#' 
#' 
#' -----------------------------------------------------------------------------
#' -----------------------------------------------------------------------------
#' -----------------------------------------------------------------------------
#' -----------------------------------------------------------------------------
#' -----------------------------------------------------------------------------
#' -----------------------------------------------------------------------------
#' # **Factorial ANOVA** 
#' This takes 2-way ANOVA a step further by considering the effect of interactions between the 
#' 2 categorical IVs. Interaction occurs when the effect of one predictor variable on the 
#' response is influenced by another predictor variable in the model. For example, the effect 
#' of fertilizer on yield may depend on the pesticide used and vice-versa (i.e., the effect of 
#' the pesticide on yield may depend on the fertilizer used). 
#' 
#' In sum, factorial ANOVA helps the researcher to investigate interaction between 2 predictor 
#' variables
#'  
#' ## PRACTICE
consumption <- c(709, 679, 699, 657, 594, 677, 592, 538, 476, 508, 505, 539)
sex <- c(rep("M",3), rep("F",3), rep("M",3), rep("F",3))
taste <- c(rep("fresh",6), rep("rancid",6))
#' 
#' Check out an interaction plot:
par(mfrow=c(1,1))
interaction.plot(sex, taste, consumption)
#' Results: mean consumption appears slightly higher in male subjects than females. And this 
#' observation is the same for both rancid and fresh lards. So there seems to be no interaction 
#' going on. That is, preference for fresh or rancid is not dependent on sex. If there was an 
#' interaction, the two lines might cross. For example, males might consume higher amounts of 
#' fresh lard than females, while females consume higher amounts of rancid lard than males)
#' 
#' Model a 2-way ANOVA:
results3 <- lm(consumption ~ sex*taste) 
#NB: The IVs are combined using multiplication (unlike addition in a basic 2-way ANOVA) since 
#..factorial ANOVA is about interaction.
anova(results3)
#' Results: as previously shown in the interaction plot, this ANOVA model shows no evidence of 
#' interaction between sex and taste (p=0.4503). However, taste alone has a significant effect 
#' on the outcome (mean consumption), p=0.0002. Sex has no significant influence (p=0.1460)
#' 
#' ## Model Assumptions
par(mfrow = c(1,2))
plot(results3, which = 1) # test for homogeneity of variance
plot(results3, which = 2) # a Q-Q plot for normality test
#' Results: there seems to be no significant violation of the two GLM assumptions
#' 
#' Some theoretical questions:
#' **1.** Would the outcome be different if the order of IVs is changed in the model (i.e., 
#' taste before sex instead of sex before taste)?
#' >-- *Answer*: Since the sample is orthogonally distributed between the two IVs (i.e., the 
#' male and female subjects are equally distributed between taste type [3 males and 3 females 
#' in each of rancid and fresh conditions] - another indication of no interaction), the model 
#' results would be the same even after reordering.
#' 
#' **2**. How about using a Type III Sum of Squares approach? 
#' >-- *Answer*: Again, the design is completely orthogonal and the results suggest no evidence 
#' of interaction. So, the Type III method would yield same results as Type I above.
#' 
#' However, if there is a need to run a Type III SS ANOVA, the procedure is as follows:
library(car) # load the car library
options(contrasts = c(unordered="contr.sum", ordered="contr.poly")) 
# set the options like this
Anova(results) # run Type III ANOVA
#' Results: As mentioned previously, the results are the same as in Type I SS since there is 
#' orthogonality and no evidence of interaction
#' 
#' 
#' -----------------------------------------------------------------------------
#' -----------------------------------------------------------------------------
#' -----------------------------------------------------------------------------
#' -----------------------------------------------------------------------------
#' -----------------------------------------------------------------------------
#' -----------------------------------------------------------------------------
#' # **Analysis of Covariance (ANCOVA)**
#' The ANCOVA is a GLM like the linear regression and ANOVA. However, whereas the two have one 
#' type of predictor (linear reg--continuous; ANOVA--categorical), ANCOVA has both continuous 
#' and categorical predictors. In summary: ANCOVA = continuous response with categorical and 
#' continuous predictor(s)
#' 
#' ## Practice 
setwd("G:/My Drive/Programming/Data Science/DATA SETS")
leprosyCSV <- read.csv("leprosyCSV.csv")
lep <- leprosyCSV
View(lep)
head(lep)
#' 
dim(lep)
#' The data has 30 rows of actual observations and 170 rows of missing observations. Remove 
#' them:
lep <- na.omit(lep) 
dim(lep)
#' All missing observations now removed.
#' Visualize the data:
par(mfrow=c(1,1))
plot(lep$bacbef, lep$bacafter, pch = c(15:17)[lep$treatmt],
     col = c("red", "blue", "black")[lep$treatmt], xlab = "Bacillus score before",
     ylab = "Bacillus score after")
legend("topleft", pch = c(15:17), legend = c("treat 1", "treat 2", "treat 3"),
       col = c("red", "blue", "black"))

#' Fit a linear model to the data:
model1 <- lm(bacafter ~ bacbef+factor(treatmt), data = lep)
#' Run the anova:
anova(model1)
#' Results: bacbef has a significant effect on bacafter but treatmt has no significant effect 
#' (NB: the first IV in the model is controlled for according to this 'default' Type I ANOVA 
#' method. That IV here is bacbef.)
#' 
#' Check the coefficient:
model1$coeff
#' NB: one category of treatmt is missing because it is subsumed within the intercept 
#' coefficient.
#'  
#' Fit regression lines to the coefficient values:
abline(coef = c(model1$coeff[1], model1$coeff[2]), col = "red") 
# above is the syntax for treatment 1 equation line: 
#       model1$coeff[1] is the intercept
#       model1$coeff[2] is the bacbef coefficient
abline(coef = c(model1$coeff[1]+model1$coeff[3], model1$coeff[2]), col = "blue") 
# above is the syntax for treatment 2 equation line:
#       model1$coeff[1] is the intercept
#       model1$coeff[3] is the treatment 1 coefficient
#       model1$coeff[2] is the bacbef coefficient
abline(coef = c(model1$coeff[1]+model1$coeff[4], model1$coeff[2]), col = "black") 
# above is the syntax for treatment 3 equation line:
#       model1$coeff[1] is the intercept
#       model1$coeff[4] is the treatment 2 coefficient
#       model1$coeff[2] is the bacbef coefficient

#' 
#' Run the ANOVA using the Type III sum of squares method (see the section on Multiple 
#' Regression)
library(car)
Anova(model1, type = "III")
#' Results: bacbef has a significant effect on bacafter while controlling for treatment. But 
#' treatment has no significant effect while controlling for bacbef. [Recall that according to 
#' the Type III approach, when interpreting the effect of one IV, all other IVs in the model 
#' are held constant/controlled for]
#' 
#' -----------------------------------------------------------------------------
#' -----------------------------------------------------------------------------
#' ## Evaluate the GLM assumptions 
par(mfrow = c(1,2))
plot(model1, which = 1) # Assumption 1: Homogeneity of variance
plot(model1, which = 2) # Assumption 2: Normality
#' Results:\
#' >-- Assumption 1 holds (the residuals are relatively homogenuous) \
#' >-- Assumption 2 holds (the residuals tend to follow a normal distribution as seen in the 
#' Q-Q plot which is approximately a straight line)
#' 
#' -----------------------------------------------------------------------------
#' -----------------------------------------------------------------------------
#' ## Parallel vs Non-Parallel Slopes
#' ANCOVA assumes approximately the same gradient for the different levels of the categorical 
#' var (see the ***parallel slopes*** produced from the regression lines earlier). However, it 
#' is possible to have a situation where the levels of the categorical var do have different 
#' gradient (***non-parallel slopes***). Such a case could be modeled by, for example, 
#' assuming an interaction effects between predictors:
model2 <- lm(bacafter ~ bacbef*factor(treatmt), data = lep)
anova(model2)
#' Results:
#' >-- The interaction effect is non-significant, i.e., there is no evidence to reject the NH 
#' that the gradients of those relationships do not differ between the different levels of the 
#' treatmt var (i.e., no interaction between treatment and bacbef).
#' >-- There is also no evidence to reject the NH that treatmt has no effect on bacafter score 
#' (NB: this is a type I sum of square so we are controlling for bacbef)
#' >-- Finally, there is a significant effect of bacbef on bacafter.
#' 
#' ### Try the type III sum of squares approach:
Anova(model2, type = "III") # NB: the 'car' library has been loaded in a previous command
#' Results: Again, only bacbef has a significant effect. And there still is no evidence of 
#' interaction between the two IVs.
#' 
model2$coeff

#' 
#' ### Plotting the slopes
#' Fit regression lines to the coefficients:
par(mfrow = c(1,1)) 
plot(lep$bacbef,lep$bacafter, pch = c(15:17)[lep$treatmt],
     col =  c("red","blue","black")[lep$treatmt],
     xlab = "Bacillus score before", ylab = "Bacilus score after")
legend( "topleft", legend = c("treat 1", "treat 2", "treat 3"), 
        pch = c(15:17),col =  c("red","blue","black")) 
abline(coef = c(model2$coeff[1], model2$coeff[2]), col = "red")
# above is the syntax for treatment 1 equation line: 
#       model2$coeff[1] is the intercept
#       model2$coeff[2] is the bacbef coefficient
abline(coef = c(model2$coeff[1]+model2$coeff[3], model2$coeff[2]+model2$coeff[5]),
       col = "blue")
# above is the syntax for treatment 2 equation line: 
#       model2$coeff[1] is the intercept
#       model2$coeff[3] is the treatment1 coefficient
#       model2$coeff[2] is the bacbef coefficient
#       model2$coeff[5] is the bacbef*treatment1 interaction coefficient
abline(coef = c(model2$coeff[1]+model2$coeff[4], model2$coeff[2]+model2$coeff[6]),
       col = "black")
# above is the syntax for treatment 3 equation line: 
#       model2$coeff[1] is the intercept
#       model2$coeff[4] is the treatment2 coefficient
#       model2$coeff[2] is the bacbef coefficient
#       model2$coeff[6] is the bacbef*treatment2 interaction coefficient

#' As can be seen in the graph, the slopes are no longer parallel. For example, the equation 
#' lines for treatments 1 and 2 would cross at a point beyond the y-axis (where x would be <0).
#' 
#' 
#' 
#' 
#' 
#' -----------------------------------------------------------------------------
#' -----------------------------------------------------------------------------
#' -----------------------------------------------------------------------------
#' -----------------------------------------------------------------------------
#' -----------------------------------------------------------------------------
#' -----------------------------------------------------------------------------
#' # **Chi-Square Test**
#' #' ## Example 1
obs <- matrix(c(40,53,43,143,199,146), byrow = F, ncol = 2)
colnames(obs) <- c("win", "no win") # win a prize or not
rownames(obs) <- c(2011:2013) # game year
obs

#' Example Question: "is there an association between winning/not winning status and year?" 
#' Put differently, "Does the probability of winning vary by year?"
#' This question can be answered using a Chi-Squared test of independence:
chisq.test(obs, correct = F)
#The 'correct=FALSE' option turns off Yates' correction (which is used with small sample sizes).
#' Results: Do not reject the NH that there is no association between winning and year (i.e., 
#' there is no evidence of an association between winning and year).
#' 
#' Example 2 (Chi-Square GOODNESS OF FIT TEST):
#' It's possible to go ahead and combine the data over all those years to answer a probability
#' question as below:
obsfreq <- c(136,488) #136 wins and 488 losses
#' Question: Assume there is a population with 1/6 probability of winning a prize (vs. 5/6 of 
#' not winning). Can we reject the NH that the data above derives from this population?
#' 
results <- chisq.test(obsfreq, p = c(1/6,5/6))
results
#' We can reject the NH that the data derives from that population (i.e., there is no evidence 
#' to suggest that the data derives from the population).
#' 
#' Follow-up question: In what way does the data deviate from said population?
results$expected
#' Results: if the data were from a population having a 1/6 chance of winning, one would expect 
#' to see 104 wins and 520 losses. However, the current data has significantly more wins and 
#' fewer losses (136 and 488, respectively) than would be expected.
#' 
#' 
#' ## Example 3 (TEST OF INDEPENDENCE)
observedfreq <- c(31,87,47,173,18,77)
observed <- matrix(observedfreq, byrow = T, ncol = 2)
rownames(observed)<- c("small", "medium", "large") # cup sizes
colnames(observed) <- c("win", "loss") # outcomes
observed
#' Question: Is there an association between outcome and cup size?
chisq.test(observed, correct = F)
#' Results: No evidence to reject the NH that the outcome is independent of cup size
#' 
#' What is the deviation (i.e., what would we expect in order to reject the NH)?
chisq.test(observed)$expected
#' The results show what the distribution would look like if we would reject the NH.
#' 
#' 
#' Example 4: A Similar question as above but with a smaller sample size
freq <- c(5,13,3,15,3,15,4,34)
ofreq <- matrix(freq, byrow = T, ncol = 2)
rownames(ofreq)<- c("extra large", "large", "medium", "small") # cup sizes
colnames(ofreq) <- c("win", "loss") # outcomes
ofreq
#' 
result <- chisq.test(ofreq, correct = T) 
#Note that the Yate's correction is now turned on, due to the small sample size.
result
#' A warning message is displayed due to the small sample size (regardless of whether Yates'
#' correction in activated or not)
#' Results: same as the previous data; No solid evidence to reject the NH hypothesis that the 
#' outcome is independent of the cup size.
#' 
result$expected
#' 

#' NB: Due to the relatively low sample size in some cells, the validity of the test may be 
#' unreliable. One way to address this is by conducting the Fisher's exact test.
fisher.test(ofreq)
#' The Fisher's test answers the question, "Considering the observed frequencies, what is the 
#' probability of obtaining that outcome or a more extreme one if the NH (of there being no 
#' association) was true?" 
#' Answer: About 45% (i.e., p=0.4484).
#' 
#' In summary: Again, there is no evidence of a relationship between outcome and cup size. So 
#' we cannot reject the NH.
