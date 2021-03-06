# global options
options(stringsAsFactors = F)
setwd(dirname(parent.frame(2)$ofile))

# Load libraries
require(tidyverse)
require(data.table)
require(ggplot2)
require(ISLR)
require(MASS)
require(gridExtra)
require(PerformanceAnalytics)
# Load data
file <- "Auto.csv"
data <- read.csv(file)

# EXPLORE DATA #
################

# 1: check obj structure, change int to factors

data <- data %>%
  mutate_if(is.integer, as.factor) %>%
  mutate_if(is.character, as.factor)
str(data) # check if organization makes sense

# Adjust as needed: 

# horsepower -> change to numeric
data$horsepower <- as.numeric(data$horsepower)

# weight -> change to numeric
data$weight <- as.numeric(data$weight)

# name -> change to character
data$name <- as.character(data$name)

# check again
str(data)
  
# 2: Get summary stats

# use summary to display stats of quants

quants <-data %>% select_if(is.numeric)
quals <- data %>% select_if(is.factor)
labels <- data %>% select_if(is.character)
  

# function to summarize key stats - NEED TO CLEAN THIS UP
get_stats <- function(quants){
  
  # get min, max, mean, etc.
  stats.table <- as.data.frame.matrix(summary(quants))
  names(stats.table) <- trimws(names(stats.table))
  # ugly hack to calculate sd & append to the summary table
  # get sd as vector
  stats.sd <- data.frame("sd" = sapply(quants, sd)) %>%
    round(digit = 4)
  # reformat
  formatted.stats.sd <- stats.sd %>%
    # preserve var names
    as.data.table(keep.rownames = T) %>%
    # add formatting to match summary table
    mutate(sd = paste("Sd : ", sd)) %>%
    # transpose to wide
    t %>% data.table(keep.rownames = T) %>%
    # drop redundant first col
    dplyr::select(-1)
  # assign first row to table names
  names(formatted.stats.sd) <- unlist(formatted.stats.sd[1,])
  # get rid of redundant first row
  final <- formatted.stats.sd[2,]
  
  # STILL TO DO: append sd to stats table
  
  stats.table.tmp <- rbind(stats.table[1:4,], final)
  stats.table <- rbind(stats.table.tmp, stats.table[5:6,])
  
  return(stats.table)
}

stats.summary <-  get_stats(quants) 

# 3. Graph & explore variables 

## Plot all factor-based graphs

# set grid of plots 1rx3c
png(filename="plot1.png", width = 1440, height = 1024)
par(mfrow=c(1,3))
attach(data)
boxplot(mpg ~ cylinders, xlab = "cyl")
boxplot(mpg ~ year, xlab = "year")
boxplot(mpg ~ origin, xlab = "origin")
dev.off()


## plot all single-var & two-var quant graphs


png(filename="plot2.png", width = 1600, height = 1600)
plot2 <- chart.Correlation(quants,hist=T) 
dev.off()


# q8 simple linear regression, mpg ~ horsepower

attach(data)
lm.fit1 <- lm(mpg ~ horsepower)
# output coefficients
lm.fit1
# output full model details, including p-values
summary(lm.fit1)
# i. Yes there is a response between response and predictor
# ii. the relationship is not so strong, but it is significant. but a lot of the change in mpg cannot be attributed to horsepower
# iii. the relationship is positive
# iv. conf. interval and prediction interval for horsepower = 98

# confidence interval
predict(lm.fit1, data.frame(horsepower = c(98)), interval = "confidence")

# prediction interval
predict(lm.fit1, data.frame(horsepower = c(98)), interval = "prediction")

# b Plot response and predictor
png(filename="plot3.png", width = 500, height = 300)
ggplot(data = data, aes(y = mpg, x = horsepower )) + 
  geom_point() + 
  geom_smooth(method="lm", se=FALSE)
dev.off()

# c check diagostic plots for this least squares fit
# create 2 x 2 grid, then plot 4 default diagnostic grids for an lm fit
png(filename="plot4.png", width = 500*2, height = 300*2)
par(mfrow=c(2,2))
plot(lm.fit1)
dev.off()

# Trends about this (mpg ~ horsepower) model so far: 

# 1. Non linear curve in residual plot
# 2. 2 clusters indicate a missing variable
# 3. Predictor appears to be normally distributed (QQ plot)
# 4. Appears to have equal variance across data
# 5. No high leverage outliers


# q9 reformat data so that they're all numeric except 'name'
data <- read.csv(file)
str(data)

data <- data %>%
  mutate_if(is.integer, as.numeric) %>%
  mutate(horsepower = as.numeric(horsepower)) %>%
  mutate(origin = as.factor(origin))
str(data)

# a) scatterplot of all the data
to_graph <- data %>% dplyr::select(-name, -origin)
png(filename="plot6.png", width = 1600, height = 1600)
pairs(to_graph)
dev.off()

# b) calculate the correlation between variables
png(filename="plot7.png", width = 1600, height = 1600)
plot7 <- chart.Correlation(to_graph,hist=T) 
dev.off()

# c) Use the lm() function to perform a multiple linear regression with mpg as the response and all other variables except name as the predictors.

mlr.fit <- lm(mpg ~.-name -origin, data = data) # origin is categorical, not numeric. 
summary(mlr.fit)

# i. Is there a relationship between the predictors and the response? Yes. 

# ii. Which predictors appear to have a statistically significant relationship to the response? MPG goes down as displacement & weight go up. MPG goes up with year, and displacement.

# iii. What does the coefficient for the year variable suggest? Mileage gets better with newer cars.

# (d) Use the plot() function to produce diagnostic plots of the linear regression fit. Comment on any problems you see with the fit.

png(filename="plot8.png", width = 1600, height = 1600)
par(mfrow=c(2,2))
plot(mlr.fit)
dev.off()

# Issues: 
  # non-linear relationship
  # some heteroscedasticity
  # skewed right, fat tails on right

# e) Use the * and : symbols to fit linear regression models with interaction effects. Do any interactions appear to be statistically significant?

mlr.fit.xy <- lm(mpg ~ . + I(weight^2) + weight:year -name -origin, data = data) 
summary(mlr.fit.xy)
png(filename="plot9.png", width = 1600, height = 1600)
par(mfrow=c(2,2))
plot(mlr.fit.xy)
dev.off()

anova(mlr.fit, mlr.fit.xy)
# Yes, squaring the weight and including an interaction effect between weight and year are both statisticlaly significant. Residuals look much better. 

# 10. This question should be answered using the Carseats data set.
data(Carseats)
Carseats
str(Carseats)

# Fit a multiple regression model to predict Sales using Price, Urban, and US.
lm.sales.1 <- lm(Sales ~ Price + Urban + US, data=Carseats)
summary(lm.sales.1)
# Interpretation: 
# 1. The mean sales is 13 units
# 2. Sales go down by 0.054 units as price goes up per unit. 
# 3. Sales & location don't seem to have any kind of relationship. 
# 4. Sales are on average 1.2 units higher in the US than out. 

# c) y = 13.04 - 0.054x1 + 1.2x2 
#    (Non-US: x2 = 0, US: x2 = 1)  

# d) price and US-based

# e) y = 13.04 - 0.054x1 + 1.2x2 
#    (Non-US: x2 = 0, US: x2 = 1)

# (f) How well do the models in (a) and (e) fit the data? 
lm.sales.2 <- lm(Sales ~ Price + US, data=Carseats)
summary(lm.sales.2)

# confidence interval
predict(lm.sales.2, data.frame(Price = c(100), US = "No"), interval = "confidence")

# prediction interval
predict(lm.sales.2, data.frame(Price = c(100), US = "No"), interval = "prediction")

# (g) Using the model from (e), obtain 95% confidence intervals for the coefficient(s).
confint(lm.sales.2)

# (h) Is there evidence of outliers or high leverage observations in the model from (e)? No.


# 11. In this problem we will investigate the t-statistic for the null hypoth- esis H0 : β = 0 in simple linear regression without an intercept. To begin, we generate a predictor x and a response y as follows.

set.seed(1)
x <- rnorm(100)
y <- 2*x + rnorm(100)

# (a) Perform a simple linear regression of y onto x, without an intercept.

data <- data.frame(
  x = x,
  y = y
  )

lm.fit2 <- lm(y~x+0, data = data)
summary(lm.fit2)

# b = 1.9939
# se = 0.1065
# t = 18.73
# p-val = <2e-16

lm.fit3 <- lm(x~y+0, data = data)
summary(lm.fit3)

# b = 0.39111
# se = 0.02089
# t = 18.73
# p-val = <2e-16


# (e) Using the results from (d), argue that the t-statistic for the regression of y onto x is the same as the t-statistic for the regression of x onto y.


# 12. This problem involves simple linear regression without an intercept.

# (a) Recall that the coefficient estimate βˆ for the linear regression of Y onto X without an intercept is given by (3.38). Under what circumstance is the coefficient estimate for the regression of X onto Y the same as the coefficient estimate for the regression of Y onto X?

# Ans: When x = y.

# (b) Generate an example in R with n = 100 observations in which the coefficient estimate for the regression of X onto Y is different from the coefficient estimate for the regression of Y onto X.

x <- rnorm(1000, mean = 5, sd = 2.5)
y <- 2*x + rnorm(1000, mean = 5, sd = 2.5)

data1 <- data.frame(
  x = x,
  y = y
)

lm.fit.12a <- lm(y ~ x, data=data1)
summary(lm.fit.12a)

lm.fit.12b <- lm(x ~ y, data=data1)
summary(lm.fit.12b)

# (c) Generate an example in R with n = 100 observations in which the coefficient estimate for the regression of X onto Y is the same as the coefficient estimate for the regression of Y onto X.

x <- rnorm(100, mean = 0, sd = 2.5)
y <- rnorm(100, mean = 0, sd = 2.5)

data1 <- data.frame(
  x = x,
  y = y
)

lm.fit.12ca <- lm(y ~ x, data=data1)
summary(lm.fit.12ca)

lm.fit.12cb <- lm(x ~ y, data=data1)
summary(lm.fit.12cb)


coef(lm.fit.12ca)
coef(lm.fit.12cb)

# 13. In this exercise you will create some simulated data and will fit simple linear regression models to it. Make sure to use set.seed(1) prior to starting part (a) to ensure consistent results.

# (a) Using the rnorm() function, create a vector, x, containing 100 observations drawn from a N (0, 1) distribution. This represents a feature, X.
set.seed(1)
x <- rnorm(100, 0, 1)

#(b) Using the rnorm() function, create a vector, eps, containing 100 observations drawn from a N(0,0.25) distribution i.e. a normal distribution with mean zero and variance 0.25.

eps <- rnorm(100, 0, 0.5)

# (c) Using x and eps, generate a vector y according to the model Y =−1+0.5X+ε. (3.39) What is the length of the vector y? What are the values of β0 and β1 in this linear model?

y <- -1 + 0.5*x + eps
data <- data.frame(
  x = x,
  y = y
)

lm.fit.13c <- lm(y ~ x, data = data)
summary(lm.fit.13c)

# length of y:
length(y)

# B0 and B1, theoretically

# B0 = -1
# B1 = 0.5

# plot the scatterplot
plot(data)

# Fit a least squares linear model to predict y using x. Comment on the model obtained. How do βˆ0 and βˆ1 compare to β0 and β1?

lm.fit.13c <- lm(y ~ x, data = data)
summary(lm.fit.13c)

# answer - coefficients are on point, very close to the theoretical values. 
coef(lm.fit.13c)

# (f) Display the least squares line on the scatterplot obtained in (d). Draw the population regression line on the plot, in a different color. Use the legend() command to create an appropriate leg- end.

png(filename="plot13c.png", width = 500, height = 300)
ggplot(data = data, aes(y = y, x = x)) + 
  geom_point() + 
  geom_smooth(method="lm", se=TRUE)
dev.off()

# (g) Now fit a polynomial regression model that predicts y using x and x2. Is there evidence that the quadratic term improves the model fit? Explain your answer.

lm.fit.13g <- lm(y ~ x + I(x^2), data = data)
summary(lm.fit.13g)
anova(lm.fit.13c, lm.fit.13g)

# Answer: no, p-value of 0.16. 

# (h - j) see how increasing and reducing the noise added to the model changes the confidence intervals

set.seed(1)
x <- rnorm(100, 0, 1)

eps_more_noise <- rnorm(100, 0, 1)
eps_less_noise <- rnorm(100, 0, 0.1)

y_more <- -1 + 0.5*x + eps_more_noise
data_more <- data.frame(
  x = x,
  y = y_more
)

y_less <- -1 + 0.5*x + eps_less_noise
data_less <- data.frame(
  x = x,
  y = y_less
)

p1 <- ggplot(data = data_less, aes(y = y, x = x)) + 
  geom_point() + 
  geom_smooth(method="lm", se=TRUE)
p2 <- ggplot(data = data, aes(y = y, x = x)) + 
  geom_point() + 
  geom_smooth(method="lm", se=TRUE)
p3 <- ggplot(data = data_more, aes(y = y, x = x)) + 
  geom_point() + 
  geom_smooth(method="lm", se=TRUE)
ggsave("plot13_more_v_less.png", arrangeGrob(p1, p2, p3))

# compare confidence intervals

# regular noise
predict(lm(y ~ x, data = data), data.frame(x = c(2)), interval="confidence")

# less noise
predict(lm(y ~ x, data = data_less), data.frame(x = c(2)), interval="confidence")

# more noise
predict(lm(y ~ x, data = data_more), data.frame(x = c(2)), interval="confidence")

# 14. This problem focuses on the collinearity problem

# (a) coefficients?
set.seed(1)
x1 <- runif(100)
x2 <- 0.5*x1 + rnorm(100)/10
y <- 2 + 2*x1 + 0.3*x2 + rnorm(100)

data_14 <- data.frame(
  x1 = x1,
  x2 = x2,
  y = y
)

# b0 = 2, b1 = 2, b2 = 0.3

# (b) correlation?
cor(x1, x2)
# Correlation is 0.835.

# Create scatterplot
p14_b <- ggplot(data = data_14, aes(y = x2, x = x1)) + 
  geom_point()
ggsave("plot14_b.png", arrangeGrob(p14_b))

# (c) Using this data, fit a least squares regression to predict y using x1 and x2. Describe the results obtained. What are βˆ0, βˆ1, and βˆ2? How do these relate to the true β0, β1, and β2? Can you reject the null hypothesis H0 : β1 = 0? How about the null hypothesis H0 : β2 = 0?
lm.fit14_c <- lm(y ~ ., data = data_14)
summary(lm.fit14_c)

# (d) Now fit a least squares regression to predict y using only x1. Comment on your results. Can you reject the null hypothesis H0 :β1 =0?
lm.fit14_d <- lm(y ~ x1, data = data_14)
summary(lm.fit14_d)

# (e) Now fit a least squares regression to predict y using only x2. Comment on your results. Can you reject the null hypothesis H0 :β1 =0?
lm.fit14_e <- lm(y ~ x2, data = data_14)
summary(lm.fit14_e)

# (f) Do the results obtained in (c)–(e) contradict each other? Explain your answer.

# Collinearity amplifies the standard error of a given model with collinear predictors, so no. 

# (g) Re-fit the linear models from (c) to (e) using this new data. What effect does this new observation have on the each of the models? In each model, is this observation an outlier? A high-leverage point? Both? Explain your answers.

x1=c(x1, 0.1)
x2=c(x2, 0.8)
y=c(y,6)

data_14g <- data.frame(
  x1 = x1,
  x2 = x2,
  y = y
)

lm.fit14_g1 <- lm(y ~ ., data = data_14)
summary(lm.fit14_g1)

lm.fit14_g2 <- lm(y ~ x1, data = data_14)
summary(lm.fit14_g2)

lm.fit14_g3 <- lm(y ~ x2, data = data_14)
summary(lm.fit14_g3)

png(filename="plot14_g1.png", width = 1600, height = 1600)
par(mfrow=c(2,2))
plot(lm.fit14_g1)
dev.off()

png(filename="plot14_g2.png", width = 1600, height = 1600)
par(mfrow=c(2,2))
plot(lm.fit14_g2)
dev.off()

png(filename="plot14_g3.png", width = 1600, height = 1600)
par(mfrow=c(2,2))
plot(lm.fit14_g3)
dev.off()


# The observation is a high-leverage point for the full model with both predictors, but is not a high-leverage point for the single-predictor models. 

# 15. This problem involves the Boston data set, which we saw in the lab for this chapter. 

# (a) For each predictor, fit a simple linear regression model to predict the response. Describe your results. In which of the models is there a statistically significant association between the predictor and the response? Create some plots to back up your assertions.

data(Boston)
Boston

str(Boston)
# factor variables: chas.

# change int to numeric 
Boston <- Boston %>%
  mutate_if(is.integer, as.numeric)

# change var:chas to factor
Boston$chas <- as.factor(Boston$chas)

# scatterplot of quant variables
png(filename = "plot15.png", width=1600, height=800)
chart.Correlation(dplyr::select(Boston, -chas), histogram = TRUE)
dev.off()

# Create a sorted list of strongest correlations/relationships

# step 1: make a list of var:data vectors

list.15 <- as.list(dplyr::select(Boston, -chas))

# step 2: combine vars into all unique pairs
corr.list <- data.frame(
  var1 = NA,
  var2 = NA,
  corr = NA
)

for(i in 1:length(list.15)) {
  for(j in 1:length(list.15)) {
    if (i != j) {
      var1 <- names(list.15[i])
      var2 <- names(list.15[j])
      corr <- cor(
        list.15[[i]], 
        list.15[[j]]
        )
      row <- c(var1, var2, corr)
      corr.list <- na.omit(rbind(corr.list, row)) %>%
        arrange(desc(corr))
    }
  }
}

# figure out how to remove duplicates

# figure out how to sort by abs value of corrs

# identify top 3 strongest correlations

# identify top 3 lowest correlations