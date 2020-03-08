# global options
options(stringsAsFactors = F)
setwd(dirname(parent.frame(2)$ofile))

# Load libraries
require(tidyverse)
require(data.table)
require(ggplot2)

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

library(PerformanceAnalytics)
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

# q9 scatterplot of all the data
png(filename="plot6.png", width = 1600, height = 1600)
#chart.Correlation(quants,hist=T)
pairs(quants)
dev.off()