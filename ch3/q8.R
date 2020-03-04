# global options
options(stringsAsFactors = F)
setwd(dirname(parent.frame(2)$ofile))

# Load libraries
require(tidyverse)
require(data.table)

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

## plot all factor-based graphs

# set grid of plots 1rx3c
par(mfrow=c(1,3))
attach(data)
boxplot(mpg ~ cylinders, xlab = "cyl")
boxplot(mpg ~ year, xlab = "year")
boxplot(mpg ~ origin, xlab = "origin")


## plot all single-var & two-var quant graphs

library(PerformanceAnalytics)
png(filename="plot.png", width = 1480, height = 1480)
plot2 <- chart.Correlation(quants,hist=T) 
dev.off()