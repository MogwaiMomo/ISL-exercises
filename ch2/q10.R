# global options
options(stringsAsFactors = F)
setwd(dirname(parent.frame(2)$ofile))

# Load libraries
require(tidyverse)
require(data.table)

# Load data
library(MASS)
data(Boston)


# Explore data

# Step 1: check obj structure, change int to factors

Boston <- Boston %>%
  mutate_if(is.integer, as.factor)
str(Boston)

# Step 2: get summary stats

# Use summary to display stats of quants: 

quants <-Boston %>% select_if(is.numeric)
quals <- Boston %>% select_if(is.factor)

get_stats <- function(quants){
  
  stats_table <- as.data.frame.matrix(summary(quants))
  
  # STILL TO DO: calculate sd, format it to fit, 
  # then append sd to stats table

  return(stats_table)
}

stats_summary <-  get_stats(quants) 




# Graph & explore variables 
 

## plot all single-var & two-var quant graphs

library(PerformanceAnalytics)

png(filename="plot2.png", width = 1480, height = 1480)
plot2 <- chart.Correlation(quants,hist=T) 
dev.off()



# (c) Are any of the predictors associated with per capita crime rate? If so, explain the relationship.


# property-tax rate- positive correlation of 0.58
#  accessibility to radial highways - positive correlation of 0.63


# (d) Do any of the suburbs of Boston appear to have particularly high crime rates? Tax rates? Pupil-teacher ratios? Comment on the range of each predictor.

# Yes, but there are no names for each observation!

# (e) How many of the suburbs in this data set bound the Charles river?

chas_ans <- Boston %>%
  filter(chas =="1")
nrow(chas_ans)

# (f) What is the median pupil-teacher ratio among the towns in this data set?

median(Boston$ptratio)

# (g) Which suburb of Boston has lowest median value of owner- occupied homes? What are the values of the other predictors for that suburb, and how do those values compare to the overall ranges for those predictors? Comment on your findings.

Boston[which.min(Boston$medv),]

# (h) In this data set, how many of the suburbs average more than seven rooms per dwelling? More than eight rooms per dwelling? Comment on the suburbs that average more than eight rooms per dwelling.

filter(Boston, rm > 7)

filter(Boston, rm > 8)


