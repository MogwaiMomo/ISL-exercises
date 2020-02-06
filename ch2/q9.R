# Global options
options(stringsAsFactors = FALSE)
setwd(dirname(parent.frame(2)$ofile))

# Load libraries
require(tidyverse)
require(data.table)

# Load data
Auto <- read.csv("Auto.csv")
Auto <- na.omit(Auto)

# Set char vars as factors
Auto$cylinders <- as.factor(Auto$cylinders)
Auto$origin <- as.factor(Auto$origin)
Auto$name <- as.factor(Auto$name)
Auto$year <- as.factor(Auto$year)
Auto$horsepower <- as.numeric(Auto$horsepower)
Auto$weight <- as.numeric(Auto$weight)
summary(Auto)

# select only numeric variables

quant <- c()
qual <- c()

for (i in names(Auto)) {
  output <- paste(i, class(Auto[,i]), sep=": ")
  print(output)
  if (class(Auto[,i]) == "factor") {
   qual <- c(i, qual) 
  }
  else {
    quant <- c(i, quant)
  }
}


quant_df <- Auto %>% select(quant)
qual_df <- Auto %>% select(qual) 


# Use summary to display stats: 

getstats <- function(df){
  
  ans_df <- data.frame(
    "name" = NA,
    "sd" = NA,
    "mean" = NA, 
    "min" = NA,
    "max" = NA
  )
  
  for (i in names(df)) {
    row <- data.frame(
      "name" = as.character(i),
      "sd" = as.character(round(sd(df[,i], na.rm = T))),
      "mean" = as.character(round(unname(summary(df[,i])[4]))), 
      "min" = as.character(round(unname(summary(df[,i])[1]))),
      "max" = as.character(round(unname(summary(df[,i])[6])))
    )
    ans_df <- rbind(row, ans_df)
  }
  q9_c <- na.omit(ans_df)  
  return(q9_c)
}

q9_c <-  getstats(quant_df) 

# q9 d) - Remove 10th-85th observations, see how stats change

q9_d <- getstats(quant_df[-(10:85),]) 

# q9 e) - graph & explore variables 

# Single var graphs:

## USE par(mfrow=c(r,c)) to make facets as needed
# quant (hist) - 5 graphs 
# qual (count/barplot) - 3 graphs (omit name, makes a worthless graph)

# plot all 8 single-var graphs

plot1 <- par(mfrow=c(2,4))
for(i in 1:length(names(quant_df))) {
  hist(quant_df[,i], main = names(quant_df)[i], xlab = names(quant_df)[i])
}

omit_name <- qual_df %>%
  select(-name)

for(i in 1:length(names(omit_name))) {
  counts <- table(omit_name[,i])
  barplot(counts, main = names(omit_name)[i], xlab = names(omit_name)[i])
}
par(plot1)


# Two var graphs:

# quant vs. quant graphs (5x5)

plot2 <- par(mfrow=c(5,5))
for(a in 1:length(names(quant_df))) {
  for(b in 1:length(names(quant_df)))
    plot(quant_df[,a], quant_df[,b], 
         main = names(quant_df)[b], 
         xlab = names(quant_df)[a],
         ylab = ""
    )
}
par(plot2)

# qual vs. quant graphs (5x5)

plot3 <- par(mfrow=c(3,5))
for(i in 1:length(names(omit_name))) {
  for(j in 1:length(names(quant_df)))
  plot(omit_name[,i], quant_df[,j], 
       main = names(quant_df)[j], 
       xlab = names(omit_name)[i]
       )
}
par(plot3)


# q9_f - which vars are highly correlated with mpg?

# weight, horsepower, displacement (inv)
# origin, year





