# Global options
options(stringsAsFactors = FALSE)
setwd(dirname(parent.frame(2)$ofile))

# Load data
college <- read.csv("college.csv")

# Change first row into rownames
rownames(college) <- college[,1]

# Remove first column (now redundant)
college <- college[,-1]

# Summarize variables
summary(college)

# Produce scatterplot matrix (of numerical variables)
pairs(college[,2:11])

# Plot boxplots of Outstate vs. Private
college$Private <- as.factor(college$Private)
plot(college$Private, college$Outstate)

# Tag college as elite or not based on the % of top 10% high school applicants

# start by creating a vector filled with 'nos'
Elite <- rep("No", nrow(college))
Elite[college$Top10perc>50] <- "Yes"
Elite <- as.factor(Elite)
college <- data.frame(college, Elite)
summary(college)
plot(college$Elite, college$Outstate)

# create 4 histograms of quantitative variables, display in 4x4 grid

op <- par(mfrow = c(2, 2))

hist1 <- hist(college$Grad.Rate)
hist2 <- hist(college$perc.alumni)
hist3 <- hist(college$S.F.Ratio)
hist4 <- hist(college$PhD)

par(op)
