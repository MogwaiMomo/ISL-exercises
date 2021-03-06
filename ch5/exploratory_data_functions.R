create.boxplot.facet <- function(df, outcome.var, omit.var) {
  if(missing(omit.var)) {
    remove.vars <- outcome.var
  } else {
    remove.vars <- c(outcome.var, omit.var)
  }
  # pivot data to long form
  df.long <- df %>%
    pivot_longer(-c(remove.vars), names_to = "Predictor", values_to = "Value")
  df.long <- df.long[, c("Predictor", outcome.var, "Value")] # reorder columns
  
  # create facet of plots
  boxplot <- ggplot(df.long, aes_string(x="Predictor", y="Value", fill=outcome.var)) +
    geom_boxplot() +
    facet_wrap(~Predictor, scales = "free") 
}


create.train.test.sets <- function(df) {
  # create a vector of logical values that assigns odd numbers to training and even numbers to test
  train.even <- ((as.numeric(rownames(df)) %% 2) == 0)
  test.odd <- ((as.numeric(rownames(df)) %% 2) != 0)
  
  # use this to create a subset of training data and test data
  df.train <- df[train.even,]
  df.test <- df[test.odd,]
  
  train.test.sets <- list(
    train = df.train,
    test = df.test
  )
  return(train.test.sets) # returns a 2-item list comprising 2 dfs
}


get.cors <- function(arg1, arg2) {
  # check for correlations (remove qualitative vars  with arg2 if need be)
  cors <- as.data.frame(cor(arg1[,-arg2]))
  # Sort the correlations and see which variables have the strongest correlations: 
  # turn rownames into a variable
  setDT(cors, keep.rownames = "var1")
  # gather corrs into single column
  cors_tidy <- cors %>% 
    pivot_longer(c(2:arg2), names_to = "var2", values_to = "cors") %>% 
    # remove self-correlations
    filter(cors != 1) %>%
    # sort from highest to lowest
    arrange(desc(cors))
  # remove even rows (duplicates)
  toDelete <- seq(1, nrow(cors_tidy), 2)
  cors <- cors_tidy[-toDelete, ]
  return(cors)
}


### NEEDS TO BE GENERICIZED FOR ANY DF AND VARS!
run.classification.model <- function(train.test.list){
  
  # set data to model and test with
  boston.train.df <- train.test.list[["train"]] %>%
    dplyr::select(-c(crim, chas))
  boston.test.df <- train.test.list[["test"]] %>%
    dplyr::select(-c(crim, chas))
  boston.test.outcomes <-  train.test.list[["test"]]$crime_class
  
  writeLines(
    c("Which classification model do you want to run?",
      "1. Logistical regression",
      "2. LDA",
      "3. QDA",
      "4. KNN"
    ))
  
  model.choice <- readline(prompt="Please input the number of your choice: \\n")
  
  if (model.choice == "1") {
    # run logistical regression
    model.fit <- glm(crime_class ~ . -rad -zn, data = boston.train.df, family=binomial)
    model.fit.probs <- predict(model.fit, newdata = boston.test.df, type ="response")
    model.fit.preds <- rep("high", length(model.fit.probs))
    model.fit.preds[model.fit.probs > 0.5] = "low"
    model.fit.preds
    
    
  } else if (model.choice == "2") {
    # run LDA
    model.fit <- lda(crime_class ~ . -rad -zn, data = boston.train.df)
    model.fit.preds <- predict(model.fit, newdata = boston.test.df)$class
    model.fit.preds
    
  } else if (model.choice == "3") {
    # run QDA
    model.fit <- qda(crime_class ~ . -rad -zn, data = boston.train.df)
    model.fit.preds <- predict(model.fit, newdata = boston.test.df)$class
    model.fit.preds
    
  } else if (model.choice == "4") {
    # run KNN
    
    # set predictors
    X.train <- boston.train.df %>%
      dplyr::select(-c(crime_class, rad, zn, black, lstat, rm))
    X.test <- boston.test.df %>%
      dplyr::select(-c(crime_class, rad, zn, black, lstat, rm))
    
    # scale predictors
    X.train <- scale(X.train)
    X.test <- scale(X.test)
    
    # set outcomes
    Y.train <- boston.train.df$crime_class
    Y.test <- boston.test.df$crime_class
    
    set.seed(1)
    
    k_neighbours <- readline(prompt="How many neighbours (k) should we use? \\n")
    k_neighbours <- as.numeric(k_neighbours)
    model.fit.preds <- knn(X.train, X.test, Y.train, k = k_neighbours)
    model.fit.preds
  }
  
  #set confusion matrix to get test error rate
  conf.matrix <- table(model.fit.preds, boston.test.outcomes)
  mean(model.fit.preds != boston.test.outcomes)
  
}