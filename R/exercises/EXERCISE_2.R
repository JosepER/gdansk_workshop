#         Author: Pedro Salas Rojo 
#         Date: 09/2023
#         Dataset: LIS 
#         Name of project: COMPARE OLS VS LASSO PERFORMANCE

rm(list = ls(all.names = TRUE))  
library(tidyverse) 
library(haven) 
library(caret)
library(xtable) 
library(glmnet)

# Get and clean data ---- 

# Define function to get household-related variables 
# Include in "hhold" all variables  

seth <- function(data_file) {     
  hhold <- c('hid', 'hpopwgt', 'hilabour')
  data1 <- read.LIS(data_file, labels = FALSE, vars = hhold)   
  return(data1)   
} 

# Define function to get personal-related variables 
# Include in "hhold" all variables  

setp <- function(data_file){ 
  pers <- c('hid', 'pid', 'pilabour', 'sex', 'age', 'marital',
            'disabled', 'educlev', 'lfs', 'status1', 'ind1_c', 'occ1_c') 
  data2 <- read.LIS(data_file, labels = FALSE, vars = pers)    
  return(data2) 
} 

# Store dataset names 
datasets <- c('pl20')  
results <- NA 

# For each dataset,  
for (ccyy in datasets) {  
  
  # Print name of dataset 
  print(ccyy) 
  
  # Reset objects 
  data1 <- NULL 
  data2 <- NULL 
  data  <- NULL 
  dat_emp <- NULL 
  tab <- NULL 
  
  # Get data with functions previouisly defined 
  data1 <- seth(paste0(ccyy,'h'))  
  data2 <- setp(paste0(ccyy,'p')) 
  
  # Merge by ID 
  data <- merge(data1, data2, by=c("hid"), sort=TRUE) 
  
  # Arrange the data
  data <- data %>% 
    dplyr::filter(age>=30 & age<=60 & pilabour > 0) %>%
    dplyr::mutate(pilabour = pilabour/2.01902978,
                  age5 = cut(age, breaks = seq(min(age), max(age), 5), 
                             right = FALSE, 
                             ordered_result = TRUE),
                  age5num = as.numeric(age5)) 
  
  # Get missing information. Drop item non responses.
  print(summary(is.na(data))) 
  data <- na.omit(data)
  print(summary(is.na(data))) 
  
  # Get subsample (easier to tackle)
  set.seed(3)
  data <- data[sample(1:nrow(data), 3000, replace = FALSE),]  
  
  # See variables
  
  for(i in c("sex", "age5num", "marital", "disabled", "educlev", "status1", "ind1_c", "occ1_c")){
    print(i)
    print(table(data[[i]]))
  }
  
  #################################
  #
  # OLS output ----
  #
  #################################
  
  # Define model, run OLS, print results and store values of coefficients
  model <- pilabour ~ (sex) + factor(marital) + factor(educlev) +
    factor(status1) + factor(ind1_c) + factor(occ1_c) + factor(age5num) + (disabled)
  
  reg <- lm(model, data = data)
  
  print(summary(reg))
  
  coef_df <- summary(reg)$coefficients
  
  # Run Cross Validation (alpha 1, lambda 0, OLS).
  
  ols_tr <- caret::train(model,
                         data = data,
                         method = "glmnet",
                         trControl = trainControl(method = "cv", number = 3, 
                                                  verboseIter = TRUE,   savePredictions = "all"),
                         tuneGrid = expand.grid(alpha = 1,         
                                                lambda = 0))
  
  results <- ols_tr[["results"]]
  lambda <- ols_tr[["bestTune"]][["lambda"]]         
  rmse_ols <- round(mean(ols_tr[["resample"]][["RMSE"]]), 2) 
  
  #################################
  #
  # LASSO regularization ----
  #
  #################################
  
  set.seed(3)
  
  # EXERCISE 2
  
  # Train LASSO to select opt* lambda
  
  # Define model 
  model <- pilabour ~ (sex) + factor(marital) + factor(educlev) +
    factor(status1) + factor(ind1_c) + factor(occ1_c) + factor(age5num) + (disabled)
  
  lasso_tr <- caret::train(model,
                           data = data,
                           method = "glmnet",
                           trControl = trainControl(method = "cv", number = 3, 
                                                    verboseIter = TRUE,   savePredictions = "all"),
                           tuneGrid = expand.grid(alpha = 1,         
                                                  lambda = 30^seq(0.01,2,0.01)))
  
  print(plot(y = lasso_tr$results$RMSE, x = lasso_tr$results$lambda,
             main = "RMSE by lambda value", xlab = "Lambda", ylab = "RMSE"))
  print(abline(v = lasso_tr[["bestTune"]][["lambda"]]))  
  
  results <- lasso_tr[["results"]]
  lambda <- lasso_tr[["bestTune"]][["lambda"]]         
  rmse_lasso <- round(mean(lasso_tr[["resample"]][["RMSE"]]), 2) 
  
  # Define LASSO setting 
  
  dep <- data$pilabour
  vec <- model.matrix( ~ sex + factor(marital) + factor(educlev) +
                         factor(status1) + factor(ind1_c) + factor(occ1_c) + 
                         factor(age5num) + disabled, data)
  
  # Run LASSO, plot LASSO and store coefficients
  
  lasso_mod <- glmnet(vec, dep, alpha=1)
  
  plot(lasso_mod, xvar = "lambda")
  abline(v=log(lambda), lty="dashed", col="black")
  
  lasso <- glmnet(vec, dep, alpha=1, lambda = lambda)
  coeff2 <- lasso$beta
  
  print(coeff2)
  
  
  print(paste0("Out of sample RMSE from OLS is: ", rmse_ols))
  print(paste0("Out of sample RMSE from LASSO is: ", rmse_lasso))
  
} 
