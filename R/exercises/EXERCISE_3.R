#         Author: Pedro Salas Rojo 
#         Dataset: LWS 
#         Name of project: SPAIN WORKSHOP LWS

rm(list = ls(all.names = TRUE))  
library(tidyverse) 
library(haven) 
library(partykit)
library(caret)
library(party)

# Get and clean data ---- 

# Define function to get household-related variables 
# Include in "hhold" all variables  
seth <- function(data_file) {     
  hhold <- c('hid', 'hpopwgt', 'hhtype', 'inum')
  data1 <- read.LIS(data_file, labels = FALSE, vars = hhold)   
  return(data1)   
} 

# Define function to get personal-related variables 
# Include in "hhold" all variables  

setp <- function(data_file){ 
  pers <- c('hid', 'pid', 'sex', 'relation', 'inum', 'age', 'marital',
            'health_c', 'educlev', 'status1', 'ind1_c', 'occ1_c',
            'basb', 'bafr1_c') 
  data2 <- read.LIS(data_file, labels = FALSE, vars = pers)    
  return(data2) 
} 

# Store names of the datasets 
datasets <- c('es17')  
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
  
  data1 <- data1 %>%
    filter(inum == 1) 
  
  data2 <- setp(paste0(ccyy,'p')) 
  
  data2 <- data2 %>%
    filter(age>=25 & age<=75) %>%
    filter(relation == 1000) %>%
    filter(inum == 1)
  
  # Merge by ID. Define age and select household heads
  
  data <- merge(data1, data2, by=c("hid"), all = TRUE, sort=TRUE) 
  
  
  # Get na.omit information 
  print(summary(is.na(data))) 
  data <- na.omit(data)
  print(summary(is.na(data))) 
  
  # See variables
  
  for(i in c("sex", "age", "marital", "health_c", "educlev", "status1", "ind1_c", 
             "occ1_c", "basb", "bafr1_c")){
    print(i)
    print(table(data[[i]]))
  }
  
  # Make basb binary, 1 = saves, 0 = does not save
  
  data$saves <- ifelse(data$basb==20, 1, 0)
  
  #################################
  #
  # Get Tree ----
  #
  #################################
  
  # EXERCISE 3
  
  # Set model
  model <- factor(saves) ~ factor(educlev)
  
  # Set cross-validation method and number of folds
  cv <- trainControl(method = "cv", number = 5, 
                      verboseIter = FALSE)        
  
  # Define grid of (1-alpha) used to tune the algorithm. 
  grid <- expand.grid(mincriterion = seq(0.9, 0.995, 0.005))
  
  tr_train <- caret::train(model,
                           data = data, 
                           method = "ctree", 
                           trControl = cv,  
                           tuneGrid = grid,
                           controls = ctree_control(minbucket = 100))
  
  results <- tr_train[["results"]]
  mincri <- round(tr_train[["bestTune"]][["mincriterion"]], 2)   
  acc <- round(mean(tr_train[["resample"]][["Accuracy"]]), 2)
  
  print(paste0("The Accuracy of this tree, with an alpha of ",as.numeric(1-mincri),", is: ", acc))
  
  
  detach("package:party", unload = TRUE)

  tree <- partykit::ctree(model,
                          data = data, 
                          control = ctree_control(testtype = "Bonferroni", 
                                                  teststat = "quad", 
                                                  alpha = 1-mincri,
                                                  minbucket = 100,
                                                  minsplit = 300,
                                                  maxdepth = 6))
  
  # Predict income and groups
  data$y_tilde <- predict(tree, type="response")
  data$groups <- predict(tree, type="node")
  print(table(data$groups))
  
  #################################
  #
  # Plot Tree ----
  #
  #################################
  
  ct_node <- as.list(tree$node)
  data$groups <- predict(tree, type = "node")
  
  pred <- data %>%
    group_by(groups) %>%
    mutate(x = stats::weighted.mean(saves)) %>%
    summarise_all(funs(mean), na.rm = TRUE)    %>%
    ungroup() %>%
    dplyr::select(groups, x) 
  
  a <- data %>%
    mutate(m = stats::weighted.mean(x = saves))
  
  mean_pop <- round(mean(a$m),3)
  
  pred <- as.data.frame(pred)
  qi <- pred
  
  for (t in 1:length(qi[,1])){
    typ<-as.numeric(names(table(data$groups)[t]))
    qi[t,2]<-length(data$groups[data$groups==typ])/length(data$groups) 
  }
  
  for(t in 1:nrow(pred)) {
    ct_node[[pred[t,1]]]$info$prediction <- as.numeric(paste(format(round(pred[t, -1], 
                                                                          digits = 3), nsmall = 2)))
    ct_node[[pred[t,1]]]$info$nobs       <- as.numeric(paste(format(round(100*qi[t, -1]  , 
                                                                          digits = 2), nsmall = 2)))
  }
  
  tree$node <- as.partynode(ct_node)
  
  print(plot(tree,  terminal_panel=node_terminal, 
             tp_args = list(FUN = function(node) 
               c("Exp. outcome",node$prediction, "Pop. Share (%)", node$nobs))))
  
} 