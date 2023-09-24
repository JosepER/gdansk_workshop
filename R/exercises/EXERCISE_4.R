#         Author: Pedro Salas Rojo 
#         Dataset: LWS 
#         Name of project: SPAIN WORKSHOP LWS

rm(list = ls(all.names = TRUE))  
library(tidyverse) 
library(haven) 
library(partykit)

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
  # Get Random Forest and Variable importance ----
  #
  #################################
  
  model <- factor(saves) ~ age + sex + factor(marital) + factor(health_c) + 
    factor(educlev) + factor(status1) + factor(ind1_c) + factor(occ1_c) 
  
  
  forest <- partykit::cforest(model,
                              data = data,
                              ntree = 100,
                              mtry = 5,
                              trace = FALSE,
                              control = ctree_control(testtype = "Bonferroni",
                                                      teststat = "quad",
                                                      mincriterion = 0,
                                                      minbucket = 10))
  
  imp <- partykit::varimp(forest)
  relimp <- round(100*imp/max(imp), 2)
  relimp <- relimp[order(-relimp)]
  print(relimp)
  
} 