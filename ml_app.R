
library(tidyverse)
library(caret)
library(mice)
library(lubridate)

df <- read_csv("Telecom_customer_churn.csv")

df <- df %>% sample_frac(.3) %>% select(-Customer_ID)

############### Detect, change, remove variables by type ###############

# change variables with < 50 levels to factor
df <- df %>% mutate_if(~n_distinct(.[]) < 50, factor)

# change character variables to factor
df <- df %>% mutate_if(is.character, as.factor) 

# remove datetime columns
df <- df %>% select_if(function(x) !is.Date(x))

############### Remove outliers #########################################

############### Impute data #############################################

# check number of NAs in each column
df %>% map(is.na) %>% map(sum) %>% unlist()

imp <- mice(data = df, print = TRUE,maxit=2) # default maxit is 5, make sure cust id is removed
df <- complete(imp, "long")

############### standardize, remove variables with NZV ##################

standardized <- preProcess(df, method=c("center", "scale","nzv")) # see also "corr"

df <- predict(standardized, df)

############### Remove correlated variables ##############################

#correlations <- preProcess(df, method=c("corr"))

remove_cor <- df %>% 
  select_if(is.numeric) %>% 
  drop_na() %>% 
  cor() %>% 
  findCorrelation(cutoff = .95)

df <- df %>% select(-remove_cor)

# split data



