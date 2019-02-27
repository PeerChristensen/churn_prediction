
library(tidyverse)
library(caret)
library(mice)
library(lubridate)

df <- read_csv("Telecom_customer_churn.csv")

# just for testing
df <- df %>% sample_frac(.3) %>% select(-Customer_ID)

na_cols <- df %>% map(is.na) %>% map(sum) %>% unlist() %>% 
  tibble(names(.)) %>% filter(. > 10) %>% select('names(.)') %>% sample_n(20)

na_cols = na_cols$`names(.)`

df = data.frame(df[,na_cols], churn = df$churn)

############### Detect, change, remove variables by type ###############

# change to numeric
df <- map_df(df, function(col) {
  
  if (suppressWarnings(all(!is.na(as.numeric(as.character(col)))))) {
    as.numeric(as.character(col))
  } else {
    col
    }
})

# change variables with < 50 levels to factor
df <- df %>% mutate_if(~n_distinct(.[]) < 50, factor)

# change character variables to factor
df <- df %>% mutate_if(is.character, as.factor) 

# remove datetime columns
df <- df %>% select_if(function(x) !is.Date(x))

############### Remove correlated variables ##############################

#correlations <- preProcess(df, method=c("corr"))

# if (df %>% map(is.numeric) %>% unlist() %>% sum() > 1) {
#   remove_cor <- df %>% 
#     select_if(is.numeric) %>% 
#     drop_na() %>% 
#     cor() %>% 
#     findCorrelation(cutoff = .95) # doesn't work as expected
#   
#   df <- df %>% select(-remove_cor)
# }

############### Impute data #############################################

# check number of NAs in each column
df %>% map(is.na) %>% map(sum) %>% unlist()

imp <- mice(data = df, print = TRUE,maxit=2) # default maxit is 5, make sure cust id is removed
df_impute <- complete(imp, "long")
df_impute <- df_impute %>% select(-.imp,-.id)


############### Partition data #########################################

# split
set.seed(42)
index <- createDataPartition(df$churn, p = 0.7, list = FALSE)

train_data <- df_impute[index, ] # only impute values in training data
test_data  <- df[-index, ]

index2 <- createDataPartition(test_data$churn, p = 0.5, list = FALSE)

valid_data <- test_data[-index2, ]
test_data <- test_data[index2, ]

############### Remove outliers #########################################


############### standardize, remove variables with NZV ##################

standardized <- preProcess(train_data, method=c("center", "scale","nzv")) # see also "corr"

train_data <- predict(standardized, train_data)
valid_data <- predict(standardized, train_data)
test_data  <- predict(standardized, train_data)




#num_cols <- df %>%
#  select_if(is.numeric)
  
#num_cols_keep <- num_cols[,!apply(m,2,function(x) any(x > 0.9))]

#remove_cols <- setdiff(names(num_cols),names(num_cols_keep))

m=df %>% 
  select_if(is.numeric) %>%
  drop_na() %>%
  cor() 

m[upper.tri(m)] <- 0
diag(m) <- 0

x <- abs(m) > .95

whichKeep <- names(which(rowSums(lower.tri(x) * x) == 0))

#df <- df %>% select_if(!is.numeric) select(!is.numeric,whichKeep)
df <- df %>% select_if(function(x) !is.numeric(x)) %>% add_column(df[,whichKeep])
