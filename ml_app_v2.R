library(dplyr)
library(mice)
library(lubridate)
library(h2o)
library(purrr)
library(tidyr)
library(tibble)
library(stringi)

df <- read.Alteryx("#1", mode="data.frame")

df <- df %>% sample_frac(.01)

n_original_rows <- nrow(df)

original_col_names <- names(df)

# save __CUST_VAR_KEY_
print(names(df))

X__CUST_VAR_KEY__ <- df$X__CUST_VAR_KEY__

write.Alteryx(data.frame(X__CUST_VAR_KEY__ = X__CUST_VAR_KEY__), 4)

df <- df %>% select(- X__CUST_VAR_KEY__)

############### Detect, change, remove variables by type ###########################

# remove empty variables 
df <- df %>% select_if(function(x) !purrr::is_empty(x))

# remove variables with n % NA/missing values
df <- df %>% select_if(function(x) sum(is.na(x) / length(x)) < .2)

# remove variables with only NA values
df <- df %>% select_if(function(x) !all(is.na(x)))

# change character variables to factor
df <- df %>% mutate_if(is.character, as.factor)

# change variables with > 50 levels to numeric, else factor
df <- df %>% mutate_if(~n_distinct(.[]) > 25, as.numeric)
df <- df %>% mutate_if(~n_distinct(.[]) < 25, factor)

# remove datetime columns
df <- df %>% select_if(function(x) !is.Date(x))

# remove sequential variables
is_sequential <- function(x){
  all(diff(x) == diff(x)[1])
}

seq_cols <- df %>% 
  select_if(is.numeric) %>% 
  select_if(function(x) !NA %in% x) %>%
  select_if(function(x) is_sequential(x)) %>%
  names()

df <- df[!names(df) %in% seq_cols]

############## remove variables with zero variance ################################

df <- df %>% select_if(function(x) n_distinct(x) > 1)

############## Names of removed variables 1 #######################################

col_names_removed_1 <- setdiff(original_col_names, names(df))
col_names_removed_1_str <- stri_join_list(list(col_names_removed_1), sep = ",  ")
col_names_removed_1_str <- paste("variables removed due to emptiness or repetition of a single value:  ",col_names_removed_1_str)

############### Remove correlated variables ########################################

cor_mat =df %>% 
  select_if(is.numeric) %>%
  drop_na() %>%
  cor() 

cor_mat[upper.tri(cor_mat)] <- 0
diag(cor_mat) <- 0

cor_mat <- abs(cor_mat) > .95

whichKeep <- names(which(rowSums(lower.tri(cor_mat) * cor_mat) == 0))

df <- df %>% 
  select_if(function(x) !is.numeric(x)) %>% 
  cbind(df[,whichKeep])

############## Names of removed variables 2 #######################################

col_names_removed_2 <- setdiff(original_col_names, names(df)) # C
print(paste("QQQQQ",col_names_removed_2))
col_names_removed_2 <- setdiff(col_names_removed_2,col_names_removed_1)
print(paste("QQQQQ",col_names_removed_2))

col_names_removed_2 <- stri_join_list(list(col_names_removed_2), sep = ",  ")
col_names_removed_2 <- paste("variables removed due to high correlation with another variable:  ",col_names_removed_2)

############### Impute data #######################################################

imp <- mice(data = df, maxit=1) # default maxit is 5, make sure cust id is removed
df <- mice::complete(imp)

############### Detect outliers ###################################################

h2o.init()

df_h2o <- as.h2o(df)
dl_model <- h2o.deeplearning(x = 1:nrow(df_h2o), training_frame = df_h2o, autoencoder = TRUE,
                             hidden = c(50, 50), epochs = 5)

df_anom <- h2o.anomaly(dl_model, df_h2o)
anomaly_mse <- as.data.frame(df_anom)

df <- df_h2o %>%
  as.data.frame() %>% 
  add_column(mse = anomaly_mse$Reconstruction.MSE)

# Put ID variable back in the data
df$X__CUST_VAR_KEY__ <- X__CUST_VAR_KEY__

df <- df %>% 
  filter(mse < (mean(mse) + (2.5*sd(mse)))) %>% 
  select(-mse)

df$missing <- apply(df, 1, function(x) sum(is.na(x) / length(x)+1)<.4) # + 1 is cust var key

df[df$missing == T,]

# % removed
print(paste("% removed data:  ",(1-nrow(df)/n_original_rows)*100))

############### standardize ########################################################

X__CUST_VAR_KEY__ <- df$X__CUST_VAR_KEY__

num_cols <- df %>% 
  select(- X__CUST_VAR_KEY__) %>%
  select_if(is.numeric) %>% 
  scale(center=T,scale=T)

fact_cols <- df %>% 
  select_if(function(x) !is.numeric(x))

df <- data.frame(X__CUST_VAR_KEY__,num_cols,fact_cols)

############## output data ##########################################################

write.Alteryx(df, 1)

n_rows_removed <- n_original_rows - nrow(df)
n_rows_removed <- paste0("number of rows removed:  ", n_rows_removed) 

write.Alteryx(n_rows_removed, 2)

cols_removed <- data.frame(variables_removed = c(col_names_removed_1_str,col_names_removed_2))

write.Alteryx(cols_removed, 3)
