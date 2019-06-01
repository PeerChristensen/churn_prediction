## Data preprocessing for ML
## Peer Christensen
## March, 2019

############### Loading packages ##################################

library(dplyr)     # General data manipulation
library(mice)      # imputation
library(lubridate) # handling dates
library(h2o)       # anomaly detection
library(tidyr)     # drop NA function
library(tibble)    # add_column function
library(stringi)   # joining strings in output
library(mlr)       # remove vars with near-zero variance 

############### Read the data #####################################

df <- read.Alteryx("#1", mode="data.frame")

# Shuffle rows
df <- df %>% sample_frac(1)

############### Initial preprocessing UNTRACKED #####################

# save __CUST_VAR_KEY_)
X__CUST_VAR_KEY__ <- df$X__CUST_VAR_KEY__

# Output X__CUST_VAR_KEY__
write.Alteryx(data.frame(X__CUST_VAR_KEY__ = X__CUST_VAR_KEY__), 4)

df <- df %>% select(-X__CUST_VAR_KEY__)

# remove empty variables 
df <- df %>% select_if(function(x) !purrr::is_empty(x))
df <- df %>% select_if(function(x) !all(is.null(x)))

# remove variables with only NA values
df <- df %>% select_if(function(x) !all(is.na(x)))

# change character variables to factor
df <- df %>% mutate_if(is.character, as.factor)

# change variables with > 50 levels to numeric, else factor
df <- df %>% mutate_if(~n_distinct(.[]) > 25, as.numeric)
df <- df %>% select_if(function(x) !all(is.na(x))) # removes NA cols if "as.numeric" fails
df <- df %>% mutate_if(~n_distinct(.[]) <= 25, factor)

# add variable suffixes
nums <- df %>% select_if(is.numeric) %>% names()

if (length(nums) > 0) {
  df <- df %>% rename_if(is.numeric, function(x) paste0(x,"AVG"))
}

cats <- df %>% select_if(is.factor) %>% names()
if (length(cats) > 0) {
  df <- df %>% rename_if(is.factor, function(x) paste0(x,"CODE"))
}

n_original_rows <- nrow(df)

############### Remove variables by type ###########################

# remove variables with n % NA/missing values
names_before_missing <- names(df)

df <- df %>% select_if(function(x) sum(is.na(x) / length(x)) < .2)
df <- df %>% select_if(function(x) sum(is.null(x) / length(x)) < .2)

names_after_missing <- names(df)

col_names_removed_1 <- setdiff(names_before_missing, names_after_missing)
col_names_removed_1 <- data.frame("Reason" = rep("missing values",length(col_names_removed_1)), "Variable" = col_names_removed_1)

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

col_names_removed_3 <- data.frame("Reason" = rep("sequential",length(seq_cols)), "Variable" =seq_cols)

# Remove rows with more than 40% missing values
df$X__CUST_VAR_KEY__ <- X__CUST_VAR_KEY__

df$non_missing <- apply(df, 1, function(x) sum(is.na(x) / length(x))<.4)

df <- df[df$non_missing == T,]

X__CUST_VAR_KEY__ <- df$X__CUST_VAR_KEY__
df <- df %>% select(- X__CUST_VAR_KEY__)

############## remove variables with zero variance ################################

names_before_nzv <- names(df)

df <- removeConstantFeatures(df, perc = .05)

nzv_names <- setdiff(names_before_nzv, names(df))

col_names_removed_4 <- data.frame("Reason" = rep("(near) zero variance",length(nzv_names)), "Variable" = nzv_names)

############## remove duplicated variables ########################################

names_before_dupl <- names(df)

df <- df[!duplicated(t(df))]

dupl_names <- setdiff(names_before_dupl, names(df))

col_names_removed_5 <- data.frame("Reason" = rep("duplicate",length(dupl_names)), "Variable" = dupl_names)

############### Remove correlated variables ########################################

names_before_cor <- names(df)

cor_mat <- df %>% 
  select_if(is.numeric) %>%
  drop_na() %>%
  cor() 

cor_mat[upper.tri(cor_mat)] <- 0
diag(cor_mat) <- 0

cor_mat <- abs(cor_mat) > .95

# row-wise removal, starting with col 1, then removing correlated variables
whichKeep <- names(which(rowSums(lower.tri(cor_mat) * cor_mat) == 0))

if (!is.null(whichKeep)){
  df <- df %>% 
    select_if(function(x) !is.numeric(x)) %>% 
    cbind(df[,whichKeep])
}

cor_names <- setdiff(names_before_cor,names(df))

col_names_removed_6 <- data.frame("Reason" = rep("correlated",length(cor_names)), "Variable" = cor_names)

############## Output removed variables 1 #######################################

removed_vars <- rbind(col_names_removed_1,col_names_removed_3,col_names_removed_4,col_names_removed_5,col_names_removed_6)
removed_vars <- removed_vars[removed_vars$Variable != "non_missing",]

write.Alteryx(removed_vars,5)

############### Impute data #######################################################

if (ncol(df) < 3) {
  df$X__CUST_VAR_KEY__ <- X__CUST_VAR_KEY__
  write.Alteryx(df, 1)
} else {
  
  imp <- mice(data = df, maxit=1)
  df <- mice::complete(imp)
  
  ############### Detect outliers ###################################################
  
  h2o.init()
  
  df_h2o <- as.h2o(df)
  dl_model <- h2o.deeplearning(x = 1:nrow(df_h2o), training_frame = df_h2o, autoencoder = TRUE,
                               hidden = c(50, 50), epochs = 5, max_w2 = 10, l1=1e-5)
  
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
  
  # % removed
  pct_removed <- paste("% removed rows:  ",round((1-nrow(df)/n_original_rows)*100,2))
  
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
  rows_removed <- data.frame(rows_removed = c(n_rows_removed,pct_removed))
  
  write.Alteryx(rows_removed, 2)
}
