
# clean telco churn file

library(tidyverse)
library(caret)
library(h2o)
library(mlr)

df <- read_csv("telco.csv") %>%
  select(-customerID,-SeniorCitizen) %>%
  drop_na() %>% 
  mutate_if(is.character, as.factor) %>%
  mutate_if(is.numeric, scale,scale=T,center=T)

df <- createDummyFeatures(df, target = "Churn")

write_csv(df,"telecom_churn_prep.csv")

index <- createDataPartition(df$Churn, p = 0.7, list = FALSE)

train_data <- df[index, ]
val_test_data  <- df[-index, ]

index2 <- createDataPartition(val_test_data$Churn, p = 0.5, list = FALSE)

valid_data <- val_test_data[-index2, ]
test_data  <- val_test_data[index2, ]

train_hf <- as.h2o(train_data)
valid_hf <- as.h2o(valid_data)
test_hf  <- as.h2o(test_data)

outcome <- "Churn"
predictors <- setdiff(names(train_hf), outcome)

