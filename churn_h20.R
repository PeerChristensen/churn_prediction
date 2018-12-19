# churn telco with h20

library(h2o)
h2o.init(nthreads = -1)

# Load libraries
library(tidyverse) # for tidy data analysis
library(caret)     # for convenient splitting
library(mice)      # mice package for Multivariate Imputation by Chained Equations (MICE)
library(keras)     # for neural nets
library(lime)      # for explaining neural nets
library(rsample)   # for splitting training and test data
library(recipes)   # for preprocessing
library(yardstick) # for evaluation
library(ggthemes)  # for additional plotting themes
library(corrplot)  # for correlation
library(magrittr)

theme_set(theme_minimal())

df <- read_csv("telco.csv") 

glimpse(df)

df <- df %>% mutate_if(is.character, as.factor)
df$SeniorCitizen <- as.factor(df$SeniorCitizen)
glimpse(df)

df %>%
  count(Churn)

df %>%
  select(-customerID) %>%
  select_if(is.factor) %>%
  select(Churn, everything()) %>%
  gather(x, y, gender:PaymentMethod) %>%
  count(Churn, x, y) %>%
  ggplot(aes(x = y, y = n, fill = Churn, color = Churn)) +
  facet_wrap(~ x, ncol = 4, scales = "free") +
  geom_bar(stat = "identity", alpha = 0.5) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        legend.position = "top") +
  scale_color_tableau() +
  scale_fill_tableau()

df %>%
  select(-customerID) %>%
  #select_if(is.numeric) %>%
  select(Churn, MonthlyCharges, tenure, TotalCharges) %>%
  gather(x, y, MonthlyCharges:TotalCharges) %>%
  ggplot(aes(x = y, fill = Churn, color = Churn)) +
  facet_wrap(~ x, ncol = 3, scales = "free") +
  geom_density(alpha = 0.5) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        legend.position = "top") +
  scale_color_tableau() +
  scale_fill_tableau()

df <- df %>%
  select(-customerID)

# missing data
md.pattern(df, plot = T)

# impute missing data
imp <- mice(data = df,  print = FALSE)
train_data_impute <- complete(imp, "long")

# or drop
# df <- df %>%
#   drop_na()

# split
set.seed(42)
index <- createDataPartition(df$Churn, p = 0.7, list = FALSE)

train_data <- df[index, ]
test_data  <- df[-index, ]

index2 <- createDataPartition(test_data$Churn, p = 0.5, list = FALSE)

valid_data <- test_data[-index2, ]
test_data <- test_data[index2, ]

# using recipes
recipe_churn <- recipe(Churn ~ ., train_data) %>%
  step_dummy(all_nominal(), -all_outcomes()) %>%
  step_center(all_predictors(), -all_outcomes()) %>%
  step_scale(all_predictors(), -all_outcomes()) %>%
  prep(data = train_data)

train_data <- bake(recipe_churn, newdata = train_data) %>%
  select(Churn, everything())

valid_data <- bake(recipe_churn, newdata = valid_data) %>%
  select(Churn, everything())

test_data <- bake(recipe_churn, newdata = test_data) %>%
  select(Churn, everything())

### H2O #####

library(h2o)
h2o.init(nthreads = -1)

h2o.no_progress()

train_hf <- as.h2o(train_data)
valid_hf <- as.h2o(valid_data)
test_hf <- as.h2o(test_data)

response <- "Churn"
features <- setdiff(colnames(train_hf), response)