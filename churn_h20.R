# churn telco with h20

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

summary(train_hf$Churn, exact_quantiles = TRUE)
summary(valid_hf$Churn, exact_quantiles = TRUE)
summary(test_hf$Churn, exact_quantiles = TRUE)

# train with autoML
aml <- h2o.automl(x = features, 
                  y = response,
                  training_frame = train_hf,
                  validation_frame = valid_hf,
                  balance_classes = TRUE,
                  max_runtime_secs = 360)

# View the AutoML Leaderboard
lb <- aml@leaderboard

best_model <- aml@leader

pred <- h2o.predict(best_model, test_hf[, -1])

h2o.mean_per_class_error(best_model, train = TRUE, valid = TRUE, xval = TRUE)

h2o.confusionMatrix(best_model, valid = TRUE)

h2o.auc(best_model, train = TRUE)

h2o.auc(best_model, valid = TRUE)

h2o.auc(best_model, xval = TRUE)

perf <- h2o.performance(best_model, test_hf)
h2o.confusionMatrix(perf)

plot(perf)

h2o.logloss(perf)

h2o.mse(perf)

h2o.auc(perf)

metrics <- as.data.frame(h2o.metric(perf))
head(metrics)

metrics %>%
  gather(x, y, f1:tpr) %>%
  ggplot(aes(x = threshold, y = y, group = x)) +
  facet_wrap(~ x, ncol = 2, scales = "free") +
  geom_line()

# examine pred thresholds
threshold <- metrics[order(-metrics$accuracy), "threshold"][1]

finalRf_predictions <- data.frame(actual = as.vector(test_hf$Churn), 
                                  as.data.frame(h2o.predict(object = best_model, 
                                                            newdata = test_hf)))

finalRf_predictions$accurate <- ifelse(finalRf_predictions$actual == 
                                         finalRf_predictions$predict, "ja", "nein")

finalRf_predictions$predict_stringent <- ifelse(finalRf_predictions$p1 > threshold, 1, 
                                                ifelse(finalRf_predictions$p0 > threshold, 0, "unsure"))
finalRf_predictions$accurate_stringent <- ifelse(finalRf_predictions$actual == 
                                                   finalRf_predictions$predict_stringent, "yes", 
                                                 ifelse(finalRf_predictions$predict_stringent == 
                                                          "unsure", "unsure", "no"))

finalRf_predictions %>%
  group_by(actual, predict) %>%
  dplyr::summarise(n = n())

finalRf_predictions %>%
  group_by(actual, predict_stringent) %>%
  dplyr::summarise(n = n())



finalRf_predictions %>%
  gather(x, y, accurate, accurate_stringent) %>%
  mutate(x = ifelse(x == "accurate", "Default threshold: 0.5", 
                    paste("threshold:", round(threshold, digits = 2)))) %>%
  ggplot(aes(x = actual, fill = y)) +
  facet_grid(~ x) +
  geom_bar(position = "dodge") +
  scale_fill_tableau()
head(finalRf_predictions)


# optimise threshold
df <- finalRf_predictions[, c(1, 3, 4)]

thresholds <- seq(from = 0, to = 1, by = 0.1)

prop_table <- data.frame(threshold = thresholds, 
                         prop_p0_true = NA, prop_p0_false = NA,
                         prop_p1_true = NA, prop_p1_false = NA)

for (threshold in thresholds) {
  
  pred_1 <- ifelse(df$p1 > threshold, 1, 0)
  pred_1_t <- ifelse(pred_1 == df$actual, TRUE, FALSE)
  
  group <- data.frame(df, 
                      "pred_true" = pred_1_t) %>%
    group_by(actual, pred_true) %>%
    dplyr::summarise(n = n())
  
  group_p0 <- filter(group, actual == "0")
  
  prop_p0_t <- sum(filter(group_p0, pred_true == TRUE)$n) / sum(group_p0$n)
  prop_p0_f <- sum(filter(group_p0, pred_true == FALSE)$n) / sum(group_p0$n)
  prop_table[prop_table$threshold == threshold, "prop_p0_true"] <- prop_p0_t
  prop_table[prop_table$threshold == threshold, "prop_p0_false"] <- prop_p0_f
  
  group_p1 <- filter(group, actual == "1")
  
  prop_p1_t <- sum(filter(group_p1, pred_true == TRUE)$n) / sum(group_p1$n)
  prop_p1_f <- sum(filter(group_p1, pred_true == FALSE)$n) / sum(group_p1$n)
  prop_table[prop_table$threshold == threshold, "prop_p1_true"] <- prop_p1_t
  prop_table[prop_table$threshold == threshold, "prop_p1_false"] <- prop_p1_f
}

prop_table %>%
  gather(x, y, prop_p0_true, prop_p1_true) %>%
  rename(Schwellenwert = threshold) %>%
  mutate(x = ifelse(x == "prop_p0_true", "prop true p0",
                    "prop true p1")) %>%
  ggplot(aes(x = Schwellenwert, y = y, color = x)) +
  geom_point() +
  geom_line() +
  scale_color_tableau()

# revenue to be gained (assuming a specific conversion rate)
conversion <- 0.7

net_win_table <- prop_table %>%
  mutate(prop_p0_true_X = prop_p0_true * customers_no_churn_n * revenue,
         prop_p0_false_X = prop_p0_false * customers_no_churn_n * (revenue -cost),
         prop_p1_false_X = prop_p1_false * customers_churn_n * 0,
         prop_p1_true_X = prop_p1_true * customers_churn_n * ((revenue * conversion) - cost)) %>%
  group_by(threshold) %>%
  summarise(net_win = sum(prop_p0_true_X + prop_p0_false_X + prop_p1_false_X + prop_p1_true_X),
            net_win_compared = net_win - net_win_default) %>%
  arrange(-net_win_compared)

net_win_table

# explain with lime
Xtrain <- as.data.frame(train_hf)
Xtest <- as.data.frame(test_hf)

# run lime() on training set
explainer <- lime::lime(x = Xtrain, 
                        model = best_model)

# run explain() on the explainer
explanation <- lime::explain(x = Xtest[1:9, ], 
                             explainer = explainer, 
                             n_labels = 1,
                             n_features = 4,
                             kernel_width = 0.5)

plot_explanations(explanation)

explanation %>%
  plot_features(ncol = 3)

# modelplotr

library(modelplotr)
## Package modelplotr loaded! Happy model plotting!

# transform datasets and model objects into scored data and calculate deciles 
prepare_scores_and_deciles(datasets=list("train","test"),
                           dataset_labels = list("train data","test data"),
                           models = list("rf","mnl","xgb","lda"),
                           model_labels = list("random forest","multinomial logit","XGBoost","Discriminant"),
                           target_column="y")

plotting_scope(select_model_label = 'XGBoost',select_dataset_label = 'test data')

plot_cumgains(highlight_decile = 2)

plot_all(save_fig = TRUE,save_fig_filename = 'Selection model Term Deposits')

