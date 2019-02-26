# churn h20 2

library(tidyverse) # for tidy data analysis
library(caret)     # for convenient splitting
library(mice)      # mice package for Multivariate Imputation by Chained Equations (MICE)
library(rsample)   # for splitting training and test data
library(h2o)

df <- read_csv("Telecom_customer_churn.csv")

df <- df %>% 
  mutate_if(is.character, as.factor) %>%
  mutate(churn = factor(churn))

df %>%
  count(churn)

df %>%
  select(-Customer_ID,-area,-ethnic,-crclscod,-dwllsize) %>%
  select_if(is.factor) %>%
  select(churn, everything()) %>%
  gather(x, y,new_cell:creditcd) %>%
  count(churn, x, y) %>%
  ggplot(aes(x = y, y = n, fill = churn, color = churn)) +
  facet_wrap(~ x, ncol = 4, scales = "free") +
  geom_bar(stat = "identity", alpha = 0.5) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        legend.position = "top") +
  scale_color_tableau() +
  scale_fill_tableau()

