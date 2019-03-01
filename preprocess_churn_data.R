
# clean telco churn file

library(tidyverse)

df <- read_csv("telco.csv") %>%
  select(-customerID,-SeniorCitizen) %>%
  drop_na() %>% 
  mutate_if(is.character, as.factor) %>%
  mutate_if(is.numeric, scale,scale=T,center=T)

write_csv("telecom_churn_prep.csv")
