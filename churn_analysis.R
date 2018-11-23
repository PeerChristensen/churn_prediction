# CHURN ANALYSIS

library(tidyverse)
library(ROCR)
library(caret)
library(MASS)
library(descr)
library(SDMTools)
library(ranger)
library(broom)

### PREPROCESS ###

df <- read.csv("churn_data.csv",stringsAsFactors = T) %>%
  dplyr::select(-X) %>%
  mutate(couponDiscount = factor(couponDiscount),
         shippingFees = factor(shippingFees),
         newsletter=factor(newsletter),
         websiteDesign = factor(websiteDesign),
         throughAffiliate = factor(throughAffiliate)) %>%
  as_tibble()

# recode churn variable
df$churn <- ifelse(df$returnCustomer=="1",0,1)

# upsample
df$churn <- factor(df$churn)

### SPLIT DATA ###

set.seed(76782)
trainId = createDataPartition(df$churn, 
                              p=0.7, list=FALSE,times=1)

train = df[trainId,]
test  =  df[-trainId,]

table(train$churn)
train <- upSample(train,train$churn)

### TRAIN AND CHECK MODEL ###
fit1 <- glm(churn ~ title + paymentMethod + couponDiscount + purchaseValue + dvd +newsletter, family = binomial, train)
summary(fit1)

# check AIC
fit2 <- stepAIC(fit1, trace = 0)
summary(fit2) # same

#pseudo R2
LogRegR2(fit1)

#vif
vif(fit1)

### EVALUATE ###
# predict
train$pred <- predict(fit1, type = "response", na.action = na.exclude)

train %>% dplyr::select(churn, pred) %>% tail()
predictions = train %>% dplyr::select(churn, pred)

#prediction object
predictObj <- prediction(predictions$pred,predictions$churn)

#performance object
performanceObj = performance(predictObj, measure = "tpr", x.measure = "fpr")
plot(performanceObj)
abline(a=0, b= 1)

# optimal cut-off
opt.cut = function(performanceObj, predictObj){
  cut.ind = mapply(FUN=function(x, y, p){
    d = (x - 0)^2 + (y-1)^2
    ind = which(d == min(d))
    c(sensitivity = y[[ind]], specificity = 1-x[[ind]], 
      cutoff = p[[ind]])
  }, performanceObj@x.values, performanceObj@y.values, predictObj@cutoffs)
}
print(opt.cut(performanceObj, predictObj))
# 0.54

# confusion Matrix
confMatrix <- confusion.matrix(train$churn,
                                  train$pred, threshold = .54)
confMatrix
#accuracy
sum(diag(confMatrix)) / sum(confMatrix)

### TEST PREDICTIONS AND ACCURACY ###
test$pred <- predict(fit1,newdata = test, type = "response", na.action = na.exclude)

confMatrixT <- confusion.matrix(test$churn,
                               test$pred, threshold = .52)
confMatrixT
#accuracy
sum(diag(confMatrixT)) / sum(confMatrixT)
# 0.594

#AUC
predictionsT = test %>% dplyr::select(churn, pred)
SDMTools::auc(predictionsT$churn,predictionsT$pred)

### RANDOM FOREST ###

#tuning 

set.seed(123456789)

tgrid <- expand.grid(
  .mtry = 2:8,
  .splitrule = "gini",
  .min.node.size = c(10, 20)
)

fitRF1 <- train(churn ~title + paymentMethod + couponDiscount + purchaseValue + dvd +newsletter, data = train,
           method = "ranger",
           tuneGrid = tgrid,
           trControl = trainControl(method = "cv", number = 5),
           num.trees = 200)

plot(fitRF1)
pred_rf <- predict(fitRF1, train)
confusionMatrix(pred_rf, train$churn)

# fit after tuning
fitRF2 <- train(churn ~ title + paymentMethod + couponDiscount + purchaseValue + dvd +newsletter, 
                data = train,
                method="rf",ntree=100,
                trControl=trainControl(method = "cv",number = 5))

pred_rf <- predict(fitRF2, train)
confusionMatrix(pred_rf, train$churn)

### TEST ###
test$pred_rf <- predict(fitRF1,newdata = test)
confusionMatrix(test$pred_rf, test$churn)

# note: for probabilities, use randomForest, not ranger

### VAR IMPORTANCE ###

#RF
plot(varImp(fitRF2))

#GLM
t_vals <- tidy(fit1) %>% 
  mutate(t=abs(estimate/std.error)) %>%
  dplyr::select(term,t) %>%
  arrange(desc(t))

p=varImp(fit2)
p <- p %>% mutate(Variable=row.names(p)) 
p %>% ggplot(aes(x=reorder(Variable,-Overall),y=Overall)) +
  geom_col()
  

