
#Step 9. Build a RandomForest and updating the Model Id
ModelIdTxt_RF <- paste0(ModelIdTxt,"_RF")
RandomForest <- h2o.randomForest(x=Predictors,
                                 y="Y_TARGET",
                                 training_frame = assign(paste("trainingh2o", unqID, sep=""), as.h2o(training)),
                                 validation_frame = assign(paste("testingh2o", unqID, sep=""), as.h2o(testing)),
                                 model_id = ModelIdTxt_RF,
                                 ignore_const_cols = TRUE,
                                 ntrees = 75,
                                 balance_classes = TRUE,
                                 stopping_metric = 'AUC',
                                 stopping_tolerance = 0.001,
                                 stopping_rounds = 2,
                                 #binomial_double_trees = TRUE,
                                 #verbose = TRUE,
                                 seed = 234)


#Step 10: Extracts and selects only correct variables
ImportanceRF <- h2o.varimp(RandomForest)
ImportanceRF$Cum_Importance <- cumsum(ImportanceRF$percentage)
ImportanceTRF <- ImportanceRF[which(ImportanceRF$Cum_Importance < Threshold),]
Predictors_CleanRF <- ImportanceTRF$variable

if(length(Predictors_CleanRF) < 5){Predictors_CleanRF <- ImportanceRF$variable[1:5]}

rm(ImportanceRF)
rm(ImportanceTRF)
rm(RandomForest)

#Step 11: Builds a new Random Forest with only relevant variables
RandomForest <- h2o.randomForest(x=Predictors_CleanRF,
                                 y="Y_TARGET",
                                 training_frame = assign(paste("trainingh2o", unqID, sep=""), as.h2o(training)),
                                 validation_frame = assign(paste("testingh2o", unqID, sep=""), as.h2o(testing)),
                                 model_id = ModelIdTxt_RF,
                                 ignore_const_cols = TRUE,
                                 ntrees = 500,
                                 balance_classes = TRUE,
                                 nfolds = nfolds,
                                 stopping_metric = 'AUC',
                                 stopping_tolerance = 0.001,
                                 stopping_rounds = 2,
                                 keep_cross_validation_predictions = TRUE,
                                 fold_assignment = "Modulo",
                                 #binomial_double_trees = TRUE,
                                 #verbose = TRUE,
                                 seed = 234)


#Build a GBM and update it
ModelIdTxt_GB <- paste0(ModelIdTxt,"_GB")
GradBoost <- h2o.gbm( x=Predictors,
                      y="Y_TARGET",
                      training_frame = assign(paste("trainingh2o", unqID, sep=""), as.h2o(training)),
                      validation_frame = assign(paste("testingh2o", unqID, sep=""), as.h2o(testing)),
                      model_id = ModelIdTxt_GB,
                      balance_classes = TRUE,
                      ignore_const_cols = TRUE,
                      distribution = "bernoulli",
                      ntrees = 50,
                      seed = 57567)

ImportanceGB <- h2o.varimp(GradBoost)
ImportanceGB$Cum_Importance <- cumsum(ImportanceGB$percentage)
ImportanceTGB <- ImportanceGB[which(ImportanceGB$Cum_Importance < Threshold),]
Predictors_CleanGB <- ImportanceTGB$variable

if(length(Predictors_CleanGB) < 5){Predictors_CleanGB <- ImportanceGB$variable[1:5]}

rm(ImportanceGB)
rm(ImportanceTGB)
rm(GradBoost)

GradientBoosting <- h2o.gbm( x=Predictors_CleanGB,
                             y="Y_TARGET",
                             training_frame = assign(paste("trainingh2o", unqID, sep=""), as.h2o(training)),
                             validation_frame = assign(paste("testingh2o", unqID, sep=""), as.h2o(testing)),
                             balance_classes = TRUE,
                             model_id = ModelIdTxt_GB,
                             ignore_const_cols = TRUE,
                             distribution = "bernoulli",
                             ntrees = 500,
                             stopping_rounds = 4,
                             stopping_metric = "AUC",
                             stopping_tolerance = 0.001,
                             fold_assignment = "Modulo",
                             keep_cross_validation_predictions = TRUE,
                             nfolds = nfolds,
                             seed = 57567)

#Build a Deep learning with non-factors
PredictorsDL2 <- grep("FRA_|DIS_|PC|_AVG_", names(DL_Cust), value=TRUE)
Temp <- DL_Cust[PredictorsDL2]
nums <- unlist(lapply(Temp, is.numeric)) 
PredictorsDL <- names(Temp[,nums])
rm(Temp)

ModelIdTxt_DL <- paste0(ModelIdTxt,"_Deeplearn")
DeepLearning<- h2o.deeplearning(x = PredictorsDL,
                                y= "Y_TARGET",
                                training_frame = assign(paste("trainingh2o", unqID, sep=""), as.h2o(training)),
                                validation_frame = assign(paste("testingh2o", unqID, sep=""), as.h2o(testing)),
                                model_id = ModelIdTxt_DL,
                                balance_classes = TRUE,
                                ignore_const_cols = TRUE,
                                standardize = FALSE,
                                seed = 6856,
                                distribution = "bernoulli")

#Extract the most important variables and use only those as predictors
ImportanceDL <- h2o.varimp(DeepLearning)
ImportanceDL$Cum_Importance <- cumsum(ImportanceDL$percentage)
ImportanceTDL <- ImportanceDL[which(ImportanceDL$Cum_Importance < Threshold),]
Predictors_CleanDL <- ImportanceTDL$variable

if(length(Predictors_CleanDL) < 5){Predictors_CleanDL <- ImportanceDL$variable[1:5]}

rm(ImportanceDL)
rm(ImportanceTDL)
rm(DeepLearning)

DeepLearning<- h2o.deeplearning(x = Predictors_CleanDL,
                                y= "Y_TARGET",
                                training_frame = assign(paste("trainingh2o", unqID, sep=""), as.h2o(training)),
                                validation_frame = assign(paste("testingh2o", unqID, sep=""), as.h2o(testing)),
                                model_id = ModelIdTxt_DL,
                                balance_classes = TRUE,
                                ignore_const_cols = TRUE,
                                standardize = FALSE,
                                seed = 6856,
                                fold_assignment = "Modulo",
                                keep_cross_validation_predictions = TRUE,
                                nfolds = nfolds,
                                distribution = "bernoulli")


#Builds a logistic regression model
ModelIdTxt_LR <- paste0(ModelIdTxt,"_logreg")
logreg <- h2o.glm(x = PredictorsDL,
                  y= "Y_TARGET",
                  training_frame = assign(paste("trainingh2o", unqID, sep=""), as.h2o(training)),
                  validation_frame = assign(paste("testingh2o", unqID, sep=""), as.h2o(testing)),
                  model_id = ModelIdTxt_LR,
                  ignore_const_cols = TRUE,
                  family = "binomial",
                  standardize = FALSE,
                  remove_collinear_columns = TRUE,
                  link = "logit",
                  max_active_predictors = 20,
                  seed = 5008)



# Build an ensemble model
ModelIdTxt_ES <- paste0(ModelIdTxt,"_Ensemble")
Ensemble <- h2o.stackedEnsemble(x = Predictors,
                                y = "Y_TARGET",
                                training_frame = assign(paste("trainingh2o", unqID, sep=""), as.h2o(training)),
                                metalearner_algorithm = 'gbm',
                                validation_frame = assign(paste("testingh2o", unqID, sep=""), as.h2o(testing)),
                                model_id = ModelIdTxt_ES,
                                base_models = list(RandomForest, GradientBoosting, DeepLearning))
