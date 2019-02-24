# variable importance

varImp_plot <- function(H2OAutoML_object) {
  
  model <- as.vector(as.character(H2OAutoML_object@leaderboard$model_id)) %>% 
    map(h2o.getModel) %>% .[[1]]
  
  print(model@algorithm == "stackedensemble")
  
  if (model@algorithm == "stackedensemble") {
    print("Ensemble model: Plotting Model importance and Variable importances of model with highest importance")
    metaLearner <- h2o.getModel(model@model$metalearner$name)
    
    # plot model importance using H2O
    h2o.varimp_plot(metalearner)
    
    # VarImp of most important model
    modelImp <- h2o.varimp(metaLearner) # data frame
    
    highestImpName <- modelImp[1,1]
    
    model  <- h2o.getModel(highestImpName)
    varImp <- h2o.varimp(model)
  } else {
    varImp <- h2o.varimp(model)
  }
    h2o.varimp_plot(model)
  
    #Variable   <- varImp[,1]
    #Importance <- varImp[,2]
    #df <- tibble(Variable,Importance)
}


