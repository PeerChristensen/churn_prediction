# simple
myL <- as.vector(as.character(aml@leaderboard$model_id))[2]

h2o.getModel(myL) %>%
  h2o.performance(valid=T) %>%
  .@metrics %>%
  .$thresholds_and_metric_scores %>%
  .[c('tpr','fpr')] %>%
  ggplot(aes(fpr,tpr))+
  geom_line()+
  geom_segment(aes(x=0,y=0,xend = 1, yend = 1),linetype = 2,col='grey')+
  xlab('False Positive Rate')+
  ylab('True Positive Rate')+
  ggtitle('ROC Curve')

###### all models ######

models <- as.vector(as.character(aml@leaderboard$model_id)) %>% map(h2o.getModel)

df <- tibble()

for (i in 1:length(models)) {
  
  perf <- h2o.performance(models[[i]],test_hf)
  tpr  <- perf@metrics$thresholds_and_metric_scores$tpr
  fpr  <- perf@metrics$thresholds_and_metric_scores$fpr

  model_id  <- models[[i]]@model_id
  algorithm <- models[[i]]@algorithm

  d <- tibble(model_id,algorithm,tpr,fpr)
  d <- add_row(d,model_id = model_id, algorithm=algorithm,tpr=0,fpr=0,.before=T)
  d <- add_row(d,model_id = model_id, algorithm=algorithm,tpr=0,fpr=0,.before=F)
  
  df <- rbind(df,d)
}

df %>% 
  ggplot(aes(fpr,tpr,colour = model_id)) + 
  geom_line(size = 1,alpha=.8) +
  geom_segment(aes(x=0,y=0,xend = 1, yend = 1),linetype = 2,col='grey', size =1) +
  coord_fixed() +
  xlab('False Positive Rate') +
  ylab('True Positive Rate') +
  ggtitle('ROC-kurver',
          subtitle = "Sammenligning af de bedste modeller") +
  theme_light() +
  theme(plot.title    = element_text(size = 16),
        plot.subtitle = element_text(size = 12,face="italic")) +
  scale_colour_viridis_d("Model")

models_auc <- tibble(model = as.vector(aml@leaderboard$model_id),auc = as.vector(aml@leaderboard$auc))

### function for preparing ROC curves for plotting

roc_curves <- function(H2OAutoML_object, plot = T, best = F, save_png = F) {
  
  if (best == T) {
    models <- as.vector(as.character(H2OAutoML_object@leader@model_id)) %>% 
      map(h2o.getModel)
  } 
  else {
    models <- as.vector(as.character(H2OAutoML_object@leaderboard$model_id)) %>% 
      map(h2o.getModel)
  }
  
  df <- tibble()
  
  for (i in 1:length(models)) {
    
    perf <- h2o.performance(models[[i]], test_hf)
    tpr  <- perf@metrics$thresholds_and_metric_scores$tpr
    fpr  <- perf@metrics$thresholds_and_metric_scores$fpr
    
    model_id  <- models[[i]]@model_id
    algorithm <- models[[i]]@algorithm
    
    d <- tibble(model_id,algorithm,tpr,fpr)
    d <- add_row(d,model_id = model_id, algorithm=algorithm,tpr=0,fpr=0,.before=T)
    d <- add_row(d,model_id = model_id, algorithm=algorithm,tpr=0,fpr=0,.before=F)
    d <- add_column(d, model_rank = i)
    
    df <- rbind(df,d)
  }
  
  df$model_id <- str_split(df$model_id, "_AutoML") %>%
    map_chr(1) %>%paste0(df$model_rank,": ",.)
  
  if (plot == T) {
    p <- df %>% 
      ggplot(aes(fpr,tpr,colour = model_id)) + 
      geom_line(size = 1,alpha=.8) +
      geom_segment(aes(x=0,y=0,xend = 1, yend = 1),linetype = 2,col='grey', size =1) +
      coord_fixed() +
      xlab('False Positive Rate') +
      ylab('True Positive Rate') +
      ggtitle('ROC curves',
              subtitle = "Comparison of the best models") +
      theme(plot.title    = element_text(size = 16),
            plot.subtitle = element_text(size = 12,face="italic",vjust=-1)) +
      scale_colour_viridis_d("Models")
    
    print(p)
    
    if (save_png == T) {
      ggsave("auc_bars.png")
    }
    
    return(df)
    
  } 
  else {
    
    return(df)
  }
}
