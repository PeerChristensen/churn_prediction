# plot lift and gains charts from h20 autoML leaderboard

# simple
myL <- as.vector(as.character(aml@leaderboard$model_id))[1]

gains_lift <- h2o.getModel(myL) %>%
  h2o.gainsLift()

# Gains (called capture rate)
gains_lift %>%
  ggplot(aes(x=cumulative_data_fraction,y=cumulative_capture_rate)) +
  geom_line(size = .8) +
  geom_point(size = 1) +
  geom_segment(aes(x=0,y=0,xend = 1, yend = 1),size = 1,linetype = 2,col='grey')+
  ggtitle("Gains chart",
          subtitle = "When we apply the model and select x % of customers,\nwhat % of the target class observations can we expect to hit?") +
  theme(plot.title = element_text(size = 16),
        plot.subtitle = element_text(size = 14, face = "italic",vjust=-1))

# Lift 
gains_lift %>%
  ggplot(aes(x=cumulative_data_fraction,y=cumulative_lift)) +
  geom_line(size = .8) +
  geom_point(size = 1) +
  geom_segment(aes(x=0,y=1,xend = 1, yend = 1),size = 1,linetype = 2,col='grey')+
ggtitle("Lift chart",
          subtitle = "When we apply the model and select x % of customers,\nhow many times better is that than using no model?") +
  theme(plot.title = element_text(size = 16),
        plot.subtitle = element_text(size = 14, face = "italic",vjust=-1))
 
# Response
prop_target <- prop.table(table(df$Churn))[2]

gains_lift %>%
  ggplot(aes(x=cumulative_data_fraction,y=cumulative_response_rate)) +
  geom_line(size = .8) +
  geom_point(size = 1) +
  geom_segment(aes(x=0,y = prop_target,xend = 1, yend = prop_target),size = 1,linetype = 2,col='grey') +
  ylim(c(0,1)) +
  ggtitle("Response chart",
          subtitle = "When we apply the model and select x % of customers, \nwhat is the expected % of target class observations in the selection?") +
  theme(plot.title = element_text(size = 16),
        plot.subtitle = element_text(size = 14, face = "italic",vjust=-1))

# combining plots
gains_lift %>% 
  as_tibble() %>%
  select(cumulative_data_fraction,cumulative_lift,cumulative_capture_rate) %>%
  gather(key = "metric", value = "value", - cumulative_data_fraction) %>%
  ggplot(aes(x=cumulative_data_fraction,y=value)) +
  geom_line() +
  facet_wrap(~metric,scales = "free")


### function to plot gains, lift and response charts of the best model

lift4gains <- function(H2OAutoML_object, response_ref = NULL, save_pngs = F) {
  
  model <- as.vector(as.character(H2OAutoML_object@leaderboard$model_id))[1]
  
  df <- h2o.getModel(model) %>% h2o.gainsLift()
  
  # Gains
  p1 <- df %>%
    ggplot(aes(x=cumulative_data_fraction,y=cumulative_capture_rate)) +
    geom_line(size = .8) +
    geom_point(size = 1) +
    geom_segment(aes(x=0,y=0,xend = 1, yend = 1),size = 1,linetype = 2,col='grey')+
    ggtitle("Gains chart",
            subtitle = "When we apply the model and select x % of customers,\nwhat % of the target class observations can we expect to hit?") +
    theme(plot.title = element_text(size = 16),
          plot.subtitle = element_text(size = 14, face = "italic",vjust=-1))
  
  print(p1)
  if (save_pngs == T) {
    ggsave("gains.png")
  }
  
  # Lift 
  p2 <- df %>%
    ggplot(aes(x=cumulative_data_fraction,y=cumulative_lift)) +
    geom_line(size = .8) +
    geom_point(size = 1) +
    geom_segment(aes(x=0,y=1,xend = 1, yend = 1),size = 1,linetype = 2,col='grey')+
    ggtitle("Lift chart",
            subtitle = "When we apply the model and select x % of customers,\nhow many times better is that than using no model?") +
    theme(plot.title = element_text(size = 16),
          plot.subtitle = element_text(size = 14, face = "italic",vjust=-1))
  
  print(p2)
  if (save_pngs == T) {
    ggsave("lift.png")
  }
  
  # Response
  p3 <- df %>%
    ggplot(aes(x=cumulative_data_fraction,y=cumulative_response_rate)) +
    geom_line(size = .8) +
    geom_point(size = 1) +
    ylim(c(0,1)) +
    ggtitle("Response chart",
            subtitle = "When we apply the model and select x % of customers, \nwhat is the expected % of target class observations in the selection?") +
    theme(plot.title = element_text(size = 16),
          plot.subtitle = element_text(size = 14, face = "italic",vjust=-1))
  
  if (!is.null(response_ref)) {
    p3 <- p3 + geom_segment(aes(x=0,y = response_ref,xend = 1, yend = response_ref),size = 1,linetype = 2,col='grey')
  }
  
  print(p3)
  if (save_pngs == T) {
    ggsave("response.png")
  }
}



