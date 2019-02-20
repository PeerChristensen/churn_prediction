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
 
gains_lift %>% 
  as_tibble() %>%
  select(cumulative_data_fraction,cumulative_lift,cumulative_capture_rate) %>%
  gather(key = "metric", value = "value", - cumulative_data_fraction) %>%
  ggplot(aes(x=cumulative_data_fraction,y=value)) +
  geom_line() +
  facet_wrap(~metric,scales = "free") +
