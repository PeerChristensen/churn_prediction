# plot lift and gains charts from h20 autoML leaderboard

# simple
myL <- as.vector(as.character(aml@leaderboard$model_id))[1]

gains_lift <- h2o.getModel(myL) %>%
  h2o.gainsLift()

# lift
gains_lift %>%
  ggplot(aes(x=cumulative_data_fraction,y=cumulative_lift)) +
  geom_line(size = .8) +
  geom_point(size = 1)

# gains (called capture rate)
gains_lift %>%
  ggplot(aes(x=cumulative_data_fraction,y=cumulative_capture_rate)) +
  geom_line(size = .8) +
  geom_point(size = 1)
 
gains_lift %>% 
  as_tibble() %>%
  select(cumulative_data_fraction,cumulative_lift,cumulative_capture_rate) %>%
  gather(key = "metric", value = "value", - cumulative_data_fraction) %>%
  ggplot(aes(x=cumulative_data_fraction,y=value)) +
  geom_line() +
  facet_wrap(~metric,scales = "free") +
