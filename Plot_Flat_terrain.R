#create flat slope prediction
pred_flat     <- ggpredict(MallM19, terms = "Flat_30")

# Plot Flat terrain
p_flat <- ggplot(pred_flat, aes(x, predicted)) +
  geom_jitter(data = data_all,
              aes(x = Flat_30, y = nb_individ),
              width = 0,
              height = 0.3,
              alpha = 0.4,
              size = 2,
              color="#2C7FB8") +
  geom_line(  
    color="#084081",
    linewidth =1) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2, fill= "#7FCDBB") +
  labs(x = "Flat terrain (%)",
       y = "Predicted carabid abundance",
       title = "Effect of flat slope on carabid abundance",
       subtitle = "Slope 0-30°") +
  theme_classic()+
  theme(panel.grid.major= element_line(color="grey85"))

p_flat
