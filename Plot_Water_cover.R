#create water prediction
pred_water    <- ggpredict(MallM14, terms = "Water_Tot")

# Plot Water cover
p_water <- ggplot(pred_water, aes(x, predicted)) +
  geom_jitter(data = data_all,
              aes(x = Water_Tot, y = nb_individ),
              width = 0,
              height = 0.3,
              alpha = 0.4,
              size=2, 
              color="#2C7FB8") +
  geom_line(
    color="#084081",
    linewidth =1
  ) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2, fill= "#7FCDBB") +
  labs(x = "Water cover (%)",
       y = "Predicted carabid abundance",
       title = "Effect of water cover on carabid abundance") +
  theme_classic()+
  theme(panel.grid.major= element_line(color="grey85"))

p_water