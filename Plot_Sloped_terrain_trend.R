#create Sloped terrain prediction
pred_slope    <- ggpredict(MallM20, terms = "Sloped_30_60")

# Plot Sloped terrain (trend)
p_slope <- ggplot(pred_slope, aes(x, predicted)) +
  geom_jitter(data = data_all,
              aes(x = Sloped_30_60, y = nb_individ),
              width = 0,
              height = 0.3,
              alpha = 0.4,
              size=2,
              color="#2C7FB8") +
  geom_line(color="#084081",
            linewidth =1) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2, fill= "#7FCDBB") +
  labs(x = "Sloped terrain",
       y = "Predicted carabid abundance",
       title = "Effect of sloped terrain on carabid abundance (trend)",
       subtitle = "Slope 30-60°") +
  theme_classic()+
  theme(panel.grid.major= element_line(color="grey85"))

p_slope