#create soil cover prediction
pred_soil     <- ggpredict(MallM8,  terms = "BG_Soil")

# Plot Soil (trend)
p_soil <- ggplot(pred_soil, aes(x, predicted)) +
  geom_jitter(data = data_all,
              aes(x = BG_Soil, y = nb_individ),
              width = 0,
              height = 0.3,
              alpha = 0.4,
              size=2,
              color="#2C7FB8") +
  geom_line(color="#084081",
            linewidth =1) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2, fill= "#7FCDBB") +
  labs(x = "Soil cover (%)",
       y = "Predicted carabid abundance",
       title = "Effect of soil cover on carabid abundance (trend)") +
  theme_classic()+
  theme(panel.grid.major= element_line(color="grey85"))


p_soil