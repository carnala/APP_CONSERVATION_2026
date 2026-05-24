#proportion of differents vegetation heights
tout_5 <- tout %>%
  pivot_longer(
    cols = c(Veg_heigth_0_10, Veg_heigth_10_30, Veg_heigth_30_60, Veg_heigth_60),
    names_to = "Coverage_Type",
    values_to = "Value"
  )

#barplot
ggplot(tout_5, aes(x = Site, y = Value, fill = Coverage_Type)) +
  geom_bar(stat = "identity", position = "fill",
           colour="grey25", linewidth=0.2) +
  labs(
    x = "Site",
    y = "Proportion",
    fill = "Vegetation Height",
    title = "Vegetation height proportion"
  ) +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_manual(values = c(
    "Veg_heigth_0_10"= "#EDF6F5",
    "Veg_heigth_10_30"= "#C7DDDA",
    "Veg_heigth_30_60"= "#7FA8A3",
    "Veg_heigth_60"= "#4E6E6A"
  ),
  labels = c(
    "Veg_heigth_0_10" = "0-10cm",
    "Veg_heigth_10_30" = "10-30cm",
    "Veg_heigth_30_60" = "30-60cm",
    "Veg_heigth_60" = "+60cm"
  )) +
  theme_classic()+
  theme(
    plot.title = element_text(hjust = 0.5)
  )

