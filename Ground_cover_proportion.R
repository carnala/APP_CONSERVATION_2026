# Soil types and vegetation proportion by site
# data transformation
tout_2 <- tout %>%
  pivot_longer(
    cols = c(sandp, gravelp, stonep, soilp, domp, mossp, herbp, woodyp, flowerp, Water_cover),
    names_to = "Coverage_Type",
    values_to = "Value"
  )

tout_2$Coverage_Type <- factor(tout_2$Coverage_Type, 
                               levels = c("Water_cover", "sandp", "gravelp", "stonep", "soilp", "domp", "woodyp", "mossp", "herbp", "flowerp"))

#Barplot
ggplot(tout_2, aes(x = Site, y = Value, fill = Coverage_Type)) +
  geom_bar(stat = "identity", position = "fill",
           colour="grey25", linewidth=0.2) +
  labs(
    x = "Site",
    y = "Proportion",
    fill = "Type of ground cover",
    title = "Ground cover proportion"
  ) +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_manual(
    values = c(
      "Water_cover"= "#5F9BCF",
      "sandp" = "#D2BE93",
      "gravelp"= "#B8B0A2",
      "stonep" = "#9F9387",             
      "soilp" = "#9B6A43",            
      "domp" = "#6F5448",               
      "woodyp" = "#48633F",             
      "mossp" = "#5B9670",            
      "herbp" = "#7EAF7A",            
      "flowerp" = "#C7DDC0"
    ),
    labels = c(
      "Water_cover" = "Water",  
      "sandp" = "Sand",
      "gravelp" = "Gravel",
      "stonep" = "Stone",
      "soilp" = "Soil",
      "domp" = "Dead organic mater",
      "mossp" = "Moss",
      "herbp" = "Herb",
      "woodyp" = "Woody",
      "flowerp" = "Flower"
    )) +
  theme_classic()  +
  theme(
    plot.title = element_text(hjust = 0.5)
  )