
install.packages("vegan")
library(vegan)

library(dplyr)
# Vérifier la structure
head(Data_plants_transposed)
# Nombre total de genres de plantes dans le fichier
ncol(plants %>% select(-Site, -section, -plant_richness))

Data_plants_transposed$Site

# Exclure la ligne Entomophilie et convertir en numeric
plants <- Data_plants_transposed %>%
  filter(Site != "Entomophilie") %>%
  mutate(across(-Site, as.numeric))

# Créer la colonne section
plants$section <- ifelse(grepl("^C", plants$Site), "Channelized", "Revitalized")

head(plants)


# Matrice de présence/absence (sans Site et section)
plant_mat <- plants %>%
  select(-Site, -section) %>%
  as.data.frame()

rownames(plant_mat) <- plants$Site





############ QUESTION ####################


plants <- plants %>%
  mutate(plant_richness = rowSums(select(., -Site, -section) > 0))

plants %>% select(Site, plant_richness)

Data_pitfall_transposed$section <- ifelse(grepl("^C", Data_pitfall_transposed$Site), 
                                          "Channelized", "Revitalized")
# Métriques invertébrés par site
invert_metrics <- Data_pitfall_transposed %>%
  mutate(
    invert_abund   = rowSums(select(., -Site, -section)),
    invert_richness = rowSums(select(., -Site, -section) > 0),
    invert_simpson = apply(select(., -Site, -section), 1, function(x) {
      p <- x / sum(x)
      1 - sum(p^2)
    })
  ) %>%
  select(Site, invert_abund, invert_richness, invert_simpson)

# Joindre avec la richesse en plantes
combined <- plants %>%
  select(Site, plant_richness) %>%
  inner_join(invert_metrics, by = "Site")

print(combined)


# 1) Régressions linéaires
summary(lm(invert_abund ~ plant_richness, data = combined))
summary(lm(invert_richness ~ plant_richness, data = combined))
summary(lm(invert_simpson ~ plant_richness, data = combined))


# 2) Scatterplots
ggplot(combined, aes(x = plant_richness, y = invert_abund)) +
  geom_point(size = 3, color = "#4E9BB9") +
  geom_smooth(method = "lm", se = TRUE, color = "#E07B54") +
  geom_text(aes(label = Site), vjust = -0.8, size = 3.5) +
  labs(title = "Plant richness influence on invertebrate abundance",
       x = "Plant genus richness", y = "Invertebrate abundance") +
  theme_bw(base_size = 13)

ggplot(combined, aes(x = plant_richness, y = invert_richness)) +
  geom_point(size = 3, color = "#4E9BB9") +
  geom_smooth(method = "lm", se = TRUE, color = "#E07B54") +
  geom_text(aes(label = Site), vjust = -0.8, size = 3.5) +
  labs(title = "Plant richness influence on invertebrate richness",
       x = "Plant genus richness", y = "Invertebrate taxa richness") +
  theme_bw(base_size = 13)

ggplot(combined, aes(x = plant_richness, y = invert_simpson)) +
  geom_point(size = 3, color = "#4E9BB9") +
  geom_smooth(method = "lm", se = TRUE, color = "#E07B54") +
  geom_text(aes(label = Site), vjust = -0.8, size = 3.5) +
  labs(title = "Plant richness influence on invertebrate diversity (Simpson)",
       x = "Plant genus richness", y = "Simpson index") +
  theme_bw(base_size = 13)


# Résultats
# Abondance p-value : 0.573 R2 : 0.056 Pas de relation
# Richesse familles p-value : 0.949 R2 : 0.001 Pas de relation
# Simpson p-value : 0.072 R2 : 0.443 Tendance mais pas significatif



# Maintenant on vérifie les résidus 
# --> Les résidus c'est la différence entre la valeur réelle 
# et la valeur prédite par la droite de régression

# On vérifie les résidus pour s'assurer que la régression linéaire est valide. 
# Si les résidus sont normaux et aléatoires → la régression est appropriée. 
# Sinon il faudrait utiliser un autre type de régression.

mod_simpson <- lm(invert_simpson ~ plant_richness, data = combined)

# Shapiro sur les résidus
shapiro.test(residuals(mod_simpson))

# Graphique des résidus
par(mfrow = c(1, 2))
plot(mod_simpson, which = 1)  # résidus vs valeurs ajustées
plot(mod_simpson, which = 2)  # QQ-plot

# p = 0.1465 > 0.05 → les résidus sont normaux 
# → la régression linéaire est validée pour Simpson 


# Maintenant pour abondance et richesse 

mod_abund    <- lm(invert_abund ~ plant_richness, data = combined)
mod_richness <- lm(invert_richness ~ plant_richness, data = combined)

shapiro.test(residuals(mod_abund))
shapiro.test(residuals(mod_richness))

# OK pour abondance (0.93), mais pas pour richness (0.00788)

# donc pour richness, on utilise plutôt une corrélation non paramétrique, ici une 
# régression de Spearman
cor.test(combined$plant_richness, combined$invert_richness, method = "spearman")

# non significatif : p-value = 0.9288





# RESUME du processus 
# Scatterplot → résultat visuel qui montre la tendance (la droite monte, descend, ou est plate)
# Régression / Spearman → confirmation statistique que la tendance est réelle et pas due au hasard
# Shapiro sur les résidus → validation que le test choisi est approprié pour les données



#### VISUALISATION COMPLETE (avec p-value et R2)
# si on ne veut pas les visualiser ensemble : il ne faut juste pas RUN p1, p2 et p3 

install.packages("patchwork")
library(patchwork)

# Simpson
p1 <- ggplot(combined, aes(x = plant_richness, y = invert_simpson)) +
  geom_point(size = 3, color = "#4E9BB9") +
  geom_smooth(method = "lm", se = TRUE, color = "#E07B54") +
  geom_text(aes(label = Site), vjust = -0.8, size = 3) +
  annotate("text", x = min(combined$plant_richness), y = max(combined$invert_simpson),
           label = "R² = 0.44\np = 0.072", hjust = 0, size = 4) +
  labs(title = "Simpson diversity",
       x = "Plant genus richness", y = "Simpson index") +
  theme_bw(base_size = 11)

# Abondance
p2 <- ggplot(combined, aes(x = plant_richness, y = invert_abund)) +
  geom_point(size = 3, color = "#4E9BB9") +
  geom_smooth(method = "lm", se = TRUE, color = "#E07B54") +
  geom_text(aes(label = Site), vjust = -0.8, size = 3) +
  annotate("text", x = min(combined$plant_richness), y = max(combined$invert_abund),
           label = "R² = 0.06\np = 0.573", hjust = 0, size = 4) +
  labs(title = "Invertebrate abundance",
       x = "Plant genus richness", y = "Total abundance") +
  theme_bw(base_size = 11)

# Richesse
p3 <- ggplot(combined, aes(x = plant_richness, y = invert_richness)) +
  geom_point(size = 3, color = "#4E9BB9") +
  geom_smooth(method = "lm", se = TRUE, color = "#E07B54") +
  geom_text(aes(label = Site), vjust = -0.8, size = 3) +
  annotate("text", x = min(combined$plant_richness), y = max(combined$invert_richness),
           label = "rho = 0.038\np = 0.929", hjust = 0, size = 4) +
  labs(title = "Invertebrate richness",
       x = "Plant genus richness", y = "Family richness") +
  theme_bw(base_size = 11)

# Combiner les 3
p1 + p2 + p3 + plot_layout(ncol = 3)
