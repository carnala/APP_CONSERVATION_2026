View (Carabidae)

library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)



# Séparer la ligne statut du reste
statut <- Carabidae[1, ]       # première ligne = statuts
carab  <- Carabidae[-1, ]      # reste = abondances

# Renommer la première colonne
colnames(carab)[1] <- "Site"
colnames(statut)[1] <- "Site"

# Convertir en numérique
carab <- carab %>% mutate(across(-Site, as.numeric))

# Ajouter la colonne section
carab$section <- ifelse(grepl("^C", carab$Site), "Channelized", "Revitalized")

head(carab)





##############################################################################
################## Abondance totale par site #################################

carab <- carab %>%
  mutate(total = rowSums(select(., -Site, -section)))

# 1) Barplot abondance par site
ggplot(carab, aes(x = Site, y = total, fill = section)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = c("Channelized" = "#E07B54", "Revitalized" = "#4E9BB9")) +
  labs(title = "Total carabid abundance per site",
       y = "Total abundance", x = NULL, fill = "Section") +
  theme_bw(base_size = 13)

# Abondance totale par section
carab_section <- carab %>%
  group_by(section) %>%
  summarise(total = sum(total))

# Barplot
ggplot(carab_section, aes(x = section, y = total, fill = section)) +
  geom_bar(stat = "identity", width = 0.5) +
  scale_fill_manual(values = c("Channelized" = "#E07B54", "Revitalized" = "#4E9BB9")) +
  labs(title = "Total carabid abundance in each area",
       y = "Total abundance", x = NULL) +
  theme_bw(base_size = 13) +
  theme(legend.position = "none")


# 2) Boxplot
ggplot(carab, aes(x = section, y = total, fill = section)) +
  geom_boxplot(alpha = 0.7, width = 0.5, outlier.colour = "red", outlier.size = 3) +
  geom_jitter(width = 0.1, size = 2) +
  scale_fill_manual(values = c("Channelized" = "#E07B54", "Revitalized" = "#4E9BB9")) +
  labs(title = "Carabid abundance in each area",
       y = "Total abundance", x = NULL) +
  theme_bw(base_size = 13) +
  theme(legend.position = "none")

# 3) Test stats
shapiro.test(carab$total[carab$section == "Channelized"])
# p-value = 0.7553 normal
shapiro.test(carab$total[carab$section == "Revitalized"])
#  p-value = 0.4461 normal


t.test(total ~ section, data = carab)
# p-value = 0.4232

# BONUS : visualisation Histogramme
# Distribution de l'abondance totale par site
carab %>%
  ggplot(aes(x = total, fill = section)) +
  geom_histogram(bins = 5, color = "white", alpha = 0.7) +
  facet_wrap(~section) +
  scale_fill_manual(values = c("Channelized" = "#E07B54", "Revitalized" = "#4E9BB9")) +
  labs(title = "Distribution of total carabid abundance",
       x = "Total abundance", y = "Count") +
  theme_bw(base_size = 13) +
  theme(legend.position = "none")

#######################################################################
############# Richesse par site (nombre d'espèces présentes)###########
carab <- carab %>%
  mutate(richness = rowSums(select(., -Site, -section, -total) > 0))

# 1) Barplot par section
carab_rich_section <- carab %>%
  group_by(section) %>%
  summarise(total_richness = sum(richness))

ggplot(carab_rich_section, aes(x = section, y = total_richness, fill = section)) +
  geom_bar(stat = "identity", width = 0.5) +
  scale_fill_manual(values = c("Channelized" = "#E07B54", "Revitalized" = "#4E9BB9")) +
  labs(title = "Total carabid species richness in each area",
       y = "Total richness", x = NULL) +
  theme_bw(base_size = 13) +
  theme(legend.position = "none")

# 2) Boxplot
ggplot(carab, aes(x = section, y = richness, fill = section)) +
  geom_boxplot(alpha = 0.7, width = 0.5, outlier.colour = "red", outlier.size = 3) +
  geom_jitter(width = 0.1, size = 2) +
  scale_fill_manual(values = c("Channelized" = "#E07B54", "Revitalized" = "#4E9BB9")) +
  labs(title = "Carabid species richness in each area",
       y = "Species richness", x = NULL) +
  theme_bw(base_size = 13) +
  theme(legend.position = "none")

# 3) Test stats
shapiro.test(carab$richness[carab$section == "Channelized"])
#  p-value = 0.683
shapiro.test(carab$richness[carab$section == "Revitalized"])
# p-value = 0.1612



t.test(richness ~ section, data = carab)
# p-value = 0.5681


# BONUS : Visualisation Histogramme 

carab %>%
  ggplot(aes(x = richness, fill = section)) +
  geom_histogram(bins = 5, color = "white", alpha = 0.7) +
  facet_wrap(~section) +
  scale_fill_manual(values = c("Channelized" = "#E07B54", "Revitalized" = "#4E9BB9")) +
  labs(title = "Distribution of carabid species richness",
       x = "Species richness", y = "Count") +
  theme_bw(base_size = 13) +
  theme(legend.position = "none")



##########################################################################
########################### SIMPSON ######################################

# 1) Calcul manuel de Simpson par site
carab_mat <- carab %>%
  select(-Site, -section, -total, -richness) %>%
  as.data.frame()

simpson_carab <- apply(carab_mat, 1, function(x) {
  p <- x / sum(x)
  1 - sum(p^2)
})

carab$simpson <- simpson_carab

simpson_carab

# Barplot par section
carab_simp_section <- carab %>%
  group_by(section) %>%
  summarise(mean_simpson = mean(simpson))

ggplot(carab_simp_section, aes(x = section, y = mean_simpson, fill = section)) +
  geom_bar(stat = "identity", width = 0.5) +
  scale_fill_manual(values = c("Channelized" = "#E07B54", "Revitalized" = "#4E9BB9")) +
  labs(title = "Mean Simpson diversity by section",
       y = "Mean Simpson index", x = NULL) +
  theme_bw(base_size = 13) +
  theme(legend.position = "none")

# 2) Boxplot
ggplot(carab, aes(x = section, y = simpson, fill = section)) +
  geom_boxplot(alpha = 0.7, width = 0.5, outlier.colour = "red", outlier.size = 3) +
  geom_jitter(width = 0.1, size = 2) +
  scale_fill_manual(values = c("Channelized" = "#E07B54", "Revitalized" = "#4E9BB9")) +
  labs(title = "Carabid Simpson diversity in each area",
       y = "Simpson index", x = NULL) +
  theme_bw(base_size = 13) +
  theme(legend.position = "none")

# 3) Shapiro
shapiro.test(carab$simpson[carab$section == "Channelized"])
shapiro.test(carab$simpson[carab$section == "Revitalized"])

# un site en C n'a AUCUN carabe --> cela crée un NA
# Retirer les NaN avant le test
carab_clean <- carab %>% filter(!is.nan(simpson))

shapiro.test(carab_clean$simpson[carab_clean$section == "Channelized"])
# p-value = 0.5198
shapiro.test(carab_clean$simpson[carab_clean$section == "Revitalized"])
# p-value = 0.2466

t.test(simpson ~ section, data = carab_clean)
# p-value = 0.2944






###############################################################
################ enlever les NA ###############################


# Données sans NaN
carab_clean <- carab %>% filter(!is.nan(simpson))
carab_clean

# 1) Barplot par section
carab_simp_section <- carab_clean %>%
  group_by(section) %>%
  summarise(mean_simpson = mean(simpson))

ggplot(carab_simp_section, aes(x = section, y = mean_simpson, fill = section)) +
  geom_bar(stat = "identity", width = 0.5) +
  scale_fill_manual(values = c("Channelized" = "#E07B54", "Revitalized" = "#4E9BB9")) +
  labs(title = "Mean Simpson diversity by section",
       y = "Mean Simpson index", x = NULL) +
  theme_bw(base_size = 13) +
  theme(legend.position = "none")

# 2) Boxplot
ggplot(carab_clean, aes(x = section, y = simpson, fill = section)) +
  geom_boxplot(alpha = 0.7, width = 0.5, outlier.colour = "red", outlier.size = 3) +
  geom_jitter(width = 0.1, size = 2) +
  scale_fill_manual(values = c("Channelized" = "#E07B54", "Revitalized" = "#4E9BB9")) +
  labs(title = "Carabid Simpson diversity by section",
       y = "Simpson index", x = NULL) +
  theme_bw(base_size = 13) +
  theme(legend.position = "none")

# 3) Test stats
shapiro.test(carab_clean$simpson[carab_clean$section == "Channelized"])
# p-value = 0.4423
shapiro.test(carab_clean$simpson[carab_clean$section == "Revitalized"])
# p-value = 0.2163

t.test(simpson ~ section, data = carab_clean)
# p-value = 0.2086






#######################
# VENN


library(ggVennDiagram)

# Espèces présentes dans chaque section
carab_chan <- carab %>%
  filter(section == "Channelized") %>%
  select(-Site, -section) %>%
  colSums() %>%
  .[. > 0] %>%
  names()

carab_rev <- carab %>%
  filter(section == "Revitalized") %>%
  select(-Site, -section) %>%
  colSums() %>%
  .[. > 0] %>%
  names()

# Diagramme de Venn



install.packages("VennDiagram")
library(VennDiagram)



venn.diagram(
  x = list(Channelized = carab_chan, Revitalized = carab_rev),
  filename = NULL,
  fill = c("#E07B54", "#56B4E9"),
  alpha = 0.5,
  label.col = "black",
  cex = 2,
  fontface = "bold",
  fontfamily = "sans",      # police des chiffres
  cat.col = c("#E07B54", "#56B4E9"),
  cat.cex = 1.5,
  cat.fontface = "bold",
  cat.fontfamily = "sans",  # police des labels
  cat.pos = c(-20, 20),
  cat.dist = 0.08,
  main = "Carabid species presence by section",
  main.cex = 1,              # taille du titre
  main.fontface = "bold",    # style du titre
  main.fontfamily = "sans"  
) %>% grid::grid.draw()