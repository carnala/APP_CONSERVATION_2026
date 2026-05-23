Data_sites$Elevation <- as.numeric(Data_sites$Elevation)
Data_sites$Elevation <- round(Data_sites$Elevation, 2) # Toute la colonne avec 2 décimales 


# CARABIDS AND OTHER GROUND INVERTEBRATES


str(Data_pitfall_transposed)
library("dplyr")
Data_pitfall_transposed <- Data_pitfall_transposed%>%mutate(across(-Site, as.numeric)) # tout est numérique
Data_pitfall_transposed$section <- ifelse(grepl("^C", Data_pitfall_transposed$Site), "Channelized", "Revitalized") #regrouper les différentes zones
head(Data_pitfall_transposed)


#######################################################
################# RICHESSE ############################

# Distribution de la richesse en familles par site
Data_pitfall_transposed %>%
  mutate(richness = rowSums(select(., -Site, -section) > 0)) %>%
  ggplot(aes(x = richness, fill = section)) +
  geom_histogram(bins = 5, color = "white", alpha = 0.7) +
  facet_wrap(~section) +
  scale_fill_manual(values = c("Channelized" = "#E07B54", "Revitalized" = "#4E9BB9")) +
  labs(title = "Distribution of invertebrate taxa richness",
       x = "Taxa richness", y = "Count") +
  theme_bw(base_size = 13) +
  theme(legend.position = "none")

## Nombre de familles totales par section 
fam_count <- Data_pitfall_transposed %>%
  pivot_longer(cols = -c(Site, section), names_to = "family", values_to = "abund") %>%
  filter(abund > 0) %>%
  group_by(section) %>%
  summarise(nb_families = n_distinct(family))
fam_count

#section     nb_families
# 1 Channelized          28
# 2 Revitalized          27

#graphique
ggplot(fam_count, aes(x = section, y = nb_families, fill = section)) +
  geom_bar(stat = "identity", width = 0.5) +
  scale_fill_manual(values = c("Channelized" = "#E07B54", "Revitalized" = "#4E9BB9")) +
  labs(title = "Number of invertebrate taxa per area",
       y = "Number of taxa", x = NULL) +
  theme_bw(base_size = 13) +
  theme(legend.position = "none")

# Boxplot richesse en familles par site --> combien de familles par site
Data_pitfall_transposed %>%
  mutate(richness = rowSums(select(., -Site, -section) > 0)) %>%
  ggplot(aes(x = section, y = richness, fill = section)) +
  geom_boxplot(alpha = 0.7, width = 0.5) +
  geom_jitter(width = 0.1, size = 2) +
  geom_text(aes(label = Site)) +
  scale_fill_manual(values = c("Channelized" = "#E07B54", "Revitalized" = "#4E9BB9")) +
  labs(title = "Invertebrate taxa richness per site in each area",
       y = "Number of taxa", x = NULL) +
  theme_bw(base_size = 13) +
  theme(legend.position = "none")


#############################
###### ABONDANCE ############

# Distribution des abondances totales par site
Data_pitfall_transposed %>%
  mutate(total = rowSums(select(., -Site, -section))) %>%
  ggplot(aes(x = total, fill = section)) +
  geom_histogram(bins = 5, color = "white", alpha = 0.7) +
  facet_wrap(~section) +
  scale_fill_manual(values = c("Channelized" = "#E07B54", "Revitalized" = "#4E9BB9")) +
  labs(title = "Distribution of total invertebrate abundance",
       x = "Total abundance", y = "Count") +
  theme_bw(base_size = 13) +
  theme(legend.position = "none")

#abondance totale 
fam_detail <- Data_pitfall_transposed %>%
  pivot_longer(cols = -c(Site, section), names_to = "family", values_to = "abund") %>%
  group_by(section, family) %>%
  summarise(total_abund = sum(abund, na.rm = TRUE), .groups = "drop")%>%
  mutate(family = reorder(family, total_abund)) #si on veut les mettre dans l'ordre croissant
# pour un ordre croissanr : placer un "-" devant total_abund

print(fam_detail, n=70)

#section     family              total_abund

#1 Channelized Ara_Acari                     5
#2 Channelized Ara_Araneae                 154
#3 Channelized Ara_Opiliones                 0
#4 Channelized Col_Carabidae                19
#5 Channelized Col_Chrysomelidae             1
# ..... blablabla --> 66)


# graphique
ggplot(fam_detail, aes(x = family, y = total_abund, fill = section)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values = c("Channelized" = "#E07B54", "Revitalized" = "#4E9BB9")) +
  labs(title = "Total invertebrate abundance per taxa in each area",
       y = "Total abundance", x = NULL, fill = "Section") +
  theme_bw(base_size = 6) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# Boxplot abondance totale par site --> combien d'individus TOTAL par site
Data_pitfall_transposed %>%
  mutate(total = rowSums(select(., -Site, -section))) %>%
  ggplot(aes(x = section, y = total, fill = section)) +
  geom_boxplot(alpha = 0.7, width = 0.5) +
  geom_jitter(width = 0.1, size = 2) +
  geom_text(aes(label = Site), vjust = 1, hjust = -2.5, size = 3) +
  scale_fill_manual(values = c("Channelized" = "#E07B54", "Revitalized" = "#4E9BB9")) +
  labs(title = "Total invertebrate abundance in each area",
       y = "Total abundance", x = NULL) +
  theme_bw(base_size = 13) +
  theme(legend.position = "none")




# Graphe qui suit : pas très important, ce n'est pas notre question d'étude
# Comparaison des familles --> NOMBRE d'individus par famille ET par section

Data_pitfall_transposed %>%
  pivot_longer(cols = -c(Site, section), names_to = "family", values_to = "abund") %>%
  ggplot(aes(x = section, y = abund, fill = section)) +
  geom_boxplot(alpha = 0.7, width = 0.5) +
  geom_jitter(width = 0.1, size = 1.5) +
  facet_wrap(~family, scales = "free_y") +
  scale_fill_manual(values = c("Channelized" = "#E07B54", "Revitalized" = "#4E9BB9")) +
  labs(title = "Abundance per family by section",
       y = "Abundance", x = NULL) +
  theme_bw(base_size = 10) +
  theme(legend.position = "none")






############################################################
# TEST STATS 
############################################################

# Comment ça marche ? Somme du total par C1, C2, ... et le test compare ces 4 ensemble

# Abondance totale par site --> individus TOTAUX
abund_total <- Data_pitfall_transposed %>%
  mutate(total = rowSums(select(., -Site, -section)))

shapiro.test(abund_total$total[abund_total$section == "Channelized"])
# p-value = 0.7576
shapiro.test(abund_total$total[abund_total$section == "Revitalized"])
# p-value = 0.9408

# Comparaison abondance totale entre les deux sections 
t.test(total ~ section, data = abund_total)
# p-value = 0.522

#############################################################

# Richesse en familles par site
richness <- Data_pitfall_transposed %>%
  mutate(richness = rowSums(select(., -Site, -section) > 0))

shapiro.test(richness$richness[richness$section == "Channelized"])
# p-value = 0.02386
shapiro.test(richness$richness[richness$section == "Revitalized"])
# p-value = 0.03357


# Comparaison richesse en familles entre les deux sections
wilcox.test(richness ~ section, data = richness)
# p-value = 0.2849



##########################################################

# Comparaison à l'intérieur même des familles 
## La partie qui suit n'est pas forcément importante (jusqu'à Simpson)

# Normalité 

Data_pitfall_transposed %>%
  pivot_longer(cols = -c(Site, section), names_to = "family", values_to = "abund") %>%
  group_by(family) %>%
  summarise(
    p_chan = tryCatch(shapiro.test(abund[section == "Channelized"])$p.value, error = function(e) NA),
    p_rev  = tryCatch(shapiro.test(abund[section == "Revitalized"])$p.value, error = function(e) NA)
  )

# Results normalité 
# family              p_chan    p_rev
# 1 Ara_Acari          0.406   NA      
# 2 Ara_Araneae        0.0155   0.673  
# 3 Ara_Opiliones     NA        0.0239 
# 4 Col_Carabidae      0.755    0.446  
# 5 Col_Chrysomelidae  0.00124  0.00124
# 6 Col_Coccinellidae  0.00124 NA      
# 7 Col_Curculionidae  0.683    0.683 
# ..... etc ......


# Wilcoxon par famille
wilcox_results <- Data_pitfall_transposed %>%
  pivot_longer(cols = -c(Site, section), names_to = "family", values_to = "abund") %>%
  group_by(family) %>%
  summarise(
    p_value = tryCatch(
      wilcox.test(abund ~ section)$p.value,
      error = function(e) NA
    )
  ) %>%
  mutate(significatif = ifelse(p_value < 0.05, "oui", "non"))

print(wilcox_results, n = Inf)

# A tibble: 33 × 3
#family              p_value significatif
# 1 Ara_Acari            1      non         
#2 Ara_Araneae          0.309  non         
#3 Ara_Opiliones        0.181  non         
#4 Col_Carabidae        0.686  non         
#5 Col_Chrysomelidae    0.247  non         
#6 Col_Coccinellidae    0.453  non         
#7 Col_Curculionidae    1      non         
#8 Col_Dytiscidae       0.453  non         
#9 Col_Elateridae       0.0603 non         
#10 Col_Lampyridae       0.453  non         
#11 Col_Leiodidae        1      non         
#12 Col_Staphylinidae    0.757  non         
#13 Collembola           0.0286 oui         
#14 Cru_Isopoda          0.301  non         
#15 Dermaptera           0.453  non         
#16 Dip_Brachycera       0.886  non         
#17 Dip_Nematocera       0.381  non         
#18 Diplopoda            0.739  non         
#19 Ephemeroptera        0.453  non         
#20 Gas_Escargots        1      non         
#21 Gas_Limace           0.0256 oui         
#22 Hem_Auchenorrhyncha  1      non         
#23 Hem_Heteroptera      0.163  non         
#24 Hem_Sternorryhncha   1      non         
#25 Hym_Apoidea          0.620  non         
#26 Hym_Chalcidoidea     1      non         
#27 Hym_Formicidae       0.114  non         
#28 Hym_Symphyta         0.181  non         
#29 Lep_Noctuidae        0.453  non         
#30 Neu_Chrysopidae      0.453  non         
#31 Plecoptera           1      non         
#32 Thysanoptera         0.453  non         
#33 Tricoptera           0.181  non         




############# SIMPSON ####################

# Calcul manuel de Simpson
fam_mat <- Data_pitfall_transposed %>%
  select(-Site, -section) %>%
  as.data.frame()

simpson_manual <- apply(fam_mat, 1, function(x) {
  p <- x / sum(x)        # proportions
  1 - sum(p^2)           # 1 - D
})

div_df <- data.frame(
  site    = Data_pitfall_transposed$Site,
  section = Data_pitfall_transposed$section,
  simpson = simpson_manual
)

print(div_df)

# site     section   simpson
#1   C1 Channelized 0.6909809
#2   C2 Channelized 0.7418599
#3   C3 Channelized 0.7380973
#4   C4 Channelized 0.7998788
#5   F1 Revitalized 0.7680000
#6   F2 Revitalized 0.5262105
#7   F3 Revitalized 0.6469238
#8   F4 Revitalized 0.5532218

# Boxplot
ggplot(div_df, aes(x = section, y = simpson, fill = section)) +
  geom_boxplot(alpha = 0.7, width = 0.5, outlier.colour = "red", outlier.size = 3) +
  geom_jitter(width = 0.1, size = 2) +
  scale_fill_manual(values = c("Channelized" = "#E07B54", "Revitalized" = "#4E9BB9")) +
  labs(title = "Invertebrate Simpson diversity index in each area",
       y = "Simpson index", x = NULL) +
  theme_bw(base_size = 13) +
  theme(legend.position = "none")

# Shapiro
shapiro.test(div_df$simpson[div_df$section == "Channelized"])
# p-value = 0.745
shapiro.test(div_df$simpson[div_df$section == "Revitalized"])
# p-value = 0.5479


t.test(simpson ~ section, data = div_df)
# p-value = 0.1142






######## VENN #############
# Familles présentes dans chaque section
invert_chan <- Data_pitfall_transposed %>%
  filter(section == "Channelized") %>%
  select(-Site, -section) %>%
  colSums() %>%
  .[. > 0] %>%
  names()

invert_rev <- Data_pitfall_transposed %>%
  filter(section == "Revitalized") %>%
  select(-Site, -section) %>%
  colSums() %>%
  .[. > 0] %>%
  names()

# Diagramme de Venn total
venn.diagram(
  x = list(Channelized = invert_chan, Revitalized = invert_rev),
  filename = NULL,
  fill = c("#E07B54", "#56B4E9"),
  alpha = 0.5,
  label.col = "black",
  cex = 2,
  fontface = "bold",
  fontfamily = "sans",
  cat.col = c("#E07B54", "#56B4E9"),
  cat.cex = 1.5,
  cat.fontface = "bold",
  cat.fontfamily = "sans",
  cat.pos = c(-20, 20),
  cat.dist = 0.05,
  main = "Invertebrate taxa presence by area",
  main.cex = 1,
  main.fontface = "bold",
  main.fontfamily = "sans"
) %>% grid::grid.draw()



library(tidyr)
# Regrouper par ordre
invert_orders <- Data_pitfall_transposed %>%
  pivot_longer(cols = -c(Site, section), names_to = "family", values_to = "abund") %>%
  mutate(order = case_when(
    grepl("^Col_", family)  ~ "Coleoptera",
    grepl("^Hym_", family)  ~ "Hymenoptera",
    grepl("^Dip_", family)  ~ "Diptera",
    grepl("^Hem_", family)  ~ "Hemiptera",
    grepl("^Ara_", family)  ~ "Arachnida",
    grepl("^Gas_", family)  ~ "Gastropoda",
    grepl("^Cru_", family)  ~ "Crustacea",
    grepl("^Neu_", family)  ~ "Neuroptera",
    grepl("^Lep_", family)  ~ "Lepidoptera",
    TRUE ~ family  # pour Collembola, Diplopoda, etc.
  )) %>%
  group_by(Site, section, order) %>%
  summarise(abund = sum(abund, na.rm = TRUE), .groups = "drop")

# Ordres présents par section
orders_chan <- invert_orders %>%
  filter(section == "Channelized", abund > 0) %>%
  pull(order) %>% unique()

orders_rev <- invert_orders %>%
  filter(section == "Revitalized", abund > 0) %>%
  pull(order) %>% unique()

# Diagramme de Venn
venn.diagram(
  x = list(Channelized = orders_chan, Revitalized = orders_rev),
  filename = NULL,
  fill = c("#E07B54", "#56B4E9"),
  alpha = 0.5,
  label.col = "black",
  cex = 2,
  fontface = "bold",
  fontfamily = "sans",
  cat.col = c("#E07B54", "#56B4E9"),
  cat.cex = 1.2,
  cat.fontface = "bold",
  cat.fontfamily = "sans",
  cat.pos = c(-20, 20),
  cat.dist = 0.05,
  main = "Invertebrate order presence by area",
  main.cex = 1,
  main.fontface = "bold",
  main.fontfamily = "sans"
) %>% grid::grid.draw()