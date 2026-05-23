# ============================================================
#  code_carabidae.R
#  NMDS + diversity metrics — Carabidae (pitfall traps)
#  Sites: C1–C4 (Control) vs F1–F4 (Focal)
# ============================================================

# ---- Packages ----
library(readxl)
library(tidyverse)
library(vegan)
library(ggplot2)
library(ggrepel)

# ---- Import & preparation ----

# Carabidae
cara_raw  <- read_excel("C:/Users/alois/OneDrive/Bureau/SEM 6/APP conservation/Carabidae.xlsx")
cara_comm <- cara_raw %>%
  filter(.[[1]] != "statut") %>%
  column_to_rownames(names(cara_raw)[1]) %>%
  mutate(across(everything(), as.numeric))
cara_sites <- rownames(cara_comm)
cara_comm <- cara_comm[cara_sites, ]


cara_type <- factor(ifelse(grepl("^C", cara_sites), "Channelized", "Revitalized"),
               levels = c("Channelized", "Revitalized"))
cara_meta <- data.frame(Site = cara_sites, Type = cara_type, row.names = cara_sites)


# ---- NMDS — Carabidae ----
cara_hel <- decostand(cara_comm, method = "hellinger")
set.seed(42)
nmds_cara <- metaMDS(cara_hel, distance = "bray", k = 2, trymax = 100)
cat("Stress NMDS Carabidae:", nmds_cara$stress, "\n")

scores_cara <- as.data.frame(scores(nmds_cara, display = "sites")) %>%
  rownames_to_column("Site") %>%
  left_join(cara_meta, by = "Site")

p_nmds_cara <- ggplot(scores_cara, aes(x = NMDS1, y = NMDS2,
                                       color = Type, fill = Type, label = Site)) +
  stat_ellipse(aes(group = Type), level = 0.8, alpha = 0.15, geom = "polygon") +
  geom_point(size = 4) +
  geom_text_repel(size = 4) +
  scale_color_manual(values = c("Channelized" = "#E07B54", "Revitalized" = "#4E9BB9")) +
  scale_fill_manual(values  = c("Channelized" = "#E07B54", "Revitalized" = "#4E9BB9")) +
  labs(
    title    = "NMDS — Carabidae",
    subtitle = paste0("stress = ",round(nmds_cara$stress, 3)),
    x        = "NMDS1",
    y        = "NMDS2",
    color    = "Area",
    fill     = "Area"
  ) +
  theme_bw()

print(p_nmds_cara)

# ---- PERMANOVA — Carabidae ----
# Tests whether Carabidae community composition differs between Control and Focal
dist_cara <- vegdist(cara_hel, method = "bray")
set.seed(42)
perm_cara <- adonis2(dist_cara ~ Type, data = cara_meta, permutations = 999)
cat("\n--- PERMANOVA — Carabidae ---\n")
print(perm_cara)

# Homogeneity of dispersions (betadisper)
bd_cara <- betadisper(dist_cara, cara_meta$Type)
set.seed(42)
bd_cara_test <- permutest(bd_cara, permutations = 999)
cat("\n--- Betadisper (homogeneity of dispersion) — Carabidae ---\n")
print(bd_cara_test)

# ---- Diversity metrics table — Carabidae ----
# Total abundance per site
abundance <- rowSums(cara_comm)

# Species richness (number of Carabidae species with at least 1 individual)
richness <- specnumber(cara_comm)

# Simpson diversity index (1 - D)
simpson <- diversity(cara_comm, index = "simpson")

# Extract PERMANOVA p-value
perm_pval <- perm_cara["Model", "Pr(>F)"]

diversity_table <- data.frame(
  Site             = cara_sites,
  Type             = cara_type,
  Abundance        = abundance,
  Richness         = richness,
  Simpson          = round(simpson, 3),
  PERMANOVA_pvalue = perm_pval
)

cat("\n--- Diversity metrics — Carabidae ---\n")
cat(paste0("PERMANOVA p-value (Control vs Focal): ", perm_pval,
           ifelse(perm_pval < 0.05, " *", " ns"), "\n\n"))
print(diversity_table)


# ---- Diversity metrics table — Carabidae (pooled Control vs Focal) ----
# All C sites merged into one, all F sites merged into one
perm_pval <- perm_cara["Model", "Pr(>F)"]

cara_pooled <- cara_comm %>%
  mutate(Type = cara_type) %>%
  group_by(Type) %>%
  summarise(across(everything(), sum), .groups = "drop") %>%
  column_to_rownames("Type")

diversity_table <- data.frame(
  Section          = rownames(cara_pooled),
  Total_Abundance  = rowSums(cara_pooled),
  Richness         = specnumber(cara_pooled),
  Simpson          = round(diversity(cara_pooled, index = "simpson"), 3),
  PERMANOVA_pvalue = paste0(perm_pval, ifelse(perm_pval < 0.05, " *", " ns"))
)

cat("\n--- Diversity metrics — Carabidae (pooled Control vs Focal) ---\n")
print(diversity_table)

