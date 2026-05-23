# ============================================================
#  code_plantes.R
#  NMDS + diversity metrics — Plant communities
#  Sites: C1–C4 (Control) vs F1–F4 (Focal)
#  Data: presence (1) / absence (0) — abundance not available
# ============================================================

# ---- Packages ----
library(readxl)
library(tidyverse)
library(vegan)
library(ggplot2)
library(ggrepel)

# ---- Import & preparation ----
raw_plants <- read_excel("C:/Users/alois/OneDrive/Bureau/SEM 6/APP conservation/Data_plants_transposed.xlsx")

# Keep the 10 focal sites 
plant_comm <- raw_plants %>%
  filter(Site %in% c("C1", "C2", "C3", "C4","CE", "F1", "F2", "F3", "F4", "FE")) %>%
  column_to_rownames("Site") %>%
  mutate(across(everything(), as.numeric))

sites <- rownames(plant_comm)
type  <- factor(ifelse(grepl("^C", sites), "Channelized", "Revitalized"),
                levels = c("Channelized", "Revitalized"))
meta  <- data.frame(Site = sites, Type = type, row.names = sites)

# ---- NMDS — Plants ----
# Presence/absence data: use Jaccard distance directly (no Hellinger needed)
set.seed(42)
nmds_plants <- metaMDS(plant_comm, distance = "jaccard", binary = TRUE,
                       k = 2, trymax = 100)
cat("Stress NMDS plants:", nmds_plants$stress, "\n")

scores_plants <- as.data.frame(scores(nmds_plants, display = "sites")) %>%
  rownames_to_column("Site") %>%
  left_join(meta, by = "Site")

p_nmds_plants <- ggplot(scores_plants, aes(x = NMDS1, y = NMDS2,
                                           color = Type, fill = Type, label = Site)) +
  stat_ellipse(aes(group = Type), level = 0.8, alpha = 0.15, geom = "polygon") +
  geom_point(size = 4) +
  geom_text_repel(size = 4) +
  scale_color_manual(values = c("Channelized" = "#E07B54", "Revitalized" = "#4E9BB9")) +
  scale_fill_manual(values  = c("Channelized" = "#E07B54", "Revitalized" = "#4E9BB9")) +
  labs(
    title    = "NMDS — Plant communities",
    subtitle = paste0("stress = ",round(nmds_plants$stress, 3)),
    x        = "NMDS1",
    y        = "NMDS2",
    color    = "Area",
    fill     = "Area"
  ) +
  theme_bw()

print(p_nmds_plants)

# ---- PERMANOVA — Plants ----
# Tests whether plant community composition differs between Control and Focal
# Uses Jaccard distance on binary data (same as NMDS)
dist_plants <- vegdist(plant_comm, method = "jaccard", binary = TRUE)
set.seed(42)
perm_plants <- adonis2(dist_plants ~ Type, data = meta, permutations = 999)
cat("\n--- PERMANOVA — Plant communities ---\n")
print(perm_plants)

# Homogeneity of dispersions (betadisper)
bd_plants <- betadisper(dist_plants, meta$Type)
set.seed(42)
bd_plants_test <- permutest(bd_plants, permutations = 999)
cat("\n--- Betadisper (homogeneity of dispersion) — Plant communities ---\n")
print(bd_plants_test)

# ---- Diversity metrics table — Plants ----
# NOTE: Data is presence/absence only.
# - Abundance cannot be computed (no count data available).
# - Species richness = number of species present per site.
# - Simpson index is calculated on binary data (reflects evenness of
#   presence/absence, not true abundance-based diversity).

# Species richness
richness <- specnumber(plant_comm)

# Simpson index on presence/absence (interpretable as beta-diversity proxy)
simpson <- diversity(plant_comm, index = "simpson")

# Extract PERMANOVA p-value
perm_pval <- perm_plants["Model", "Pr(>F)"]

diversity_table <- data.frame(
  Site             = sites,
  Type             = type,
  Abundance        = "N/A (presence/absence only)",
  Richness         = richness,
  Simpson          = round(simpson, 3),
  PERMANOVA_pvalue = perm_pval
)

cat("\n--- Diversity metrics — Plant communities ---\n")
cat("Note: Abundance not available (presence/absence data).\n")
cat("      Simpson index computed on binary data.\n")
cat(paste0("PERMANOVA p-value (Control vs Focal): ", perm_pval,
           ifelse(perm_pval < 0.05, " *", " ns"), "\n\n"))
print(diversity_table)


# ---- Diversity metrics table — Plants (pooled Control vs Focal) ----
# All C sites merged into one, all F sites merged into one (sum of presences)
# A species is present in the pooled group if it appears in at least one site
perm_pval <- perm_plants["Model", "Pr(>F)"]

plant_pooled <- plant_comm %>%
  mutate(Type = type) %>%
  group_by(Type) %>%
  summarise(across(everything(), sum), .groups = "drop") %>%
  column_to_rownames("Type")

# Convert back to binary (presence in at least 1 site of the group = 1)
plant_pooled_bin <- decostand(plant_pooled, method = "pa")

diversity_table <- data.frame(
  Section          = rownames(plant_pooled_bin),
  Abundance        = "N/A (presence/absence only)",
  Richness         = specnumber(plant_pooled_bin),
  Simpson          = round(diversity(plant_pooled_bin, index = "simpson"), 3),
  PERMANOVA_pvalue = paste0(perm_pval, ifelse(perm_pval < 0.05, " *", " ns"))
)

cat("\n--- Diversity metrics — Plant communities (pooled Control vs Focal) ---\n")
cat("Note: Abundance not available. Simpson computed on binary data.\n\n")
print(diversity_table)

