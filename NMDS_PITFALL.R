# ============================================================
#  code_pitfall.R
#  NMDS + diversity metrics — All arthropod families (pitfall traps)
#  Sites: C1–C4 (Control) vs F1–F4 (Focal)
# ============================================================

# ---- Packages ----
library(readxl)
library(tidyverse)
library(vegan)
library(ggplot2)
library(ggrepel)

# ---- Import & preparation ----
raw  <- read_excel("C:/Users/alois/OneDrive/Bureau/SEM 6/APP conservation/Data_pitfall_transposed.xlsx")
comm <- raw %>% column_to_rownames("Site")

sites <- rownames(comm)
type  <- factor(ifelse(grepl("^C", sites), "Channelized", "Revitalized"),
                levels = c("Channelized", "Revitalized"))
meta  <- data.frame(Site = sites, Type = type, row.names = sites)

# ---- NMDS — All families ----
comm_hel <- decostand(comm, method = "hellinger")
set.seed(42)
nmds_all <- metaMDS(comm_hel, distance = "bray", k = 2, trymax = 100)
cat("Stress NMDS all families:", nmds_all$stress, "\n")

scores_all <- as.data.frame(scores(nmds_all, display = "sites")) %>%
  rownames_to_column("Site") %>%
  left_join(meta, by = "Site")

p_nmds_all <- ggplot(scores_all, aes(x = NMDS1, y = NMDS2,
                                     color = Type, fill = Type, label = Site)) +
  stat_ellipse(aes(group = Type), level = 0.8, alpha = 0.15, geom = "polygon") +
  geom_point(size = 4) +
  geom_text_repel(size = 4) +
  scale_color_manual(values = c("Channelized" = "#E07B54", "Revitalized" = "#4E9BB9")) +
  scale_fill_manual(values  = c("Channelized" = "#E07B54", "Revitalized" = "#4E9BB9")) +
  labs(
    title    = "NMDS — invertebrates communities",
    subtitle = paste0("stress = ",round(nmds_all$stress, 3)),
    x        = "NMDS1",
    y        = "NMDS2",
    color    = "Area",
    fill     = "Area"
  ) +
  theme_bw()

print(p_nmds_all)

# ---- PERMANOVA — All families ----
# Tests whether community composition differs significantly between Control and Focal
# Uses the same Bray-Curtis distance on Hellinger-transformed data as the NMDS
dist_all <- vegdist(comm_hel, method = "bray")
set.seed(42)
perm_all <- adonis2(dist_all ~ Type, data = meta, permutations = 999)
cat("\n--- PERMANOVA — All arthropod families ---\n")
print(perm_all)

# Homogeneity of dispersions (betadisper) — checks PERMANOVA assumption
# A significant result here means groups differ in variability, not just composition
bd_all <- betadisper(dist_all, meta$Type)
set.seed(42)
bd_all_test <- permutest(bd_all, permutations = 999)
cat("\n--- Betadisper (homogeneity of dispersion) — All arthropod families ---\n")
print(bd_all_test)

# ---- Diversity metrics table — All families ----
# Total abundance per site
abundance <- rowSums(comm)

# Species richness (number of families with at least 1 individual)
richness <- specnumber(comm)

# Simpson diversity index (1 - D)
simpson <- diversity(comm, index = "simpson")

# Extract PERMANOVA p-value (row "Type", column "Pr(>F)")
perm_pval <- perm_all["Model", "Pr(>F)"]

diversity_table <- data.frame(
  Site             = sites,
  Type             = type,
  Abundance        = abundance,
  Richness         = richness,
  Simpson          = round(simpson, 3),
  PERMANOVA_pvalue = perm_pval   # same value repeated per site for reference
)

cat("\n--- Diversity metrics — All arthropod families ---\n")
cat(paste0("PERMANOVA p-value (Control vs Focal): ", perm_pval,
           ifelse(perm_pval < 0.05, " *", " ns"), "\n\n"))
print(diversity_table)


# ---- Diversity metrics table — All families (pooled Control vs Focal) ----
# All C sites merged into one, all F sites merged into one
abundance <- rowSums(comm)
richness  <- specnumber(comm)
simpson   <- diversity(comm, index = "simpson")
perm_pval <- perm_all["Model", "Pr(>F)"]

site_df <- data.frame(Type = type, Abundance = abundance,
                      Richness = richness, Simpson = simpson)

# Pool raw counts then recompute metrics on the merged community
comm_pooled <- comm %>%
  mutate(Type = type) %>%
  group_by(Type) %>%
  summarise(across(everything(), sum), .groups = "drop") %>%
  column_to_rownames("Type")

diversity_table <- data.frame(
  Section          = rownames(comm_pooled),
  Total_Abundance  = rowSums(comm_pooled),
  Richness         = specnumber(comm_pooled),
  Simpson          = round(diversity(comm_pooled, index = "simpson"), 3),
  PERMANOVA_pvalue = paste0(perm_pval, ifelse(perm_pval < 0.05, " *", " ns"))
)

cat("\n--- Diversity metrics — All arthropod families (pooled Control vs Focal) ---\n")
print(diversity_table)

