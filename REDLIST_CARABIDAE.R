# ============================================================
#  redlist_carabids.R
#  Red-listed Carabidae — richness comparison Control vs Focal
#  Conservation statuses: LC = Least Concern | NT = Near Threatened
#                         VU = Vulnerable    | EN = Endangered
#  Red-listed species = NT + VU + EN (all non-LC)
#
#  Visualizations:
#    1. Venn diagram (2 circles) with species annotated by status
#    2. UpSet plot with species colored by status
#    3. Dot plot — all species x sections, colored by status
# ============================================================

# ---- Packages ----
library(readxl)
library(tidyverse)
library(ggplot2)
library(ggvenn)       # install.packages("ggvenn") if needed
library(ggupset)      # install.packages("ggupset") if needed
library(ggrepel)

# ---- Color palette for conservation status ----
status_colors <- c(
  "LC" = "#6BAE75",   # green
  "NT" = "#F0C040",   # yellow
  "VU" = "#E07B54"    # orange-red
)

# ---- Import data ----
cara_raw <- read_excel("Carabidae.xlsx")

# Extract conservation status (first row)
statut_row <- cara_raw %>% filter(.[[1]] == "statut")
species    <- names(cara_raw)[-1]
statuts    <- as.character(statut_row[1, -1])
statut_df  <- data.frame(Species = species, Status = statuts,
                         stringsAsFactors = FALSE)

# Community matrix (sites x species)
cara_comm <- cara_raw %>%
  filter(.[[1]] != "statut") %>%
  column_to_rownames(names(cara_raw)[1]) %>%
  mutate(across(everything(), as.numeric))

sites <- rownames(cara_comm)
type  <- factor(ifelse(grepl("^C", sites), "Control", "Focal"),
                levels = c("Control", "Focal"))

# ---- Presence per group (all species) ----
cara_control_all <- cara_comm[grepl("^C", sites), , drop = FALSE]
cara_focal_all   <- cara_comm[grepl("^F", sites), , drop = FALSE]

present_control_all <- species[colSums(cara_control_all) > 0]
present_focal_all   <- species[colSums(cara_focal_all)   > 0]

# ---- Red-listed species ----
redlist_species <- statut_df %>% filter(Status != "LC") %>% pull(Species)

cat("Red-listed species:\n")
print(statut_df %>% filter(Status != "LC"))

# ---- Wilcoxon test on red-listed richness per site ----
richness_rl <- data.frame(
  Site     = sites,
  Type     = type,
  Richness = rowSums(cara_comm[, redlist_species, drop = FALSE] > 0)
)

richness_control <- richness_rl %>% filter(Type == "Control") %>% pull(Richness)
richness_focal   <- richness_rl %>% filter(Type == "Focal")   %>% pull(Richness)

wilcox_test <- wilcox.test(richness_control, richness_focal,
                           exact = FALSE, alternative = "two.sided")
wpval <- round(wilcox_test$p.value, 3)
wsig  <- ifelse(wilcox_test$p.value < 0.05, " *", " ns")

cat("\n--- Wilcoxon test — Red-listed richness ---\n")
cat(paste0("W = ", wilcox_test$statistic, " | p = ", wpval, wsig, "\n"))



# ==============================================================
# VISUALIZATION — Combined: custom Venn + UpSet barplot side by side
# ==============================================================
library(patchwork)
library(ggforce)   # install.packages("ggforce") if needed

# ---- Compute zone membership for all species ----
zone_df <- statut_df %>%
  mutate(
    In_Control = Species %in% present_control_all,
    In_Focal   = Species %in% present_focal_all,
    Zone = case_when(
      In_Control & !In_Focal ~ "Channelized only",
      !In_Control & In_Focal ~ "Revitalized only",
      In_Control & In_Focal  ~ "Shared",
      TRUE                   ~ "Absent"
    )
  ) %>%
  filter(Zone != "Absent")

n_ctrl   <- sum(zone_df$Zone == "Channelized only")
n_focal  <- sum(zone_df$Zone == "Revitalized only")
n_shared <- sum(zone_df$Zone == "Shared")

# ---- Panel A: custom Venn (counts only, no percentages) ----
venn_circles <- data.frame(
  x    = c(-1,  1),
  y    = c( 0,  0),
  r    = c( 2,  2),
  fill = c("#E07B54", "#4E9BB9"),
  label = c("Chanelized", "Revitalized")
)

# Red-listed counts per zone
rl_ctrl   <- zone_df %>% filter(Zone == "Channelized only", Status != "LC") %>% nrow()
rl_focal  <- zone_df %>% filter(Zone == "Revitalized only",   Status != "LC") %>% nrow()
rl_shared <- zone_df %>% filter(Zone == "Shared",       Status != "LC") %>% nrow()

p_venn2 <- ggplot() +
  # Circles
  geom_circle(data = venn_circles,
              aes(x0 = x, y0 = y, r = r, fill = fill),
              alpha = 0.25, color = NA, inherit.aes = FALSE) +
  geom_circle(data = venn_circles,
              aes(x0 = x, y0 = y, r = r, color = fill),
              fill = NA, linewidth = 1.2, inherit.aes = FALSE) +
  scale_fill_identity() +
  scale_color_identity() +
  # Zone labels (section names)
  annotate("text", x = -2.2, y =  2.2, label = "Channelized",
           color = "orange", fontface = "bold", size = 5) +
  annotate("text", x =  2.2, y =  2.2, label = "Revitalized",
           color = "blue", fontface = "bold", size = 5) +
  # Species counts per zone
  annotate("text", x = -1.5, y = 0,
           label = paste0(n_ctrl),
           size = 4, color = "grey20") +
  annotate("text", x =  1.5, y = 0,
           label = paste0(n_focal),
           size = 4, color = "grey20") +
  annotate("text", x = 0, y = 0,
           label = paste0(n_shared),
           size = 4, color = "grey20") +
  coord_fixed(xlim = c(-4, 4), ylim = c(-2.5, 2.5)) +
  labs(title = "A — Species overlap") +
  theme_void() +
  theme(plot.title = element_text(face = "bold", size = 12, hjust = 0.5))

# ---- Panel B: UpSet-style barplot with zone labels ----
bar_df <- zone_df %>%
  mutate(
    Zone = factor(Zone, levels = c("Channelized only", "Shared", "Revitalized only")),
    Status = factor(Status, levels = c("VU", "NT", "LC"))
  )

# Label for x axis showing zone + which sites
zone_labels <- c(
  "Channelized only" = "Channelized only\n(C1–C4)",
  "Shared"       = "Shared\n(both)",
  "Revitalized only"   = "Revitalized only\n(F1–F4)"
)

p_bar2 <- ggplot(bar_df, aes(x = Zone, fill = Status)) +
  geom_bar(color = "black", width = 0.55) +
  geom_text(stat = "count", aes(label = after_stat(count)),
            position = position_stack(vjust = 1.08),
            size = 4, fontface = "bold", color = "grey20") +
  scale_fill_manual(values = status_colors,
                    name   = "Conservation\nstatus",
                    labels = c("VU — Vulnerable",
                               "NT — Near Threatened",
                               "LC — Least Concern")) +
  scale_x_discrete(labels = zone_labels) +
  labs(
    title = "B — Species count by zone and status",
    x     = NULL,
    y     = "Number of species"
  ) +
  theme_bw() +
  theme(
    plot.title      = element_text(face = "bold", size = 12),
    axis.text.x     = element_text(size = 10, color = "grey20"),
    legend.position = "right"
  )

# ---- Combine with patchwork ----
p_combined <- p_venn2 + p_bar2 +
  plot_layout(widths = c(1, 1)) +
  plot_annotation(
    title    = "Carabidae — Species distribution across sections",
    theme    = theme(
      plot.title    = element_text(face = "bold", size = 14, hjust = 0.5),
      plot.subtitle = element_text(size = 11, hjust = 0.5, color = "grey40")
    )
  )
print(p_combined)

# ---- Summary table ----
cat("\n--- Red-listed richness per site ---\n")
print(richness_rl)

cat(paste0("\nWilcoxon rank-sum test (red-listed richness Control vs Focal):\n",
           "W = ", wilcox_test$statistic,
           " | p-value = ", wpval, wsig, "\n"))

