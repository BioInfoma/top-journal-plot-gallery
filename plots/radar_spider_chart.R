# Radar / Spider Chart for Multi-Parametric Profiling
# Tier 2 (Intermediate) - Multivariate

make_plot <- function(data_dir = "data") {
  library(ggplot2)
  if (file.exists("theme_journal.R")) source("theme_journal.R", local = TRUE)
  
  # Simulate 6 standardized phenotypic dimensions across 3 experimental arms
  categories <- c("Metabolic Rate", "Immune Activation", "Cytotoxicity", "Bioavailability", "Clearance", "Target Affinity")
  n_cats <- length(categories)
  
  df_radar <- data.frame(
    category = rep(categories, 3),
    arm = rep(c("Control", "Lead Compound A", "Lead Compound B"), each = n_cats),
    score = c(
      45, 30, 20, 50, 60, 40,   # Control
      85, 75, 90, 70, 45, 95,   # Compound A
      60, 90, 65, 85, 80, 70    # Compound B
    )
  )
  df_radar$category <- factor(df_radar$category, levels = categories)
  
  # Close polygon loops
  df_closed <- do.call(rbind, lapply(unique(df_radar$arm), function(a) {
    sub_d <- df_radar[df_radar$arm == a, ]
    rbind(sub_d, sub_d[1, ])
  }))
  
  p <- ggplot(df_closed, aes(x = category, y = score, group = arm, color = arm, fill = arm)) +
    geom_polygon(alpha = 0.22, linewidth = 0.95) +
    geom_point(data = df_radar, aes(x = category, y = score, color = arm), size = 3.2, stroke = 0.5) +
    coord_polar() +
    scale_y_continuous(limits = c(0, 100), breaks = seq(20, 100, by = 20)) +
    scale_color_manual(values = c("Control" = "#64748B", "Lead Compound A" = "#E64B35", "Lead Compound B" = "#3C5488")) +
    scale_fill_manual(values = c("Control" = "#64748B", "Lead Compound A" = "#E64B35", "Lead Compound B" = "#3C5488")) +
    labs(
      title = "Multi-Parameter Radar Profile: Compound Screening Fingerprint",
      subtitle = "Standardized 0-100 score polygon across six key pharmacological axes",
      color = "Cohort / Candidate",
      fill = "Cohort / Candidate",
      caption = "Source: Simulated lead optimization pharmacology panel"
    )
  
  if (exists("theme_topjournal")) {
    p <- p + theme_topjournal(grid = "y") +
      theme(
        axis.text.y = element_text(size = 7.5, color = "#64748B"),
        axis.text.x = element_text(size = 9.5, face = "bold", color = "#1E293B"),
        axis.title = element_blank(),
        axis.line = element_blank(),
        legend.position = "top"
      )
  } else {
    p <- p + theme_minimal()
  }
  p
}

explain <- function() {
  list(
    title = "Multivariate Radar (Spider) Chart",
    how_to_read = "Several quantitative axes radiate outward from a central pole. Connected colored vertices create a closed polygon that captures the multi-parametric fingerprint of each test condition.",
    when_to_use = "Effective for comparing multi-attribute drug candidate profiles (potency, selectivity, toxicity, solubility) or complex multi-domain phenotyping scores.",
    pitfalls = "Radial ordering of variables can distort perceived polygon area (different permutations produce different shapes). Always ensure axes share standardized scaling.",
    journal_archetype = "Nature Chemical Biology and Science Translational Medicine pharmacological candidate profiles.",
    data_recipe = "Requires multiple continuous metrics across 2-4 comparative groups."
  )
}
