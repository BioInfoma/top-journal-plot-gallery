# Raincloud Plot: Half-Violin, Boxplot, and Raw Replicates
# Tier 2 (Intermediate) - Distribution

make_plot <- function(data_dir = "data") {
  library(ggplot2)
  if (file.exists("theme_journal.R")) source("theme_journal.R", local = TRUE)
  
  df <- read.csv(file.path(data_dir, "grp.csv"))
  df$group <- factor(df$group, levels = c("Control", "DrugA", "DrugB", "DrugC"))
  
  # Base ggplot2 implementation of raincloud architecture
  # We construct smooth half-densities, offset boxplots, and jittered drops
  dens_list <- lapply(levels(df$group), function(g) {
    sub_v <- df$value[df$group == g]
    d <- density(sub_v)
    # Scale density height to max 0.35 offset
    d_norm <- d$y / max(d$y) * 0.35
    g_idx <- which(levels(df$group) == g)
    data.frame(
      group = g,
      x = d$x,
      y_base = g_idx,
      y_top = g_idx + d_norm
    )
  })
  dens_df <- do.call(rbind, dens_list)
  dens_df$group <- factor(dens_df$group, levels = levels(df$group))
  
  # Prepare jittered raw points below the baseline
  set.seed(42)
  df$group_num <- as.numeric(df$group)
  df$jitter_y <- df$group_num - 0.12 - runif(nrow(df), 0, 0.16)
  
  p <- ggplot() +
    # The Cloud: Half-density ribbon
    geom_ribbon(data = dens_df, aes(x = x, ymin = y_base, ymax = y_top, fill = group), alpha = 0.65, color = "#222222", linewidth = 0.35) +
    # The Raindrops: Jittered raw data points below
    geom_point(data = df, aes(x = value, y = jitter_y, fill = group), shape = 21, size = 1.9, stroke = 0.25, alpha = 0.6, color = "#222222") +
    # The Umbrella: Narrow horizontal boxplot
    geom_boxplot(data = df, aes(x = value, y = group_num - 0.05, fill = group), width = 0.08, outlier.shape = NA, alpha = 0.9, color = "#222222", linewidth = 0.45) +
    scale_y_continuous(
      breaks = 1:length(levels(df$group)),
      labels = levels(df$group),
      limits = c(0.6, length(levels(df$group)) + 0.5)
    ) +
    labs(
      title = "Raincloud Plot: Complete Distribution Transparency",
      subtitle = "Upper cloud shows probability density; narrow bar displays IQR/median; lower drops show raw replicates",
      x = "Measured Phenotype Score",
      y = "Experimental Cohort",
      caption = "Source: Simulated biological replicate assay (n = 30 per condition)"
    )
  
  if (exists("theme_topjournal")) {
    p <- p + theme_topjournal(grid = "x") + scale_fill_topjournal() + theme(legend.position = "none")
  } else {
    p <- p + theme_classic()
  }
  p
}

explain <- function() {
  list(
    title = "Multimodal Raincloud Plot",
    how_to_read = "Combines three complementary representations in a single compact row: an upper 'cloud' showing smoothed continuous probability density, a central 'umbrella' mini-boxplot marking median and quartiles, and lower 'raindrops' displaying every raw data point.",
    when_to_use = "Widely recognized by modern peer reviewers as the gold standard for continuous group comparisons, overcoming the limitations of bar and standard box plots.",
    pitfalls = "Maintain enough spacing between cohorts so the cloud densities do not collide with adjacent raindrops. Ensure the density bandwidth is appropriate for the sample size.",
    journal_archetype = "Wellcome Open Research and Nature Human Behaviour open science benchmarks.",
    data_recipe = "Requires 1 categorical factor with 2+ levels and 1 continuous numeric response variable."
  )
}
