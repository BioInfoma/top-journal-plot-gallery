# Ridgeline Plot (Joyplot) Across Multiple Groups
# Tier 1 (Simple) - Distribution

make_plot <- function(data_dir = "data") {
  library(ggplot2)
  if (file.exists("theme_journal.R")) source("theme_journal.R", local = TRUE)
  
  df <- read.csv(file.path(data_dir, "grp.csv"))
  df$group <- factor(df$group, levels = c("DrugC", "DrugB", "DrugA", "Control"))
  
  if (requireNamespace("ggridges", quietly = TRUE)) {
    p <- ggplot(df, aes(x = value, y = group, fill = group, color = group)) +
      ggridges::geom_density_ridges(
        scale = 1.4,
        alpha = 0.65,
        linewidth = 0.55,
        rel_min_height = 0.01,
        color = "#222222"
      ) +
      labs(
        title = "Ridgeline Density Stacks Across Experimental Arms",
        subtitle = "Partially overlapping density ridges reveal progressive shifts in distribution shape",
        x = "Measured Response Value",
        y = "Experimental Cohort",
        caption = "Source: Simulated multi-arm biological replicate assay"
      )
  } else {
    p <- ggplot(df, aes(x = value, fill = group)) +
      geom_density(alpha = 0.4) +
      facet_wrap(~ group, ncol = 1) +
      labs(title = "Density Facets (Install 'ggridges' for ridgeline overlay)", x = "Value", y = "Density")
  }
  
  if (exists("theme_topjournal")) {
    p <- p + theme_topjournal(grid = "x") + scale_fill_topjournal() + scale_color_topjournal() + theme(legend.position = "none")
  } else {
    p <- p + theme_classic()
  }
  p
}

explain <- function() {
  list(
    title = "Ridgeline Distribution Plot (Joyplot)",
    how_to_read = "Partially overlapping density curves are stacked vertically along the y-axis, providing a 3D-like perspective of distribution shifts across ordered or categorical series.",
    when_to_use = "Spectacular when comparing distributions across many groups (5 to 20 categories, conditions, or ordered time periods) where standard density curves would become an illegible tangle.",
    pitfalls = "High vertical overlap scaling (`scale > 2.0`) can hide the tails or entire peaks of lower curves. Choose a moderate overlap so all ridge contours remain legible.",
    journal_archetype = "Science reports on multi-year climate shifts, temporal cell cycle stages, or single-cell marker expression across dozens of cell types.",
    data_recipe = "Requires 1 continuous measurement column and 1 categorical factor with 4+ levels."
  )
}
