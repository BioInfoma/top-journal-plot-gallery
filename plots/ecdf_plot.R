# Empirical Cumulative Distribution Function (ECDF)
# Tier 2 (Intermediate) - Distribution

make_plot <- function(data_dir = "data") {
  library(ggplot2)
  if (file.exists("theme_journal.R")) source("theme_journal.R", local = TRUE)
  
  df <- read.csv(file.path(data_dir, "xy.csv"))
  
  p <- ggplot(df, aes(x = y, color = group)) +
    stat_ecdf(geom = "step", linewidth = 1.05, alpha = 0.9) +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0, 1.02)) +
    labs(
      title = "Empirical Cumulative Distribution Function (ECDF)",
      subtitle = "Fraction of observations less than or equal to x, comparing cohort distributions without binning",
      x = "Phenotypic Response Value (Y)",
      y = "Cumulative Probability F(y)",
      color = "Cohort",
      caption = "Source: Simulated continuous phenotypic assay (N = 200)"
    )
  
  if (exists("theme_topjournal")) {
    p <- p + theme_topjournal(grid = "xy") + scale_color_topjournal()
  } else {
    p <- p + theme_classic()
  }
  p
}

explain <- function() {
  list(
    title = "Empirical Cumulative Distribution Function (ECDF)",
    how_to_read = "Tracks the cumulative proportion of sample observations that fall at or below each value along the continuous horizontal axis. Curves that sit further to the right represent cohorts with systematically higher values.",
    when_to_use = "Superior to histograms and density plots because it requires zero arbitrary parameters (no bin widths, no bandwidth smoothing) and preserves 100% of sample information.",
    pitfalls = "Subtle bimodal features can be harder to spot as inflection points along an S-curve compared to direct peaks on a violin or density plot.",
    journal_archetype = "Physical Review Letters and Biometrika rigorous non-parametric comparison standards.",
    data_recipe = "Requires continuous numeric measurements across experimental groups."
  )
}
