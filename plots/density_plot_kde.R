# Kernel Density Estimate (KDE) Comparison
# Tier 1 (Simple) - Distribution

make_plot <- function(data_dir = "data") {
  library(ggplot2)
  if (file.exists("theme_journal.R")) source("theme_journal.R", local = TRUE)
  
  df <- read.csv(file.path(data_dir, "xy.csv"))
  
  p <- ggplot(df, aes(x = y, fill = group, color = group)) +
    geom_density(alpha = 0.35, linewidth = 0.9) +
    labs(
      title = "Kernel Density Estimation Across Biological Cohorts",
      subtitle = "Smoothed probability densities reveal shift in central tendency and skewness",
      x = "Phenotypic Response Metric",
      y = "Estimated Probability Density",
      color = "Cohort",
      fill = "Cohort",
      caption = "Source: Simulated continuous phenotypic assay (N = 200)"
    )
  
  if (exists("theme_topjournal")) {
    p <- p + theme_topjournal(grid = "y") + scale_color_topjournal() + scale_fill_topjournal()
  } else {
    p <- p + theme_classic()
  }
  p
}

explain <- function() {
  list(
    title = "Comparative Kernel Density (KDE) Plot",
    how_to_read = "Continuous smoothed curves trace the estimated probability density function of each group along a shared numeric axis. Overlapping shaded areas highlight shared density versus distinct cohort shifts.",
    when_to_use = "Superb for comparing continuous distributions across 2 to 4 groups where histograms would result in cluttered overlapping bars.",
    pitfalls = "Kernel bandwidth selection strongly governs smoothness: oversmoothing can mask true secondary peaks (bimodality), while undersmoothing introduces spurious noise peaks.",
    journal_archetype = "Nature Communications comparative population genomics distributions.",
    data_recipe = "Requires 1 continuous numeric response variable and 1 categorical grouping factor."
  )
}
