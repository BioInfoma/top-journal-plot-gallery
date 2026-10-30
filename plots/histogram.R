# Publication Histogram with Density Overlay
# Tier 1 (Simple) - Distribution

make_plot <- function(data_dir = "data") {
  library(ggplot2)
  if (file.exists("theme_journal.R")) source("theme_journal.R", local = TRUE)
  
  df <- read.csv(file.path(data_dir, "xy.csv"))
  
  p <- ggplot(df, aes(x = x)) +
    geom_histogram(aes(y = after_stat(density)), bins = 24, fill = "#4DBBD5", color = "#222222", linewidth = 0.35, alpha = 0.75) +
    geom_density(color = "#E64B35", linewidth = 1.05) +
    labs(
      title = "Empirical Distribution of Standardized Biomarker Assay",
      subtitle = "Binned histogram overlaid with smooth kernel density estimate curve",
      x = "Standardized Value (z-score)",
      y = "Probability Density",
      caption = "Source: Simulated single-variable continuous assay (N = 200)"
    )
  
  if (exists("theme_topjournal")) {
    p <- p + theme_topjournal(grid = "y")
  } else {
    p <- p + theme_classic()
  }
  p
}

explain <- function() {
  list(
    title = "Binned Histogram with Density Overlay",
    how_to_read = "The continuous x-axis is partitioned into equal-width bins, where vertical bar heights represent the probability density of observations within that interval. The overlaid red smooth curve provides a continuous kernel density estimate (KDE).",
    when_to_use = "Essential first step in exploratory data analysis to evaluate unimodality vs multimodality, skewness, outliers, and deviations from normality.",
    pitfalls = "Histogram appearance is heavily sensitive to the chosen bin width and bin origin. Always inspect multiple bin settings or verify using an overlaid continuous density or Q-Q plot.",
    journal_archetype = "Nature Methods baseline reporting for empirical quantitative distributions.",
    data_recipe = "Requires 1 numeric continuous variable."
  )
}
