# Quantile-Quantile (Q-Q) Normality Diagnostic Plot
# Tier 1 (Simple) - Diagnostics

make_plot <- function(data_dir = "data") {
  library(ggplot2)
  if (file.exists("theme_journal.R")) source("theme_journal.R", local = TRUE)
  
  df <- read.csv(file.path(data_dir, "xy.csv"))
  
  p <- ggplot(df, aes(sample = x)) +
    stat_qq(shape = 21, size = 2.4, fill = "#3C5488", color = "#222222", stroke = 0.35, alpha = 0.8) +
    stat_qq_line(color = "#E64B35", linewidth = 0.95, linetype = "solid") +
    labs(
      title = "Normal Quantile-Quantile (Q-Q) Diagnostic Plot",
      subtitle = "Points aligned along the 45-degree diagonal confirm consistency with a Gaussian distribution",
      x = "Theoretical Gaussian Quantiles",
      y = "Sample Empirical Quantiles",
      caption = "Source: Simulated standardized laboratory assay (N = 200)"
    )
  
  if (exists("theme_topjournal")) {
    p <- p + theme_topjournal(grid = "xy")
  } else {
    p <- p + theme_classic()
  }
  p
}

explain <- function() {
  list(
    title = "Normal Quantile-Quantile (Q-Q) Diagnostic Plot",
    how_to_read = "Empirical quantiles of the observed sample are plotted against the corresponding theoretical quantiles of a standard Gaussian distribution. Perfect alignment along the reference line indicates an exact normal distribution.",
    when_to_use = "Primary diagnostic tool for validating distributional assumptions before running parametric linear models, t-tests, or ANOVAs.",
    pitfalls = "Curvature at the extremities indicates heavy or light tails (kurtosis), while an S-shape or systematic curve indicates skewness. Outliers at the extremes require careful evaluation.",
    journal_archetype = "Statistical diagnostic panels in Nature Methods and Biometrika.",
    data_recipe = "Requires 1 numeric continuous variable."
  )
}
