# Scatter Plot with Trend Lines and Marginal Confidence
# Tier 1 (Simple) - Relationship

make_plot <- function(data_dir = "data") {
  library(ggplot2)
  if (file.exists("theme_journal.R")) source("theme_journal.R", local = TRUE)
  
  df <- read.csv(file.path(data_dir, "xy.csv"))
  
  p <- ggplot(df, aes(x = x, y = y, color = group, fill = group)) +
    geom_point(alpha = 0.7, size = 2.8, shape = 21, stroke = 0.35, color = "#222222") +
    geom_smooth(method = "lm", formula = y ~ x, se = TRUE, alpha = 0.15, linewidth = 0.85) +
    labs(
      title = "Bivariate Association Across Experimental Cohorts",
      subtitle = "Points show individual replicates; shaded bands represent 95% confidence intervals",
      x = "Standardized Predictor (X)",
      y = "Normalized Phenotypic Response (Y)",
      color = "Cohort",
      fill = "Cohort",
      caption = "Source: Simulated cohort trial dataset (N = 200)"
    )
  
  if (exists("theme_topjournal")) {
    p <- p + theme_topjournal(grid = "xy") + scale_fill_topjournal() + scale_color_topjournal()
  } else {
    p <- p + theme_classic()
  }
  p
}

explain <- function() {
  list(
    title = "Scatter Plot with Linear Fit",
    how_to_read = "Each point maps a single biological or physical observation across two continuous axes. Fitted linear regression lines indicate the direction and strength of the correlation, while the surrounding shaded envelope depicts the 95% confidence interval of the mean response.",
    when_to_use = "Use when examining relationships, potential confounding factors, or clusters between two continuous variables across discrete experimental groups.",
    pitfalls = "Beware of overplotting in dense datasets (use alpha blending or hexagonal binning). Avoid extrapolating trend lines beyond the observed data range, and verify that non-linear relationships are not forced into linear models.",
    journal_archetype = "Nature Methods standards for bivariate correlation and model regression reporting.",
    data_recipe = "Requires 2 continuous numeric columns (X, Y) and 1 categorical factor column (Cohort/Group)."
  )
}
