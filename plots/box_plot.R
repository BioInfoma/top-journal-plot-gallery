# Publication Box Plot with Jittered Data Points
# Tier 1 (Simple) - Distribution

make_plot <- function(data_dir = "data") {
  library(ggplot2)
  if (file.exists("theme_journal.R")) source("theme_journal.R", local = TRUE)
  
  df <- read.csv(file.path(data_dir, "grp.csv"))
  df$group <- factor(df$group, levels = c("Control", "DrugA", "DrugB", "DrugC"))
  
  p <- ggplot(df, aes(x = group, y = value, fill = group)) +
    geom_boxplot(width = 0.5, outlier.shape = NA, alpha = 0.75, color = "#222222", linewidth = 0.45) +
    geom_jitter(width = 0.18, size = 1.8, alpha = 0.55, shape = 21, stroke = 0.25, color = "#222222") +
    labs(
      title = "Tukey Boxplot with Individual Replicate Overlays",
      subtitle = "Solid bar denotes median; box spans IQR (25th–75th percentiles); whiskers show 1.5× IQR",
      x = "Experimental Cohort",
      y = "Quantitative Assay Output",
      caption = "Source: Simulated multi-arm bioassay dataset (n = 30 per group)"
    )
  
  if (exists("theme_topjournal")) {
    p <- p + theme_topjournal(grid = "y") + scale_fill_topjournal() + theme(legend.position = "none")
  } else {
    p <- p + theme_classic()
  }
  p
}

explain <- function() {
  list(
    title = "Tukey Box Plot with Raw Data Overlays",
    how_to_read = "The central horizontal line marks the median (50th percentile). The box limits show the Interquartile Range (IQR, 25th to 75th percentiles), and whiskers extend to the furthest non-outlier points within 1.5× IQR. Overlaid jittered points reveal the underlying sample size and exact spread.",
    when_to_use = "Standard publication choice when comparing continuous distributions across discrete groups without assuming normality.",
    pitfalls = "Never omit raw points when sample sizes are small (n < 50), because boxplots can completely hide bimodal or clustered sub-distributions.",
    journal_archetype = "Science & Cell guidelines on transparent data reporting.",
    data_recipe = "Requires 1 categorical group factor and 1 continuous response variable."
  )
}
