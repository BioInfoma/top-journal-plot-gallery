# Violin Plot with Embedded Mini-Boxplot
# Tier 1 (Simple) - Distribution

make_plot <- function(data_dir = "data") {
  library(ggplot2)
  if (file.exists("theme_journal.R")) source("theme_journal.R", local = TRUE)
  
  df <- read.csv(file.path(data_dir, "grp.csv"))
  df$group <- factor(df$group, levels = c("Control", "DrugA", "DrugB", "DrugC"))
  
  p <- ggplot(df, aes(x = group, y = value, fill = group)) +
    geom_violin(trim = FALSE, alpha = 0.72, color = "#222222", linewidth = 0.4) +
    geom_boxplot(width = 0.14, fill = "#FFFFFF", color = "#222222", outlier.shape = NA, linewidth = 0.5) +
    stat_summary(fun = median, geom = "point", shape = 21, size = 2.2, fill = "#FFFFFF", color = "#222222") +
    labs(
      title = "Violin Density Profiles with Embedded Quartile Boxplots",
      subtitle = "Contour width mirrors full kernel density; central box displays IQR and median",
      x = "Experimental Condition",
      y = "Normalized Expression Level",
      caption = "Source: Simulated biological replicate cohort (n = 30 per group)"
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
    title = "Violin Plot with Embedded Boxplot",
    how_to_read = "The outer curvature shows a mirrored kernel density estimate, where wider sections denote higher data concentration. The nested white boxplot provides precise nonparametric summary statistics (median point, IQR box, and range).",
    when_to_use = "Preferred over simple boxplots when cohorts have adequate sample sizes (n >= 30) and you want to detect multimodality, heavy tails, or asymmetrical distributions.",
    pitfalls = "Violin plots can mislead on very small samples (n < 15) by generating smoothed contours where data is actually sparse. In those cases, use jitter plots or strip charts instead.",
    journal_archetype = "Nature Biotechnology single-cell and high-throughput distribution panels.",
    data_recipe = "Requires 1 categorical condition factor and 1 continuous numeric response."
  )
}
