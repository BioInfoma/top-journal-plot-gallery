# Three-Variable Scientific Bubble Chart
# Tier 1 (Simple) - Relationship

make_plot <- function(data_dir = "data") {
  library(ggplot2)
  if (file.exists("theme_journal.R")) source("theme_journal.R", local = TRUE)
  
  df <- read.csv(file.path(data_dir, "xy.csv"))
  
  p <- ggplot(df, aes(x = x, y = y, size = size, fill = group)) +
    geom_point(shape = 21, alpha = 0.72, stroke = 0.35, color = "#222222") +
    scale_size_continuous(range = c(2.5, 9.5), name = "Replicate Weight") +
    labs(
      title = "Multidimensional Phenotype Mapping",
      subtitle = "Point coordinates encode continuous predictors; disc area mirrors replicate weight",
      x = "Normalized Predictor (X)",
      y = "Normalized Response (Y)",
      fill = "Cohort",
      caption = "Source: Simulated multi-parameter screening dataset (N = 200)"
    )
  
  if (exists("theme_topjournal")) {
    p <- p + theme_topjournal(grid = "xy") + scale_fill_topjournal()
  } else {
    p <- p + theme_classic()
  }
  p
}

explain <- function() {
  list(
    title = "Multivariate Bubble Chart",
    how_to_read = "Encodes three continuous variables simultaneously: horizontal position (X), vertical position (Y), and circle area (size), alongside a categorical variable mapped to fill color.",
    when_to_use = "Valuable for screening experiments, epidemiology (incidence vs mortality vs population size), and gene expression metrics (fold change vs significance vs base expression).",
    pitfalls = "Circle sizes must be mapped strictly to circle area, not circle radius/diameter, to avoid squaring human visual perception of magnitude. High point overlap can obscure smaller underlying bubbles.",
    journal_archetype = "Science and Global Burden of Disease multidimensional health metrics.",
    data_recipe = "Requires 3 continuous numeric columns (X, Y, Size) and 1 categorical factor."
  )
}
