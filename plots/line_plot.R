# Line Plot with Temporal Trajectories
# Tier 1 (Simple) - Trend

make_plot <- function(data_dir = "data") {
  library(ggplot2)
  if (file.exists("theme_journal.R")) source("theme_journal.R", local = TRUE)
  
  df <- read.csv(file.path(data_dir, "ts.csv"))
  df$date <- as.Date(df$date)
  
  p <- ggplot(df, aes(x = date, y = value, color = group)) +
    geom_line(linewidth = 0.95, alpha = 0.9) +
    geom_point(size = 1.3, alpha = 0.8) +
    labs(
      title = "Longitudinal Biomarker Dynamics Across Study Sites",
      subtitle = "Monthly monitoring showing baseline shifts and cyclical oscillations",
      x = "Observation Date",
      y = "Normalized Signal Intensity (AU)",
      color = "Site",
      caption = "Source: Simulated longitudinal multi-site surveillance dataset"
    )
  
  if (exists("theme_topjournal")) {
    p <- p + theme_topjournal(grid = "y") + scale_color_topjournal()
  } else {
    p <- p + theme_classic()
  }
  p
}

explain <- function() {
  list(
    title = "Longitudinal Line Plot",
    how_to_read = "Chronological time or sequential order is encoded on the horizontal axis, while measured quantities are tracked vertically. Continuous lines connect successive timepoints to emphasize continuity, rate of change, and multi-cohort diverging trajectories.",
    when_to_use = "Ideal for longitudinal cohorts, pharmacokinetic assays, seasonal surveillance, and growth curves where order cannot be shuffled.",
    pitfalls = "Do not connect unordered categorical factors with lines, as this falsely implies continuity. Watch out for irregular sampling intervals that can distort perceived rates of acceleration.",
    journal_archetype = "Lancet Infectious Diseases temporal surveillance curves.",
    data_recipe = "Requires 1 date/numeric time column, 1 continuous measurement column, and 1 grouping factor."
  )
}
