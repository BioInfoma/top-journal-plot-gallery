# Stacked / Proportional Area Chart
# Tier 1 (Simple) - Trend

make_plot <- function(data_dir = "data") {
  library(ggplot2)
  if (file.exists("theme_journal.R")) source("theme_journal.R", local = TRUE)
  
  df <- read.csv(file.path(data_dir, "ts.csv"))
  df$date <- as.Date(df$date)
  
  # Ensure non-negative values for meaningful area stacking
  min_val <- min(df$value, na.rm = TRUE)
  if (min_val < 0) df$value <- df$value - min_val + 1
  
  p <- ggplot(df, aes(x = date, y = value, fill = group)) +
    geom_area(alpha = 0.8, color = "#FFFFFF", linewidth = 0.3) +
    labs(
      title = "Cumulative Longitudinal Volume Across Surveillance Sites",
      subtitle = "Filled area highlights total volume progression and proportional contribution over time",
      x = "Date",
      y = "Cumulative Volume Index",
      fill = "Site",
      caption = "Source: Simulated multi-site surveillance time series"
    )
  
  if (exists("theme_topjournal")) {
    p <- p + theme_topjournal(grid = "y") + scale_fill_topjournal()
  } else {
    p <- p + theme_classic()
  }
  p
}

explain <- function() {
  list(
    title = "Stacked Area Chart",
    how_to_read = "The vertical sum of the stacked colored bands depicts total cumulative quantity over time, while the height of any individual band reflects its specific subgroup contribution.",
    when_to_use = "Use when tracking changes in total composition, energy consumption, epidemiological burdens, or financial flows over continuous time intervals.",
    pitfalls = "Except for the baseline layer, intermediate bands have fluctuating non-flat baselines, making subtle changes in their individual magnitudes difficult for human eyes to compare accurately.",
    journal_archetype = "Our World in Data and Lancet Global Health disease burden dynamics.",
    data_recipe = "Requires 1 date/ordered numeric variable, 1 continuous non-negative value, and 1 grouping factor."
  )
}
