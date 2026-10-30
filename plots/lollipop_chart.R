# Ranked Lollipop Chart
# Tier 1 (Simple) - Comparison

make_plot <- function(data_dir = "data") {
  library(ggplot2)
  if (file.exists("theme_journal.R")) source("theme_journal.R", local = TRUE)
  
  df <- read.csv(file.path(data_dir, "grp.csv"))
  
  agg <- aggregate(value ~ group, data = df, mean)
  agg <- agg[order(agg$value), ]
  agg$group <- factor(agg$group, levels = agg$group)
  
  p <- ggplot(agg, aes(x = value, y = group, color = group)) +
    geom_segment(aes(x = 0, xend = value, y = group, yend = group), linewidth = 1.1, color = "#CCCCCC") +
    geom_point(size = 5.2, shape = 21, stroke = 1.2, fill = "#FFFFFF") +
    geom_point(size = 3.2, aes(fill = group), shape = 21, stroke = 0) +
    labs(
      title = "Ranked Performance Index Across Treatment Regimens",
      subtitle = "Minimalist lollipop markers maximize data-to-ink ratio compared to heavy bar charts",
      x = "Mean Response Metric (AU)",
      y = "Regimen",
      caption = "Source: Simulated multi-arm bioassay cohort"
    )
  
  if (exists("theme_topjournal")) {
    p <- p + theme_topjournal(grid = "x") + scale_color_topjournal() + scale_fill_topjournal() + theme(legend.position = "none")
  } else {
    p <- p + theme_classic()
  }
  p
}

explain <- function() {
  list(
    title = "Ranked Lollipop Chart",
    how_to_read = "Categories are arranged along the vertical axis, with thin horizontal guide lines connecting each origin to a prominent endpoint marker representing the measured quantity.",
    when_to_use = "A high-performance modern alternative to standard bar charts when comparing numerous ranked groups, keeping the visual field light and uncluttered.",
    pitfalls = "Ensure the origin line starts at a meaningful baseline (often zero or cohort average). Avoid truncating baseline stems if they distort relative magnitude differences.",
    journal_archetype = "Nature Communications ranking of gene pathway enrichments or gene ontology terms.",
    data_recipe = "Requires 1 categorical group name and 1 numeric summary value."
  )
}
