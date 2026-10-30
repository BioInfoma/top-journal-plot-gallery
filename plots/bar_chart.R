# Publication Bar Chart with Means and Summary Statistics
# Tier 1 (Simple) - Comparison

make_plot <- function(data_dir = "data") {
  library(ggplot2)
  if (file.exists("theme_journal.R")) source("theme_journal.R", local = TRUE)
  
  df <- read.csv(file.path(data_dir, "grp.csv"))
  
  # Aggregate mean and standard error per group
  agg <- aggregate(value ~ group, data = df, function(v) c(mean = mean(v), se = sd(v)/sqrt(length(v))))
  agg_df <- data.frame(group = agg$group, mean = agg$value[, "mean"], se = agg$value[, "se"])
  agg_df$group <- factor(agg_df$group, levels = c("Control", "DrugA", "DrugB", "DrugC"))
  
  p <- ggplot(agg_df, aes(x = group, y = mean, fill = group)) +
    geom_col(width = 0.58, color = "#222222", linewidth = 0.35, alpha = 0.88) +
    geom_errorbar(aes(ymin = mean - se, ymax = mean + se), width = 0.18, linewidth = 0.55, color = "#222222") +
    labs(
      title = "Mean Therapeutic Response Across Treatment Arms",
      subtitle = "Bars indicate cohort means; error bars denote ± 1 Standard Error of the Mean (SEM)",
      x = "Experimental Arm",
      y = "Mean Activity Score",
      fill = "Arm",
      caption = "Source: Simulated randomized comparative trial dataset (n = 30 / arm)"
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
    title = "Categorical Bar Chart with SEM Whiskers",
    how_to_read = "The height of each rectangular bar corresponds to the central group mean. Symmetrical vertical error whiskers represent standard error of the mean (SEM), reflecting sampling precision across cohorts.",
    when_to_use = "Use for comparing discrete summary aggregates such as counts, proportions, or mean responses across predefined test conditions.",
    pitfalls = "Classic 'dynamite plunger' bar plots conceal underlying distribution shapes, multimodality, and outlier clusters. Modern top-journal guidelines strongly prefer overlaying raw jitter points or using box/raincloud plots for continuous variables.",
    journal_archetype = "Cell and Nature standard comparison figures.",
    data_recipe = "Requires 1 categorical group variable and 1 continuous response variable."
  )
}
