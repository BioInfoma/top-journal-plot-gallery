# Grouped and Stacked Bar Chart
# Tier 1 (Simple) - Comparison

make_plot <- function(data_dir = "data") {
  library(ggplot2)
  if (file.exists("theme_journal.R")) source("theme_journal.R", local = TRUE)
  
  df <- read.csv(file.path(data_dir, "grp.csv"))
  
  agg <- aggregate(value ~ group + subgroup, data = df, mean)
  agg$group <- factor(agg$group, levels = c("Control", "DrugA", "DrugB", "DrugC"))
  
  p <- ggplot(agg, aes(x = group, y = value, fill = subgroup)) +
    geom_col(position = position_dodge(width = 0.72), width = 0.62, color = "#222222", linewidth = 0.35, alpha = 0.88) +
    labs(
      title = "Stratified Therapeutic Efficacy by Demographic Subgroup",
      subtitle = "Side-by-side grouped bars facilitate direct pairwise cohort comparisons",
      x = "Experimental Condition",
      y = "Mean Efficacy Index",
      fill = "Biological Sex",
      caption = "Source: Simulated multi-arm subgroup dataset"
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
    title = "Grouped (Dodged) Bar Chart",
    how_to_read = "Clusters of bars correspond to primary experimental conditions, with adjacent bars differentiating demographic or genetic strata within each arm. Direct visual alignment allows comparison both within a condition and across arms.",
    when_to_use = "Use when examining interaction effects between two categorical factors (e.g. Treatment × Sex or Genotype × Diet) on a continuous or aggregate metric.",
    pitfalls = "Stacking bars can make baseline-displaced segments difficult to evaluate accurately. Grouped (dodged) layouts are generally superior when comparing subsegment magnitudes across conditions.",
    journal_archetype = "New England Journal of Medicine subgroup breakdown panels.",
    data_recipe = "Requires 1 primary categorical variable, 1 secondary stratification factor, and 1 continuous measurement."
  )
}
