# Strip / Jitter Plot with Summary Crossbars
# Tier 1 (Simple) - Distribution

make_plot <- function(data_dir = "data") {
  library(ggplot2)
  if (file.exists("theme_journal.R")) source("theme_journal.R", local = TRUE)
  
  df <- read.csv(file.path(data_dir, "grp.csv"))
  df$group <- factor(df$group, levels = c("Control", "DrugA", "DrugB", "DrugC"))
  
  p <- ggplot(df, aes(x = group, y = value, fill = group)) +
    geom_jitter(width = 0.22, size = 2.4, shape = 21, stroke = 0.35, alpha = 0.75, color = "#222222") +
    stat_summary(fun = mean, geom = "crossbar", width = 0.38, color = "#111111", linewidth = 0.6) +
    stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.2, color = "#111111", linewidth = 0.65) +
    labs(
      title = "Transparent Point Strip Plot with Mean ± SEM Crossbars",
      subtitle = "Every replicate point is plotted directly to ensure complete data visibility",
      x = "Experimental Cohort",
      y = "Measured Phenotype Score",
      caption = "Source: Simulated biological replicate cohort (n = 30 per condition)"
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
    title = "Strip Plot with Crossbars",
    how_to_read = "Every single individual replicate is rendered as a distinct circle, with subtle horizontal jitter added to prevent overlapping points. The bold horizontal crossbar and whiskers indicate the sample mean ± SEM.",
    when_to_use = "The gold standard for small-to-moderate sample sizes (n = 5 to 50 per group) where showing every raw datum ensures maximum scientific integrity.",
    pitfalls = "When sample sizes exceed hundreds or thousands of points, points fuse together into solid blobs; switch to violin, ridgeline, or raincloud plots for larger cohorts.",
    journal_archetype = "EMBO Journal and Nature Cell Biology transparency guidelines.",
    data_recipe = "Requires 1 categorical factor and 1 continuous measurement."
  )
}
