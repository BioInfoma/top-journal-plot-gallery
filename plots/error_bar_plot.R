# Error Bar Plot with Confidence Intervals
# Tier 1 (Simple) - Comparison

make_plot <- function(data_dir = "data") {
  library(ggplot2)
  if (file.exists("theme_journal.R")) source("theme_journal.R", local = TRUE)
  
  df <- read.csv(file.path(data_dir, "grp.csv"))
  
  # Calculate mean and 95% Confidence Intervals
  calc_ci <- function(v) {
    m <- mean(v)
    se <- sd(v) / sqrt(length(v))
    ci <- qt(0.975, df = length(v) - 1) * se
    c(mean = m, lower = m - ci, upper = m + ci)
  }
  
  res <- do.call(data.frame, aggregate(value ~ group + subgroup, data = df, calc_ci))
  colnames(res) <- c("group", "subgroup", "mean", "lower", "upper")
  res$group <- factor(res$group, levels = c("Control", "DrugA", "DrugB", "DrugC"))
  
  p <- ggplot(res, aes(x = group, y = mean, color = subgroup, group = subgroup)) +
    geom_line(position = position_dodge(width = 0.35), linewidth = 0.75, linetype = "dashed", alpha = 0.7) +
    geom_errorbar(aes(ymin = lower, ymax = upper), position = position_dodge(width = 0.35), width = 0.18, linewidth = 0.7) +
    geom_point(position = position_dodge(width = 0.35), size = 3.2, shape = 21, fill = "#FFFFFF", stroke = 1.1) +
    labs(
      title = "Estimated Treatment Effects with 95% Confidence Intervals",
      subtitle = "Points show subgroup means; whiskers indicate Student's t 95% confidence intervals",
      x = "Study Arm",
      y = "Estimated Marginal Mean Response",
      color = "Subgroup",
      caption = "Source: Simulated clinical trial subgroup cohort"
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
    title = "Point and Error Bar Plot (95% CI)",
    how_to_read = "Central circular symbols denote estimated cohort means. Symmetrical vertical whiskers represent 95% confidence intervals, defining the range of plausible values for the true population mean.",
    when_to_use = "Standard representation when reporting inferential estimates from ANOVA, mixed-effects regression models, or multi-factor factorial designs.",
    pitfalls = "Always specify in the caption whether whiskers denote Standard Deviation (SD, variability), Standard Error (SEM, precision), or 95% CI (inference), as they convey radically different statistical properties.",
    journal_archetype = "Science translational medicine clinical summary panels.",
    data_recipe = "Requires 1 primary category, 1 secondary stratification factor, and 1 continuous measurement."
  )
}
