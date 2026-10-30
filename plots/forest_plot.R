# Forest Plot for Clinical Meta-Analysis
# Tier 2 (Intermediate) - Meta-analysis

make_plot <- function(data_dir = "data") {
  library(ggplot2)
  if (file.exists("theme_journal.R")) source("theme_journal.R", local = TRUE)
  
  df <- read.csv(file.path(data_dir, "meta.csv"))
  
  # Calculate inverse-variance weighted pooled summary effect
  weights <- 1 / (df$se^2)
  pooled_effect <- sum(df$effect * weights) / sum(weights)
  pooled_se <- sqrt(1 / sum(weights))
  pooled_lower <- pooled_effect - 1.96 * pooled_se
  pooled_upper <- pooled_effect + 1.96 * pooled_se
  
  # Sort studies by effect size for clean visual presentation
  df <- df[order(df$effect), ]
  df$study <- factor(df$study, levels = c("Pooled Summary (FE)", df$study))
  
  summary_row <- data.frame(
    study = factor("Pooled Summary (FE)", levels = levels(df$study)),
    n = sum(df$n),
    se = pooled_se,
    effect = pooled_effect,
    lower = pooled_lower,
    upper = pooled_upper
  )
  
  p <- ggplot(df, aes(x = effect, y = study)) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "#888888", linewidth = 0.6) +
    geom_vline(xintercept = pooled_effect, linetype = "dotted", color = "#E64B35", linewidth = 0.55) +
    geom_errorbar(aes(xmin = lower, xmax = upper), width = 0.22, orientation = "y", color = "#222222", linewidth = 0.7) +
    geom_point(aes(size = n), shape = 22, fill = "#3C5488", color = "#222222", stroke = 0.5) +
    # Add diamond/summary marker for pooled estimate
    geom_errorbar(data = summary_row, aes(xmin = lower, xmax = upper), width = 0.35, orientation = "y", color = "#E64B35", linewidth = 1.1) +
    geom_point(data = summary_row, aes(x = effect, y = study), shape = 23, size = 4.5, fill = "#E64B35", color = "#111111", stroke = 0.9) +
    scale_size_continuous(range = c(2.5, 6.0), name = "Sample Size (N)") +
    labs(
      title = "Forest Plot: Meta-Analysis of Treatment Effect Sizes",
      subtitle = "Square marker sizes reflect study weighting; whiskers indicate 95% CI; red diamond shows pooled effect",
      x = "Estimated Standardized Mean Difference (Effect Size)",
      y = "Clinical Study",
      caption = "Source: Simulated multi-center evidence synthesis cohort (12 trials)"
    )
  
  if (exists("theme_topjournal")) {
    p <- p + theme_topjournal(grid = "x") + theme(legend.position = "right")
  } else {
    p <- p + theme_classic()
  }
  p
}

explain <- function() {
  list(
    title = "Evidence Synthesis Forest Plot",
    how_to_read = "Each row corresponds to an independent study. Square symbols mark the estimated effect size, with square area proportional to statistical weight (sample size / precision). Horizontal whiskers depict 95% confidence intervals. The bottom diamond summarizes the pooled meta-analytic effect.",
    when_to_use = "The indispensable pillar of Cochrane systematic reviews, clinical trials, and epidemiological evidence synthesis.",
    pitfalls = "Check whether confidence intervals cross the vertical null-effect line of zero (or 1.0 for Odds/Hazard Ratios). Scrutinize between-study heterogeneity (I-squared index) before trusting a fixed-effect pooled summary.",
    journal_archetype = "The Lancet and BMJ meta-analyses and systematic reviews.",
    data_recipe = "Requires study names, estimated effect sizes, standard errors, and sample sizes."
  )
}
