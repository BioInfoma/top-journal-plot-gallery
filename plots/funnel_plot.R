# Publication Bias Funnel Plot for Meta-Analysis
# Tier 2 (Intermediate) - Meta-analysis

make_plot <- function(data_dir = "data") {
  library(ggplot2)
  if (file.exists("theme_journal.R")) source("theme_journal.R", local = TRUE)
  
  df <- read.csv(file.path(data_dir, "meta.csv"))
  
  # Calculate summary pooled fixed-effect estimate
  weights <- 1 / (df$se^2)
  pooled_effect <- sum(df$effect * weights) / sum(weights)
  
  # Generate triangular 95% and 99% pseudo-confidence funnel limits
  se_seq <- seq(0, max(df$se) * 1.15, length.out = 150)
  funnel_df <- data.frame(
    se = se_seq,
    lower_95 = pooled_effect - 1.96 * se_seq,
    upper_95 = pooled_effect + 1.96 * se_seq
  )
  
  p <- ggplot() +
    geom_ribbon(data = funnel_df, aes(ymin = lower_95, ymax = upper_95, x = se), 
                fill = "#94A3B8", alpha = 0.18) +
    geom_line(data = funnel_df, aes(y = lower_95, x = se), color = "#64748B", linetype = "dashed", linewidth = 0.5) +
    geom_line(data = funnel_df, aes(y = upper_95, x = se), color = "#64748B", linetype = "dashed", linewidth = 0.5) +
    geom_hline(yintercept = pooled_effect, color = "#DC2626", linewidth = 0.8) +
    geom_point(data = df, aes(x = se, y = effect), size = 3.2, shape = 21, fill = "#3C5488", color = "#222222", stroke = 0.45, alpha = 0.85) +
    coord_flip() +
    scale_x_reverse() + # Standard inverted standard error on vertical axis
    labs(
      title = "Funnel Plot for Publication Bias Assessment",
      subtitle = "Treatment effect size plotted against inverted standard error; shaded cone marks 95% pseudo-confidence limits",
      x = "Standard Error of Effect Size (Inverted Precision)",
      y = "Observed Effect Size (Standardized Mean Difference)",
      caption = "Source: Simulated evidence synthesis meta-analysis (12 clinical studies)"
    )
  
  if (exists("theme_topjournal")) {
    p <- p + theme_topjournal(grid = "xy")
  } else {
    p <- p + theme_classic()
  }
  p
}

explain <- function() {
  list(
    title = "Publication Bias Funnel Plot",
    how_to_read = "Each point represents an individual study in a systematic review. Estimated effect sizes are on the horizontal axis, and study standard error is on the vertical axis (inverted, so large precise studies sit at the top). Symmetrical distribution within the triangular funnel indicates absence of publication bias.",
    when_to_use = "Mandatory in Cochrane reviews, clinical epidemiology, and meta-analyses to check whether small studies with negative or null findings were suppressed from publication.",
    pitfalls = "Asymmetry in a funnel plot does not always prove publication bias; it can also reflect genuine differences between small and large trials (small-study effects), differing patient populations, or clinical heterogeneity.",
    journal_archetype = "The Lancet and BMJ meta-analysis diagnostic guidelines (PRISMA statement).",
    data_recipe = "Requires study effect sizes and their standard errors."
  )
}
