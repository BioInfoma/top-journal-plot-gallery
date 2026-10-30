# GWAS Quantile-Quantile (Q-Q) Plot with Genomic Inflation (Lambda GC)
# Tier 2 (Intermediate) - Genomics

make_plot <- function(data_dir = "data") {
  library(ggplot2)
  if (file.exists("theme_journal.R")) source("theme_journal.R", local = TRUE)
  
  df <- read.csv(file.path(data_dir, "gwas.csv"))
  p_vals <- sort(df$p[df$p > 0])
  n <- length(p_vals)
  
  # Calculate expected uniform quantiles
  expected <- -log10((1:n) / (n + 1))
  observed <- -log10(p_vals)
  
  # Compute Genomic Inflation Factor Lambda GC (median chi-sq observed / expected median)
  chisq_obs <- qchisq(1 - p_vals, df = 1)
  lambda_gc <- round(median(chisq_obs) / qchisq(0.5, df = 1), 3)
  
  # 95% concentration envelope
  k <- 1:n
  lower_env <- -log10(qbeta(0.025, k, n - k + 1))
  upper_env <- -log10(qbeta(0.975, k, n - k + 1))
  
  plot_df <- data.frame(
    expected = expected,
    observed = observed,
    lower = lower_env,
    upper = upper_env
  )
  
  # Subsample non-significant points to keep plot crisp and performant
  if (nrow(plot_df) > 2000) {
    top_part <- plot_df[plot_df$observed > 3, ]
    low_part <- plot_df[plot_df$observed <= 3, ]
    low_sampled <- low_part[seq(1, nrow(low_part), by = ceiling(nrow(low_part) / 1500)), ]
    plot_df <- rbind(low_sampled, top_part)
  }
  
  p <- ggplot(plot_df, aes(x = expected, y = observed)) +
    geom_ribbon(aes(ymin = lower, ymax = upper), fill = "#94A3B8", alpha = 0.25) +
    geom_abline(slope = 1, intercept = 0, color = "#DC2626", linewidth = 0.8) +
    geom_point(color = "#3C5488", alpha = 0.65, size = 1.4, shape = 16) +
    annotate("label", x = max(plot_df$expected) * 0.25, y = max(plot_df$observed) * 0.85, 
             label = paste0("Genomic Inflation (λ_GC) = ", lambda_gc), 
             fontface = "bold", size = 3.8, fill = "#FFFFFF", color = "#1E293B") +
    labs(
      title = "GWAS Q-Q Plot: Statistical Distribution of Association P-values",
      subtitle = "Observed vs theoretical null distribution; shaded gray envelope represents 95% confidence interval",
      x = expression(bold("Expected -log"[10] ~ "(P-value under null)")),
      y = expression(bold("Observed -log"[10] ~ "(P-value)")),
      caption = "Source: Simulated GWAS summary statistics (N = 8,800 SNPs)"
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
    title = "GWAS Quantile-Quantile (Q-Q) Diagnostic Plot",
    how_to_read = "Expected -log10(p) values under the uniform null hypothesis are plotted along the horizontal axis, and observed -log10(p) values are plotted vertically. The red 45-degree diagonal represents the theoretical null expectation, with shaded gray 95% confidence envelopes.",
    when_to_use = "Essential companion to every GWAS Manhattan plot to assess systemic bias, cryptic population stratification, or inflation of test statistics across the genome.",
    pitfalls = "A genomic inflation factor (lambda GC) > 1.05 suggests uncorrected population structure or relatedness; an early departure from the diagonal across all SNPs indicates systematic bias rather than genuine polygenic signal.",
    journal_archetype = "Nature Genetics standard GWAS diagnostic panel.",
    data_recipe = "Requires full list of genome-wide association test p-values."
  )
}
