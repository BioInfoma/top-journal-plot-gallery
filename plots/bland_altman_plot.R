# Bland-Altman Method Agreement Analysis
# Tier 1 (Simple) - Diagnostics

make_plot <- function(data_dir = "data") {
  library(ggplot2)
  if (file.exists("theme_journal.R")) source("theme_journal.R", local = TRUE)
  
  df <- read.csv(file.path(data_dir, "clf.csv"))
  
  df$mean_val <- (df$method_a + df$method_b) / 2
  df$diff_val <- df$method_a - df$method_b
  
  mean_diff <- mean(df$diff_val, na.rm = TRUE)
  sd_diff <- sd(df$diff_val, na.rm = TRUE)
  upper_loa <- mean_diff + 1.96 * sd_diff
  lower_loa <- mean_diff - 1.96 * sd_diff
  
  p <- ggplot(df, aes(x = mean_val, y = diff_val)) +
    geom_hline(yintercept = 0, color = "#999999", linetype = "dotted", linewidth = 0.5) +
    geom_hline(yintercept = mean_diff, color = "#3C5488", linewidth = 0.85) +
    geom_hline(yintercept = c(upper_loa, lower_loa), color = "#E64B35", linetype = "dashed", linewidth = 0.75) +
    geom_point(alpha = 0.55, size = 2.2, shape = 21, fill = "#4DBBD5", color = "#222222", stroke = 0.3) +
    annotate("text", x = max(df$mean_val), y = mean_diff, label = paste0("Mean Bias: ", round(mean_diff, 2)), 
             hjust = 1, vjust = -0.7, size = 3.3, fontface = "bold", color = "#3C5488") +
    annotate("text", x = max(df$mean_val), y = upper_loa, label = paste0("+1.96 SD: ", round(upper_loa, 2)), 
             hjust = 1, vjust = -0.7, size = 3.3, color = "#E64B35") +
    annotate("text", x = max(df$mean_val), y = lower_loa, label = paste0("-1.96 SD: ", round(lower_loa, 2)), 
             hjust = 1, vjust = 1.5, size = 3.3, color = "#E64B35") +
    labs(
      title = "Bland-Altman Agreement Analysis Between Clinical Assays",
      subtitle = "Pairwise difference (Method A - Method B) plotted against average measurement value",
      x = "Mean of Both Methods [(Method A + Method B) / 2]",
      y = "Difference (Method A - Method B)",
      caption = "Source: Simulated diagnostic assay comparison dataset (N = 500)"
    )
  
  if (exists("theme_topjournal")) {
    p <- p + theme_topjournal(grid = "y")
  } else {
    p <- p + theme_classic()
  }
  p
}

explain <- function() {
  list(
    title = "Bland-Altman Agreement Plot",
    how_to_read = "The difference between two paired measurement instruments is plotted on the y-axis against their average on the x-axis. The solid blue line denotes the mean systemic bias; the dashed red lines mark the 95% Limits of Agreement (LoA, Mean ± 1.96 SD).",
    when_to_use = "The regulatory standard in clinical chemistry and medicine when validating a new diagnostic device or assay against an established gold standard.",
    pitfalls = "High correlation (high Pearson r) does NOT mean methods agree. Only Bland-Altman reveals proportional bias (slope in differences) or heteroscedasticity (funneling spread as magnitude grows).",
    journal_archetype = "The Lancet landmark 1986 methodology citation for clinical measurement validation.",
    data_recipe = "Requires 2 paired numeric continuous columns from two measuring instruments on the same subjects."
  )
}
