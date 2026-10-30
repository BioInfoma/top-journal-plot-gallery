# Precision-Recall (PR) Curve with Area Under Curve (AUPRC)
# Tier 2 (Intermediate) - Classification

make_plot <- function(data_dir = "data") {
  library(ggplot2)
  if (file.exists("theme_journal.R")) source("theme_journal.R", local = TRUE)
  
  df <- read.csv(file.path(data_dir, "clf.csv"))
  
  thresholds <- sort(unique(df$score), decreasing = TRUE)
  if (length(thresholds) > 120) {
    thresholds <- quantile(thresholds, probs = seq(1, 0, length.out = 120))
  }
  
  n_pos <- sum(df$truth == 1)
  baseline_prevalence <- n_pos / nrow(df)
  
  pr_pts <- do.call(rbind, lapply(thresholds, function(th) {
    pred <- df$score >= th
    tp <- sum(pred & df$truth == 1)
    fp <- sum(pred & df$truth == 0)
    recall <- tp / n_pos
    precision <- if ((tp + fp) == 0) 1 else tp / (tp + fp)
    data.frame(recall = recall, precision = precision)
  }))
  pr_pts <- rbind(data.frame(recall = 0, precision = 1), pr_pts)
  pr_pts <- pr_pts[order(pr_pts$recall, -pr_pts$precision), ]
  
  # Trapezoidal approximation of AUPRC
  auprc <- sum(diff(pr_pts$recall) * (head(pr_pts$precision, -1) + tail(pr_pts$precision, -1)) / 2)
  
  p <- ggplot(pr_pts, aes(x = recall, y = precision)) +
    geom_ribbon(aes(ymin = baseline_prevalence, ymax = precision), fill = "#00A087", alpha = 0.15) +
    geom_line(color = "#00A087", linewidth = 1.15) +
    geom_hline(yintercept = baseline_prevalence, linetype = "dashed", color = "#777777", linewidth = 0.55) +
    annotate("label", x = 0.25, y = 0.25, 
             label = paste0("AUPRC = ", round(auprc, 3), "\nBaseline Prevalence = ", round(baseline_prevalence, 2)), 
             fontface = "bold", size = 3.6, fill = "#FFFFFF", color = "#00A087") +
    scale_x_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0, 1.01)) +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0, 1.01)) +
    labs(
      title = "Precision-Recall (PR) Curve for Imbalanced Classification",
      subtitle = "Precision (PPV) vs Recall (Sensitivity); horizontal dashed line shows random chance baseline",
      x = "Recall (True Positive Rate / Sensitivity)",
      y = "Precision (Positive Predictive Value)",
      caption = "Source: Simulated diagnostic machine learning test set (N = 500, Prevalence = 30%)"
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
    title = "Precision-Recall (PR) Curve",
    how_to_read = "Plots Precision (Positive Predictive Value) on the vertical axis against Recall (Sensitivity) horizontally across varying score thresholds. The horizontal dashed line denotes the positive class prevalence (random classifier baseline).",
    when_to_use = "Essential when evaluating classifiers on heavily imbalanced datasets (e.g. rare disease screening, somatic mutation detection, fraud/fraudulent events) where ROC curves provide an unrealistically inflated perspective.",
    pitfalls = "Unlike ROC curves where random guessing yields a constant 0.5 AUC diagonal, the baseline AUPRC equals the positive class prevalence. Curves must never be compared across datasets with different class proportions without adjusting.",
    journal_archetype = "Nature Machine Intelligence and Bioinformatics benchmark metrics.",
    data_recipe = "Requires binary ground-truth labels and continuous prediction probabilities."
  )
}
