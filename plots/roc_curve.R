# Receiver Operating Characteristic (ROC) Curve
# Tier 2 (Intermediate) - Classification

make_plot <- function(data_dir = "data") {
  library(ggplot2)
  if (file.exists("theme_journal.R")) source("theme_journal.R", local = TRUE)
  
  df <- read.csv(file.path(data_dir, "clf.csv"))
  
  # Compute empirical TPR and FPR across score thresholds
  thresholds <- sort(unique(df$score), decreasing = TRUE)
  # Sample 100 thresholds for smooth curve
  if (length(thresholds) > 100) {
    thresholds <- quantile(thresholds, probs = seq(1, 0, length.out = 100))
  }
  
  n_pos <- sum(df$truth == 1)
  n_neg <- sum(df$truth == 0)
  
  roc_pts <- do.call(rbind, lapply(thresholds, function(th) {
    pred <- df$score >= th
    tpr <- sum(pred & df$truth == 1) / n_pos
    fpr <- sum(pred & df$truth == 0) / n_neg
    data.frame(fpr = fpr, tpr = tpr)
  }))
  roc_pts <- rbind(data.frame(fpr = 0, tpr = 0), roc_pts, data.frame(fpr = 1, tpr = 1))
  roc_pts <- roc_pts[order(roc_pts$fpr, roc_pts$tpr), ]
  
  # Trapezoidal numerical integration for AUC
  auc <- sum((roc_pts$fpr[-1] - roc_pts$fpr[-nrow(roc_pts)]) * (roc_pts$tpr[-1] + roc_pts$tpr[-nrow(roc_pts)]) / 2)
  auc_label <- paste0("AUC = ", round(auc, 3))
  
  p <- ggplot(roc_pts, aes(x = fpr, y = tpr)) +
    geom_ribbon(aes(ymin = 0, ymax = tpr), fill = "#3C5488", alpha = 0.12) +
    geom_line(color = "#E64B35", linewidth = 1.2) +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "#777777", linewidth = 0.55) +
    scale_x_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0, 1.01)) +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0, 1.01)) +
    annotate("label", x = 0.65, y = 0.25, label = auc_label, 
             fontface = "bold", size = 4.2, color = "#E64B35", fill = "#FFFFFF") +
    labs(
      title = "Receiver Operating Characteristic (ROC) Curve",
      subtitle = "True Positive Rate (Sensitivity) vs False Positive Rate (1 - Specificity) across decision thresholds",
      x = "False Positive Rate (1 - Specificity)",
      y = "True Positive Rate (Sensitivity)",
      caption = "Source: Simulated diagnostic machine learning classifier (N = 500)"
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
    title = "Receiver Operating Characteristic (ROC) Curve",
    how_to_read = "Tracks trade-offs between Sensitivity (True Positive Rate) and 1 - Specificity (False Positive Rate) as the classification decision threshold varies. The diagonal dashed line denotes random chance (AUC = 0.50), while curves hugging the top-left corner indicate superior discriminatory performance.",
    when_to_use = "Standard evaluation metric for binary diagnostic tests, risk prediction biomarkers, and machine learning classification algorithms.",
    pitfalls = "In heavily class-imbalanced datasets (e.g. rare diseases where positive cases < 1%), ROC curves can present an overly optimistic assessment; complement with a Precision-Recall (PR) curve.",
    journal_archetype = "Nature Medicine clinical AI and biomarker validation benchmark figures.",
    data_recipe = "Requires binary ground-truth labels (0/1) and continuous predicted probability or risk scores."
  )
}
