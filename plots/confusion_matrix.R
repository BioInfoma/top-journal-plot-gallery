# Clinical / Machine Learning Confusion Matrix Heatmap
# Tier 2 (Intermediate) - Classification

make_plot <- function(data_dir = "data") {
  library(ggplot2)
  if (file.exists("theme_journal.R")) source("theme_journal.R", local = TRUE)
  
  df <- read.csv(file.path(data_dir, "clf.csv"))
  
  # Binary classification threshold at score = 0.50
  pred_class <- ifelse(df$score >= 0.50, 1, 0)
  actual_class <- df$truth
  
  cm <- table(Actual = factor(actual_class, levels = c(1, 0), labels = c("Condition Positive", "Condition Negative")),
              Predicted = factor(pred_class, levels = c(1, 0), labels = c("Predicted Positive", "Predicted Negative")))
  
  cm_df <- as.data.frame(cm)
  total_n <- sum(cm_df$Freq)
  cm_df$Percentage <- paste0(round(100 * cm_df$Freq / total_n, 1), "%")
  cm_df$Label <- paste0(cm_df$Freq, "\n(", cm_df$Percentage, ")")
  
  # Diagnostic metrics
  tp <- cm["Condition Positive", "Predicted Positive"]
  tn <- cm["Condition Negative", "Predicted Negative"]
  fp <- cm["Condition Negative", "Predicted Positive"]
  fn <- cm["Condition Positive", "Predicted Negative"]
  
  sens <- round(tp / (tp + fn) * 100, 1)
  spec <- round(tn / (tn + fp) * 100, 1)
  acc <- round((tp + tn) / total_n * 100, 1)
  
  p <- ggplot(cm_df, aes(x = Predicted, y = Actual, fill = Freq)) +
    geom_tile(color = "#FFFFFF", linewidth = 1.2) +
    geom_text(aes(label = Label), size = 5.2, fontface = "bold", color = "#1E293B") +
    scale_fill_gradient(low = "#EFF6FF", high = "#3B82F6", name = "Count") +
    scale_y_discrete(limits = rev(levels(cm_df$Actual))) +
    labs(
      title = "Binary Diagnostic Classification Matrix",
      subtitle = paste0("Accuracy: ", acc, "% | Sensitivity (Recall): ", sens, "% | Specificity: ", spec, "%"),
      x = "Model Predicted Class",
      y = "Ground Truth Reference Class",
      caption = "Source: Simulated diagnostic test validation cohort (N = 500)"
    )
  
  if (exists("theme_topjournal")) {
    p <- p + theme_topjournal(grid = "none") +
      theme(
        axis.text = element_text(size = rel(1.05), face = "bold"),
        legend.position = "right"
      )
  } else {
    p <- p + theme_minimal()
  }
  p
}

explain <- function() {
  list(
    title = "Diagnostic Confusion Matrix Heatmap",
    how_to_read = "A 2x2 contingency table cross-tabulating actual ground-truth clinical status (rows) against predicted classification outcomes (columns). Diagonal cells (top-left and bottom-right) represent correct classifications (True Positives and True Negatives).",
    when_to_use = "Essential for assessing diagnostic test utility, algorithmic fairness, and class-specific error trade-offs in clinical epidemiology and machine learning.",
    pitfalls = "High overall accuracy can be misleading in imbalanced settings (e.g. 99% accuracy by predicting negative on a 1% rare disease). Always report sensitivity, specificity, and positive predictive value alongside raw counts.",
    journal_archetype = "The Lancet Digital Health and Nature Medicine diagnostic AI performance tables.",
    data_recipe = "Requires ground truth binary labels and categorical or thresholded model predictions."
  )
}
