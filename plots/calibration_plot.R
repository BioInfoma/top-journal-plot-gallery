# Clinical Risk Model Calibration Curve (Reliability Diagram)
# Tier 3 (Advanced) - Classification

make_plot <- function(data_dir = "data") {
  library(ggplot2)
  if (file.exists("theme_journal.R")) source("theme_journal.R", local = TRUE)
  
  df <- read.csv(file.path(data_dir, "clf.csv"))
  
  # Bin predicted probabilities into 10 deciles
  df$bin <- cut(df$score, breaks = seq(0, 1, by = 0.1), include.lowest = TRUE)
  
  cal_df <- do.call(rbind, lapply(split(df, df$bin), function(sub_d) {
    if (nrow(sub_d) == 0) return(NULL)
    mean_pred <- mean(sub_d$score)
    obs_prop <- mean(sub_d$truth)
    n <- nrow(sub_d)
    se <- sqrt(obs_prop * (1 - obs_prop) / n)
    data.frame(
      mean_pred = mean_pred,
      obs_prop = obs_prop,
      lower = max(0, obs_prop - 1.96 * se),
      upper = min(1, obs_prop + 1.96 * se),
      n = n
    )
  }))
  
  # Calculate Brier score
  brier <- mean((df$score - df$truth)^2)
  
  p <- ggplot(cal_df, aes(x = mean_pred, y = obs_prop)) +
    # Ideal calibration 45-degree diagonal
    geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "#64748B", linewidth = 0.65) +
    # Calibration line with confidence bars
    geom_line(color = "#1D4ED8", linewidth = 1.05) +
    geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.02, color = "#1E293B", linewidth = 0.55) +
    geom_point(aes(size = n), shape = 21, fill = "#1D4ED8", color = "#1E293B", stroke = 0.6) +
    # Subplot / rug for prediction distribution at bottom
    geom_rug(data = df, aes(x = score, y = NULL), color = "#3B82F6", alpha = 0.25, sides = "b") +
    scale_x_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0, 1)) +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0, 1)) +
    scale_size_continuous(range = c(2.5, 6.5), name = "Subjects per Decile:") +
    annotate("label", x = 0.22, y = 0.85, 
             label = paste0("Calibration Quality:\nBrier Score = ", round(brier, 4), "\nDiagonal = Perfect Calibration"), 
             fontface = "bold", size = 3.3, color = "#0F172A", fill = "#FFFFFF") +
    labs(
      title = "Clinical Risk Model Calibration Curve (Reliability Diagram)",
      subtitle = "Predicted risk probability (deciles) vs observed empirical event rate; rug shows score distribution",
      x = "Mean Predicted Event Probability (%)",
      y = "Observed Empirical Event Rate (%)",
      caption = "Source: Simulated diagnostic clinical prediction validation cohort (N = 500)"
    )
  
  if (exists("theme_topjournal")) {
    p <- p + theme_topjournal(grid = "xy") +
      theme(
        legend.position = "top",
        legend.direction = "horizontal",
        legend.title = element_text(face = "bold", size = 8.5)
      )
  } else {
    p <- p + theme_classic()
  }
  p
}

explain <- function() {
  list(
    title = "Risk Prediction Calibration Curve (Reliability Diagram)",
    how_to_read = "The horizontal axis groups subjects into bins of predicted risk, while the vertical axis shows the actual observed event proportion in each bin. Points lying closely along the 45-degree dashed diagonal indicate a well-calibrated model where a predicted 20% risk corresponds to exactly 20% observed incidence.",
    when_to_use = "Mandatory in clinical epidemiology (TRIPOD guidelines) and medical decision modeling; discrimination (AUC) without calibration is dangerous for clinical decision making.",
    pitfalls = "A model can have a stellar AUC of 0.90 but be disastrously miscalibrated (e.g. systematically overestimating mortality risk by twofold across all patients).",
    journal_archetype = "The Lancet, JAMA, and BMJ prognostic and clinical prediction guidelines.",
    data_recipe = "Requires binary outcome ground-truth and continuous predicted risk probabilities."
  )
}
