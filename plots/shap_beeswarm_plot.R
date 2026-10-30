# SHAP Summary Beeswarm Plot (Machine Learning Model Explainability)
# Tier 3 (Advanced) - Machine learning

make_plot <- function(data_dir = "data") {
  library(ggplot2)
  if (file.exists("theme_journal.R")) source("theme_journal.R", local = TRUE)
  
  # Simulate SHAP values for top 8 machine learning predictive features across 250 patients
  set.seed(42)
  n <- 250
  features <- c(
    "Tumor Mutation Burden",
    "CD8+ T-Cell Infiltration",
    "PD-L1 Expression Level",
    "Patient Age",
    "Interferon-γ Signature",
    "Serum Lactate (LDH)",
    "Prior Lines of Therapy",
    "Albumin Level"
  )
  
  shap_list <- list()
  for (i in seq_along(features)) {
    # Generate feature values [0, 1]
    feat_val <- runif(n, 0, 1)
    # SHAP value correlated with feature value (positive or negative impact)
    direction <- ifelse(i %in% c(1, 2, 3, 5), 1, -1)
    importance_scale <- (9 - i) * 0.35
    shap_val <- (feat_val - 0.5) * direction * importance_scale + rnorm(n, 0, 0.15)
    
    # Add vertical beeswarm jitter
    jitter_y <- i + rnorm(n, 0, 0.12)
    
    shap_list[[i]] <- data.frame(
      feature = features[i],
      feature_idx = i,
      shap_value = shap_val,
      feature_value = feat_val,
      y_pos = jitter_y
    )
  }
  df_shap <- do.call(rbind, shap_list)
  df_shap$feature <- factor(df_shap$feature, levels = rev(features))
  
  p <- ggplot(df_shap, aes(x = shap_value, y = feature)) +
    geom_vline(xintercept = 0, color = "#64748B", linewidth = 0.8, linetype = "solid") +
    geom_point(aes(color = feature_value), size = 2.0, alpha = 0.72, 
               position = position_jitter(height = 0.22, width = 0)) +
    scale_color_gradient(
      low = "#2563EB", # Low value (Blue)
      high = "#DC2626", # High value (Red)
      name = "Feature Value:",
      breaks = c(0.1, 0.9),
      labels = c("Low", "High")
    ) +
    scale_x_continuous(limits = c(-2.8, 2.8), breaks = seq(-2, 2, by = 1)) +
    labs(
      title = "SHAP Summary Beeswarm Plot: Machine Learning Feature Attribution",
      subtitle = "Shapley values quantify directional impact on model log-odds prediction; points colored by normalized feature value",
      x = "SHAP Value (Impact on Model Log-Odds Output)",
      y = "Predictive Clinical Feature (Ranked by Global Importance)",
      caption = "Source: Simulated gradient boosting machine (XGBoost / SHAP tree explainer on 250 patients)"
    )
  
  if (exists("theme_topjournal")) {
    p <- p + theme_topjournal(grid = "x") +
      theme(
        axis.text.y = element_text(face = "bold", size = rel(0.95), color = "#0F172A"),
        legend.position = "top",
        legend.direction = "horizontal",
        legend.title = element_text(face = "bold", size = 8.5),
        legend.key.width = unit(1.4, "cm"),
        legend.key.height = unit(0.3, "cm")
      )
  } else {
    p <- p + theme_classic()
  }
  p
}

explain <- function() {
  list(
    title = "SHAP Summary Beeswarm Explainability Plot",
    how_to_read = "Features are ranked vertically by global importance (sum of absolute SHAP values). Along each horizontal lane, every point represents a single observation; its horizontal position denotes whether that feature pushed the model prediction higher (positive SHAP) or lower (negative SHAP). Point color indicates the actual feature value (red = high, blue = low).",
    when_to_use = "The international standard for interpretable machine learning in medicine and high-impact science (Lundberg et al., Nature Machine Intelligence) to explain complex non-linear ensemble models and neural networks.",
    pitfalls = "Correlated features can split attribution scores, diminishing their apparent individual SHAP values; always evaluate feature collinearity before interpreting mechanistic causality.",
    journal_archetype = "Nature Machine Intelligence and Nature Medicine clinical AI explainability benchmark figures.",
    data_recipe = "Requires matrix of calculated SHAP values and corresponding normalized raw feature values."
  )
}
