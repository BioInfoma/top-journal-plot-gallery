# Publication Correlation Matrix (Correlogram)
# Tier 2 (Intermediate) - Matrix

make_plot <- function(data_dir = "data") {
  library(ggplot2)
  if (file.exists("theme_journal.R")) source("theme_journal.R", local = TRUE)
  
  mat_raw <- read.csv(file.path(data_dir, "mat.csv"), row.names = 1, check.names = FALSE)
  
  # Compute pairwise Pearson correlation across top 14 representative features
  sub_mat <- as.matrix(mat_raw[1:14, ])
  cor_mat <- cor(t(sub_mat))
  
  # Tidy correlation dataframe
  cor_df <- expand.grid(Var1 = rownames(cor_mat), Var2 = colnames(cor_mat))
  cor_df$Correlation <- as.vector(cor_mat)
  cor_df$Label <- ifelse(cor_df$Var1 == cor_df$Var2, "1", sprintf("%.2f", cor_df$Correlation))
  
  p <- ggplot(cor_df, aes(x = Var1, y = Var2, fill = Correlation)) +
    geom_tile(color = "#FFFFFF", linewidth = 0.5) +
    geom_text(aes(label = Label), size = 2.8, color = ifelse(abs(cor_df$Correlation) > 0.5, "#FFFFFF", "#1E293B")) +
    scale_fill_gradient2(
      low = "#3C5488",
      mid = "#F8FAFC",
      high = "#E64B35",
      midpoint = 0,
      limits = c(-1, 1),
      name = "Pearson r"
    ) +
    labs(
      title = "Pairwise Feature Correlation Matrix (Correlogram)",
      subtitle = "Pearson correlation coefficients across representative biological features; scale spans -1 to +1",
      x = "Feature Symbol",
      y = "Feature Symbol",
      caption = "Source: Simulated multi-parameter expression matrix"
    )
  
  if (exists("theme_topjournal")) {
    p <- p + theme_topjournal(grid = "none") +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1, size = rel(0.85)),
        axis.text.y = element_text(size = rel(0.85)),
        legend.position = "right"
      )
  } else {
    p <- p + theme_minimal()
  }
  p
}

explain <- function() {
  list(
    title = "Pairwise Correlation Matrix (Correlogram)",
    how_to_read = "A symmetric grid displaying pairwise correlation coefficients (Pearson r or Spearman rho) between continuous variables. Color hue and saturation encode direction (positive vs negative) and magnitude, with overlaid numeric values denoting exact coefficients.",
    when_to_use = "Standard exploratory step in multi-omic profiling, psychometrics, and clinical trials to detect collinearity, co-regulated clusters, and redundant variables.",
    pitfalls = "Beware of confounding variables or outliers driving spurious correlations (Anscombe's quartet). Inspect scatter plots for non-linear relationships that Pearson's r fails to capture.",
    journal_archetype = "Nature Reviews Genetics and Cell Metabolism multi-omic correlation maps.",
    data_recipe = "Requires multiple continuous numeric features across matched samples."
  )
}
