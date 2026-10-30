# PCA Scree Plot with Cumulative Variance Explained
# Tier 2 (Intermediate) - Dimension reduction

make_plot <- function(data_dir = "data") {
  library(ggplot2)
  if (file.exists("theme_journal.R")) source("theme_journal.R", local = TRUE)
  
  mat_raw <- read.csv(file.path(data_dir, "mat.csv"), row.names = 1, check.names = FALSE)
  
  # Run PCA on transposed matrix (samples x features)
  pca_res <- prcomp(t(mat_raw), scale. = TRUE)
  
  eig_val <- pca_res$sdev^2
  var_pct <- eig_val / sum(eig_val) * 100
  cum_pct <- cumsum(var_pct)
  
  n_comp <- min(12, length(var_pct))
  scree_df <- data.frame(
    PC = factor(paste0("PC", 1:n_comp), levels = paste0("PC", 1:n_comp)),
    pc_num = 1:n_comp,
    Variance = var_pct[1:n_comp],
    Cumulative = cum_pct[1:n_comp]
  )
  
  p <- ggplot(scree_df, aes(x = PC)) +
    geom_col(aes(y = Variance), fill = "#3C5488", width = 0.62, alpha = 0.85, color = "#222222", linewidth = 0.35) +
    geom_line(aes(y = Cumulative / 2.5, group = 1), color = "#E64B35", linewidth = 1.05) +
    geom_point(aes(y = Cumulative / 2.5), color = "#E64B35", size = 2.8, shape = 21, fill = "#FFFFFF", stroke = 1.0) +
    scale_y_continuous(
      name = "Variance Explained (%)",
      sec.axis = sec_axis(~ . * 2.5, name = "Cumulative Variance Explained (%)")
    ) +
    labs(
      title = "PCA Scree Plot: Variance Spectrum Across Components",
      subtitle = "Bars represent individual component variance; red curve tracks cumulative explained variance",
      x = "Principal Component",
      caption = "Source: Simulated multi-dimensional expression matrix (30 samples x 200 genes)"
    )
  
  if (exists("theme_topjournal")) {
    p <- p + theme_topjournal(grid = "y") +
      theme(
        axis.title.y.right = element_text(color = "#E64B35", face = "bold", margin = margin(l = 6)),
        axis.text.y.right = element_text(color = "#E64B35")
      )
  } else {
    p <- p + theme_classic()
  }
  p
}

explain <- function() {
  list(
    title = "PCA Scree Plot (Elbow Criterion)",
    how_to_read = "Bars display the percentage of total dataset variance accounted for by each successive principal component, ordered from greatest to least. The overlaid red line shows cumulative variance.",
    when_to_use = "Essential statistical diagnostic when deciding how many dimensions to retain for downstream clustering, factor analysis, or latent variable regression.",
    pitfalls = "Avoid arbitrary cutoffs (e.g. always picking top 2 or 3 PCs). Inspect the 'elbow' point where the eigenvalue curve flattens into baseline noise.",
    journal_archetype = "Bioinformatics and Nature Communications dimensionality reduction diagnostics.",
    data_recipe = "Requires numeric multivariate feature matrix (samples × features)."
  )
}
