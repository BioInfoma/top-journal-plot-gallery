# Principal Component Analysis (PCA) Score Plot with Ellipses
# Tier 2 (Intermediate) - Dimension reduction

make_plot <- function(data_dir = "data") {
  library(ggplot2)
  if (file.exists("theme_journal.R")) source("theme_journal.R", local = TRUE)
  
  mat_raw <- read.csv(file.path(data_dir, "mat.csv"), row.names = 1, check.names = FALSE)
  meta <- read.csv(file.path(data_dir, "mat_meta.csv"))
  
  # Samples are columns; transpose for PCA (samples x genes)
  pca_res <- prcomp(t(mat_raw), scale. = TRUE)
  var_explained <- round(100 * (pca_res$sdev^2 / sum(pca_res$sdev^2)), 1)
  
  df_pca <- data.frame(
    PC1 = pca_res$x[, 1],
    PC2 = pca_res$x[, 2],
    sample = rownames(pca_res$x)
  )
  df_pca <- merge(df_pca, meta, by = "sample")
  
  p <- ggplot(df_pca, aes(x = PC1, y = PC2, color = condition, fill = condition)) +
    stat_ellipse(geom = "polygon", alpha = 0.18, level = 0.95, linewidth = 0.5) +
    geom_point(size = 3.8, shape = 21, stroke = 0.45, color = "#222222", alpha = 0.9) +
    labs(
      title = "Principal Component Analysis (PCA) Ordination",
      subtitle = "Two-dimensional projection of high-dimensional expression variance; ellipses show 95% confidence regions",
      x = paste0("PC1 (", var_explained[1], "% variance explained)"),
      y = paste0("PC2 (", var_explained[2], "% variance explained)"),
      color = "Condition",
      fill = "Condition",
      caption = "Source: Simulated transcriptomic profiling matrix (200 genes x 30 samples)"
    )
  
  if (exists("theme_topjournal")) {
    p <- p + theme_topjournal(grid = "xy") + scale_color_topjournal() + scale_fill_topjournal()
  } else {
    p <- p + theme_classic()
  }
  p
}

explain <- function() {
  list(
    title = "PCA Score Projection with Group Ellipses",
    how_to_read = "High-dimensional feature space is projected onto orthogonal axes that maximize sample variance. Distances between sample points correspond to linear transcriptomic divergence. 95% confidence ellipses demonstrate cluster cohesion and separation.",
    when_to_use = "Essential initial check for batch effects, sample outliers, biological replicate reproducibility, and global clustering in high-throughput data.",
    pitfalls = "PCA captures only linear relationships. If total variance explained on the first two axes is low (< 30%), a 2D projection may hide major non-linear structure; inspect a scree plot.",
    journal_archetype = "Cell Systems and Nature Biotechnology global sample quality assurance.",
    data_recipe = "Requires a numeric feature matrix (features × samples) and a sample metadata annotation table."
  )
}
