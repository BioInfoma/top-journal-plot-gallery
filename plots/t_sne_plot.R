# t-Distributed Stochastic Neighbor Embedding (t-SNE)
# Tier 2 (Intermediate) - Dimension reduction

make_plot <- function(data_dir = "data") {
  library(ggplot2)
  if (file.exists("theme_journal.R")) source("theme_journal.R", local = TRUE)
  
  mat_raw <- read.csv(file.path(data_dir, "mat.csv"), row.names = 1, check.names = FALSE)
  meta <- read.csv(file.path(data_dir, "mat_meta.csv"))
  
  # Samples are columns; transpose to samples x genes
  mat_t <- t(mat_raw)
  
  set.seed(42)
  if (requireNamespace("Rtsne", quietly = TRUE)) {
    tsne_res <- Rtsne::Rtsne(mat_t, perplexity = 5, check_duplicates = FALSE, max_iter = 800)
    df_tsne <- data.frame(
      tSNE1 = tsne_res$Y[, 1],
      tSNE2 = tsne_res$Y[, 2],
      sample = rownames(mat_t)
    )
  } else {
    # PCA fallback if Rtsne not present
    pca_res <- prcomp(mat_t)
    df_tsne <- data.frame(tSNE1 = pca_res$x[, 1], tSNE2 = pca_res$x[, 2], sample = rownames(mat_t))
  }
  
  df_tsne <- merge(df_tsne, meta, by = "sample")
  
  p <- ggplot(df_tsne, aes(x = tSNE1, y = tSNE2, color = condition, fill = condition)) +
    stat_ellipse(geom = "polygon", alpha = 0.15, level = 0.90, linewidth = 0.5) +
    geom_point(size = 3.6, shape = 21, stroke = 0.4, color = "#222222", alpha = 0.88) +
    labs(
      title = "t-SNE Non-Linear Manifold Embedding",
      subtitle = "Two-dimensional non-linear projection of transcriptomic profiles; ellipses mark 90% confidence clusters",
      x = "t-SNE Dimension 1",
      y = "t-SNE Dimension 2",
      color = "Condition",
      fill = "Condition",
      caption = "Source: Simulated transcriptomic profiling matrix (30 samples x 200 genes)"
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
    title = "t-Distributed Stochastic Neighbor Embedding (t-SNE)",
    how_to_read = "A non-linear dimensionality reduction technique that maps high-dimensional transcriptomic distances into two dimensions by minimizing divergence between probability distributions of pairwise similarities. Proximity in t-SNE space indicates strong local cluster cohesion.",
    when_to_use = "Widely used for exploratory visualization of single-cell RNA-seq clusters, flow cytometry panels, and complex biological heterogeneity.",
    pitfalls = "Global distances between distant clusters in t-SNE are generally uninterpretable (cluster spacing does not reflect linear relatedness). Cluster density and size are sensitive to the chosen perplexity parameter.",
    journal_archetype = "Nature and Cell landmark single-cell atlas visualizations.",
    data_recipe = "Requires high-dimensional numerical feature matrix (samples × features) and sample cluster annotations."
  )
}
