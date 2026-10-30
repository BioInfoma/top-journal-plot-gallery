# Uniform Manifold Approximation and Projection (UMAP)
# Tier 2 (Intermediate) - Dimension reduction

make_plot <- function(data_dir = "data") {
  library(ggplot2)
  if (file.exists("theme_journal.R")) source("theme_journal.R", local = TRUE)
  
  mat_raw <- read.csv(file.path(data_dir, "mat.csv"), row.names = 1, check.names = FALSE)
  meta <- read.csv(file.path(data_dir, "mat_meta.csv"))
  mat_t <- t(mat_raw)
  
  set.seed(42)
  if (requireNamespace("uwot", quietly = TRUE)) {
    umap_res <- uwot::umap(mat_t, n_neighbors = 8, min_dist = 0.3, n_components = 2)
    df_umap <- data.frame(
      UMAP1 = umap_res[, 1],
      UMAP2 = umap_res[, 2],
      sample = rownames(mat_t)
    )
  } else {
    pca_res <- prcomp(mat_t)
    df_umap <- data.frame(UMAP1 = pca_res$x[, 1], UMAP2 = pca_res$x[, 2], sample = rownames(mat_t))
  }
  
  df_umap <- merge(df_umap, meta, by = "sample")
  
  p <- ggplot(df_umap, aes(x = UMAP1, y = UMAP2, color = condition, fill = condition)) +
    stat_ellipse(geom = "polygon", alpha = 0.16, level = 0.90, linewidth = 0.5) +
    geom_point(size = 3.8, shape = 21, stroke = 0.45, color = "#222222", alpha = 0.9) +
    labs(
      title = "UMAP Manifold Projection of Cellular Profiles",
      subtitle = "Non-linear manifold preservation balancing both local cluster fidelity and global topological continuity",
      x = "UMAP Dimension 1",
      y = "UMAP Dimension 2",
      color = "Condition",
      fill = "Condition",
      caption = "Source: Simulated transcriptomic profiling matrix (30 samples x 200 features)"
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
    title = "Uniform Manifold Approximation and Projection (UMAP)",
    how_to_read = "UMAP models high-dimensional data as a Riemannian manifold, preserving both fine-grained local neighborhoods and meaningful broad global topology in 2D space. Points that group closely share similar multi-gene expression patterns.",
    when_to_use = "The premier dimension-reduction standard in modern single-cell genomics, spatial transcriptomics, and multi-omics profiling pipelines (Seurat, Scanpy).",
    pitfalls = "Do not over-interpret empty spaces or distances between distant clusters as quantitative metrics. Always evaluate multiple neighbor sizes (`n_neighbors`) to avoid artificial fragmentation.",
    journal_archetype = "Cell and Nature Biotechnology single-cell transcriptomic atlases.",
    data_recipe = "Requires high-dimensional numerical expression matrix and cluster/phenotype annotations."
  )
}
