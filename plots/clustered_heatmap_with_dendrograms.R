# Clustered Heatmap with Dual Hierarchical Dendrograms
# Tier 2 (Intermediate) - Matrix

make_plot <- function(data_dir = "data") {
  mat_raw <- read.csv(file.path(data_dir, "mat.csv"), row.names = 1, check.names = FALSE)
  meta <- read.csv(file.path(data_dir, "mat_meta.csv"))
  rownames(meta) <- meta$sample
  
  # Select top 28 most variable features across samples for crisp dendrogram visualization
  gene_vars <- apply(mat_raw, 1, var)
  top_mat <- as.matrix(mat_raw[order(gene_vars, decreasing = TRUE)[1:28], 1:24])
  
  anno_col <- data.frame(Condition = meta[colnames(top_mat), "condition"])
  rownames(anno_col) <- colnames(top_mat)
  
  anno_colors <- list(
    Condition = c("Ctrl" = "#00A087", "Treat1" = "#E64B35", "Treat2" = "#3C5488")
  )
  
  heat_colors <- colorRampPalette(c("#1D4ED8", "#F8FAFC", "#DC2626"))(100)
  
  if (requireNamespace("pheatmap", quietly = TRUE)) {
    p <- pheatmap::pheatmap(
      top_mat,
      color = heat_colors,
      scale = "row",
      clustering_distance_rows = "correlation",
      clustering_distance_cols = "euclidean",
      clustering_method = "complete",
      annotation_col = anno_col,
      annotation_colors = anno_colors,
      show_colnames = TRUE,
      show_rownames = TRUE,
      fontsize = 8,
      fontsize_row = 7.5,
      fontsize_col = 8,
      main = "Hierarchical Bi-Clustered Heatmap with Dual Dendrograms",
      silent = TRUE
    )
    return(p$gtable)
  } else {
    heatmap(top_mat, scale = "row", col = heat_colors)
  }
}

explain <- function() {
  list(
    title = "Hierarchical Bi-Clustered Heatmap with Dendrograms",
    how_to_read = "Rows (genes/features) and columns (samples) are permuted according to hierarchical clustering trees (dendrograms) branching along the top and left margins. Color intensity reflects row-standardized z-scores (diverging from baseline), revealing co-regulated gene modules and sample subclasses.",
    when_to_use = "Essential for unsupervised discovery of molecular subtypes, co-expression modules, biomarker signatures, and treatment response patterns in high-throughput omics.",
    pitfalls = "Clustering algorithms are sensitive to the distance metric (Euclidean vs Pearson correlation) and linkage criteria (complete vs Ward). Always specify whether features were z-score scaled prior to clustering.",
    journal_archetype = "The landmark Eisen et al. PNAS 1998 and Cancer Cell molecular classification heatmaps.",
    data_recipe = "Requires numeric expression matrix with sample annotation metadata."
  )
}
