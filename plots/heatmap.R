# Publication Matrix Heatmap
# Tier 1 (Simple) - Matrix

make_plot <- function(data_dir = "data") {
  library(ggplot2)
  if (file.exists("theme_journal.R")) source("theme_journal.R", local = TRUE)
  
  mat_raw <- read.csv(file.path(data_dir, "mat.csv"), row.names = 1, check.names = FALSE)
  
  # Select a compact 25 gene x 15 sample submatrix for sharp visualization
  sub_mat <- as.matrix(mat_raw[1:25, 1:15])
  df <- expand.grid(Gene = rownames(sub_mat), Sample = colnames(sub_mat))
  df$Expression <- as.vector(sub_mat)
  
  p <- ggplot(df, aes(x = Sample, y = Gene, fill = Expression)) +
    geom_tile(color = "#FFFFFF", linewidth = 0.35) +
    scale_fill_gradient2(
      low = "#3C5488",
      mid = "#F7F7F7",
      high = "#E64B35",
      midpoint = 0,
      name = "z-score"
    ) +
    labs(
      title = "Transcriptomic Expression Matrix Across Replicates",
      subtitle = "Color intensity reflects standardized gene abundance (z-score centered at baseline)",
      x = "Experimental Biological Sample",
      y = "Feature / Gene Symbol",
      caption = "Source: Simulated multi-condition transcriptomic matrix"
    )
  
  if (exists("theme_topjournal")) {
    p <- p + theme_topjournal(grid = "none") +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1, size = rel(0.8)),
        axis.text.y = element_text(size = rel(0.75)),
        legend.position = "right"
      )
  } else {
    p <- p + theme_minimal()
  }
  p
}

explain <- function() {
  list(
    title = "High-Density Matrix Heatmap",
    how_to_read = "A two-dimensional grid where rows represent biological features (genes, metabolites, proteins) and columns represent individual samples. The color of each tile indicates standardized continuous abundance or activity relative to baseline.",
    when_to_use = "Essential for omics profiling, differential expression, correlation matrices, and perturbation screens across high-dimensional matrices.",
    pitfalls = "Choice of color palette is critical: rainbow/jet palettes produce perceptual distortion artifacts; use perceptually balanced diverging or sequential gradients. Always specify whether data has been z-score standardized by row or column.",
    journal_archetype = "Cell and Nature Genetics core omics expression figures.",
    data_recipe = "Requires a numeric 2D matrix (features × samples) or a long dataframe with row, col, and value."
  )
}
