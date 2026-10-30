# Publication Volcano Plot for Differential Expression
# Tier 2 (Intermediate) - Omics

make_plot <- function(data_dir = "data") {
  library(ggplot2)
  if (file.exists("theme_journal.R")) source("theme_journal.R", local = TRUE)
  
  df <- read.csv(file.path(data_dir, "de.csv"))
  
  fc_cutoff <- 1.0
  p_cutoff <- 0.05
  
  df$negLog10P <- -log10(pmax(df$padj, 1e-50))
  df$status <- "Not Significant"
  df$status[df$padj < p_cutoff & df$log2FC >= fc_cutoff] <- "Upregulated"
  df$status[df$padj < p_cutoff & df$log2FC <= -fc_cutoff] <- "Downregulated"
  df$status <- factor(df$status, levels = c("Upregulated", "Downregulated", "Not Significant"))
  
  # Identify top 8 most significant genes for direct labeling
  sig_genes <- df[df$status != "Not Significant", ]
  top_genes <- head(sig_genes[order(sig_genes$padj), ], 8)
  
  status_colors <- c("Upregulated" = "#E64B35", "Downregulated" = "#3C5488", "Not Significant" = "#CCCCCC")
  
  p <- ggplot(df, aes(x = log2FC, y = negLog10P, color = status)) +
    geom_point(alpha = 0.6, size = 1.6, shape = 16) +
    scale_color_manual(values = status_colors) +
    geom_vline(xintercept = c(-fc_cutoff, fc_cutoff), linetype = "dashed", color = "#666666", linewidth = 0.45) +
    geom_hline(yintercept = -log10(p_cutoff), linetype = "dashed", color = "#666666", linewidth = 0.45) +
    labs(
      title = "Transcriptomic Volcano Plot (Differential Expression)",
      subtitle = "Significance threshold: FDR < 0.05 (horizontal line); Magnitude threshold: |log2FC| >= 1.0 (vertical lines)",
      x = expression(bold(log[2] ~ "Fold Change")),
      y = expression(bold(-log[10] ~ "Adjusted P-value (FDR)")),
      color = "Regulation Status",
      caption = "Source: Simulated RNA-seq differential gene expression assay (5,000 genes)"
    )
  
  if (requireNamespace("ggrepel", quietly = TRUE) && nrow(top_genes) > 0) {
    p <- p + ggrepel::geom_text_repel(
      data = top_genes,
      aes(label = gene),
      size = 3.2,
      fontface = "bold",
      color = "#111111",
      max.overlaps = 15,
      box.padding = 0.35
    )
  }
  
  if (exists("theme_topjournal")) {
    p <- p + theme_topjournal(grid = "xy")
  } else {
    p <- p + theme_classic()
  }
  p
}

explain <- function() {
  list(
    title = "Differential Expression Volcano Plot",
    how_to_read = "The x-axis measures magnitude of change (log2 fold change), while the y-axis shows statistical significance (-log10 FDR adjusted p-value). Points in the top-left and top-right quadrants represent statistically and biologically significant differentially regulated genes.",
    when_to_use = "Ubiquitous in transcriptomics (RNA-seq), proteomics, and metabolomics to survey thousands of features simultaneously and spot top lead candidates.",
    pitfalls = "Never plot raw p-values without Benjamini-Hochberg FDR correction. Avoid choosing fold-change thresholds arbitrarily without biological rationale.",
    journal_archetype = "Nature, Science, and Cell omics and single-cell differential analysis figures.",
    data_recipe = "Requires feature IDs, log2 fold change, and multiple-testing adjusted p-values."
  )
}
