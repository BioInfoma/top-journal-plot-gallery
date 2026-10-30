# MA Plot (Minus vs Average) for Differential Expression
# Tier 2 (Intermediate) - Omics

make_plot <- function(data_dir = "data") {
  library(ggplot2)
  if (file.exists("theme_journal.R")) source("theme_journal.R", local = TRUE)
  
  df <- read.csv(file.path(data_dir, "de.csv"))
  p_cutoff <- 0.05
  
  df$status <- "Non-significant"
  df$status[df$padj < p_cutoff & df$log2FC > 0] <- "Upregulated"
  df$status[df$padj < p_cutoff & df$log2FC < 0] <- "Downregulated"
  df$status <- factor(df$status, levels = c("Upregulated", "Downregulated", "Non-significant"))
  
  status_colors <- c("Upregulated" = "#E64B35", "Downregulated" = "#3C5488", "Non-significant" = "#B0BEC5")
  
  p <- ggplot(df, aes(x = baseMean, y = log2FC, color = status)) +
    geom_point(alpha = 0.55, size = 1.6, shape = 16) +
    geom_hline(yintercept = 0, color = "#222222", linewidth = 0.6) +
    geom_hline(yintercept = c(-1, 1), linetype = "dashed", color = "#666666", linewidth = 0.45) +
    scale_x_log10(
      breaks = scales::trans_breaks("log10", function(x) 10^x),
      labels = scales::trans_format("log10", scales::math_format(10^.x))
    ) +
    scale_color_manual(values = status_colors) +
    labs(
      title = "MA Plot: Transcriptomic Ratio vs Mean Abundance",
      subtitle = expression(bold("M") ~ "(log"[2] ~ "Fold Change) plotted against" ~ bold("A") ~ "(Mean Normalized Counts)"),
      x = expression(bold("Mean Normalized Expression (A, log"[10] ~ "scale)")),
      y = expression(bold("log"[2] ~ "Fold Change (M)")),
      color = "FDR < 0.05",
      caption = "Source: Simulated differential transcriptomic count matrix (5,000 genes)"
    )
  
  if (exists("theme_topjournal")) {
    p <- p + theme_topjournal(grid = "xy")
  } else {
    p <- p + theme_classic()
  }
  p
}

explain <- function() {
  list(
    title = "MA Plot (Minus vs Average)",
    how_to_read = "The horizontal axis represents mean baseline expression intensity (log scale), while the vertical axis represents log2 fold change between experimental conditions. The center line at y = 0 represents identical expression. Highlighted points denote statistically significant differential abundance.",
    when_to_use = "Standard diagnostic in RNA-seq, microarray, and ChIP-seq analysis (DESeq2/edgeR) to evaluate normalization quality and detect intensity-dependent variance trends.",
    pitfalls = "Look out for asymmetric 'flaring' or systematic non-zero centering at low expression counts, which indicates inadequate normalization or sequencing depth bias.",
    journal_archetype = "Genome Biology and Nature Methods RNA-seq normalization benchmark figures.",
    data_recipe = "Requires base mean normalized expression, log2 fold change, and FDR adjusted p-values."
  )
}
