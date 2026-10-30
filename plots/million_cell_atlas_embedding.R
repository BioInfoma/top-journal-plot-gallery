# Million-Cell Whole-Organism Atlas Embedding (Human Cell Atlas Style)
# Tier 4 (Mind-Blowing) - Single-cell

make_plot <- function(data_dir = "data") {
  library(ggplot2)
  if (file.exists("theme_journal.R")) source("theme_journal.R", local = TRUE)
  
  # Simulate 1,200 single cells representing an ultra-dense whole-organism atlas
  set.seed(42)
  n <- 1200
  
  # 8 major organ lineage clusters
  organs <- c("Immune / Hematopoietic", "Neuronal / Glial", "Epithelial Gut", 
              "Endothelial Vascular", "Stromal Fibroblasts", "Hepatocytes", 
              "Cardiomyocytes", "Rare Stem Progenitors")
  
  cluster_centers <- data.frame(
    organ = organs,
    cx = c(-8, 5, 2, -4, 6, -1, -6, 0),
    cy = c(4, 7, -6, -5, -2, -8, -1, 1)
  )
  
  cell_list <- list()
  for (i in 1:8) {
    n_k <- ifelse(i == 8, 40, sample(120:180, 1))
    cx <- cluster_centers$cx[i]
    cy <- cluster_centers$cy[i]
    cell_list[[i]] <- data.frame(
      UMAP1 = rnorm(n_k, cx, 1.15),
      UMAP2 = rnorm(n_k, cy, 1.15),
      organ = cluster_centers$organ[i]
    )
  }
  df_atlas <- do.call(rbind, cell_list)
  df_atlas$organ <- factor(df_atlas$organ, levels = organs)
  
  organ_colors <- c(
    "Immune / Hematopoietic" = "#E64B35",
    "Neuronal / Glial" = "#3C5488",
    "Epithelial Gut" = "#00A087",
    "Endothelial Vascular" = "#4DBBD5",
    "Stromal Fibroblasts" = "#F39B7F",
    "Hepatocytes" = "#8491B4",
    "Cardiomyocytes" = "#DC0000",
    "Rare Stem Progenitors" = "#7E6148"
  )
  
  p <- ggplot(df_atlas, aes(x = UMAP1, y = UMAP2, color = organ)) +
    # Density contours underneath points
    geom_density_2d(linewidth = 0.35, alpha = 0.5, color = "#94A3B8") +
    geom_point(alpha = 0.65, size = 1.3, stroke = 0) +
    scale_color_manual(values = organ_colors, name = "Tissue Lineage:") +
    guides(color = guide_legend(nrow = 2, override.aes = list(size = 3.5, alpha = 1))) +
    labs(
      title = "Million-Cell Atlas Embedding: Organism-Wide Cellular Diversity",
      subtitle = "Single-cell transcriptomic reference embedding (Human Cell Atlas / Tabula Sapiens standard)",
      x = "Global UMAP Dimension 1",
      y = "Global UMAP Dimension 2",
      caption = "Source: Simulated whole-organism single-cell RNA-seq reference atlas (N = 1,200 representative cells)"
    )
  
  if (exists("theme_topjournal")) {
    p <- p + theme_topjournal(grid = "xy") +
      theme(
        legend.position = "top",
        legend.direction = "horizontal",
        legend.title = element_text(face = "bold", size = 8.5),
        legend.text = element_text(size = 8.0)
      )
  } else {
    p <- p + theme_classic()
  }
  p
}

explain <- function() {
  list(
    title = "Million-Cell Organism-Wide Single-Cell Atlas",
    how_to_read = "A global 2D non-linear manifold projection of hundreds of thousands to millions of single-cell transcriptomes across an entire organism or tissue consortium. Clustered point constellations represent distinct cellular lineages, with background density contours demarcating population densities.",
    when_to_use = "The visual hallmark of major international consortia (Human Cell Atlas, Tabula Sapiens, Fly Cell Atlas) cataloging every cell type in healthy and diseased human physiology.",
    pitfalls = "Overplotting severely obscures rare cell populations (like tissue-resident stem cells); use rasterization (scattermore) and sub-sampling density contours to preserve visibility.",
    journal_archetype = "Science, Nature, and Cell mega-atlas consortium landmark papers.",
    data_recipe = "Requires unified multi-organ single-cell expression matrices and cell-ontology annotations."
  )
}
