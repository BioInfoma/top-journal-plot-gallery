# Hierarchical Compositional Treemap (Taxonomic Tree)
# Tier 3 (Advanced) - Composition

make_plot <- function(data_dir = "data") {
  library(ggplot2)
  if (file.exists("theme_journal.R")) source("theme_journal.R", local = TRUE)
  
  # Simulate hierarchical microbiome phylum-to-genus abundance tree
  df_tree <- data.frame(
    phylum = c(rep("Firmicutes", 4), rep("Bacteroidota", 3), rep("Actinomycetota", 2), rep("Pseudomonadota", 2)),
    genus = c("Faecalibacterium", "Ruminococcus", "Clostridium", "Lactobacillus",
              "Bacteroides", "Prevotella", "Parabacteroides",
              "Bifidobacterium", "Collinsella",
              "Escherichia", "Klebsiella"),
    abundance = c(28, 18, 14, 10,  24, 16, 8,  12, 6,  9, 5)
  )
  
  # Calculate treemap rectangle coordinates (squarified / partitioned layout)
  # Standardized 2D grid [0, 100] x [0, 100]
  # Phylum 1: Firmicutes (70 total) -> x: 0 to 55, y: 0 to 100
  # Phylum 2: Bacteroidota (48 total) -> x: 55 to 100, y: 40 to 100
  # Phylum 3/4: Actinomycetota (18) & Pseudomonadota (14) -> x: 55 to 100, y: 0 to 40
  
  tiles <- data.frame(
    genus = df_tree$genus,
    phylum = df_tree$phylum,
    abundance = df_tree$abundance,
    xmin = c(0, 0, 0, 0,    55, 55, 55,  55, 75,  55, 80),
    xmax = c(55, 55, 55, 55, 78, 100, 100, 75, 100, 80, 100),
    ymin = c(60, 35, 15, 0,  40, 65, 40,  20, 20,  0, 0),
    ymax = c(100, 60, 35, 15, 100, 100, 65, 40, 40,  20, 20)
  )
  tiles$label <- paste0(tiles$genus, "\n(", tiles$abundance, "%)")
  
  phylum_colors <- c(
    "Firmicutes" = "#1D4ED8",
    "Bacteroidota" = "#059669",
    "Actinomycetota" = "#D97706",
    "Pseudomonadota" = "#DC2626"
  )
  
  p <- ggplot(tiles) +
    geom_rect(aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = phylum), 
              color = "#FFFFFF", linewidth = 1.0, alpha = 0.88) +
    geom_text(aes(x = (xmin + xmax) / 2, y = (ymin + ymax) / 2, label = label), 
              color = "#FFFFFF", fontface = "bold", size = 2.9, lineheight = 0.85) +
    scale_fill_manual(values = phylum_colors, name = "Phylum Group:") +
    scale_x_continuous(expand = c(0, 0)) +
    scale_y_continuous(expand = c(0, 0)) +
    coord_fixed(ratio = 1) +
    labs(
      title = "Hierarchical Treemap: Taxonomic Community Composition",
      subtitle = "Nested rectangular partitions represent relative taxon abundance proportions within phyla",
      caption = "Source: Simulated metagenomic gut microbiome profiling survey (N = 11 dominant genera)"
    ) +
    theme_void() +
    theme(
      plot.title = element_text(face = "bold", size = 13, color = "#0F172A", margin = margin(b = 4)),
      plot.subtitle = element_text(size = 9.5, color = "#475569", margin = margin(b = 10)),
      plot.caption = element_text(size = 8, color = "#94A3B8", hjust = 0),
      legend.position = "top",
      legend.direction = "horizontal",
      legend.title = element_text(face = "bold", size = 8.5),
      legend.text = element_text(size = 8.5),
      plot.margin = margin(12, 14, 12, 12)
    )
  p
}

explain <- function() {
  list(
    title = "Hierarchical Compositional Treemap",
    how_to_read = "A rectangular display of nested categories where the area of each nested box corresponds strictly to its quantitative proportion (percentage of the whole). Bounded colors denote major parent clades (Phyla), partitioned into sub-boxes for constituent child genera.",
    when_to_use = "Superb for presenting deeply nested compositions (e.g. microbiome taxonomy, whole-genome functional gene categories, public budget allocations) without polar angle perception distortions.",
    pitfalls = "Very small lower-abundance sub-categories can become too compressed for readable text labels; group trace elements into an aggregated category.",
    journal_archetype = "Microbiome and ISME Journal taxonomic hierarchical abundance overviews.",
    data_recipe = "Requires hierarchical parent-child category labels and numerical abundance weights."
  )
}
