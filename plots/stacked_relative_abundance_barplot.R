# Stacked 100% Relative Abundance Barplot (Microbiome Taxonomy)
# Tier 3 (Advanced) - Microbiome

make_plot <- function(data_dir = "data") {
  library(ggplot2)
  if (file.exists("theme_journal.R")) source("theme_journal.R", local = TRUE)
  
  abund_raw <- read.csv(file.path(data_dir, "abund.csv"), row.names = 1, check.names = FALSE)
  meta <- read.csv(file.path(data_dir, "abund_meta.csv"))
  rownames(meta) <- meta$sample
  
  # Calculate relative proportions per sample
  rel_abund <- sweep(as.matrix(abund_raw), 2, colSums(abund_raw), "/")
  
  # Group lower abundance taxa into 'Other' to maintain publication clarity
  taxa_mean <- rowMeans(rel_abund)
  top_taxa <- names(sort(taxa_mean, decreasing = TRUE)[1:7])
  
  df_long <- expand.grid(Taxon = rownames(rel_abund), sample = colnames(rel_abund), stringsAsFactors = FALSE)
  df_long$Proportion <- as.vector(rel_abund)
  df_long$TaxonGroup <- ifelse(df_long$Taxon %in% top_taxa, df_long$Taxon, "Other Taxa")
  df_long$TaxonGroup <- factor(df_long$TaxonGroup, levels = c("Other Taxa", rev(top_taxa)))
  
  df_long <- merge(df_long, meta, by = "sample")
  # Subset to 16 representative samples (8 Healthy, 8 Disease)
  sub_samples <- c(head(meta$sample[meta$group == "Healthy"], 8), head(meta$sample[meta$group == "Disease"], 8))
  df_sub <- df_long[df_long$sample %in% sub_samples, ]
  
  taxa_palette <- c(
    "#94A3B8", # Other Taxa (Gray)
    "#E64B35", "#4DBBD5", "#00A087", "#3C5488", "#F39B7F", "#8491B4", "#DC0000"
  )
  
  p <- ggplot(df_sub, aes(x = sample, y = Proportion, fill = TaxonGroup)) +
    geom_col(width = 0.82, color = "#FFFFFF", linewidth = 0.3) +
    facet_grid(~ group, scales = "free_x", space = "free_x") +
    scale_fill_manual(values = taxa_palette, name = "Phylum / Genus:") +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1), expand = c(0, 0)) +
    guides(fill = guide_legend(nrow = 2, byrow = TRUE)) +
    labs(
      title = "Microbial Community Taxonomic Composition",
      subtitle = "100% stacked relative abundance comparing Healthy vs Disease cohorts",
      x = "Individual Sequencing Sample",
      y = "Relative Abundance Proportion (%)",
      caption = "Source: Simulated 16S rRNA taxonomic profiling survey (N = 16 samples)"
    )
  
  if (exists("theme_topjournal")) {
    p <- p + theme_topjournal(grid = "y") +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1, size = rel(0.85), face = "bold"),
        legend.position = "top",
        legend.direction = "horizontal",
        legend.title = element_text(face = "bold", size = 8.5),
        legend.text = element_text(size = 8.5),
        strip.text = element_text(face = "bold", size = rel(0.95))
      )
  } else {
    p <- p + theme_classic()
  }
  p
}

explain <- function() {
  list(
    title = "100% Stacked Taxonomic Abundance Barplot",
    how_to_read = "Each column represents an individual metagenomic sample normalized to 100% total composition. Colored segments indicate the relative proportions of dominant microbial taxa, grouped by clinical condition.",
    when_to_use = "Universal figure in microbiome, environmental metagenomics, and cell-type deconvolution studies to convey community shifts (e.g. dysbiosis, bloom of pathogens).",
    pitfalls = "Avoid displaying more than 8-10 distinct color categories; group minor or low-abundance taxa into 'Other' to prevent visual chaos. Stacking order must remain strictly consistent across samples.",
    journal_archetype = "Nature Microbiology and Microbiome comparative community profiles.",
    data_recipe = "Requires taxonomic abundance count matrix and sample metadata."
  )
}
