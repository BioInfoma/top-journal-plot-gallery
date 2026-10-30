# Cancer Genomic Oncoplot (Mutation Waterfall Matrix)
# Tier 3 (Advanced) - Genomics

make_plot <- function(data_dir = "data") {
  library(ggplot2)
  if (file.exists("theme_journal.R")) source("theme_journal.R", local = TRUE)
  
  # Simulate 10 key cancer driver genes across 30 tumor samples (maftools archetype)
  genes <- c("TP53", "KRAS", "PIK3CA", "EGFR", "APC", "PTEN", "ARID1A", "BRAF", "BRCA2", "ATM")
  samples <- paste0("Tumor_", sprintf("%02d", 1:30))
  
  set.seed(42)
  mut_types <- c("Missense", "Nonsense", "Frameshift", "Multi-hit", "Wildtype")
  probs <- c(0.18, 0.08, 0.06, 0.03, 0.65)
  
  df_grid <- expand.grid(Gene = genes, Sample = samples, stringsAsFactors = FALSE)
  df_grid$Mutation <- sample(mut_types, nrow(df_grid), replace = TRUE, prob = probs)
  
  # Ensure TP53 and KRAS have high realistic prevalence
  df_grid$Mutation[df_grid$Gene == "TP53" & runif(nrow(df_grid)) < 0.6] <- "Missense"
  df_grid$Mutation[df_grid$Gene == "KRAS" & runif(nrow(df_grid)) < 0.4] <- "Missense"
  
  # Calculate per-gene mutation frequencies
  gene_freq <- aggregate(Mutation != "Wildtype" ~ Gene, data = df_grid, mean)
  colnames(gene_freq)[2] <- "Pct"
  gene_freq <- gene_freq[order(gene_freq$Pct), ]
  df_grid$Gene <- factor(df_grid$Gene, levels = gene_freq$Gene)
  
  mut_colors <- c(
    "Missense" = "#2563EB",       # Blue
    "Nonsense" = "#DC2626",       # Red
    "Frameshift" = "#D97706",     # Amber
    "Multi-hit" = "#9333EA",      # Purple
    "Wildtype" = "#F1F5F9"        # Soft Gray
  )
  
  p <- ggplot(df_grid, aes(x = Sample, y = Gene, fill = Mutation)) +
    geom_tile(color = "#FFFFFF", linewidth = 0.55) +
    scale_fill_manual(values = mut_colors, name = "Somatic Alteration:") +
    labs(
      title = "Pan-Cancer Oncoplot: Recurrent Driver Mutation Landscape",
      subtitle = "Waterfall matrix of somatic alterations across tumor cohorts; rows ranked by alteration prevalence",
      x = "Individual Tumor Sample (N = 30)",
      y = "Cancer Driver Gene",
      caption = "Source: Simulated whole-exome somatic variant catalog (TCGA / maftools standard)"
    )
  
  if (exists("theme_topjournal")) {
    p <- p + theme_topjournal(grid = "none") +
      theme(
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.y = element_text(face = "bold", size = rel(0.95)),
        legend.position = "top",
        legend.direction = "horizontal",
        legend.title = element_text(face = "bold", size = 8.5)
      )
  } else {
    p <- p + theme_minimal()
  }
  p
}

explain <- function() {
  list(
    title = "Cancer Genomic Oncoplot (Mutation Matrix)",
    how_to_read = "A matrix where rows represent driver genes and columns represent individual tumor samples. Color-coded tiles indicate the specific class of somatic alteration (e.g. missense, nonsense, frameshift), while rows are ranked from most frequently mutated at the top to least frequently mutated.",
    when_to_use = "The universally required summary graphic in cancer whole-exome sequencing papers (TCGA, ICGC, MSK-IMPACT) to demonstrate mutational landscape, mutual exclusivity, and co-occurrence.",
    pitfalls = "Sort patients hierarchically by gene mutation status (waterfall sorting) rather than random patient IDs so mutational patterns and exclusivity stand out clearly.",
    journal_archetype = "Cell, Cancer Cell, and Nature Genetics pan-cancer landscape papers (maftools standard).",
    data_recipe = "Requires Mutation Annotation Format (MAF) table with gene symbols, tumor barcodes, and variant classifications."
  )
}
