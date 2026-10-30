# Gene Ontology / Pathway Enrichment Dot Plot
# Tier 2 (Intermediate) - Omics

make_plot <- function(data_dir = "data") {
  library(ggplot2)
  if (file.exists("theme_journal.R")) source("theme_journal.R", local = TRUE)
  
  # Representative biological pathway enrichment results
  pathways <- c(
    "Interferon-α/β Signaling",
    "G1/S Cell Cycle Transition",
    "DNA Repair & Homologous Recomb.",
    "Oxidative Phosphorylation",
    "TNF-α Signaling via NF-κB",
    "Extracellular Matrix Assembly",
    "Hypoxia & Glycolytic Response",
    "Apoptosis & Programmed Death",
    "Innate Immune Cascade"
  )
  
  counts <- c(48, 42, 38, 35, 31, 28, 25, 22, 18)
  gene_ratios <- c(0.18, 0.16, 0.14, 0.13, 0.11, 0.10, 0.09, 0.08, 0.06)
  padj <- c(1.2e-12, 3.4e-10, 8.1e-9, 2.5e-7, 1.1e-6, 4.3e-5, 1.8e-4, 5.2e-4, 1.2e-3)
  
  df_enrich <- data.frame(
    pathway = factor(pathways, levels = rev(pathways)),
    count = counts,
    gene_ratio = gene_ratios,
    padj = padj,
    negLogP = -log10(padj)
  )
  
  p <- ggplot(df_enrich, aes(x = gene_ratio, y = pathway, color = negLogP, size = count)) +
    geom_point(alpha = 0.9, stroke = 0.5) +
    scale_color_gradient(
      low = "#3B82F6",
      high = "#DC2626",
      name = expression(bold(-log[10] ~ "(FDR)"))
    ) +
    scale_size_continuous(range = c(3.5, 9.0), name = "Overlap Count") +
    scale_x_continuous(labels = scales::percent_format(accuracy = 1)) +
    labs(
      title = "Functional Pathway Enrichment (Over-Representation Analysis)",
      subtitle = "Dot area mirrors leading-edge gene count; color reflects statistical significance (-log10 FDR)",
      x = "Gene Ratio (Enriched Features / Pathway Size)",
      y = "Biological Pathway",
      caption = "Source: Simulated differential expression pathway over-representation analysis"
    )
  
  if (exists("theme_topjournal")) {
    p <- p + theme_topjournal(grid = "xy") +
      theme(
        axis.text.y = element_text(face = "bold", size = rel(0.9)),
        legend.position = "top",
        legend.direction = "horizontal",
        legend.box = "horizontal",
        legend.title = element_text(face = "bold", size = 8.5),
        legend.text = element_text(size = 8.5)
      )
  } else {
    p <- p + theme_classic()
  }
  p
}

explain <- function() {
  list(
    title = "Pathway / Gene Ontology Enrichment Dot Plot",
    how_to_read = "Enriched functional terms or biological pathways are listed along the vertical axis. The horizontal coordinate indicates the Gene Ratio (fraction of pathway genes found in the experimental list). Point area mirrors the absolute number of matched genes, and color saturation denotes statistical significance (-log10 FDR).",
    when_to_use = "The universal standard in transcriptomics, proteomics, and functional genomics to translate long gene lists into biological mechanisms (GO, KEGG, Reactome, MSigDB).",
    pitfalls = "Avoid reporting raw p-values without FDR adjustment, which produces rampant false positives in pathways with extensive overlapping genes. Cluster redundant terms to prevent pathway bloat.",
    journal_archetype = "Cell and Nature Reviews Molecular Cell Biology pathway analysis figures (clusterProfiler standard).",
    data_recipe = "Requires pathway names, gene ratios, count of overlap genes, and FDR adjusted p-values."
  )
}
