# Codon Usage Bias Analysis (ENC vs GC3 Wright Curve)
# Tier 4 (Mind-Blowing) - Evolution

make_plot <- function(data_dir = "data") {
  library(ggplot2)
  if (file.exists("theme_journal.R")) source("theme_journal.R", local = TRUE)
  
  # Wright's theoretical null curve: ENC = 2 + s + 29 / (s^2 + (1 - s)^2) where s = GC3
  s_seq <- seq(0.05, 0.95, length.out = 100)
  enc_null <- 2 + s_seq + 29 / (s_seq^2 + (1 - s_seq)^2)
  df_null <- data.frame(gc3 = s_seq, enc = enc_null)
  
  # Simulate 300 genes from a bacterial / viral genome
  set.seed(42)
  n_genes <- 300
  gc3_vals <- rnorm(n_genes, 0.52, 0.12)
  gc3_vals <- pmin(0.92, pmax(0.12, gc3_vals))
  
  # Genes subjected to translational selection fall noticeably BELOW the null curve
  selection_prob <- runif(n_genes)
  enc_vals <- 2 + gc3_vals + 29 / (gc3_vals^2 + (1 - gc3_vals)^2)
  # Depress ENC for highly expressed genes (strong codon selection)
  enc_vals <- enc_vals - ifelse(selection_prob > 0.6, runif(n_genes, 6, 18), rnorm(n_genes, 1, 2))
  enc_vals <- pmin(61, pmax(21, enc_vals))
  
  gene_class <- ifelse(enc_vals < (2 + gc3_vals + 29 / (gc3_vals^2 + (1 - gc3_vals)^2) - 5),
                       "Translational Selection Pressure", "Neutral Mutational Mutation")
  
  df_genes <- data.frame(
    gc3 = gc3_vals,
    enc = enc_vals,
    class = factor(gene_class, levels = c("Translational Selection Pressure", "Neutral Mutational Mutation"))
  )
  
  p <- ggplot() +
    # Wright's theoretical neutral evolution boundary curve
    geom_line(data = df_null, aes(x = gc3, y = enc), color = "#2563EB", linewidth = 1.15, linetype = "solid") +
    # Empirical gene values
    geom_point(data = df_genes, aes(x = gc3, y = enc, color = class, fill = class), 
               shape = 21, size = 2.4, stroke = 0.35, alpha = 0.8) +
    scale_color_manual(values = c("Translational Selection Pressure" = "#DC2626", "Neutral Mutational Mutation" = "#64748B"), name = "Evolutionary Regime:") +
    scale_fill_manual(values = c("Translational Selection Pressure" = "#DC2626", "Neutral Mutational Mutation" = "#94A3B8"), name = "Evolutionary Regime:") +
    annotate("text", x = 0.52, y = 60, label = "Wright's Theoretical Neutral Evolution Curve", 
             fontface = "bold", color = "#1D4ED8", size = 3.3) +
    annotate("text", x = 0.52, y = 33, label = "Genes under Strong Codon Selection", 
             fontface = "bold", color = "#DC2626", size = 3.2) +
    scale_x_continuous(breaks = seq(0.1, 0.9, by = 0.2), limits = c(0.08, 0.95)) +
    scale_y_continuous(breaks = seq(20, 61, by = 10), limits = c(20, 63)) +
    labs(
      title = "Codon Usage Bias: ENC vs GC3s Evolutionary Landscape",
      subtitle = "Points below the blue curve indicate natural selection for translational efficiency over mutation drift",
      x = "GC Content at Synonymous Third Codon Position (GC3s)",
      y = "Effective Number of Codons (ENC, 20 = Max Bias, 61 = Random)",
      caption = "Source: Simulated viral / microbial coding sequence genome (N = 300 genes)"
    )
  
  if (exists("theme_topjournal")) {
    p <- p + theme_topjournal(grid = "xy") +
      theme(
        legend.position = "top",
        legend.direction = "horizontal",
        legend.title = element_text(face = "bold", size = 8.5)
      )
  } else {
    p <- p + theme_classic()
  }
  p
}

explain <- function() {
  list(
    title = "Codon Usage Bias ENC vs GC3 Evolutionary Plot",
    how_to_read = "The horizontal axis shows GC frequency at the third synonymous codon position (GC3s). The vertical axis tracks the Effective Number of Codons (ENC, ranging from 20 for extreme bias to 61 for equal codon use). The solid blue line denotes Wright's null hypothesis of neutral mutation drift; genes falling well below the curve are constrained by natural selection for translational speed and accuracy.",
    when_to_use = "Essential in viral genomics, molecular evolution, and synthetic biology to distinguish mutational GC pressure from host-adaptation translational selection.",
    pitfalls = "Short genes (< 100 codons) exhibit high stochastic variance in ENC; filter out short coding sequences to prevent artificial dispersion.",
    journal_archetype = "Molecular Biology and Evolution and Journal of Virology codon adaptation studies.",
    data_recipe = "Requires coding sequence FASTA with calculated ENC and GC3s statistics per gene."
  )
}
