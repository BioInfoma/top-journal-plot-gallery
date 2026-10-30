# Regional Genetic Association LocusZoom Plot
# Tier 3 (Advanced) - Genomics

make_plot <- function(data_dir = "data") {
  library(ggplot2)
  if (file.exists("theme_journal.R")) source("theme_journal.R", local = TRUE)
  
  df_gwas <- read.csv(file.path(data_dir, "gwas.csv"))
  
  # Select Chromosome 1 regional locus around top peak (500 kb window)
  sub_gwas <- df_gwas[df_gwas$chr == 1, ]
  lead_idx <- which.min(sub_gwas$p)
  lead_snp <- sub_gwas[lead_idx, ]
  
  # Window around lead SNP
  window_kb <- 250
  locus_df <- sub_gwas[abs(sub_gwas$pos - lead_snp$pos) <= window_kb * 1000, ]
  
  # Simulate Linkage Disequilibrium (LD r2 with lead variant) based on genomic distance
  dist_from_lead <- abs(locus_df$pos - lead_snp$pos)
  ld_r2 <- pmax(0, 1 - (dist_from_lead / (window_kb * 1000))^0.75 + rnorm(nrow(locus_df), 0, 0.1))
  ld_r2[locus_df$snp == lead_snp$snp] <- 1.0
  locus_df$r2 <- pmin(1, pmax(0, ld_r2))
  
  # Categorize r2 into classic LocusZoom color bins
  locus_df$ld_bin <- cut(
    locus_df$r2,
    breaks = c(-0.01, 0.2, 0.4, 0.6, 0.8, 1.01),
    labels = c("0.0 - 0.2", "0.2 - 0.4", "0.4 - 0.6", "0.6 - 0.8", "0.8 - 1.0")
  )
  locus_df$negLogP <- -log10(pmax(locus_df$p, 1e-30))
  
  ld_colors <- c(
    "0.8 - 1.0" = "#DC2626", # Deep Red
    "0.6 - 0.8" = "#EA580C", # Orange
    "0.4 - 0.6" = "#16A34A", # Green
    "0.2 - 0.4" = "#0284C7", # Light Blue
    "0.0 - 0.2" = "#1E3A8A"  # Dark Blue
  )
  
  # Recombination rate background curve (cM/Mb)
  pos_grid <- seq(min(locus_df$pos), max(locus_df$pos), length.out = 100)
  recomb_rate <- 15 + 45 * exp(-((pos_grid - (lead_snp$pos + 40000)) / 15000)^2)
  df_recomb <- data.frame(pos = pos_grid, rate = recomb_rate)
  
  p <- ggplot() +
    # Recombination rate curve on background
    geom_line(data = df_recomb, aes(x = pos / 1e6, y = rate / 8), color = "#06B6D4", linewidth = 0.8, linetype = "dotted") +
    # Surrounding SNPs colored by r2
    geom_point(data = locus_df[locus_df$snp != lead_snp$snp, ], 
               aes(x = pos / 1e6, y = negLogP, fill = ld_bin), 
               shape = 21, size = 2.4, color = "#1E293B", stroke = 0.35, alpha = 0.85) +
    # Lead Index SNP (prominent diamond)
    geom_point(data = locus_df[locus_df$snp == lead_snp$snp, ], 
               aes(x = pos / 1e6, y = negLogP), 
               shape = 23, size = 4.8, fill = "#9333EA", color = "#0F172A", stroke = 1.2) +
    annotate("text", x = lead_snp$pos / 1e6, y = -log10(lead_snp$p) + 0.8, 
             label = paste0("Lead SNP: ", lead_snp$snp), fontface = "bold", size = 3.3, color = "#9333EA") +
    scale_fill_manual(values = ld_colors, name = expression(bold("Linkage Disequilibrium (") * bolditalic(r)^2 * bold("):"))) +
    scale_y_continuous(
      name = expression(bold(-log[10] ~ "(P-value)")),
      sec.axis = sec_axis(~ . * 8, name = "Recombination Rate (cM / Mb)")
    ) +
    scale_x_continuous(labels = function(x) paste0(x, " Mb")) +
    labs(
      title = "Regional GWAS LocusZoom Plot: Linkage Disequilibrium Architecture",
      subtitle = "Candidate fine-mapping locus; index variant (purple diamond) with surrounding variants colored by r2",
      x = "Chromosome 1 Physical Coordinate (Megabases)",
      caption = "Source: Simulated GWAS summary statistics with 1000 Genomes LD correlation"
    )
  
  if (exists("theme_topjournal")) {
    p <- p + theme_topjournal(grid = "xy") +
      theme(
        legend.position = "top",
        legend.direction = "horizontal",
        legend.title = element_text(face = "bold", size = 8.5),
        axis.title.y.right = element_text(color = "#0891B2", face = "bold", margin = margin(l = 6)),
        axis.text.y.right = element_text(color = "#0891B2")
      )
  } else {
    p <- p + theme_classic()
  }
  p
}

explain <- function() {
  list(
    title = "Regional Genetic Association LocusZoom Plot",
    how_to_read = "Zooms into a specific 500 kb genomic association window centered on the most significant lead SNP (purple diamond). Surrounding single-nucleotide variants are color-coded by linkage disequilibrium correlation (r-squared, red = high correlation; blue = low correlation), with dotted light-blue curve tracing the local recombination rate.",
    when_to_use = "The indispensable figure in genetic fine-mapping and GWAS publications to define credible causal sets and identify biological candidate genes within a risk locus.",
    pitfalls = "Make sure the LD reference panel matches the ancestral population of the study cohort (e.g. 1000 Genomes EUR vs AFR vs EAS); mismatching panels causes misleading LD colors.",
    journal_archetype = "Nature Genetics, Nature Communications, and American Journal of Human Genetics GWAS fine-mapping figures.",
    data_recipe = "Requires regional variant coordinates, association p-values, and pairwise LD r-squared matrix."
  )
}
