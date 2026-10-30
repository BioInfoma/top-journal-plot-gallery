# Genome-Wide Association Study (GWAS) Manhattan Plot
# Tier 2 (Intermediate) - Genomics

make_plot <- function(data_dir = "data") {
  library(ggplot2)
  if (file.exists("theme_journal.R")) source("theme_journal.R", local = TRUE)
  
  df <- read.csv(file.path(data_dir, "gwas.csv"))
  df$chr <- as.integer(df$chr)
  df$negLogP <- -log10(pmax(df$p, 1e-30))
  
  # Calculate cumulative genomic positions across chromosomes
  chr_lengths <- aggregate(pos ~ chr, data = df, max)
  chr_lengths$cum_offset <- c(0, head(cumsum(as.numeric(chr_lengths$pos)), -1))
  
  df <- merge(df, chr_lengths[, c("chr", "cum_offset")], by = "chr")
  df$tot_pos <- df$pos + df$cum_offset
  
  # Compute chromosome midpoint ticks for x-axis
  axis_df <- aggregate(tot_pos ~ chr, data = df, function(v) (min(v) + max(v)) / 2)
  
  # Filter every 2nd tick label for cleaner axis if chromosomes are dense
  axis_df$label <- as.character(axis_df$chr)
  axis_df$label[axis_df$chr %% 2 == 0 & axis_df$chr > 10] <- ""
  
  # Alternating publication palette
  df$color_group <- factor(df$chr %% 2)
  chr_colors <- c("0" = "#3C5488", "1" = "#4DBBD5")
  
  # Identify top genome-wide hits for red highlight
  gwas_sig <- 5e-8
  suggestive_sig <- 1e-5
  df$is_hit <- df$p < gwas_sig
  
  p <- ggplot(df, aes(x = tot_pos, y = negLogP)) +
    geom_point(aes(color = color_group), alpha = 0.65, size = 1.2, shape = 16) +
    geom_point(data = df[df$is_hit, ], color = "#E64B35", size = 2.4, shape = 18) +
    scale_color_manual(values = chr_colors, guide = "none") +
    scale_x_continuous(breaks = axis_df$tot_pos, labels = axis_df$label, expand = c(0.01, 0)) +
    geom_hline(yintercept = -log10(gwas_sig), color = "#DC2626", linewidth = 0.6, linetype = "solid") +
    geom_hline(yintercept = -log10(suggestive_sig), color = "#2563EB", linewidth = 0.5, linetype = "dashed") +
    annotate("text", x = max(df$tot_pos) * 0.98, y = -log10(gwas_sig) + 0.45, 
             label = "Genome-wide significance (5e-8)", hjust = 1, size = 3.2, fontface = "bold", color = "#DC2626") +
    labs(
      title = "GWAS Manhattan Plot: Single-Nucleotide Polymorphism Associations",
      subtitle = "Points represent SNPs ordered across chromosomes 1-22; prominent skyscrapers mark associated loci",
      x = "Chromosome",
      y = expression(bold(-log[10] ~ "(P-value)")),
      caption = "Source: Simulated genome-wide association scan (~8,800 variants)"
    )
  
  if (exists("theme_topjournal")) {
    p <- p + theme_topjournal(grid = "y") +
      theme(panel.grid.major.x = element_blank())
  } else {
    p <- p + theme_classic()
  }
  p
}

explain <- function() {
  list(
    title = "GWAS Manhattan Plot",
    how_to_read = "Chromosomal coordinates (chromosomes 1 to 22) run along the horizontal axis, while statistical significance (-log10 p-value) is plotted vertically. Tall 'skyscrapers' of points signify genomic regions strongly associated with the trait of interest.",
    when_to_use = "The universally required figure in human genomics and complex trait genetics for reporting Genome-Wide Association Study (GWAS) results.",
    pitfalls = "Spurious skyscraper peaks can result from population stratification, cryptic relatedness, or genotyping batch artifacts. Always verify findings with a genomic inflation factor (lambda GC) and GWAS Q-Q plot.",
    journal_archetype = "Nature Genetics landmark GWAS disease association papers.",
    data_recipe = "Requires chromosome, base-pair coordinate, variant identifier (rsID), and association p-value."
  )
}
