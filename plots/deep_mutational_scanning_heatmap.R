# Deep Mutational Scanning (DMS) Variant Effect Heatmap
# Tier 4 (Mind-Blowing) - Protein

make_plot <- function(data_dir = "data") {
  library(ggplot2)
  if (file.exists("theme_journal.R")) source("theme_journal.R", local = TRUE)
  
  # 20 canonical amino acids
  aa_letters <- c("A", "C", "D", "E", "F", "G", "H", "I", "K", "L", 
                  "M", "N", "P", "Q", "R", "S", "T", "V", "W", "Y")
  n_pos <- 28
  
  # Simulate functional fitness scores for all single amino acid substitutions
  set.seed(42)
  dms_grid <- expand.grid(pos = 1:n_pos, aa = aa_letters)
  
  # Wildtype sequence definition
  wt_seq <- sample(aa_letters, n_pos, replace = TRUE)
  dms_grid$is_wt <- wt_seq[dms_grid$pos] == dms_grid$aa
  
  # Functional fitness score: mostly neutral around 0, deleterious negative, rare gain-of-function
  dms_grid$fitness <- rnorm(nrow(dms_grid), mean = -0.4, sd = 1.1)
  # Wildtype is identically 0 by definition
  dms_grid$fitness[dms_grid$is_wt] <- 0
  # Certain catalytic positions (e.g. pos 12-15) are strictly intolerant of mutation
  catalytic_mask <- dms_grid$pos %in% c(12, 13, 14, 21) & !dms_grid$is_wt
  dms_grid$fitness[catalytic_mask] <- rnorm(sum(catalytic_mask), mean = -3.2, sd = 0.5)
  dms_grid$fitness <- pmin(2.5, pmax(-4.0, dms_grid$fitness))
  
  p <- ggplot(dms_grid, aes(x = factor(pos), y = aa, fill = fitness)) +
    geom_tile(color = "#FFFFFF", linewidth = 0.3) +
    # Mark wildtype residues with subtle dot
    geom_point(data = dms_grid[dms_grid$is_wt, ], shape = 16, size = 1.1, color = "#1E293B") +
    scale_fill_gradient2(
      low = "#1E40AF",   # Severe loss of function / deleterious (Deep Blue)
      mid = "#F8FAFC",   # Wildtype-like neutral (White, 0)
      high = "#DC2626",  # Gain of function / hyperactive (Red)
      midpoint = 0,
      limits = c(-4.0, 2.5),
      name = "Fitness Effect:"
    ) +
    scale_x_discrete(breaks = seq(2, n_pos, by = 2), labels = paste0("p.", seq(2, n_pos, by = 2))) +
    labs(
      title = "Deep Mutational Scanning (DMS) Comprehensive Variant Fitness Landscape",
      subtitle = "High-throughput functional assay of all single-amino-acid substitutions; black dots indicate wildtype residue",
      x = "Target Protein Codon Coordinate",
      y = "Substituted Amino Acid",
      caption = "Source: Simulated deep mutational scanning saturation mutagenesis assay (MaveDB standard)"
    )
  
  if (exists("theme_topjournal")) {
    p <- p + theme_topjournal(grid = "none") +
      theme(
        axis.text.x = element_text(size = rel(0.85), face = "bold"),
        axis.text.y = element_text(size = rel(0.8), family = "mono", face = "bold"),
        legend.position = "top",
        legend.direction = "horizontal",
        legend.title = element_text(face = "bold", size = 8.5),
        legend.key.width = unit(1.6, "cm"),
        legend.key.height = unit(0.35, "cm")
      )
  } else {
    p <- p + theme_minimal()
  }
  p
}

explain <- function() {
  list(
    title = "Deep Mutational Scanning (DMS) Variant Effect Heatmap",
    how_to_read = "Every single-amino-acid substitution across a protein sequence is arranged in a 2D grid: horizontal axis denotes residue position, and vertical axis lists the 20 substitute amino acids. Color encodes experimental functional fitness (blue = deleterious loss-of-function; red = gain-of-function). Wildtype amino acids are marked with central black dots.",
    when_to_use = "The premier methodology in functional genomics, antibody engineering, and clinical genetics (Fowler & Fields, MaveDB, AlphaMissense) to benchmark variant pathogenicity before mutations appear in patients.",
    pitfalls = "Selection assays probe a specific biological activity (e.g. binding affinity or cell growth); a variant may be functional in binding but defective in protein folding in vivo.",
    journal_archetype = "Nature, Science, and Cell saturation mutagenesis and variant effect mapping.",
    data_recipe = "Requires MaveDB-formatted deep mutational scanning enrichment scores and wildtype sequence."
  )
}
