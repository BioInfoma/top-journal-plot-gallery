# Protein Lollipop Mutation Diagram (Gene Domain Architecture)
# Tier 3 (Advanced) - Genomics

make_plot <- function(data_dir = "data") {
  library(ggplot2)
  if (file.exists("theme_journal.R")) source("theme_journal.R", local = TRUE)
  
  # Simulate somatic mutation hotspots along a 400 amino-acid oncoprotein (e.g. TP53 archetype)
  set.seed(42)
  protein_len <- 393
  
  # Functional protein domains
  domains <- data.frame(
    xmin = c(1, 102, 325, 363),
    xmax = c(61, 292, 356, 393),
    ymin = -0.8,
    ymax = 0.8,
    domain = c("TAD (Transactivation)", "DNA-Binding Domain", "Tetramerization", "Regulatory Domain"),
    fill_col = c("#94A3B8", "#3B82F6", "#10B981", "#F59E0B")
  )
  
  # Recurrent somatic mutation positions
  mut_positions <- c(175, 245, 248, 273, 282, 110, 158, 220, 337, 342)
  mut_counts <- c(38, 24, 45, 32, 28, 8, 14, 16, 12, 6)
  mut_types <- c("Missense", "Missense", "Missense", "Missense", "Missense", 
                 "Nonsense", "In-frame Del", "Missense", "Frameshift", "Nonsense")
  
  df_muts <- data.frame(
    pos = mut_positions,
    count = mut_counts,
    type = factor(mut_types, levels = c("Missense", "Nonsense", "Frameshift", "In-frame Del"))
  )
  
  type_cols <- c(
    "Missense" = "#2563EB",
    "Nonsense" = "#DC2626",
    "Frameshift" = "#D97706",
    "In-frame Del" = "#059669"
  )
  
  p <- ggplot() +
    # Protein backbone backbone line
    geom_segment(aes(x = 1, xend = protein_len, y = 0, yend = 0), color = "#64748B", linewidth = 2.5) +
    # Protein functional domains
    geom_rect(data = domains, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = domain), 
              color = "#1E293B", linewidth = 0.4, alpha = 0.85) +
    geom_text(data = domains, aes(x = (xmin + xmax) / 2, y = 0, label = domain), 
              color = "#FFFFFF", fontface = "bold", size = 2.6) +
    # Lollipop stems
    geom_segment(data = df_muts, aes(x = pos, xend = pos, y = 0.8, yend = count + 2), 
                 color = "#475569", linewidth = 0.75) +
    # Lollipop marker heads
    geom_point(data = df_muts, aes(x = pos, y = count + 2, fill = type, size = count), 
               shape = 21, color = "#1E293B", stroke = 0.6) +
    geom_text(data = df_muts[df_muts$count >= 20, ], aes(x = pos, y = count + 5.5, label = paste0("p.", pos)), 
              fontface = "bold", size = 2.8, color = "#0F172A") +
    scale_fill_manual(values = c(type_cols, setNames(domains$fill_col, domains$domain)), name = "Mutation Class:") +
    scale_size_continuous(range = c(2.8, 6.5), name = "Mutation Frequency:") +
    scale_x_continuous(breaks = seq(0, 400, by = 50), limits = c(-5, 410), expand = c(0, 0)) +
    scale_y_continuous(limits = c(-2.5, 56), expand = c(0, 0)) +
    labs(
      title = "Oncoprotein Mutation Lollipop Architecture (cBioPortal Standard)",
      subtitle = "Somatic variant recurrence along functional protein domains; circle size reflects cohort recurrence count",
      x = "Amino Acid Residue Position (Codon)",
      y = "Recurrent Mutation Count in Cohort",
      caption = "Source: Simulated pan-cancer somatic exome sequencing registry (N = 500 tumors)"
    )
  
  if (exists("theme_topjournal")) {
    p <- p + theme_topjournal(grid = "y") +
      theme(
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
    title = "Protein Mutation Lollipop Diagram",
    how_to_read = "The horizontal axis represents the linear amino acid sequence of a target protein, with colored rectangular boxes denoting annotated functional protein domains. Vertical stems and circular lollipop heads indicate recurrent somatic mutation hotspots, with circle area scaling with sample frequency.",
    when_to_use = "The international standard in cancer genomics (cBioPortal, COSMIC) to distinguish driver mutational hotspots from background passenger variants.",
    pitfalls = "Make sure codon positions match the canonical Ensembl/RefSeq transcript isoform (e.g. p53 canonical vs alternative splicing variants).",
    journal_archetype = "Nature Genetics, Cancer Discovery, and Science somatic mutational landscapes.",
    data_recipe = "Requires amino acid mutation coordinates, variant consequence classifications, and protein domain boundaries."
  )
}
