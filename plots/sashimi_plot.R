# RNA-seq Alternative Splicing Sashimi Plot
# Tier 3 (Advanced) - Genomics

make_plot <- function(data_dir = "data") {
  library(ggplot2)
  if (file.exists("theme_journal.R")) source("theme_journal.R", local = TRUE)
  
  # Three exons with alternative exon-skipping event (Exon 1 -> Exon 2 -> Exon 3, vs Exon 1 -> Exon 3)
  exons <- data.frame(
    exon = c("Exon 1", "Exon 2 (Skipped)", "Exon 3"),
    xmin = c(100, 450, 800),
    xmax = c(250, 550, 950),
    ymin = -18,
    ymax = 0,
    type = c("Constitutive", "Alternative Exon", "Constitutive")
  )
  
  # Base coverage over exon regions
  x_seq <- seq(50, 1000, by = 5)
  cov_y <- rep(0, length(x_seq))
  for (i in 1:nrow(exons)) {
    in_exon <- x_seq >= exons$xmin[i] & x_seq <= exons$xmax[i]
    cov_y[in_exon] <- runif(sum(in_exon), 45, 65)
  }
  # Add smooth Gaussian edges to exon coverage
  cov_y <- ksmooth(x_seq, cov_y, kernel = "normal", bandwidth = 15)$y
  df_cov <- data.frame(x = x_seq, y = cov_y)
  
  # Splice Junction Arcs
  # Arc 1: Exon 1 -> Exon 2 (Canonical junction, 142 reads)
  t_seq <- seq(0, 1, length.out = 30)
  arc1_x <- 250 + t_seq * (450 - 250)
  arc1_y <- 35 + 28 * sin(pi * t_seq)
  
  # Arc 2: Exon 2 -> Exon 3 (Canonical junction, 138 reads)
  arc2_x <- 550 + t_seq * (800 - 550)
  arc2_y <- 35 + 32 * sin(pi * t_seq)
  
  # Arc 3: Exon 1 -> Exon 3 (Exon skipping junction, 64 reads)
  arc3_x <- 250 + t_seq * (800 - 250)
  arc3_y <- 55 + 50 * sin(pi * t_seq)
  
  df_arcs <- rbind(
    data.frame(x = arc1_x, y = arc1_y, arc_id = "Junction 1-2", reads = 142, col = "#2563EB"),
    data.frame(x = arc2_x, y = arc2_y, arc_id = "Junction 2-3", reads = 138, col = "#2563EB"),
    data.frame(x = arc3_x, y = arc3_y, arc_id = "Exon Skipping 1-3", reads = 64, col = "#DC2626")
  )
  
  p <- ggplot() +
    # Intron baseline guide line
    geom_segment(aes(x = 50, xend = 1000, y = -9, yend = -9), color = "#94A3B8", linewidth = 1.2) +
    # Exon rectangular blocks
    geom_rect(data = exons, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = type), 
              color = "#1E293B", linewidth = 0.5) +
    geom_text(data = exons, aes(x = (xmin + xmax) / 2, y = -9, label = exon), 
              color = "#FFFFFF", fontface = "bold", size = 2.8) +
    # Exon read coverage area
    geom_area(data = df_cov, aes(x = x, y = y), fill = "#93C5FD", alpha = 0.65) +
    geom_line(data = df_cov, aes(x = x, y = y), color = "#2563EB", linewidth = 0.5) +
    # Splice junction connecting arcs
    geom_path(data = df_arcs, aes(x = x, y = y, group = arc_id, color = arc_id), linewidth = 1.15) +
    # Junction read count badges
    annotate("label", x = 350, y = 62, label = "142 reads", fontface = "bold", size = 2.8, color = "#2563EB", fill = "#FFFFFF") +
    annotate("label", x = 675, y = 66, label = "138 reads", fontface = "bold", size = 2.8, color = "#2563EB", fill = "#FFFFFF") +
    annotate("label", x = 525, y = 104, label = "64 reads (Exon 2 Skipping)", fontface = "bold", size = 2.8, color = "#DC2626", fill = "#FFFFFF") +
    scale_fill_manual(values = c("Constitutive" = "#1E3A8A", "Alternative Exon" = "#DC2626"), name = "Exon Classification:") +
    scale_color_manual(values = c("Junction 1-2" = "#2563EB", "Junction 2-3" = "#2563EB", "Exon Skipping 1-3" = "#DC2626"), name = "Splice Junction:") +
    scale_y_continuous(breaks = seq(0, 100, by = 25), limits = c(-22, 115)) +
    scale_x_continuous(breaks = seq(100, 950, by = 200), labels = paste0(seq(100, 950, by = 200), " bp")) +
    labs(
      title = "RNA-seq Sashimi Plot: Quantitative Alternative Splicing",
      subtitle = "Exon coverage depth with curved junction splice reads quantifying exon exclusion/inclusion",
      x = "Genomic Locus Coordinate (Base Pairs)",
      y = "Normalized Coverage Depth & Junction Counts",
      caption = "Source: Simulated RNA-seq paired-end split read alignments (MISO / rmats standard)"
    )
  
  if (exists("theme_topjournal")) {
    p <- p + theme_topjournal(grid = "y") +
      theme(
        legend.position = "top",
        legend.direction = "horizontal",
        legend.box = "horizontal",
        legend.title = element_text(face = "bold", size = 8.5)
      )
  } else {
    p <- p + theme_classic()
  }
  p
}

explain <- function() {
  list(
    title = "RNA-seq Alternative Splicing Sashimi Plot",
    how_to_read = "The bottom track depicts exon models (boxes) and introns (lines). Shaded curves display sequencing read coverage across exons. Curved arches bridging exons represent split junction reads spanning splice junctions, labeled with numerical junction read counts.",
    when_to_use = "The international standard in transcriptomics (rMATS, MISO, ggsashimi) to quantify and validate alternative splicing events (exon skipping, alternative 5'/3' splice sites, intron retention).",
    pitfalls = "Normalize junction read counts when comparing across sequencing libraries with different sequencing depths (percent spliced in, PSI / delta-PSI).",
    journal_archetype = "Nature, Cell, and Genes & Development RNA processing and alternative splicing figures.",
    data_recipe = "Requires BAM alignment file with split reads and transcript exon coordinate annotations."
  )
}
