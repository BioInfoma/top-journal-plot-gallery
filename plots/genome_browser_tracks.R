# Multi-Omic Genome Browser Track View (IGV / UCSC Style)
# Tier 3 (Advanced) - Genomics

make_plot <- function(data_dir = "data") {
  library(ggplot2)
  if (file.exists("theme_journal.R")) source("theme_journal.R", local = TRUE)
  
  # Simulate 100 kb genomic window with 3 functional tracks: RNA-seq, ATAC-seq, H3K27ac ChIP-seq
  set.seed(42)
  pos_seq <- seq(1000, 100000, by = 500)
  n_pts <- length(pos_seq)
  
  # Promoter / Enhancer signal peaks around pos 35,000 and 65,000
  p1 <- exp(-((pos_seq - 35000) / 4000)^2)
  p2 <- exp(-((pos_seq - 65000) / 3500)^2)
  
  rna_signal <- p1 * 85 + p2 * 12 + rpois(n_pts, lambda = 3)
  atac_signal <- p1 * 60 + p2 * 95 + rpois(n_pts, lambda = 2)
  k27ac_signal <- p1 * 50 + p2 * 80 + rpois(n_pts, lambda = 4)
  
  df_tracks <- rbind(
    data.frame(pos = pos_seq, signal = rna_signal, track = "1. RNA-seq (Expression)"),
    data.frame(pos = pos_seq, signal = atac_signal, track = "2. ATAC-seq (Open Chromatin)"),
    data.frame(pos = pos_seq, signal = k27ac_signal, track = "3. H3K27ac (Active Enhancer)")
  )
  df_tracks$track <- factor(df_tracks$track, levels = c("1. RNA-seq (Expression)", "2. ATAC-seq (Open Chromatin)", "3. H3K27ac (Active Enhancer)"))
  
  # Gene Exon Annotation Model
  exons <- data.frame(
    xmin = c(25000, 34000, 48000, 64000),
    xmax = c(27500, 36500, 50000, 68000),
    ymin = -15,
    ymax = -5,
    exon = paste0("Exon ", 1:4)
  )
  
  track_colors <- c(
    "1. RNA-seq (Expression)" = "#2563EB",
    "2. ATAC-seq (Open Chromatin)" = "#059669",
    "3. H3K27ac (Active Enhancer)" = "#DC2626"
  )
  
  p <- ggplot() +
    # Multi-omic area density tracks
    geom_area(data = df_tracks, aes(x = pos / 1000, y = signal, fill = track), alpha = 0.75, color = NA) +
    geom_line(data = df_tracks, aes(x = pos / 1000, y = signal, color = track), linewidth = 0.5) +
    facet_wrap(~ track, ncol = 1, scales = "free_y") +
    scale_fill_manual(values = track_colors) +
    scale_color_manual(values = track_colors) +
    scale_x_continuous(breaks = seq(10, 100, by = 15), labels = paste0(seq(10, 100, by = 15), " kb")) +
    labs(
      title = "Multi-Omic Epigenomic & Transcriptomic Browser Tracks (IGV Style)",
      subtitle = "Co-localized chromatin accessibility (ATAC) and enhancer acetylation (H3K27ac) at active promoter",
      x = "Genomic Position on Chromosome 12 (Kilobases)",
      y = "Normalized Coverage Depth (RPKM)",
      caption = "Source: Simulated multi-omic NGS sequencing profiles (RNA-seq, ATAC-seq, ChIP-seq)"
    )
  
  if (exists("theme_topjournal")) {
    p <- p + theme_topjournal(grid = "y") +
      theme(
        legend.position = "none",
        strip.text = element_text(face = "bold", size = rel(0.95), hjust = 0),
        strip.background = element_rect(fill = "#F1F5F9", color = NA)
      )
  } else {
    p <- p + theme_classic()
  }
  p
}

explain <- function() {
  list(
    title = "Multi-Omic Genome Browser Tracks (UCSC / IGV Style)",
    how_to_read = "Stacked panels align along a shared horizontal genomic coordinate axis. Filled area curves depict normalized read coverage depth for distinct functional genomic assays (e.g. transcript abundance, chromatin accessibility, histone marks). Vertical co-localization of peaks indicates active regulatory elements (promoters and enhancers).",
    when_to_use = "The indispensable figure in functional genomics (ENCODE, Roadmap Epigenomics) to prove that candidate genetic variants overlap active regulatory machinery.",
    pitfalls = "Ensure identical x-axis zoom limits and state whether y-axis coverage scales are shared across tracks or group-normalized to avoid misleading height comparisons.",
    journal_archetype = "Nature, Science, and Cell genomic loci and epigenetic regulation figures.",
    data_recipe = "Requires BigWig genomic coverage signals and gene annotation GTF coordinates."
  )
}
