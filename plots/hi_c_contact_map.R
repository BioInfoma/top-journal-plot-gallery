# Hi-C Chromatin Conformation Contact Map (TAD Architecture)
# Tier 3 (Advanced) - Genomics

make_plot <- function(data_dir = "data") {
  library(ggplot2)
  if (file.exists("theme_journal.R")) source("theme_journal.R", local = TRUE)
  
  # Simulate 40x40 genomic bin Hi-C interaction frequency matrix with 3 prominent TADs
  set.seed(42)
  n_bins <- 40
  contact_mat <- matrix(0, n_bins, n_bins)
  
  for (i in 1:n_bins) {
    for (j in 1:n_bins) {
      d <- abs(i - j)
      # Base decay with genomic distance (power law decay)
      base_val <- 80 / ((d + 1)^0.85)
      contact_mat[i, j] <- base_val + rnorm(1, 0, 1.2)
    }
  }
  
  # Inject 3 distinct Topologically Associating Domains (TADs) along diagonal
  # TAD 1: bins 1 to 12
  contact_mat[1:12, 1:12] <- contact_mat[1:12, 1:12] + 25
  # TAD 2: bins 14 to 26
  contact_mat[14:26, 14:26] <- contact_mat[14:26, 14:26] + 35
  # TAD 3: bins 28 to 40
  contact_mat[28:40, 28:40] <- contact_mat[28:40, 28:40] + 30
  # Strong chromatin loop corner peak between bin 15 and bin 25
  contact_mat[15, 25] <- contact_mat[25, 15] <- 75
  
  # Convert to log-transformed contact intensity
  log_mat <- log10(pmax(contact_mat, 1))
  
  df_hic <- expand.grid(Bin1 = 1:n_bins, Bin2 = 1:n_bins)
  df_hic$ContactFrequency <- as.vector(log_mat)
  
  # TAD boundary boxes for annotation
  tad_boxes <- data.frame(
    xmin = c(1, 14, 28) - 0.5,
    xmax = c(12, 26, 40) + 0.5,
    ymin = c(1, 14, 28) - 0.5,
    ymax = c(12, 26, 40) + 0.5
  )
  
  p <- ggplot(df_hic, aes(x = Bin1, y = Bin2, fill = ContactFrequency)) +
    geom_tile() +
    # Outline TAD boundaries
    geom_rect(data = tad_boxes, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax), 
              fill = NA, color = "#FFFFFF", linewidth = 0.8, linetype = "dashed", inherit.aes = FALSE) +
    # Highlight specific loop interaction peak
    annotate("point", x = 15, y = 25, shape = 21, size = 4.5, stroke = 1.1, color = "#00FFFF", fill = NA) +
    annotate("text", x = 18, y = 28, label = "CTCF/Cohesin Loop", color = "#FFFFFF", fontface = "bold", size = 3.0) +
    scale_fill_gradientn(
      colors = c("#FFFFFF", "#FFF7ED", "#FDBA74", "#EA580C", "#991B1B", "#450A0A"),
      name = expression(bold("log"[10] ~ "(Contacts)"))
    ) +
    scale_x_continuous(breaks = seq(5, 40, by = 10), labels = paste0(seq(500, 4000, by = 1000), " kb"), expand = c(0, 0)) +
    scale_y_continuous(breaks = seq(5, 40, by = 10), labels = paste0(seq(500, 4000, by = 1000), " kb"), expand = c(0, 0)) +
    labs(
      title = "Hi-C Chromatin Conformation Matrix: 3D Genome Organization",
      subtitle = "Contact probability matrix; dashed white squares mark Topologically Associating Domains (TADs)",
      x = "Genomic Coordinate (Chromosome 8 Region)",
      y = "Genomic Coordinate (Chromosome 8 Region)",
      caption = "Source: Simulated in situ Hi-C high-resolution chromosomal interaction matrix"
    )
  
  if (exists("theme_topjournal")) {
    p <- p + theme_topjournal(grid = "none") +
      theme(
        axis.text = element_text(size = rel(0.85)),
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
    title = "Hi-C Chromatin Conformation Contact Heatmap",
    how_to_read = "A symmetric 2D matrix where both axes represent linear genomic coordinates along a chromosome. Pixel color intensity reflects the frequency of physical 3D chromatin collisions. High-intensity triangles along the diagonal define Topologically Associating Domains (TADs), and isolated off-diagonal puncta mark looping interactions (e.g. enhancer-promoter or CTCF loops).",
    when_to_use = "The foundational figure in 3D epigenomics and chromosomal biology (Lieberman-Aiden et al.) to reveal how the genome folds in three-dimensional nuclear space.",
    pitfalls = "Raw Hi-C counts decay sharply with linear sequence distance; data must be matrix-balanced (ICE or KR normalization) to correct for GC content and restriction enzyme cutting biases.",
    journal_archetype = "Cell, Nature Genetics, and Science 3D genome architecture papers.",
    data_recipe = "Requires binned pairwise genomic interaction contact matrix and normalization weights."
  )
}
