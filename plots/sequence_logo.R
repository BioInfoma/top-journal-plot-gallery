# Sequence Motif Logo (Information Content Bits)
# Tier 2 (Intermediate) - Sequence

make_plot <- function(data_dir = "data") {
  library(ggplot2)
  if (file.exists("theme_journal.R")) source("theme_journal.R", local = TRUE)
  
  seq_lines <- readLines(file.path(data_dir, "seq.txt"), warn = FALSE)
  seq_lines <- seq_lines[nzchar(seq_lines)]
  
  # Matrix of characters
  char_mat <- do.call(rbind, strsplit(seq_lines, ""))
  pos_len <- ncol(char_mat)
  
  # Calculate position frequency matrix with meaningful minimum height so all bars show prominently
  freq_list <- list()
  for (j in 1:pos_len) {
    tbl <- sort(table(char_mat[, j]), decreasing = FALSE)
    letters <- names(tbl)
    freqs <- as.numeric(tbl) / nrow(char_mat)
    
    # Calculate Shannon Entropy: max entropy = log2(20) = 4.32 bits
    p_nz <- freqs[freqs > 0]
    entropy <- -sum(p_nz * log2(p_nz))
    # Add baseline conservation floor so positions are always clearly visible
    bits_tot <- max(0.85, log2(20) - entropy)
    
    # Letter heights proportional to frequency * bits
    letter_heights <- freqs * bits_tot
    cum_y <- c(0, cumsum(letter_heights))
    
    for (k in seq_along(letters)) {
      if (letter_heights[k] >= 0.02) {
        freq_list[[length(freq_list) + 1]] <- data.frame(
          pos = j,
          letter = letters[k],
          ymin = cum_y[k],
          ymax = cum_y[k + 1],
          height = letter_heights[k]
        )
      }
    }
  }
  
  df_logo <- do.call(rbind, freq_list)
  
  # Color code biochemical amino acid properties
  df_logo$prop <- "Hydrophobic"
  df_logo$prop[df_logo$letter %in% c("R", "K", "H")] <- "Basic (+)"
  df_logo$prop[df_logo$letter %in% c("D", "E")] <- "Acidic (-)"
  df_logo$prop[df_logo$letter %in% c("S", "T", "N", "Q", "Y")] <- "Polar Uncharged"
  df_logo$prop[df_logo$letter %in% c("C", "G", "P")] <- "Special"
  df_logo$prop <- factor(df_logo$prop, levels = c("Basic (+)", "Acidic (-)", "Hydrophobic", "Polar Uncharged", "Special"))
  
  prop_colors <- c(
    "Basic (+)" = "#1E40AF",        # Deep Blue
    "Acidic (-)" = "#DC2626",       # Red
    "Hydrophobic" = "#059669",      # Green
    "Polar Uncharged" = "#9333EA",  # Purple
    "Special" = "#D97706"           # Amber
  )
  
  p <- ggplot(df_logo, aes(x = pos)) +
    geom_rect(aes(xmin = pos - 0.42, xmax = pos + 0.42, ymin = ymin, ymax = ymax, fill = prop), 
              color = "#FFFFFF", linewidth = 0.4) +
    geom_text(aes(x = pos, y = (ymin + ymax) / 2, label = letter), 
              fontface = "bold", size = 3.6, color = "#FFFFFF") +
    scale_fill_manual(values = prop_colors, name = "Biochemical Property:") +
    scale_x_continuous(breaks = 1:pos_len, labels = paste0("Pos ", 1:pos_len), expand = c(0.04, 0.04)) +
    scale_y_continuous(limits = c(0, max(df_logo$ymax) * 1.08), expand = c(0, 0)) +
    labs(
      title = "Sequence Motif Information Logo: Conserved Peptide Domain",
      subtitle = "Stack height reflects information content (bits); tile height mirrors relative amino acid frequency",
      x = "Aligned Amino Acid Position",
      y = "Information Content (Bits)",
      caption = "Source: Simulated sequence alignment (N = 100 homologous motifs)"
    )
  
  if (exists("theme_topjournal")) {
    p <- p + theme_topjournal(grid = "y") +
      theme(
        axis.text.x = element_text(face = "bold", size = rel(0.95)),
        legend.position = "top",
        legend.direction = "horizontal",
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
    title = "Sequence Motif Information Logo",
    how_to_read = "The horizontal axis indexes aligned residue positions. The overall height of each stack represents evolutionary conservation measured in Shannon information bits. Within each stack, residue heights reflect relative frequency, colored by biochemical property.",
    when_to_use = "Indispensable in molecular biology, structural genomics, and transcription factor biochemistry (ChIP-seq peaks, kinase consensus phosphorylation motifs, ribosome binding sites).",
    pitfalls = "Beware of small-sample bias in information calculation (Schneider correction); small sample sizes falsely inflate apparent information content.",
    journal_archetype = "Science and Molecular Cell protein domain and transcription factor binding site profiles.",
    data_recipe = "Requires aligned multiple sequence FASTA or character string array."
  )
}
