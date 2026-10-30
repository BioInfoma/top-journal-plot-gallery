# Pangenome Variation Graph (vg / Minigraph Graphical Genome)
# Tier 4 (Mind-Blowing) - Genomics

make_plot <- function(data_dir = "data") {
  library(ggplot2)
  if (file.exists("theme_journal.R")) source("theme_journal.R", local = TRUE)
  
  # Sequence nodes in a variation graph (linear reference backbone + alternative structural variants)
  # Backbone: Nodes 1 -> 2 -> 3 -> 4 -> 5 -> 6 -> 7
  # Inversion / Structural variant bubble: Node 2 splits into 2A (Reference) and 2B (Alternative Allele)
  # Large insertion bubble: Node 5 bypasses into Node 5_INS
  
  nodes <- data.frame(
    id = c("N1 (Core)", "N2_Ref", "N2_Alt", "N3 (Core)", "N4 (Core)", "N5_Ref", "N5_Ins", "N6 (Core)"),
    x = c(1, 2.5, 2.5, 4, 5.5, 7, 7, 8.5),
    y = c(0, 0.6, -0.6, 0, 0, 0, 0.8, 0),
    seq_len = c(450, 180, 195, 320, 210, 50, 420, 310),
    type = c("Core Backbone", "Reference Allele", "Inversion SV", "Core Backbone", 
             "Core Backbone", "Reference Allele", "Novel Insertion", "Core Backbone")
  )
  
  # Directed edges between nodes
  edges <- data.frame(
    from = c(1, 1, 2, 3, 4, 4, 5, 4, 6, 7),
    to = c(2, 3, 4, 4, 5, 6, 7, 7, 8, 8),
    x = c(1, 1, 2.5, 2.5, 4, 5.5, 5.5, 5.5, 7, 7),
    y = c(0, 0, 0.6, -0.6, 0, 0, 0, 0, 0, 0.8),
    xend = c(2.5, 2.5, 4, 4, 5.5, 7, 7, 7, 8.5, 8.5),
    yend = c(0.6, -0.6, 0, 0, 0, 0, 0.8, 0, 0, 0),
    edge_type = c("Branch", "Branch", "Merge", "Merge", "Backbone", "Backbone", "Branch", "Bypass", "Backbone", "Merge")
  )
  
  node_cols <- c(
    "Core Backbone" = "#1E40AF",
    "Reference Allele" = "#64748B",
    "Inversion SV" = "#DC2626",
    "Novel Insertion" = "#059669"
  )
  
  p <- ggplot() +
    # Edge paths connecting sequence nodes
    geom_segment(data = edges, aes(x = x, y = y, xend = xend, yend = yend), 
                 color = "#94A3B8", linewidth = 1.2, arrow = arrow(length = unit(0.2, "cm"), type = "closed")) +
    # Node blocks representing contiguous DNA sequence segments
    geom_point(data = nodes, aes(x = x, y = y, fill = type, size = seq_len), 
               shape = 21, color = "#0F172A", stroke = 1.0) +
    geom_text(data = nodes, aes(x = x, y = y + ifelse(y >= 0, 0.28, -0.28), label = id), 
              fontface = "bold", size = 2.8, color = "#0F172A") +
    scale_fill_manual(values = node_cols, name = "Sequence Node Type:") +
    scale_size_continuous(range = c(4.5, 9.5), name = "Segment Length (bp):") +
    scale_x_continuous(limits = c(0.4, 9.2), expand = c(0, 0)) +
    scale_y_continuous(limits = c(-1.1, 1.4), expand = c(0, 0)) +
    labs(
      title = "Human Pangenome Variation Graph (Minigraph / vg Graph Genome)",
      subtitle = "Sequence nodes and directed edges encode shared core backbone and structural variant bubbles",
      caption = "Source: Simulated Human Pangenome Reference Consortium (HPRC) variation graph"
    ) +
    theme_void() +
    theme(
      plot.title = element_text(face = "bold", size = 13, color = "#0F172A", margin = margin(b = 4)),
      plot.subtitle = element_text(size = 9.5, color = "#475569", margin = margin(b = 10)),
      plot.caption = element_text(size = 8, color = "#94A3B8", hjust = 0),
      legend.position = "top",
      legend.direction = "horizontal",
      legend.box = "horizontal",
      legend.title = element_text(face = "bold", size = 8.5),
      legend.text = element_text(size = 8.5),
      plot.margin = margin(12, 14, 12, 12)
    )
  p
}

explain <- function() {
  list(
    title = "Human Pangenome Sequence Variation Graph",
    how_to_read = "Replaces a single linear reference genome with a bidirected sequence graph. Nodes represent non-overlapping genomic sequence blocks, and directed edges represent sequence adjacency. Splitting bubbles indicate complex structural variants, inversions, and multi-allelic insertions across diverse human haplotypes.",
    when_to_use = "The emerging international standard for human genomics (Human Pangenome Reference Consortium, HPRC) to eliminate reference allele bias and map complex structural variation.",
    pitfalls = "Linear coordinate systems (e.g. GRCh38 positions) do not apply straightforwardly across graph bubbles; path-based coordinates are required.",
    journal_archetype = "Nature, Science, and Nature Biotechnology Human Pangenome Reference Consortium (HPRC) landmark papers.",
    data_recipe = "Requires Graphical Fragment Assembly (GFA) pangenome graph files."
  )
}
