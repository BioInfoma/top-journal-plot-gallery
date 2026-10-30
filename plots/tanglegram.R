# Co-Speciation Tanglegram (Dual Opposing Phylogenies)
# Tier 3 (Advanced) - Phylogenetics

make_plot <- function(data_dir = "data") {
  library(ggplot2)
  if (file.exists("theme_journal.R")) source("theme_journal.R", local = TRUE)
  
  # Simulate paired Host vs Parasite co-speciation trees
  taxa <- c("Primates", "Rodentia", "Chiroptera", "Carnivora", "Artiodactyla", "Cetacea", "Lagomorpha", "Afrotheria")
  n_t <- length(taxa)
  
  # Left tree tips ordered 1:n
  y_left <- 1:n_t
  # Right tree permuted to illustrate host-switching events
  right_perm <- c(1, 2, 4, 3, 5, 6, 8, 7)
  y_right <- 1:n_t
  
  # Connectors
  connectors <- data.frame(
    taxa = taxa,
    y_from = y_left,
    y_to = right_perm,
    is_switch = y_left != right_perm
  )
  
  # Left tree branch segments (simple cladogram)
  left_branches <- data.frame(
    x = c(0, 0.4, 0.4, 0.8, 0.8, 0.4, 0.8, 0.8),
    xend = c(0.4, 0.4, 0.8, 1.2, 1.2, 0.8, 1.2, 1.2),
    y = c(4.5, 2.5, 6.5, 1.5, 3.5, 5.5, 7.5, 8.5),
    yend = c(4.5, 1.5, 5.5, 1.0, 3.0, 5.0, 7.0, 8.0)
  )
  
  # Right tree branch segments
  right_branches <- data.frame(
    x = 4.0 - left_branches$x,
    xend = 4.0 - left_branches$xend,
    y = left_branches$y,
    yend = left_branches$yend
  )
  
  p <- ggplot() +
    # Central connecting association lines
    geom_segment(data = connectors, aes(x = 1.3, xend = 2.7, y = y_from, yend = y_to, color = is_switch), 
                 linewidth = 1.1, alpha = 0.85) +
    # Left Host Phylogeny
    geom_segment(data = left_branches, aes(x = x, xend = xend, y = y, yend = yend), color = "#1E293B", linewidth = 0.9) +
    geom_point(data = connectors, aes(x = 1.3, y = y_from), size = 2.8, shape = 21, fill = "#3B82F6", color = "#1E293B") +
    geom_text(data = connectors, aes(x = 1.22, y = y_from, label = taxa), hjust = 1, fontface = "bold", size = 3.0, color = "#1E293B") +
    # Right Symbiont/Virus Phylogeny
    geom_segment(data = right_branches, aes(x = x, xend = xend, y = y, yend = yend), color = "#1E293B", linewidth = 0.9) +
    geom_point(data = connectors, aes(x = 2.7, y = y_to), size = 2.8, shape = 21, fill = "#EF4444", color = "#1E293B") +
    geom_text(data = connectors, aes(x = 2.78, y = y_to, label = paste0(taxa[right_perm], "-Virus")), hjust = 0, fontface = "bold", size = 3.0, color = "#1E293B") +
    scale_color_manual(values = c("FALSE" = "#10B981", "TRUE" = "#EF4444"), 
                       labels = c("Co-speciation Congruence", "Host-Switching / Cross-transmission"),
                       name = "Evolutionary Event:") +
    scale_x_continuous(limits = c(-0.8, 4.8), expand = c(0, 0)) +
    labs(
      title = "Evolutionary Tanglegram: Host-Pathogen Co-Speciation",
      subtitle = "Dual facing phylogenies; parallel green lines denote congruent co-speciation; crossed red lines show host shifts",
      caption = "Source: Simulated comparative mammalian host vs viral lineage co-evolutionary trees"
    ) +
    theme_void() +
    theme(
      plot.title = element_text(face = "bold", size = 13, color = "#0F172A", margin = margin(b = 4)),
      plot.subtitle = element_text(size = 9.5, color = "#475569", margin = margin(b = 10)),
      plot.caption = element_text(size = 8, color = "#94A3B8", hjust = 0),
      legend.position = "top",
      legend.direction = "horizontal",
      legend.title = element_text(face = "bold", size = 8.5),
      legend.text = element_text(size = 8.5),
      plot.margin = margin(12, 14, 12, 12)
    )
  p
}

explain <- function() {
  list(
    title = "Evolutionary Co-Speciation Tanglegram",
    how_to_read = "Two opposing phylogenetic trees face each other (e.g. host cladogram on the left, pathogen cladogram on the right). Connecting lines link associated taxa across the center: parallel horizontal lines signify strict co-speciation, while crossed lines reveal host-switching or cross-species spillover events.",
    when_to_use = "Essential in evolutionary biology, parasitology, virology, and symbiosis to evaluate whether interacting lineages diversified in tandem or through opportunistic cross-transmission.",
    pitfalls = "Nodes in both trees should be rotated (heuristic untangling) to minimize artificial line crossings, ensuring that remaining crossed lines reflect genuine macroevolutionary incongruence.",
    journal_archetype = "Nature Ecology & Evolution and Systematic Biology co-phylogenetic analyses.",
    data_recipe = "Requires two phylogenetic trees and a paired interaction association table."
  )
}
