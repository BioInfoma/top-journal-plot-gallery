# Evolutionary Co-Speciation Tanglegram (Dual Facing Phylogenies)
# Tier 3 (Advanced) - Phylogenetics

make_plot <- function(data_dir = "data") {
  library(ggplot2)
  if (file.exists("theme_journal.R")) source("theme_journal.R", local = TRUE)
  
  # 8 Taxa representing Mammalian Hosts vs Associated RNA Viruses
  host_taxa <- c(
    "Primates", "Rodentia", "Chiroptera (Bats)", "Carnivora", 
    "Artiodactyla", "Cetacea", "Eulipotyphla", "Afrotheria"
  )
  n_t <- length(host_taxa)
  y_host <- 1:n_t
  
  # Right tree virus tips permuted to demonstrate host-switching spillovers
  virus_taxa <- c(
    "Primate-LV", "Rodent-LV", "Carnivore-LV*", "Bat-CoV*", 
    "Cetacean-LV", "Artiodactyl-LV", "Afrotheria-LV", "Eulipotyphla-LV"
  )
  virus_tip_order <- c(1, 2, 4, 3, 6, 5, 8, 7)
  y_virus <- 1:n_t
  
  # Function to generate rectangular cladogram segments
  build_tree_segments <- function(x_root = 0.05, x_tip = 1.05, is_left = TRUE) {
    nodes_l1 <- c(1.5, 3.5, 5.5, 7.5) # y coords
    nodes_l2 <- c(2.5, 6.5)           # y coords
    y_root   <- 4.5
    
    seg <- list()
    
    if (is_left) {
      x_l3 <- x_root
      x_l2 <- 0.40
      x_l1 <- 0.75
      x_t  <- x_tip
    } else {
      x_l3 <- x_root
      x_l2 <- x_root - (0.40 - 0.05)
      x_l1 <- x_root - (0.75 - 0.05)
      x_t  <- x_tip
    }
    
    # Root stem
    seg[[1]] <- data.frame(x = ifelse(is_left, 0, x_root + 0.15), 
                           xend = x_l3, y = y_root, yend = y_root)
    # Root vertical bar
    seg[[2]] <- data.frame(x = x_l3, xend = x_l3, y = nodes_l2[1], yend = nodes_l2[2])
    # Root to L2 horizontals
    seg[[3]] <- data.frame(x = x_l3, xend = x_l2, y = nodes_l2[1], yend = nodes_l2[1])
    seg[[4]] <- data.frame(x = x_l3, xend = x_l2, y = nodes_l2[2], yend = nodes_l2[2])
    
    # L2 vertical bars
    seg[[5]] <- data.frame(x = x_l2, xend = x_l2, y = nodes_l1[1], yend = nodes_l1[2])
    seg[[6]] <- data.frame(x = x_l2, xend = x_l2, y = nodes_l1[3], yend = nodes_l1[4])
    # L2 to L1 horizontals
    seg[[7]]  <- data.frame(x = x_l2, xend = x_l1, y = nodes_l1[1], yend = nodes_l1[1])
    seg[[8]]  <- data.frame(x = x_l2, xend = x_l1, y = nodes_l1[2], yend = nodes_l1[2])
    seg[[9]]  <- data.frame(x = x_l2, xend = x_l1, y = nodes_l1[3], yend = nodes_l1[3])
    seg[[10]] <- data.frame(x = x_l2, xend = x_l1, y = nodes_l1[4], yend = nodes_l1[4])
    
    # L1 vertical bars & horizontals to tips
    idx <- 11
    for (i in 1:4) {
      y_c1 <- (i - 1) * 2 + 1
      y_c2 <- (i - 1) * 2 + 2
      y_p  <- nodes_l1[i]
      seg[[idx]] <- data.frame(x = x_l1, xend = x_l1, y = y_c1, yend = y_c2); idx <- idx + 1
      seg[[idx]] <- data.frame(x = x_l1, xend = x_t, y = y_c1, yend = y_c1); idx <- idx + 1
      seg[[idx]] <- data.frame(x = x_l1, xend = x_t, y = y_c2, yend = y_c2); idx <- idx + 1
    }
    
    do.call(rbind, seg)
  }
  
  # Generate Left (Host) and Right (Viral) tree branch segments
  left_tree  <- build_tree_segments(x_root = 0.10, x_tip = 1.00, is_left = TRUE)
  right_tree <- build_tree_segments(x_root = 5.30, x_tip = 4.40, is_left = FALSE)
  
  # Host Tips and Viral Tips
  df_host_tips <- data.frame(
    x = 1.00,
    y = y_host,
    label = host_taxa
  )
  
  df_virus_tips <- data.frame(
    x = 4.40,
    y = y_virus,
    label = virus_taxa[virus_tip_order]
  )
  
  # Connectors linking homologous associations
  con_rows <- list()
  for (i in 1:n_t) {
    y_start <- y_host[i]
    y_end   <- which(virus_tip_order == i)
    is_switch <- (y_start != y_end)
    
    t_seq <- seq(0, 1, length.out = 30)
    smooth_t <- 3 * t_seq^2 - 2 * t_seq^3
    
    con_x <- 2.15 + t_seq * (3.25 - 2.15)
    con_y <- y_start + smooth_t * (y_end - y_start)
    
    con_rows[[i]] <- data.frame(
      x = con_x,
      y = con_y,
      line_id = i,
      event = ifelse(is_switch, "Host-Switching / Spillover", "Strict Co-Speciation"),
      is_switch = is_switch
    )
  }
  df_connectors <- do.call(rbind, con_rows)
  
  p <- ggplot() +
    # Left Host Phylogeny Branches
    geom_segment(data = left_tree, aes(x = x, xend = xend, y = y, yend = yend), 
                 color = "#1E293B", linewidth = 0.85, lineend = "square") +
    geom_point(data = df_host_tips, aes(x = x, y = y), size = 2.2, shape = 21, fill = "#1E40AF", color = "#0F172A", stroke = 0.8) +
    geom_text(data = df_host_tips, aes(x = 1.08, y = y, label = label), 
              hjust = 0, size = 3.1, fontface = "bold", color = "#0F172A") +
    
    # Right Viral Phylogeny Branches
    geom_segment(data = right_tree, aes(x = x, xend = xend, y = y, yend = yend), 
                 color = "#1E293B", linewidth = 0.85, lineend = "square") +
    geom_point(data = df_virus_tips, aes(x = x, y = y), size = 2.2, shape = 21, fill = "#DC2626", color = "#0F172A", stroke = 0.8) +
    geom_text(data = df_virus_tips, aes(x = 4.32, y = y, label = label), 
              hjust = 1, size = 3.1, fontface = "bold", color = "#0F172A") +
    
    # Central Connecting Association Curves
    geom_path(data = df_connectors, aes(x = x, y = y, group = line_id, color = event, linetype = event), 
              linewidth = 1.15, alpha = 0.88) +
    
    # Subtree Domain Headers
    annotate("text", x = 0.55, y = 8.85, label = "Host Phylogeny (Mammalia)", fontface = "bold", size = 3.6, color = "#1E40AF") +
    annotate("text", x = 4.85, y = 8.85, label = "Symbiont Phylogeny (RNA Viruses)", fontface = "bold", size = 3.6, color = "#DC2626") +
    annotate("text", x = 2.70, y = 8.85, label = "Tanglegram Association", fontface = "bold", size = 3.4, color = "#475569") +
    
    # Incongruence / Spillover Callout Annotation
    annotate("label", x = 2.70, y = 3.5, label = "Zoonotic Host-Switching\n(Bat-CoV -> Carnivora)", 
             fill = "#FEF2F2", color = "#991B1B", size = 2.7, fontface = "bold") +
    
    scale_color_manual(values = c("Strict Co-Speciation" = "#10B981", "Host-Switching / Spillover" = "#EF4444"), name = "Evolutionary Event:") +
    scale_linetype_manual(values = c("Strict Co-Speciation" = "solid", "Host-Switching / Spillover" = "dashed"), name = "Evolutionary Event:") +
    scale_x_continuous(limits = c(-0.1, 5.5), expand = c(0, 0)) +
    scale_y_continuous(limits = c(0.4, 9.3), expand = c(0, 0)) +
    labs(
      title = "Evolutionary Co-Speciation Tanglegram: Host-Pathogen Phylogenetic Congruence",
      subtitle = "Dual facing bifurcating cladograms; green parallel ribbons indicate co-speciation; red dashed crossing paths denote host-switching",
      caption = "Source: Simulated comparative host-pathogen cophylogenetic distance matrix (ParaFitGlobal = 0.18, p = 0.002)"
    ) +
    theme_void() +
    theme(
      plot.title = element_text(face = "bold", size = 12.5, color = "#0F172A", margin = margin(b = 4)),
      plot.subtitle = element_text(size = 9.2, color = "#475569", margin = margin(b = 10)),
      plot.caption = element_text(size = 8, color = "#94A3B8", hjust = 0),
      legend.position = "top",
      legend.direction = "horizontal",
      legend.title = element_text(face = "bold", size = 8.5),
      legend.text = element_text(size = 8.5),
      plot.margin = margin(14, 16, 14, 14)
    )
  p
}

explain <- function() {
  list(
    title = "Evolutionary Co-Speciation Tanglegram",
    how_to_read = "Two opposing bifurcating phylogenetic trees face inward (host cladogram on the left, pathogen/symbiont cladogram on the right). Connecting ribbons link associated taxa: parallel horizontal trajectories signify strict co-divergence/co-speciation, whereas intersecting crossing lines reveal cross-species spillover or host-switching events.",
    when_to_use = "The gold standard in evolutionary biology, parasitology, virology, and microbiome research to test the Fahrenholz rule and evaluate whether host and symbiont diversified synchronously.",
    pitfalls = "Nodes must be optimally rotated (heuristic untangling) to minimize artificial visual line crossings, ensuring that observed line crossings represent true macroevolutionary incongruence rather than random tip ordering.",
    journal_archetype = "Nature Ecology & Evolution, Systematic Biology, and Science cophylogenetic analyses.",
    data_recipe = "Requires two phylogenetic tree topologies (Newick / phylo) and an interaction association matrix."
  )
}
