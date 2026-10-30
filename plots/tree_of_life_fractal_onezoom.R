# Fractal Tree of Life (OneZoom Evolutionary Explorer)
# Tier 4 (Mind-Blowing) - Phylogenetics

make_plot <- function(data_dir = "data") {
  library(ggplot2)
  if (file.exists("theme_journal.R")) source("theme_journal.R", local = TRUE)
  
  # Generate recursive fractal spiral branching tree of life (OneZoom architecture)
  # Deep-time root splits into major domains: Archaea, Bacteria, Eukaryota
  set.seed(42)
  
  # Recursive branch generator
  build_fractal_tree <- function(x, y, angle, length, depth, max_depth = 5) {
    if (depth > max_depth) return(NULL)
    
    xend <- x + length * cos(angle)
    yend <- y + length * sin(angle)
    
    current_seg <- data.frame(
      x = x, y = y, xend = xend, yend = yend,
      depth = depth,
      branch_width = (max_depth - depth + 1)^1.2
    )
    
    # Branching angles (golden ratio phyllotaxis / bifurcation)
    left_branch <- build_fractal_tree(xend, yend, angle + 0.42, length * 0.72, depth + 1, max_depth)
    right_branch <- build_fractal_tree(xend, yend, angle - 0.38, length * 0.68, depth + 1, max_depth)
    
    rbind(current_seg, left_branch, right_branch)
  }
  
  df_tree <- build_fractal_tree(0, 0, pi/2, 4.5, depth = 1, max_depth = 5)
  
  # Terminal leaf nodes with IUCN Red List conservation status
  leaves <- df_tree[df_tree$depth == 5, ]
  iucn_status <- sample(c("Least Concern", "Vulnerable", "Critically Endangered", "Extinct in Wild"), 
                        nrow(leaves), replace = TRUE, prob = c(0.55, 0.25, 0.15, 0.05))
  leaves$status <- factor(iucn_status, levels = c("Least Concern", "Vulnerable", "Critically Endangered", "Extinct in Wild"))
  
  iucn_cols <- c(
    "Least Concern" = "#059669",        # Green
    "Vulnerable" = "#F59E0B",           # Amber
    "Critically Endangered" = "#DC2626",# Red
    "Extinct in Wild" = "#1E293B"       # Black
  )
  
  p <- ggplot() +
    # Fractal evolutionary branches with tapering width
    geom_segment(data = df_tree, aes(x = x, y = y, xend = xend, yend = yend, linewidth = branch_width), 
                 color = "#1E293B", lineend = "round") +
    # Terminal species leaf nodes colored by conservation status
    geom_point(data = leaves, aes(x = xend, y = yend, color = status), size = 2.4, alpha = 0.9) +
    scale_color_manual(values = iucn_cols, name = "IUCN Conservation Status:") +
    scale_linewidth_continuous(range = c(0.4, 2.5), guide = "none") +
    coord_fixed() +
    labs(
      title = "Fractal Tree of Life: Multi-Scale Planetary Biodiversity (OneZoom)",
      subtitle = "Self-similar branching geometry spanning evolutionary deep time; tips denote species extinction risk",
      caption = "Source: Simulated OneZoom fractal tree-of-life mathematical topology (Rosindell et al. standard)"
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
    title = "Fractal Tree of Life (OneZoom Mathematical Explorer)",
    how_to_read = "A self-similar recursive fractal branching tree where major evolutionary clades split into progressively finer sub-branches without leaf crowding. Branch thickness scales with evolutionary lineage age, and terminal tip leaves are color-coded by modern IUCN Red List extinction vulnerability.",
    when_to_use = "The premier interactive framework for macroevolutionary education and planetary biodiversity visualization (OneZoom, Rosindell et al., PLOS Biology).",
    pitfalls = "Fractal trees maintain equal visual space for all lineages regardless of actual species counts; pair with explicit species count annotations to avoid underestimating hyper-diverse clades (like Coleoptera beetles).",
    journal_archetype = "PLOS Biology and Nature Ecology & Evolution large-scale tree-of-life exploration.",
    data_recipe = "Requires comprehensive taxonomic tree of life and IUCN Red List conservation database."
  )
}
