# Topological Data Analysis (TDA) Persistent Homology Barcode
# Tier 4 (Mind-Blowing) - Topology

make_plot <- function(data_dir = "data") {
  library(ggplot2)
  if (file.exists("theme_journal.R")) source("theme_journal.R", local = TRUE)
  
  # Simulate persistent homology features across spatial filtration scale epsilon
  # H0 = Connected components (die as clusters merge)
  # H1 = 1-dimensional topological loops/holes (birth when loop forms, death when hole fills)
  set.seed(42)
  
  # H0 features (12 bars)
  h0_bars <- data.frame(
    dim = "H0 (Connected Components)",
    feature_id = paste0("H0_", 1:12),
    birth = 0,
    death = c(runif(11, 0.1, 0.65), 1.8), # One component persists indefinitely
    y_rank = 1:12
  )
  
  # H1 features (8 bars: prominent persistent loop + short noise loops)
  h1_bars <- data.frame(
    dim = "H1 (Topological Loops / Holes)",
    feature_id = paste0("H1_", 1:8),
    birth = c(0.25, 0.32, 0.40, 0.38, 0.45, 0.50, 0.35, 0.28),
    death = c(1.65, 0.52, 0.58, 0.49, 0.62, 0.68, 0.48, 0.42), # H1_1 is true signal
    y_rank = 13:20
  )
  
  df_barcode <- rbind(h0_bars, h1_bars)
  df_barcode$dim <- factor(df_barcode$dim, levels = c("H0 (Connected Components)", "H1 (Topological Loops / Holes)"))
  
  dim_colors <- c(
    "H0 (Connected Components)" = "#2563EB",
    "H1 (Topological Loops / Holes)" = "#DC2626"
  )
  
  p <- ggplot(df_barcode, aes(y = factor(y_rank))) +
    # Persistent homology horizontal barcode lifespan intervals
    geom_segment(aes(x = birth, xend = death, y = factor(y_rank), yend = factor(y_rank), color = dim), 
                 linewidth = 2.4, lineend = "round") +
    facet_grid(dim ~ ., scales = "free_y", space = "free_y") +
    scale_color_manual(values = dim_colors, name = "Homology Dimension:") +
    scale_x_continuous(breaks = seq(0, 1.8, by = 0.3), limits = c(0, 1.9), expand = c(0, 0)) +
    annotate("text", x = 1.1, y = 8, label = "True Persistent Topological Hole\n(High Lifetime Death - Birth)", 
             fontface = "bold", size = 3.2, color = "#DC2626") +
    labs(
      title = "Topological Data Analysis (TDA): Persistent Homology Barcode",
      subtitle = "Feature lifespan across Vietoris-Rips filtration scale (ε); long bars represent robust geometric signal",
      x = "Filtration Spatial Proximity Parameter (ε Scale)",
      y = "Topological Feature Index",
      caption = "Source: Simulated Vietoris-Rips simplicial filtration on point cloud data (GUDHI / TDA standard)"
    )
  
  if (exists("theme_topjournal")) {
    p <- p + theme_topjournal(grid = "x") +
      theme(
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        strip.text = element_text(face = "bold", size = rel(0.95), color = "#0F172A"),
        strip.background = element_rect(fill = "#F1F5F9", color = NA),
        legend.position = "none"
      )
  } else {
    p <- p + theme_classic()
  }
  p
}

explain <- function() {
  list(
    title = "Topological Data Analysis (TDA) Persistent Barcode",
    how_to_read = "Horizontal bars represent the lifespan of topological features as a spatial proximity ball radius (epsilon) expands. H0 tracks merging connected components; H1 tracks the emergence (birth) and closure (death) of multi-dimensional loops and cavities. Short bars represent geometric noise; long persisting bars reveal authentic topological invariants.",
    when_to_use = "Essential in computational topology, drug discovery, viral evolution, and structural chemistry to capture high-order global shape and connectivity beyond linear Euclidean metrics.",
    pitfalls = "Features born at large epsilon values can be artifacts of point cloud boundary effects; compare against null randomized point distributions.",
    journal_archetype = "Nature, PNAS, and Science topological data analysis and viral space reconstructions.",
    data_recipe = "Requires point cloud metric coordinates and Vietoris-Rips / alpha-complex persistence interval outputs."
  )
}
