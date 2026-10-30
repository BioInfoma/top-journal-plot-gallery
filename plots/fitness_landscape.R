# Evolutionary Fitness Landscape (Wright Adaptive Peaks)
# Tier 4 (Mind-Blowing) - Evolution

make_plot <- function(data_dir = "data") {
  library(ggplot2)
  if (file.exists("theme_journal.R")) source("theme_journal.R", local = TRUE)
  
  # Simulate 2D genotype/phenotype space with rugged fitness peaks (Wrightian landscape)
  set.seed(42)
  grid_pts <- expand.grid(g1 = seq(-4, 4, length.out = 80), g2 = seq(-4, 4, length.out = 80))
  
  # Adaptive peak 1 (Global optimum) at (2, 2)
  p1 <- 10 * exp(-((grid_pts$g1 - 2)^2 + (grid_pts$g2 - 2)^2) / 1.8)
  # Adaptive peak 2 (Sub-optimal local peak) at (-2, -1)
  p2 <- 6.5 * exp(-((grid_pts$g1 + 2)^2 + (grid_pts$g2 + 1)^2) / 2.2)
  # Adaptive peak 3 at (-1, 2.5)
  p3 <- 5.0 * exp(-((grid_pts$g1 + 1)^2 + (grid_pts$g2 - 2.5)^2) / 1.5)
  
  fitness <- p1 + p2 + p3 + 0.5 * sin(grid_pts$g1 * 2) * cos(grid_pts$g2 * 2)
  grid_pts$fitness <- fitness
  
  # Evolutionary trajectory ascending local vs global peaks
  traj_local <- data.frame(
    g1 = c(-3.2, -2.8, -2.4, -2.0),
    g2 = c(-2.5, -2.0, -1.4, -1.0)
  )
  traj_global <- data.frame(
    g1 = c(0.2, 0.8, 1.4, 2.0),
    g2 = c(0.0, 0.6, 1.3, 2.0)
  )
  
  p <- ggplot(grid_pts, aes(x = g1, y = g2)) +
    geom_contour_filled(aes(z = fitness), bins = 9) +
    geom_contour(aes(z = fitness), color = "#FFFFFF", linewidth = 0.25, alpha = 0.45) +
    # Evolutionary paths
    geom_path(data = traj_local, aes(x = g1, y = g2), color = "#FFFFFF", linewidth = 1.1, linetype = "dashed",
              arrow = arrow(length = unit(0.2, "cm"), type = "closed")) +
    geom_path(data = traj_global, aes(x = g1, y = g2), color = "#FFFFFF", linewidth = 1.2,
              arrow = arrow(length = unit(0.2, "cm"), type = "closed")) +
    # Optimum labels
    annotate("text", x = 2, y = 2, label = "Global Optimum Peak\n(High Fitness)", fontface = "bold", size = 3.3, color = "#FFFFFF") +
    annotate("text", x = -2, y = -1, label = "Local Sub-Optimum\n(Trapped)", fontface = "bold", size = 3.1, color = "#FFFFFF") +
    annotate("text", x = 0, y = -3, label = "Fitness Valley (Deleterious)", fontface = "bold", size = 3.0, color = "#0F172A") +
    scale_fill_viridis_d(option = "magma", name = "Relative Organismal Fitness:") +
    scale_x_continuous(breaks = -4:4, expand = c(0, 0)) +
    scale_y_continuous(breaks = -4:4, expand = c(0, 0)) +
    coord_fixed() +
    labs(
      title = "Rugged Evolutionary Fitness Landscape (Wrightian Adaptive Peaks)",
      subtitle = "Multi-peak fitness topology; dashed trajectory shows evolutionary trapping at local sub-optimum",
      x = "Phenotypic / Genotypic Coordinate 1",
      y = "Phenotypic / Genotypic Coordinate 2",
      caption = "Source: Simulated Wrightian epistasis adaptive landscape model"
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
    title = "Evolutionary Fitness Landscape (Wrightian Adaptive Peaks)",
    how_to_read = "A continuous 2D representation of high-dimensional genotype space. Elevated peaks denote high organismal reproductive fitness, separated by fitness valleys (lethal or deleterious intermediates). Trajectory arrows trace populations navigating epistasis and genetic drift.",
    when_to_use = "Foundational conceptual framework in evolutionary biology, antibiotic resistance evolution, and viral immune evasion to model how mutations constrain accessible evolutionary pathways.",
    pitfalls = "High-dimensional genotype spaces contain thousands of dimensions; 2D projections can hide ridge-like percolation paths through higher dimensions that bypass apparent 2D valleys.",
    journal_archetype = "Nature, Science, and Evolution adaptive landscape and epistasis papers.",
    data_recipe = "Requires combinatorial genotype variant matrices with measured organismal replication fitness."
  )
}
