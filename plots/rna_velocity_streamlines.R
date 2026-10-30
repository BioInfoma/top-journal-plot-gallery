# RNA Velocity Vector Streamlines (scVelo Style)
# Tier 3 (Advanced) - Single-cell

make_plot <- function(data_dir = "data") {
  library(ggplot2)
  if (file.exists("theme_journal.R")) source("theme_journal.R", local = TRUE)
  
  # Simulate 350 single cells with spliced vs unspliced dynamics on UMAP
  set.seed(42)
  n <- 350
  theta <- runif(n, 0, 1.6 * pi)
  r <- rnorm(n, 5, 0.6)
  
  umap1 <- r * cos(theta) + rnorm(n, 0, 0.3)
  umap2 <- r * sin(theta) + rnorm(n, 0, 0.3)
  
  # Cell clusters along differentiation spiral
  cluster <- ifelse(theta < 1.2, "Quiescent Stem",
             ifelse(theta < 3.2, "Transit-Amplifying", "Differentiated Lineage"))
  cluster <- factor(cluster, levels = c("Quiescent Stem", "Transit-Amplifying", "Differentiated Lineage"))
  
  df_cells <- data.frame(
    UMAP1 = umap1,
    UMAP2 = umap2,
    cluster = cluster
  )
  
  # Grid vector field (velocity arrows indicating direction of differentiation)
  grid_theta <- seq(0.2, 1.5 * pi, length.out = 16)
  grid_r <- 5.0
  arrow_x <- grid_r * cos(grid_theta)
  arrow_y <- grid_r * sin(grid_theta)
  
  # Tangent velocity direction (-sin, cos)
  vx <- -sin(grid_theta) * 1.2
  vy <- cos(grid_theta) * 1.2
  
  df_arrows <- data.frame(
    x = arrow_x,
    y = arrow_y,
    xend = arrow_x + vx,
    yend = arrow_y + vy
  )
  
  cluster_cols <- c(
    "Quiescent Stem" = "#10B981",
    "Transit-Amplifying" = "#3B82F6",
    "Differentiated Lineage" = "#F59E0B"
  )
  
  p <- ggplot() +
    # Single cells
    geom_point(data = df_cells, aes(x = UMAP1, y = UMAP2, color = cluster), 
               alpha = 0.65, size = 2.0) +
    # RNA velocity directional vector arrows
    geom_segment(data = df_arrows, aes(x = x, y = y, xend = xend, yend = yend), 
                 arrow = arrow(length = unit(0.22, "cm"), type = "closed"), 
                 color = "#0F172A", linewidth = 0.95, alpha = 0.9) +
    scale_color_manual(values = cluster_cols, name = "Cell State:") +
    labs(
      title = "RNA Velocity Vector Field & Dynamic Cell Fate Streamlines",
      subtitle = "Directional arrows project cellular state transitions inferred from unspliced-to-spliced mRNA ratios",
      x = "UMAP Dimension 1",
      y = "UMAP Dimension 2",
      caption = "Source: Simulated single-cell kinetics modeling (scVelo / velocyto dynamical model)"
    )
  
  if (exists("theme_topjournal")) {
    p <- p + theme_topjournal(grid = "xy") +
      theme(
        legend.position = "top",
        legend.direction = "horizontal",
        legend.title = element_text(face = "bold", size = 8.5)
      )
  } else {
    p <- p + theme_classic()
  }
  p
}

explain <- function() {
  list(
    title = "RNA Velocity Directional Streamlines (scVelo Style)",
    how_to_read = "Single cells are embedded in 2D latent space, colored by cell type. Superimposed vector arrows indicate the predicted future state and directional trajectory of individual cells over the next several hours, derived by modeling the ratio of unspliced (nascent) to spliced (mature) mRNA.",
    when_to_use = "Essential in developmental biology and regenerative medicine (scVelo, velocyto) to determine the true thermodynamic directionality of cell differentiation without relying on prior root guesses.",
    pitfalls = "RNA velocity assumes constant splicing and degradation rates; multi-kinetic genes or transient transcriptional bursts can create artifactual velocity reverse arrows.",
    journal_archetype = "Nature, Nature Biotechnology, and Cell dynamical differentiation landscapes.",
    data_recipe = "Requires single-cell spliced and unspliced mRNA count matrices."
  )
}
