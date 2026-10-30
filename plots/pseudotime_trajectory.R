# Single-Cell Pseudotime Differentiation Trajectory (Monocle Style)
# Tier 3 (Advanced) - Single-cell

make_plot <- function(data_dir = "data") {
  library(ggplot2)
  if (file.exists("theme_journal.R")) source("theme_journal.R", local = TRUE)
  
  # Simulate 400 single cells differentiating along a branching lineage
  set.seed(42)
  n <- 400
  # Continuous latent differentiation progression (pseudotime 0 to 10)
  t_val <- sort(runif(n, 0, 10))
  
  # Trajectory coordinates in 2D embedding
  # Stem / Progenitor (t: 0 to 4)
  # Lineage 1 branch (t: 4 to 10)
  # Lineage 2 branch (t: 4 to 10)
  branch <- ifelse(t_val < 4, "Root (Progenitor)", 
            ifelse(runif(n) < 0.5, "Lineage 1 (Neuronal)", "Lineage 2 (Glial)"))
  
  umap1 <- ifelse(branch == "Root (Progenitor)", t_val * 1.5,
           ifelse(branch == "Lineage 1 (Neuronal)", 6 + (t_val - 4) * 1.8 + rnorm(n, 0, 0.4),
                  6 + (t_val - 4) * 1.2 + rnorm(n, 0, 0.4)))
  umap2 <- ifelse(branch == "Root (Progenitor)", sin(t_val * 0.8) * 1.5,
           ifelse(branch == "Lineage 1 (Neuronal)", 1.5 + (t_val - 4) * 1.6 + rnorm(n, 0, 0.5),
                  1.5 - (t_val - 4) * 1.5 + rnorm(n, 0, 0.5)))
  
  df_cells <- data.frame(
    UMAP1 = umap1 + rnorm(n, 0, 0.35),
    UMAP2 = umap2 + rnorm(n, 0, 0.35),
    pseudotime = t_val,
    branch = branch
  )
  
  # Backbone trajectory line segments
  t_backbone <- seq(0, 10, length.out = 100)
  bb_stem <- data.frame(x = seq(0, 6, length.out = 40), y = sin(seq(0, 4, length.out = 40) * 0.8) * 1.5)
  bb_lin1 <- data.frame(x = 6 + seq(0, 6, length.out = 50) * 1.8, y = 1.5 + seq(0, 6, length.out = 50) * 1.6)
  bb_lin2 <- data.frame(x = 6 + seq(0, 6, length.out = 50) * 1.2, y = 1.5 - seq(0, 6, length.out = 50) * 1.5)
  
  p <- ggplot() +
    # Single cells colored by continuous pseudotime
    geom_point(data = df_cells, aes(x = UMAP1, y = UMAP2, color = pseudotime), 
               alpha = 0.8, size = 2.2, stroke = 0.2) +
    # Trajectory principal curve backbones
    geom_path(data = bb_stem, aes(x = x, y = y), color = "#0F172A", linewidth = 1.6) +
    geom_path(data = bb_lin1, aes(x = x, y = y), color = "#0F172A", linewidth = 1.6) +
    geom_path(data = bb_lin2, aes(x = x, y = y), color = "#0F172A", linewidth = 1.6) +
    # Milestone branch point nodes
    annotate("point", x = 0, y = 0, shape = 21, size = 4.5, fill = "#10B981", color = "#0F172A", stroke = 1.2) +
    annotate("text", x = 0, y = -1.2, label = "Root / State 1", fontface = "bold", size = 3.2, color = "#047857") +
    annotate("point", x = 6, y = 1.5, shape = 21, size = 4.5, fill = "#F59E0B", color = "#0F172A", stroke = 1.2) +
    annotate("text", x = 6, y = 2.8, label = "Bifurcation Point", fontface = "bold", size = 3.2, color = "#B45309") +
    annotate("text", x = 16.5, y = 11, label = "Lineage 1 (Neuronal)", fontface = "bold", size = 3.2, color = "#0F172A") +
    annotate("text", x = 13, y = -7.5, label = "Lineage 2 (Glial)", fontface = "bold", size = 3.2, color = "#0F172A") +
    scale_color_viridis_c(option = "plasma", name = "Inferred Pseudotime:") +
    labs(
      title = "Single-Cell Developmental Pseudotime Trajectory (Monocle Style)",
      subtitle = "Branching principal graph tracing cellular differentiation kinetics from stem to mature fates",
      x = "UMAP Dimension 1",
      y = "UMAP Dimension 2",
      caption = "Source: Simulated single-cell RNA sequencing trajectory inference (N = 400 cells)"
    )
  
  if (exists("theme_topjournal")) {
    p <- p + theme_topjournal(grid = "xy") +
      theme(
        legend.position = "top",
        legend.direction = "horizontal",
        legend.key.width = unit(1.8, "cm"),
        legend.key.height = unit(0.35, "cm"),
        legend.title = element_text(face = "bold", size = 8.5)
      )
  } else {
    p <- p + theme_classic()
  }
  p
}

explain <- function() {
  list(
    title = "Single-Cell Branching Pseudotime Trajectory",
    how_to_read = "Single cells are embedded in 2D latent space, colored along a continuous gradient of inferred pseudotime (plasma palette, zero = undifferentiated progenitor). The bold black branching graph traces developmental fate commitments past decision bifurcation nodes.",
    when_to_use = "Essential in developmental biology, stem cell differentiation, and immunology (Monocle, Slingshot, Palantir) to model dynamic processes from static single-cell snapshots.",
    pitfalls = "Pseudotime measures progression along a manifold, NOT absolute clock time in hours or days; verify root origin biologically with stem cell marker genes rather than arbitrary graph extremities.",
    journal_archetype = "Nature, Science, and Cell developmental cell fate mapping.",
    data_recipe = "Requires single-cell dimensionality reduction coordinates and trajectory graph inference."
  )
}
