# Deep Learning 3D Loss Landscape & Optimization Surface (Li et al. Style)
# Tier 4 (Mind-Blowing) - Machine learning

make_plot <- function(data_dir = "data") {
  library(ggplot2)
  if (file.exists("theme_journal.R")) source("theme_journal.R", local = TRUE)
  
  # Simulate 2D projection of high-dimensional deep neural network loss landscape
  # Filter-normalized directions alpha and beta around trained local minimum
  set.seed(42)
  grid_pts <- expand.grid(
    alpha = seq(-1.5, 1.5, length.out = 80),
    beta = seq(-1.5, 1.5, length.out = 80)
  )
  
  # Non-convex loss surface with flat vs sharp local minima and chaotic saddle barriers
  # Global flat minimum at (0, 0)
  r2 <- grid_pts$alpha^2 + grid_pts$beta^2
  loss_smooth <- log10(1 + 4.5 * r2)
  # High-frequency non-convex ripple perturbations
  ripple <- 0.18 * cos(6 * grid_pts$alpha) * sin(6 * grid_pts$beta)
  # Steep non-convex ravine barriers
  barrier <- 0.6 * exp(-((grid_pts$alpha - 0.7)^2 + (grid_pts$beta + 0.6)^2) / 0.15)
  
  grid_pts$loss <- loss_smooth + ripple - barrier
  grid_pts$loss <- grid_pts$loss - min(grid_pts$loss)
  
  # Optimization descent trajectory (SGD with momentum towards flat minimum)
  sgd_traj <- data.frame(
    alpha = c(1.3, 1.0, 0.75, 0.5, 0.25, 0.08, 0.0),
    beta = c(-1.2, -0.9, -0.55, -0.3, -0.15, -0.05, 0.0)
  )
  
  p <- ggplot(grid_pts, aes(x = alpha, y = beta)) +
    geom_contour_filled(aes(z = loss), bins = 9) +
    geom_contour(aes(z = loss), color = "#FFFFFF", linewidth = 0.25, alpha = 0.45) +
    # SGD Optimization trajectory line
    geom_path(data = sgd_traj, aes(x = alpha, y = beta), 
              color = "#FFFFFF", linewidth = 1.3, linetype = "dashed",
              arrow = arrow(length = unit(0.22, "cm"), type = "closed")) +
    # Flat generalization minimum marker
    annotate("point", x = 0, y = 0, shape = 21, size = 4.8, fill = "#10B981", color = "#FFFFFF", stroke = 1.2) +
    annotate("text", x = 0, y = 0.24, label = "Optimal Flat Minimum\n(High Generalization)", 
             fontface = "bold", size = 3.2, color = "#FFFFFF") +
    scale_fill_viridis_d(option = "turbo", name = "Cross-Entropy Loss (Log Scale):") +
    scale_x_continuous(breaks = seq(-1.5, 1.5, by = 0.5), expand = c(0, 0)) +
    scale_y_continuous(breaks = seq(-1.5, 1.5, by = 0.5), expand = c(0, 0)) +
    coord_fixed() +
    labs(
      title = "Deep Neural Network Loss Landscape: Optimization Geometry",
      subtitle = "Filter-normalized random directions; SGD trajectory converging into a wide flat generalization basin",
      x = "Filter-Normalized Direction α (Weight Space)",
      y = "Filter-Normalized Direction β (Weight Space)",
      caption = "Source: Simulated deep neural network loss surface visualization (Li et al. NeurIPS standard)"
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
    title = "Deep Neural Network 3D Loss Landscape (Li et al. Standard)",
    how_to_read = "A continuous optimization loss surface projected along two filter-normalized random weight directions (alpha and beta) centered at a trained neural network checkpoint. Broad, shallow blue basins indicate 'flat minima' associated with superior test generalization; narrow spiked valleys represent 'sharp minima' prone to overfitting.",
    when_to_use = "Essential in modern deep learning research (NeurIPS, ICML, ICLR) to evaluate architectural innovations (e.g. skip-connections in ResNets, batch normalization, transformer self-attention).",
    pitfalls = "Without filter normalization, scale-invariance in network weights (e.g. scaling weights and biases simultaneously) creates deceptive sharpness or flatness; always apply filter normalization.",
    journal_archetype = "NeurIPS, ICML, and Nature Machine Intelligence neural network optimization papers.",
    data_recipe = "Requires neural network model checkpoint weights and evaluated loss grid evaluations."
  )
}
