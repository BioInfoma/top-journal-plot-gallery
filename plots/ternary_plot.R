# Equilateral Ternary Compositional Plot (Triplot)
# Tier 3 (Advanced) - Composition

make_plot <- function(data_dir = "data") {
  library(ggplot2)
  if (file.exists("theme_journal.R")) source("theme_journal.R", local = TRUE)
  
  # Simulate 200 compositional observations across 3 tri-lineage fates
  # (e.g. Endoderm, Mesoderm, Ectoderm germ layer differentiation proportions)
  set.seed(42)
  n <- 200
  
  # Dirichlet random compositions (A + B + C = 1)
  # Cluster 1: High Endoderm
  c1 <- t(apply(matrix(rgamma(70 * 3, shape = c(6, 2, 2)), 70, 3, byrow = TRUE), 1, function(x) x / sum(x)))
  # Cluster 2: High Mesoderm
  c2 <- t(apply(matrix(rgamma(70 * 3, shape = c(2, 7, 2)), 70, 3, byrow = TRUE), 1, function(x) x / sum(x)))
  # Cluster 3: High Ectoderm
  c3 <- t(apply(matrix(rgamma(60 * 3, shape = c(2, 2, 6)), 60, 3, byrow = TRUE), 1, function(x) x / sum(x)))
  
  comp_mat <- rbind(c1, c2, c3)
  colnames(comp_mat) <- c("Endoderm", "Mesoderm", "Ectoderm")
  
  # Project 3D simplex proportions into 2D Cartesian coordinates (equilateral triangle)
  # Base vertices: (0,0), (1,0), (0.5, sqrt(3)/2)
  a <- comp_mat[, "Endoderm"]
  b <- comp_mat[, "Mesoderm"]
  c <- comp_mat[, "Ectoderm"]
  
  x_cart <- 0.5 * (2 * b + c) / (a + b + c)
  y_cart <- (sqrt(3) / 2) * c / (a + b + c)
  
  df_ternary <- data.frame(
    x = x_cart,
    y = y_cart,
    fate = factor(c(rep("Endoderm Lineage", 70), rep("Mesoderm Lineage", 70), rep("Ectoderm Lineage", 60))),
    Endoderm = a,
    Mesoderm = b,
    Ectoderm = c
  )
  
  # Equilateral triangle boundary frame
  triangle_frame <- data.frame(
    x = c(0, 1, 0.5, 0),
    y = c(0, 0, sqrt(3)/2, 0)
  )
  
  p <- ggplot() +
    # Triangle boundary and internal gridlines
    geom_polygon(data = triangle_frame, aes(x = x, y = y), fill = "#F8FAFC", color = "#334155", linewidth = 1.0) +
    # Internal guide lines (at 50% levels)
    geom_segment(aes(x = 0.25, y = sqrt(3)/4, xend = 0.75, yend = sqrt(3)/4), color = "#CBD5E1", linetype = "dashed") +
    geom_segment(aes(x = 0.5, y = 0, xend = 0.75, yend = sqrt(3)/4), color = "#CBD5E1", linetype = "dashed") +
    geom_segment(aes(x = 0.5, y = 0, xend = 0.25, yend = sqrt(3)/4), color = "#CBD5E1", linetype = "dashed") +
    # Data points
    geom_point(data = df_ternary, aes(x = x, y = y, fill = fate, color = fate), 
               shape = 21, size = 2.8, stroke = 0.45, alpha = 0.85) +
    # Vertex labels
    annotate("text", x = 0, y = -0.06, label = "Endoderm (100%)", fontface = "bold", size = 3.6, color = "#DC2626") +
    annotate("text", x = 1, y = -0.06, label = "Mesoderm (100%)", fontface = "bold", size = 3.6, color = "#2563EB") +
    annotate("text", x = 0.5, y = sqrt(3)/2 + 0.05, label = "Ectoderm (100%)", fontface = "bold", size = 3.6, color = "#059669") +
    scale_fill_manual(values = c("Endoderm Lineage" = "#DC2626", "Mesoderm Lineage" = "#2563EB", "Ectoderm Lineage" = "#059669"), name = "Commitment Fate:") +
    scale_color_manual(values = c("Endoderm Lineage" = "#7F1D1D", "Mesoderm Lineage" = "#1E3A8A", "Ectoderm Lineage" = "#064E3B"), name = "Commitment Fate:") +
    coord_fixed() +
    labs(
      title = "Ternary Compositional Simplex: Tri-Lineage Cell Fate Potency",
      subtitle = "Relative proportions of three mutually constraining components summing to 100%",
      caption = "Source: Simulated single-cell pluripotent embryonic germ-layer differentiation panel (N = 200)"
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
    title = "Equilateral Ternary Compositional Plot",
    how_to_read = "An equilateral triangle representing the simplex space of three continuous compositional variables that strictly sum to 1.0 (or 100%). Each vertex corresponds to 100% purity in one component, with points positioned according to their barycentric proportions.",
    when_to_use = "Essential in geochemistry (mineral ratios), soil classification (sand-silt-clay), metallurgy, and stem cell biology (tri-lineage fate potency).",
    pitfalls = "Ternary data are non-Euclidean compositional vectors; standard Euclidean distance calculations or naive correlations create spurious negative correlation artifacts (Aitchison's compositional geometry).",
    journal_archetype = "Nature, Science, and Geochimica et Cosmochimica Acta compositional phase diagrams.",
    data_recipe = "Requires three non-negative compositional columns summing to a constant total."
  )
}
