# Spatial Transcriptomics Tissue Expression Map (10x Visium / Xenium Style)
# Tier 3 (Advanced) - Single-cell

make_plot <- function(data_dir = "data") {
  library(ggplot2)
  if (file.exists("theme_journal.R")) source("theme_journal.R", local = TRUE)
  
  # Simulate 600 spatial sequencing spots on a histological tissue slice
  set.seed(42)
  n <- 600
  x_coords <- runif(n, 10, 90)
  y_coords <- runif(n, 10, 90)
  
  # Define spatial tumor microenvironment zones
  dist_center <- sqrt((x_coords - 50)^2 + (y_coords - 50)^2)
  zone <- ifelse(dist_center < 18, "Tumor Core",
          ifelse(dist_center < 32, "Invasive Front", "Immune Stroma"))
  zone <- factor(zone, levels = c("Tumor Core", "Invasive Front", "Immune Stroma"))
  
  # Marker gene expression with high spatial localized enrichment at the invasive front
  marker_expr <- ifelse(zone == "Invasive Front", rnorm(n, 8.5, 1.2),
                 ifelse(zone == "Tumor Core", rnorm(n, 3.2, 0.9), rnorm(n, 1.8, 0.6)))
  marker_expr <- pmax(0, marker_expr)
  
  df_spatial <- data.frame(
    x = x_coords,
    y = y_coords,
    zone = zone,
    expression = marker_expr
  )
  
  p <- ggplot(df_spatial, aes(x = x, y = y)) +
    # Faux histological tissue outline ellipse
    annotate("polygon", x = 50 + 44 * cos(seq(0, 2*pi, length.out = 100)),
                        y = 50 + 42 * sin(seq(0, 2*pi, length.out = 100)),
             fill = "#F1F5F9", color = "#CBD5E1", linewidth = 0.8) +
    # Spatially barcoded sequencing spots colored by localized gene expression
    geom_point(aes(color = expression, size = expression), alpha = 0.88, stroke = 0.2) +
    scale_color_viridis_c(option = "magma", name = "Log2 Expression:") +
    scale_size_continuous(range = c(2.0, 4.8), guide = "none") +
    # Spatial morphological zone annotations
    annotate("text", x = 50, y = 50, label = "Tumor Core", fontface = "bold", size = 3.2, color = "#FFFFFF") +
    annotate("text", x = 50, y = 74, label = "Invasive Margin", fontface = "bold", size = 3.2, color = "#F59E0B") +
    annotate("text", x = 50, y = 87, label = "Immune Stroma", fontface = "bold", size = 3.2, color = "#3B82F6") +
    coord_fixed() +
    labs(
      title = "In Situ Spatial Transcriptomics Map: Microenvironment Zonation",
      subtitle = "Spatially resolved RNA expression (MKI67 / invasion marker) across intact histological tissue slice",
      x = "Tissue Spatial Coordinate X (Microns)",
      y = "Tissue Spatial Coordinate Y (Microns)",
      caption = "Source: Simulated 10x Genomics Visium / Xenium spatial transcriptomics array (N = 600 spots)"
    )
  
  if (exists("theme_topjournal")) {
    p <- p + theme_topjournal(grid = "none") +
      theme(
        axis.text = element_text(size = rel(0.85)),
        legend.position = "top",
        legend.direction = "horizontal",
        legend.title = element_text(face = "bold", size = 8.5),
        legend.key.width = unit(1.6, "cm"),
        legend.key.height = unit(0.35, "cm")
      )
  } else {
    p <- p + theme_minimal()
  }
  p
}

explain <- function() {
  list(
    title = "In Situ Spatial Transcriptomics Tissue Map",
    how_to_read = "Points represent microscopic spatially barcoded sequencing spots (or segmented single cells) positioned on physical 2D histological coordinates. Color saturation directly maps localized gene expression intensity across tissue microenvironments.",
    when_to_use = "The frontier visualization in oncology, pathology, and neuroscience (10x Visium, Xenium, MERFISH, Stereo-seq) to uncover cell-cell interactions, spatial gradients, and tumor margins without losing tissue architecture.",
    pitfalls = "Capture spots in sequencing-based platforms (e.g. Visium 55µm) contain mixtures of multiple cells; deconvolute spot cell-type proportions before attributing signal to a single cell class.",
    journal_archetype = "Nature, Science, and Cell spatial transcriptomics and tumor atlas publications.",
    data_recipe = "Requires spatial 2D pixel/micron coordinates matched with normalized gene expression vectors."
  )
}
