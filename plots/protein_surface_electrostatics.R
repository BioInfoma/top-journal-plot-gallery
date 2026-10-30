# Protein Surface Electrostatic Potential Distribution (APBS / PyMOL Style)
# Tier 4 (Mind-Blowing) - Structure

make_plot <- function(data_dir = "data") {
  library(ggplot2)
  if (file.exists("theme_journal.R")) source("theme_journal.R", local = TRUE)
  
  # Simulate 2D projection of a macromolecular solvent-accessible surface (Poisson-Boltzmann electrostatics)
  set.seed(42)
  grid_pts <- expand.grid(x = seq(-30, 30, length.out = 80), y = seq(-30, 30, length.out = 80))
  
  # Protein globular boundary
  dist_origin <- sqrt(grid_pts$x^2 + grid_pts$y^2)
  in_protein <- dist_origin <= (22 + 4 * cos(3 * atan2(grid_pts$y, grid_pts$x)))
  
  # Electrostatic potential surface:
  # Positive patch (Basic cleft, e.g. DNA binding grove) at x ~ -10, y ~ 8
  # Negative patch (Acidic pocket, e.g. catalytic aspartates) at x ~ 12, y ~ -10
  pot <- 6.5 * exp(-((grid_pts$x + 10)^2 + (grid_pts$y - 8)^2) / 60) - 
         6.5 * exp(-((grid_pts$x - 12)^2 + (grid_pts$y + 10)^2) / 50) +
         rnorm(nrow(grid_pts), 0, 0.4)
  
  grid_pts$potential <- pot
  grid_pts$in_protein <- in_protein
  df_surf <- grid_pts[grid_pts$in_protein, ]
  
  p <- ggplot(df_surf, aes(x = x, y = y, fill = potential)) +
    geom_raster(interpolate = TRUE) +
    scale_fill_gradient2(
      low = "#DC2626", # Negative Potential (Red, -5 kT/e)
      mid = "#F8FAFC", # Neutral (White, 0 kT/e)
      high = "#1D4ED8", # Positive Potential (Blue, +5 kT/e)
      midpoint = 0,
      limits = c(-5.5, 5.5),
      name = "Potential [kT / e]:"
    ) +
    annotate("text", x = -10, y = 8, label = "Basic / DNA-Binding Cleft\n(+5 kT/e)", fontface = "bold", size = 3.2, color = "#FFFFFF") +
    annotate("text", x = 12, y = -10, label = "Acidic Catalytic Pocket\n(-5 kT/e)", fontface = "bold", size = 3.2, color = "#FFFFFF") +
    coord_fixed() +
    labs(
      title = "Macromolecular Surface Electrostatics (Poisson-Boltzmann)",
      subtitle = "Solvent-accessible electrostatic surface potential; red = negative charge, blue = positive charge",
      x = "Structure Angstrom Coordinate X (Å)",
      y = "Structure Angstrom Coordinate Y (Å)",
      caption = "Source: Simulated continuum electrostatic Poisson-Boltzmann solver (APBS / PyMOL standard)"
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
    title = "Protein Surface Electrostatic Potential Map",
    how_to_read = "A 2D orthographic projection of a molecular solvent-accessible surface colored according to the continuum Poisson-Boltzmann electrostatic potential: blazing blue denotes positive surface charge (+5 kT/e, lysine/arginine enriched), red denotes negative charge (-5 kT/e, aspartate/glutamate enriched), and white is neutral.",
    when_to_use = "The universal standard in structural biology and biophysics (APBS, PyMOL) to identify nucleic-acid binding grooves, catalytic triads, and protein-protein binding interfaces.",
    pitfalls = "Electrostatic calculations are heavily dependent on assigned ionic strength and dielectric constants; verify solvent ionic concentration (e.g. 150 mM physiological salt).",
    journal_archetype = "Nature Structural & Molecular Biology, Structure, and Science macromolecular binding mechanisms.",
    data_recipe = "Requires PDB atomic coordinates with APBS/PDB2PQR electrostatic OpenDX grid potentials."
  )
}
