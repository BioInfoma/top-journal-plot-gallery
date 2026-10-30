# Molecular Dynamics Free-Energy Landscape (FEL / PMF)
# Tier 4 (Mind-Blowing) - Structure

make_plot <- function(data_dir = "data") {
  library(ggplot2)
  if (file.exists("theme_journal.R")) source("theme_journal.R", local = TRUE)
  
  # Simulate 2D reaction coordinate free energy surface: RMSD vs Radius of Gyration (Rg)
  set.seed(42)
  grid_coords <- expand.grid(
    rmsd = seq(0.8, 6.5, length.out = 80),
    rg = seq(14.0, 22.0, length.out = 80)
  )
  
  # Native folded ground state basin (rmsd ~ 1.5, rg ~ 15.5, lowest energy 0 kcal/mol)
  b1 <- 7.5 * exp(-((grid_coords$rmsd - 1.5)^2 / 0.6 + (grid_coords$rg - 15.5)^2 / 1.5))
  # Intermediate folding intermediate (rmsd ~ 3.2, rg ~ 17.5, energy ~ 2.5 kcal/mol)
  b2 <- 5.0 * exp(-((grid_coords$rmsd - 3.2)^2 / 0.8 + (grid_coords$rg - 17.5)^2 / 2.0))
  # Unfolded state ensemble (rmsd ~ 5.2, rg ~ 20.0, energy ~ 4 kcal/mol)
  b3 <- 3.5 * exp(-((grid_coords$rmsd - 5.2)^2 / 1.2 + (grid_coords$rg - 20.0)^2 / 2.5))
  
  # Delta G (Gibbs Free Energy in kcal/mol)
  delta_g <- 8.0 - (b1 + b2 + b3) + rnorm(nrow(grid_coords), 0, 0.1)
  delta_g <- delta_g - min(delta_g) # Set ground state minimum to 0 kcal/mol
  
  df_fel <- data.frame(grid_coords, delta_g = delta_g)
  
  p <- ggplot(df_fel, aes(x = rmsd, y = rg)) +
    geom_contour_filled(aes(z = delta_g), bins = 9) +
    geom_contour(aes(z = delta_g), color = "#FFFFFF", linewidth = 0.25, alpha = 0.5) +
    # Mark conformational state minima
    annotate("point", x = 1.5, y = 15.5, shape = 21, size = 4.2, fill = "#10B981", color = "#FFFFFF", stroke = 1.0) +
    annotate("text", x = 1.5, y = 14.8, label = "Native Basin\n(0 kcal/mol)", fontface = "bold", size = 3.2, color = "#FFFFFF") +
    annotate("point", x = 3.2, y = 17.5, shape = 21, size = 3.8, fill = "#F59E0B", color = "#FFFFFF", stroke = 0.9) +
    annotate("text", x = 3.2, y = 18.2, label = "Metastable Intermediate", fontface = "bold", size = 3.0, color = "#FFFFFF") +
    annotate("point", x = 5.2, y = 20.0, shape = 21, size = 3.8, fill = "#EF4444", color = "#FFFFFF", stroke = 0.9) +
    annotate("text", x = 5.2, y = 20.7, label = "Unfolded Ensemble", fontface = "bold", size = 3.0, color = "#FFFFFF") +
    scale_fill_viridis_d(option = "turbo", name = "ΔG [kcal / mol]:") +
    scale_x_continuous(breaks = 1:6, labels = paste0(1:6, " Å")) +
    scale_y_continuous(breaks = seq(14, 22, by = 2), labels = paste0(seq(14, 22, by = 2), " Å")) +
    labs(
      title = "Macromolecular Free-Energy Landscape (Potential of Mean Force)",
      subtitle = "Conformational sampling along RMSD and Gyration Radius (Rg); basins mark thermodynamic minima",
      x = "Backbone Root-Mean-Square Deviation (RMSD to Native, Å)",
      y = "Radius of Gyration (Rg, Compactness, Å)",
      caption = "Source: Simulated all-atom enhanced sampling molecular dynamics trajectory (GROMACS / AMBER)"
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
    title = "Macromolecular Free-Energy Landscape (FEL / PMF)",
    how_to_read = "A continuous thermodynamic surface plotted against two collective reaction coordinates (e.g. RMSD and Radius of Gyration). Deep blue/purple contour basins indicate thermodynamically favored conformational ground states (low free energy Delta G), while ridges and saddle points mark transition-state barriers.",
    when_to_use = "Essential in computational biophysics, protein folding, and drug discovery (GROMACS, metadynamics) to understand allosteric transitions, binding mechanisms, and kinetic trapping.",
    pitfalls = "Choice of collective variables (CVs) is critical; picking poor reaction coordinates hides true free-energy barriers by projecting orthogonal degrees of freedom on top of each other.",
    journal_archetype = "PNAS, JACS, and Nature Structural & Molecular Biology molecular dynamics landscapes.",
    data_recipe = "Requires long-timescale molecular dynamics trajectory frames with calculated reaction coordinates."
  )
}
