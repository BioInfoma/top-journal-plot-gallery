# Cryo-EM 3D Density Map & Gold-Standard FSC Resolution Curve
# Tier 4 (Mind-Blowing) - Structure

make_plot <- function(data_dir = "data") {
  library(ggplot2)
  if (file.exists("theme_journal.R")) source("theme_journal.R", local = TRUE)
  
  # Dual-panel structural figure:
  # Panel 1: Cryo-EM Electron Density Slice with Fitted Atomic Ribbon
  # Panel 2: Fourier Shell Correlation (FSC) curve showing 2.4 Å resolution threshold
  
  # Panel 1: FSC Curve
  spatial_freq <- seq(0, 0.6, length.out = 100) # 1 / Angstroms
  # Sigmoidal drop of FSC
  fsc_val <- 1 / (1 + exp(18 * (spatial_freq - 0.41))) + rnorm(100, 0, 0.015)
  fsc_val <- pmin(1, pmax(-0.05, fsc_val))
  
  df_fsc <- data.frame(
    freq = spatial_freq,
    res_angstrom = ifelse(spatial_freq == 0, Inf, 1 / spatial_freq),
    fsc = fsc_val
  )
  
  # 0.143 Gold-Standard Resolution intersection at ~ 0.416 (2.4 Å)
  res_cutoff <- 2.4
  
  p1 <- ggplot(df_fsc, aes(x = freq, y = fsc)) +
    geom_hline(yintercept = 0.143, linetype = "dashed", color = "#DC2626", linewidth = 0.6) +
    geom_hline(yintercept = 0.5, linetype = "dotted", color = "#64748B", linewidth = 0.5) +
    geom_line(color = "#1D4ED8", linewidth = 1.1) +
    geom_vline(xintercept = 1 / res_cutoff, linetype = "dashed", color = "#DC2626", linewidth = 0.6) +
    annotate("label", x = 0.43, y = 0.45, 
             label = paste0("Gold-Standard FSC = 0.143\nGlobal Resolution: ", res_cutoff, " Å"), 
             fontface = "bold", size = 3.2, color = "#DC2626", fill = "#FFFFFF") +
    scale_y_continuous(limits = c(-0.05, 1.05), breaks = seq(0, 1, by = 0.2)) +
    scale_x_continuous(breaks = seq(0.1, 0.5, by = 0.1), labels = c("10 Å", "5 Å", "3.3 Å", "2.5 Å", "2.0 Å")) +
    labs(
      title = "A. Gold-Standard Fourier Shell Correlation (FSC)",
      x = "Spatial Frequency (Equivalent Resolution in Ångströms)",
      y = "FSC Coefficient"
    ) +
    theme_classic(base_size = 9) +
    theme(plot.title = element_text(face = "bold", size = 10, color = "#0F172A"))
  
  # Panel 2: Density Contour Slice with Modeled Alpha-Helix Backbone
  grid_pts <- expand.grid(x = seq(-15, 15, length.out = 60), y = seq(-15, 15, length.out = 60))
  density_val <- 8 * exp(-(grid_pts$x^2 + grid_pts$y^2) / 45) + rnorm(nrow(grid_pts), 0, 0.3)
  df_density <- data.frame(grid_pts, density = density_val)
  
  helix_pts <- data.frame(
    x = 4.5 * sin(seq(0, 3*pi, length.out = 25)),
    y = seq(-10, 10, length.out = 25)
  )
  
  p2 <- ggplot(df_density, aes(x = x, y = y)) +
    geom_contour_filled(aes(z = density), bins = 6, alpha = 0.75) +
    geom_path(data = helix_pts, aes(x = x, y = y), color = "#0F172A", linewidth = 1.3) +
    geom_point(data = helix_pts, aes(x = x, y = y), size = 2.4, shape = 21, fill = "#F59E0B", color = "#0F172A") +
    scale_fill_viridis_d(option = "cividis", name = "Density Map:") +
    coord_fixed() +
    labs(
      title = "B. 3D Coulomb Density Mesh with Fitted Atomic Alpha-Helix",
      x = "Structure X (Å)",
      y = "Structure Y (Å)"
    ) +
    theme_classic(base_size = 9) +
    theme(plot.title = element_text(face = "bold", size = 10, color = "#0F172A"), legend.position = "none")
  
  if (requireNamespace("patchwork", quietly = TRUE)) {
    composite <- (p1 | p2) +
      patchwork::plot_annotation(
        title = "Cryo-EM High-Resolution Reconstruction: Atomic Model Fit & FSC Validation",
        subtitle = "Near-atomic resolution 2.4 Å map; FSC 0.143 gold-standard metric and density slice",
        caption = "Source: Simulated single-particle cryo-electron microscopy pipeline (RELION / CryoSPARC standard)"
      ) &
      theme(
        plot.title = element_text(face = "bold", size = 12, color = "#0F172A"),
        plot.subtitle = element_text(size = 9.5, color = "#475569")
      )
    return(composite)
  } else {
    p1
  }
}

explain <- function() {
  list(
    title = "Cryo-EM Density Reconstruction & FSC Validation",
    how_to_read = "Combines two essential validation panels: (A) The Fourier Shell Correlation (FSC) curve plotting correlation between independent half-dataset reconstructions across spatial frequencies, with the gold-standard 0.143 cutoff determining global map resolution (e.g. 2.4 Å). (B) Cross-sectional Coulomb potential density map with fitted atomic alpha-helical backbone coordinates.",
    when_to_use = "Mandatory in cryo-electron microscopy structural biology (PDB / EMDB deposits, RELION, CryoSPARC) to validate model fidelity and avoid map over-fitting.",
    pitfalls = "Beware of mask-induced correlation artifacts; phase randomization beyond the solvent mask is required to guarantee honest 0.143 FSC resolution reporting.",
    journal_archetype = "Nature, Science, and Cell macromolecular cryo-EM structure papers.",
    data_recipe = "Requires MRC electron density volume files and half-map FSC text tables."
  )
}
