# Cosmic Microwave Background (CMB) Angular Power Spectrum (Planck / WMAP)
# Tier 4 (Mind-Blowing) - Astronomy

make_plot <- function(data_dir = "data") {
  library(ggplot2)
  if (file.exists("theme_journal.R")) source("theme_journal.R", local = TRUE)
  
  # Simulate the canonical Planck 2018 CMB TT angular power spectrum
  # Multipole moment ell (from ell = 2 to 2500)
  ell_grid <- c(seq(2, 40, by = 2), seq(45, 2500, by = 15))
  
  # Theoretical Lambda-CDM model acoustic peaks:
  # 1st acoustic peak at ell ~ 220 (Spatial flatness)
  # 2nd acoustic peak at ell ~ 540 (Baryon density)
  # 3rd acoustic peak at ell ~ 800 (Dark matter density)
  # Silk damping tail beyond ell > 1000
  p1 <- 5800 * exp(-((ell_grid - 220) / 120)^2)
  p2 <- 2600 * exp(-((ell_grid - 540) / 110)^2)
  p3 <- 2500 * exp(-((ell_grid - 810) / 115)^2)
  p4 <- 1200 * exp(-((ell_grid - 1130) / 120)^2)
  p5 <- 800 * exp(-((ell_grid - 1420) / 130)^2)
  
  # Exponential Silk damping
  damping <- exp(-(ell_grid / 1400)^1.4)
  dl_model <- (900 + p1 + p2 + p3 + p4 + p5) * damping
  
  # Empirical satellite observation binned data points with cosmic variance error bars
  set.seed(42)
  obs_ell <- c(seq(10, 80, by = 15), seq(120, 2400, by = 55))
  obs_interp <- approx(ell_grid, dl_model, xout = obs_ell)$y
  # Cosmic variance error bar ~ sqrt(2 / (2*ell + 1)) * D_ell
  obs_err <- obs_interp * sqrt(2 / (2 * obs_ell + 1)) * 1.8 + runif(length(obs_ell), 15, 60)
  obs_dl <- obs_interp + rnorm(length(obs_ell), 0, obs_err * 0.45)
  
  df_model <- data.frame(ell = ell_grid, dl = dl_model)
  df_obs <- data.frame(ell = obs_ell, dl = obs_dl, err = obs_err)
  
  p <- ggplot() +
    # Theoretical best-fit Lambda-CDM cosmological model curve
    geom_line(data = df_model, aes(x = ell, y = dl), color = "#2563EB", linewidth = 1.05) +
    # Observed Planck satellite data points with error bars
    geom_errorbar(data = df_obs, aes(x = ell, ymin = dl - err, ymax = dl + err), 
                  width = 0, color = "#1E293B", linewidth = 0.4, alpha = 0.6) +
    geom_point(data = df_obs, aes(x = ell, y = dl), 
               shape = 21, size = 1.8, fill = "#DC2626", color = "#1E293B", stroke = 0.3) +
    # Annotate cosmological acoustic peaks
    annotate("text", x = 220, y = 6000, label = "1st Peak (ℓ ~ 220)\nSpatial Flatness (Ω_k = 0)", 
             fontface = "bold", size = 3.0, color = "#1E40AF") +
    annotate("text", x = 540, y = 3000, label = "2nd Peak\n(Baryon Density)", 
             fontface = "bold", size = 2.8, color = "#1E40AF") +
    annotate("text", x = 810, y = 2850, label = "3rd Peak\n(Dark Matter Density)", 
             fontface = "bold", size = 2.8, color = "#1E40AF") +
    annotate("text", x = 1600, y = 1400, label = "Silk Damping Tail", 
             fontface = "italic", size = 2.8, color = "#64748B") +
    scale_x_continuous(breaks = c(2, 500, 1000, 1500, 2000, 2500), limits = c(2, 2500)) +
    scale_y_continuous(breaks = seq(0, 6000, by = 1000), limits = c(0, 6400)) +
    labs(
      title = "Cosmic Microwave Background (CMB) Temperature Angular Power Spectrum",
      subtitle = "Multipole moment ℓ vs power D_ℓ = ℓ(ℓ+1)C_ℓ/2π; red points show Planck data, blue line shows ΛCDM fit",
      x = "Multipole Moment ℓ (Inverse Angular Scale on the Sky)",
      y = expression(bold("Temperature Power Spectrum ") * bolditalic(D[l]) * bold(" [") * bold(mu) * bold("K"^2 * "]")),
      caption = "Source: Simulated Planck Collaboration 2018 baseline ΛCDM cosmological parameters"
    )
  
  if (exists("theme_topjournal")) {
    p <- p + theme_topjournal(grid = "xy")
  } else {
    p <- p + theme_classic()
  }
  p
}

explain <- function() {
  list(
    title = "Cosmic Microwave Background (CMB) Angular Power Spectrum",
    how_to_read = "Plots the temperature anisotropy power of the early universe (380,000 years after the Big Bang) against multipole moment ell (high ell = smaller angular patches on the sky). The position and relative heights of the acoustic peaks definitively prove the geometry of the universe: the 1st peak proves spatial flatness (Omega_k = 0), the 2nd proves baryon density, and the 3rd constrains cold dark matter.",
    when_to_use = "The supreme foundational graphic of modern observational cosmology and physics (Planck Satellite, WMAP, COBE).",
    pitfalls = "At low multipoles (ell < 30), statistical error is fundamentally constrained by cosmic variance because our universe offers only a single observable sky.",
    journal_archetype = "Astronomy & Astrophysics, Physical Review Letters, and Nature cosmological parameter releases.",
    data_recipe = "Requires multipole ell values, binned temperature power D_ell, and cosmic variance covariance matrix."
  )
}
