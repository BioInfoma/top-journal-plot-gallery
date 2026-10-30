# Hubble-Lemaître Expansion Diagram (Type Ia Supernovae)
# Tier 4 (Mind-Blowing) - Astronomy

make_plot <- function(data_dir = "data") {
  library(ggplot2)
  if (file.exists("theme_journal.R")) source("theme_journal.R", local = TRUE)
  
  # Simulate 120 Type Ia Supernovae standard candle cosmological measurements
  set.seed(42)
  distances_mpc <- runif(120, 20, 950) # Distance in Megaparsecs (Mpc)
  h0 <- 70.4 # Hubble constant km/s/Mpc
  # Recessional velocity v = H0 * d + cosmological acceleration component
  velocities <- h0 * distances_mpc + 0.015 * (distances_mpc^1.4) + rnorm(120, 0, 1800)
  
  df_hubble <- data.frame(
    distance = distances_mpc,
    velocity = velocities,
    redshift = velocities / 299792 # z = v/c
  )
  
  # Linear model fit for Hubble constant
  hubble_fit <- lm(velocity ~ distance - 1, data = df_hubble)
  est_h0 <- round(coef(hubble_fit), 1)
  
  p <- ggplot(df_hubble, aes(x = distance, y = velocity)) +
    geom_smooth(method = "lm", formula = y ~ x - 1, color = "#DC2626", linewidth = 1.05, se = TRUE, alpha = 0.15) +
    geom_point(aes(color = redshift), size = 2.8, shape = 21, stroke = 0.4, fill = "#38BDF8", color = "#1E293B", alpha = 0.82) +
    scale_y_continuous(labels = scales::comma_format()) +
    scale_x_continuous(labels = scales::comma_format()) +
    annotate("label", x = 150, y = 60000, 
             label = paste0("Hubble Constant Fit:\nH_0 = ", est_h0, " km/s/Mpc\nLinear Hubble-Lemaître Law: v = H_0 × d"), 
             fontface = "bold", size = 3.6, color = "#0F172A", fill = "#FFFFFF") +
    labs(
      title = "Hubble-Lemaître Diagram: Metric Expansion of the Universe",
      subtitle = "Type Ia Supernovae standard candles demonstrating cosmological redshift proportional to distance",
      x = "Luminosity Distance to Host Galaxy [Megaparsecs, Mpc]",
      y = "Recessional Velocity [km / second]",
      caption = "Source: Simulated High-Z Supernova / Supernova Cosmology Project standard candle dataset (N = 120)"
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
    title = "Hubble-Lemaître Cosmic Expansion Diagram",
    how_to_read = "Extragalactic distance (in Megaparsecs, calibrated via Cepheid variables and Type Ia supernovae) is plotted on the horizontal axis against recessional velocity (calculated from spectral Doppler redshift) on the vertical axis. The linear slope represents the Hubble Constant (H0), defining the current cosmic expansion rate.",
    when_to_use = "Foundational in cosmology to measure the expansion rate and age of the universe, and historically to discover dark energy and cosmic acceleration.",
    pitfalls = "At extreme distances (high redshift z > 0.5), relativistic cosmological models deviate from a simple linear line due to dark energy acceleration; fitting a simple linear line at high z will misestimate H0.",
    journal_archetype = "The Astrophysical Journal and Science Nobel-prize winning cosmic acceleration papers.",
    data_recipe = "Requires galaxy/supernova distance estimates and corresponding recessional velocities or redshifts."
  )
}
