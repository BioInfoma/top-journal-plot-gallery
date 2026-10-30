# Hertzsprung-Russell (H-R) Diagram of Stellar Astrophysics
# Tier 4 (Mind-Blowing) - Astronomy

make_plot <- function(data_dir = "data") {
  library(ggplot2)
  if (file.exists("theme_journal.R")) source("theme_journal.R", local = TRUE)
  
  # Simulate 700 stars across stellar evolutionary classes
  set.seed(42)
  # 1. Main Sequence stars (diagonal from hot/bright to cool/dim)
  n_ms <- 450
  t_ms <- 10^runif(n_ms, 3.45, 4.45) # 2,800 K to 28,000 K
  lum_ms <- (t_ms / 5778)^4 * 10^rnorm(n_ms, 0, 0.45)
  
  # 2. Red Giants / Supergiants (cool surface ~3000-5000K, high luminosity)
  n_rg <- 150
  t_rg <- runif(n_rg, 3000, 5200)
  lum_rg <- 10^runif(n_rg, 1.8, 4.5)
  
  # 3. White Dwarfs (hot surface ~8000-25000K, very low luminosity ~10^-4 to 10^-2)
  n_wd <- 100
  t_wd <- runif(n_wd, 8000, 26000)
  lum_wd <- 10^runif(n_wd, -4.2, -1.5)
  
  df_stars <- data.frame(
    temp = c(t_ms, t_rg, t_wd),
    lum = c(lum_ms, lum_rg, lum_wd),
    class = c(rep("Main Sequence", n_ms), rep("Red Giants & Supergiants", n_rg), rep("White Dwarfs", n_wd))
  )
  
  # Color stars by blackbody physical temperature (Blue -> White -> Yellow -> Red)
  star_colors <- c(
    "Main Sequence" = "#F59E0B",            # Sun-like Gold
    "Red Giants & Supergiants" = "#DC2626", # Red
    "White Dwarfs" = "#38BDF8"              # Blue/Cyan
  )
  
  p <- ggplot(df_stars, aes(x = temp, y = lum)) +
    geom_point(aes(color = class, size = lum), alpha = 0.75, shape = 16) +
    # The Sun benchmark (5,778 K, Lum = 1.0)
    annotate("point", x = 5778, y = 1.0, shape = 21, size = 4.5, fill = "#FACC15", color = "#1E293B", stroke = 1.2) +
    annotate("text", x = 5778 * 1.18, y = 1.8, label = "☉ The Sun\n(1 L_sun, 5,778 K)", fontface = "bold", size = 3.2, color = "#1E293B") +
    scale_x_log10(
      limits = c(35000, 2400),
      breaks = c(30000, 10000, 6000, 4000, 3000),
      labels = c("30,000 K\n(Class O)", "10,000 K\n(Class A)", "6,000 K\n(Class G)", "4,000 K\n(Class K)", "3,000 K\n(Class M)")
    ) +
    scale_y_log10(
      breaks = 10^seq(-4, 4, by = 2),
      labels = scales::trans_format("log10", scales::math_format(10^.x)),
      limits = c(1e-5, 1e5)
    ) +
    scale_color_manual(values = star_colors, name = "Stellar Branch") +
    scale_size_continuous(range = c(1.6, 5.0), guide = "none") +
    annotate("text", x = 18000, y = 2e3, label = "Main Sequence Band", fontface = "italic", color = "#94A3B8", size = 3.8, angle = -32) +
    annotate("text", x = 3800, y = 3e3, label = "Red Giants / Supergiants", fontface = "bold", color = "#DC2626", size = 3.6) +
    annotate("text", x = 16000, y = 8e-4, label = "White Dwarfs", fontface = "bold", color = "#0284C7", size = 3.6) +
    labs(
      title = "Hertzsprung-Russell (H-R) Diagram: Stellar Evolution",
      subtitle = "Stellar luminosity vs effective surface temperature; note inverted temperature axis",
      x = "Effective Surface Temperature (Kelvin, Inverted Axis)",
      y = expression(bold("Luminosity relative to Sun [") * bolditalic(L / L)[bold("☉")] * bold(", Log Scale]")),
      caption = "Source: Simulated Gaia satellite stellar astrometry and photometry catalog"
    )
  
  if (exists("theme_topjournal")) {
    p <- p + theme_topjournal(grid = "xy") +
      theme(
        axis.text.x = element_text(size = rel(0.85)),
        legend.position = "top"
      )
  } else {
    p <- p + theme_classic()
  }
  p
}

explain <- function() {
  list(
    title = "Hertzsprung-Russell (H-R) Astrophysical Diagram",
    how_to_read = "Plots stellar luminosity (power output relative to the Sun on a log scale) against effective surface temperature (conventionally plotted backward, with blazing blue stars on the left and cool red stars on the right). Most stars lie along the diagonal 'Main Sequence'.",
    when_to_use = "The foundational diagram of modern astrophysics for classifying stars, establishing stellar ages, and tracing life cycles from main sequence to white dwarf or supernova.",
    pitfalls = "Remember that the horizontal temperature axis is inverted (hot to cold) by historical astronomical convention; flipping it normal confuses astrophysicists.",
    journal_archetype = "The Astrophysical Journal and Astronomy & Astrophysics stellar population figures.",
    data_recipe = "Requires stellar surface temperatures (or B-V color index) and absolute magnitudes/luminosities."
  )
}
