# Gravitational Wave Time-Frequency Chirp Spectrogram (LIGO GW150914 Style)
# Tier 4 (Mind-Blowing) - Physics

make_plot <- function(data_dir = "data") {
  library(ggplot2)
  if (file.exists("theme_journal.R")) source("theme_journal.R", local = TRUE)
  
  # Simulate LIGO gravitational wave binary black hole merger chirp (GW150914 archetype)
  # Time window from -0.20 to +0.05 seconds around merger (t = 0 is coalescence merger peak)
  set.seed(42)
  time_seq <- seq(-0.20, 0.05, length.out = 120)
  freq_seq <- seq(25, 400, length.out = 90)
  
  grid_tf <- expand.grid(time = time_seq, freq = freq_seq)
  
  # Relativistic post-Newtonian frequency chirp: f(t) = C * (t_merger - t)^(-3/8)
  # Chirp trajectory rising from 35 Hz at t = -0.15s to 260 Hz at merger t = 0
  chirp_center <- ifelse(grid_tf$time < 0, 
                         40 + 220 * exp(18 * grid_tf$time), 
                         260 * exp(-35 * grid_tf$time)) # Ringdown decay
  
  # Normalized energy density / Q-transform power
  delta_f <- abs(grid_tf$freq - chirp_center)
  energy_power <- 18 * exp(-(delta_f^2) / (2 * (25 + 15 * abs(grid_tf$time))^2)) + rnorm(nrow(grid_tf), 0, 0.5)
  energy_power <- pmax(0, energy_power)
  
  grid_tf$energy <- energy_power
  
  # Theoretical strain curve overlay at merger
  chirp_line <- data.frame(
    time = seq(-0.18, 0.02, length.out = 80),
    freq = ifelse(seq(-0.18, 0.02, length.out = 80) < 0,
                  40 + 220 * exp(18 * seq(-0.18, 0.02, length.out = 80)),
                  260 * exp(-35 * seq(-0.18, 0.02, length.out = 80)))
  )
  
  p <- ggplot(grid_tf, aes(x = time, y = freq)) +
    geom_raster(aes(fill = energy), interpolate = TRUE) +
    # Theoretical General Relativity numerical relativity trajectory
    geom_path(data = chirp_line, aes(x = time, y = freq), color = "#FFFFFF", linewidth = 1.05, linetype = "dashed") +
    scale_fill_viridis_c(option = "turbo", name = "Normalized Wave Energy (SNR):") +
    scale_x_continuous(breaks = seq(-0.2, 0.05, by = 0.05), labels = paste0(seq(-0.2, 0.05, by = 0.05), " s")) +
    scale_y_continuous(breaks = c(35, 100, 200, 300, 400), limits = c(25, 400)) +
    annotate("text", x = -0.01, y = 300, label = "Merger Coalescence Peak\n(f ~ 260 Hz)", 
             fontface = "bold", size = 3.3, color = "#FFFFFF") +
    annotate("text", x = -0.14, y = 75, label = "Early Inspiral Track", 
             fontface = "bold", size = 3.0, color = "#FFFFFF") +
    labs(
      title = "Gravitational Wave Time-Frequency Chirp Spectrogram (GW150914)",
      subtitle = "Q-transform time-frequency map of binary black hole coalescence; dashed line tracks general relativistic fit",
      x = "Time Relative to Merger Coalescence Event (Seconds)",
      y = "Gravitational Wave Frequency (Hertz, Hz)",
      caption = "Source: Simulated LIGO Hanford / Livingston gravitational wave observatory strain telemetry"
    )
  
  if (exists("theme_topjournal")) {
    p <- p + theme_topjournal(grid = "none") +
      theme(
        axis.text = element_text(size = rel(0.85)),
        legend.position = "top",
        legend.direction = "horizontal",
        legend.title = element_text(face = "bold", size = 8.5),
        legend.key.width = unit(1.8, "cm"),
        legend.key.height = unit(0.35, "cm")
      )
  } else {
    p <- p + theme_minimal()
  }
  p
}

explain <- function() {
  list(
    title = "Gravitational Wave Time-Frequency Chirp Spectrogram",
    how_to_read = "A time-frequency spectrogram (Q-transform) where the horizontal axis tracks milliseconds leading up to and following black hole coalescence, and the vertical axis tracks gravitational wave frequency in Hertz. Color intensity depicts gravitational wave strain energy. The upward sweeping curve ('chirp') reflects the accelerating orbital velocity and frequency of two merging black holes.",
    when_to_use = "The historic Nobel-prize winning icon of astrophysics (LIGO/Virgo, Abbott et al., Physical Review Letters 2016) proving Einstein's general relativity and the existence of binary black hole systems.",
    pitfalls = "Instrumental glitched noise bursts can mimic chirp signatures; authentic gravitational waves must be verified across multiple independent laser interferometers (Hanford, Livingston, Virgo) with sub-millisecond light-travel arrival consistency.",
    journal_archetype = "Physical Review Letters, Nature, and Science gravitational astrophysics releases.",
    data_recipe = "Requires high-frequency strain time-series data with continuous wavelet / Q-transform decomposition."
  )
}
