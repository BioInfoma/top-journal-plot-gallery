# Climate Warming Stripes (Ed Hawkins Temperature Anomaly Visualization)
# Tier 4 (Mind-Blowing) - Climate

make_plot <- function(data_dir = "data") {
  library(ggplot2)
  if (file.exists("theme_journal.R")) source("theme_journal.R", local = TRUE)
  
  # Simulate 175 years of global mean surface temperature anomalies (1850-2024)
  set.seed(42)
  years <- 1850:2024
  n_yr <- length(years)
  
  # Base historical trend with acceleration in recent decades
  t_norm <- (years - 1850) / (2024 - 1850)
  warming_trend <- -0.3 + 0.1 * t_norm + 1.35 * (t_norm^3.2)
  noise <- rnorm(n_yr, mean = 0, sd = 0.12)
  anomaly <- warming_trend + noise
  
  df_stripes <- data.frame(
    year = years,
    anomaly = anomaly
  )
  
  # Ed Hawkins 8-tone diverging climate palette (Deep Blue to Dark Red)
  stripe_palette <- c(
    "#08306B", "#2171B5", "#6BAED6", "#BDD7E7", # Cool anomalies
    "#FCAE91", "#FB6A4A", "#CB181D", "#67000D"  # Warm anomalies
  )
  
  p <- ggplot(df_stripes, aes(x = year, y = 1, fill = anomaly)) +
    geom_tile(width = 1, color = NA) +
    scale_fill_gradientn(
      colors = stripe_palette,
      limits = c(-0.6, 1.4),
      name = "Temp Anomaly (°C)"
    ) +
    scale_x_continuous(breaks = seq(1850, 2020, by = 25), expand = c(0, 0)) +
    scale_y_continuous(expand = c(0, 0)) +
    labs(
      title = "Global Climate Warming Stripes (1850 – 2024)",
      subtitle = "Each vertical stripe represents annual global mean surface temperature relative to the pre-industrial baseline",
      x = "Year (1850 – 2024)",
      caption = "Design: Inspired by Prof. Ed Hawkins (Univ. of Reading) | Data: Simulated HadCRUT / Berkeley Earth trajectory"
    ) +
    theme_classic() +
    theme(
      plot.title = element_text(face = "bold", size = 13, color = "#0F172A", margin = margin(b = 4)),
      plot.subtitle = element_text(size = 9.5, color = "#475569", margin = margin(b = 10)),
      plot.caption = element_text(size = 8, color = "#94A3B8", hjust = 0, margin = margin(t = 8)),
      axis.title.y = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank(),
      axis.line.y = element_blank(),
      axis.line.x = element_line(color = "#334155", linewidth = 0.5),
      axis.text.x = element_text(face = "bold", size = 9.5, color = "#1E293B"),
      legend.position = "bottom",
      legend.key.width = unit(1.8, "cm"),
      legend.key.height = unit(0.3, "cm"),
      legend.title = element_text(size = 8.5, face = "bold", vjust = 0.8),
      plot.margin = margin(12, 16, 12, 12)
    )
  p
}

explain <- function() {
  list(
    title = "Global Climate Warming Stripes",
    how_to_read = "Chronological time advances from left (1850) to right (present). Each vertical stripe represents a single calendar year, color-coded by its global mean surface temperature anomaly relative to pre-industrial averages (deep blues indicate cooler years; bright crimsons indicate unprecedented record warming).",
    when_to_use = "Celebrated globally as one of the most powerful science communication graphics in history for conveying rapid, undeniable climate change without technical jargon.",
    pitfalls = "Stripping away axes and numbers maximizes public emotional impact, but for formal peer-reviewed academic papers, always provide the explicit numerical anomaly scale and baseline reference period.",
    journal_archetype = "Nature, Science, and IPCC Sixth Assessment Report (AR6) climate summaries.",
    data_recipe = "Requires annual series of global/regional mean temperature anomalies."
  )
}
