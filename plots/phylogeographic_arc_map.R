# Viral Phylogeographic Transmission Arc Map
# Tier 3 (Advanced) - Phylodynamics

make_plot <- function(data_dir = "data") {
  library(ggplot2)
  if (file.exists("theme_journal.R")) source("theme_journal.R", local = TRUE)
  
  # Global outbreak transmission hubs
  hubs <- data.frame(
    city = c("Origin (Hub A)", "London", "New York", "Tokyo", "Singapore", "Sydney", "São Paulo", "Nairobi"),
    lon = c(114.17, -0.12, -74.00, 139.69, 103.82, 151.21, -46.63, 36.82),
    lat = c(22.31, 51.50, 40.71, 35.68, 1.35, -33.86, -23.55, -1.29),
    cases = c(850, 420, 610, 310, 240, 190, 180, 140)
  )
  
  # Transmission dispersal routes (Origin to international hubs)
  routes <- data.frame(
    from_lon = 114.17,
    from_lat = 22.31,
    to_lon = c(-0.12, -74.00, 139.69, 103.82, 151.21, -46.63, 36.82),
    to_lat = c(51.50, 40.71, 35.68, 1.35, -33.86, -23.55, -1.29),
    transmissions = c(45, 62, 28, 35, 18, 14, 12),
    time_wave = factor(c("Wave 1", "Wave 1", "Wave 1", "Wave 1", "Wave 2", "Wave 2", "Wave 2"), 
                       levels = c("Wave 1", "Wave 2"))
  )
  
  # Generate curved great-circle / parabolic arcs
  arc_list <- list()
  for (i in 1:nrow(routes)) {
    r <- routes[i, ]
    t_seq <- seq(0, 1, length.out = 40)
    # Parabolic height offset for 3D trajectory effect
    arc_lon <- r$from_lon + t_seq * (r$to_lon - r$from_lon)
    arc_lat <- r$from_lat + t_seq * (r$to_lat - r$from_lat) + 18 * sin(pi * t_seq)
    arc_list[[i]] <- data.frame(
      lon = arc_lon,
      lat = arc_lat,
      wave = r$time_wave,
      flow_id = i
    )
  }
  df_arcs <- do.call(rbind, arc_list)
  
  p <- ggplot() +
    # Simplified world map base
    borders("world", colour = "#CBD5E1", fill = "#F1F5F9", linewidth = 0.3) +
    # Transmission flow arcs
    geom_path(data = df_arcs, aes(x = lon, y = lat, group = flow_id, color = wave), 
              linewidth = 1.15, alpha = 0.85) +
    # Outbreak node points
    geom_point(data = hubs, aes(x = lon, y = lat, size = cases), 
               shape = 21, fill = "#DC2626", color = "#FFFFFF", stroke = 1.0, alpha = 0.9) +
    geom_text(data = hubs, aes(x = lon, y = lat - 7, label = city), 
              fontface = "bold", size = 2.7, color = "#1E293B") +
    scale_color_manual(values = c("Wave 1" = "#2563EB", "Wave 2" = "#D97706"), name = "Dispersal Wave:") +
    scale_size_continuous(range = c(2.5, 7.5), name = "Genomic Isolates:") +
    scale_x_continuous(limits = c(-130, 175), expand = c(0, 0)) +
    scale_y_continuous(limits = c(-55, 75), expand = c(0, 0)) +
    labs(
      title = "Phylogeographic Arc Map: Global Pathogen Dispersal Routes",
      subtitle = "Spatial reconstruction of international transmission lineages; arcs depict epidemiological spread",
      caption = "Source: Simulated discrete phylogeographic diffusion model (BEAST / SPREAD)"
    ) +
    theme_void() +
    theme(
      plot.title = element_text(face = "bold", size = 13, color = "#0F172A", margin = margin(b = 4)),
      plot.subtitle = element_text(size = 9.5, color = "#475569", margin = margin(b = 10)),
      plot.caption = element_text(size = 8, color = "#94A3B8", hjust = 0),
      legend.position = "top",
      legend.direction = "horizontal",
      legend.box = "horizontal",
      legend.title = element_text(face = "bold", size = 8.5),
      legend.text = element_text(size = 8.5),
      plot.margin = margin(12, 14, 12, 12)
    )
  p
}

explain <- function() {
  list(
    title = "Viral Phylogeographic Transmission Arc Map",
    how_to_read = "Global geographic coordinates anchor outbreak epicenters and destination cities. Curved arcs illustrate directional migration pathways inferred from Bayesian discrete trait phylogeography, with arc color indexing epidemiological wave.",
    when_to_use = "The international standard in genomic epidemiology (WHO, Nextstrain) to trace global viral introductions, border transmission dynamics, and local community seeding.",
    pitfalls = "Sampling bias across countries strongly distorts inferred migration routes; countries with high sequencing capacity appear as false super-spreaders if uncorrected.",
    journal_archetype = "Science and Nature pandemic phylogeography (e.g. Nextstrain real-time pathogen tracking).",
    data_recipe = "Requires geographic coordinates (lat/lon) of transmission nodes and posterior migration Markov jump counts."
  )
}
