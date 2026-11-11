# Viral Phylogeographic Transmission Arc Map (Nextstrain / Science Standard)
# Tier 3 (Advanced) - Phylodynamics

make_plot <- function(data_dir = "data") {
  library(ggplot2)
  if (requireNamespace("maps", quietly = TRUE)) library(maps)
  if (file.exists("theme_journal.R")) source("theme_journal.R", local = TRUE)
  
  # Load world boundaries safely with local fallback
  coord_file <- file.path(data_dir, "world_map_coords.csv")
  if (file.exists(coord_file)) {
    world_map <- read.csv(coord_file)
  } else if (requireNamespace("maps", quietly = TRUE)) {
    world_map <- ggplot2::map_data("world")
    world_map <- world_map[world_map$region != "Antarctica", ]
  } else {
    stop("Geographic world boundary dataset not found. Please provide data/world_map_coords.csv")
  }
  
  # Global genomic surveillance outbreak nodes
  hubs <- data.frame(
    city = c("Hong Kong (Origin)", "London", "New York", "Tokyo", "Singapore", "Sydney", "São Paulo", "Nairobi", "Johannesburg"),
    lon = c(114.17, -0.12, -74.00, 139.69, 103.82, 151.21, -46.63, 36.82, 28.04),
    lat = c(22.31, 51.50, 40.71, 35.68, 1.35, -33.86, -23.55, -1.29, -26.20),
    isolates = c(1240, 680, 890, 410, 350, 280, 310, 220, 490),
    region = c("East Asia", "Europe", "North America", "East Asia", "Southeast Asia", "Oceania", "South America", "East Africa", "Southern Africa")
  )
  
  # Directional transmission routes
  routes <- data.frame(
    from_lon = c(114.17, 114.17, 114.17, 114.17, -0.12, -0.12, -74.00, 28.04),
    from_lat = c(22.31, 22.31, 22.31, 22.31, 51.50, 51.50, 40.71, -26.20),
    to_lon = c(-0.12, -74.00, 139.69, 103.82, 36.82, 28.04, -46.63, 151.21),
    to_lat = c(51.50, 40.71, 35.68, 1.35, -1.29, -26.20, -23.55, -33.86),
    wave = factor(c("Wave 1 (Founder)", "Wave 1 (Founder)", "Wave 1 (Founder)", "Wave 1 (Founder)", 
                    "Wave 2 (Lineage B.1)", "Wave 2 (Lineage B.1)", "Wave 2 (Lineage B.1)", "Wave 3 (Omicron)"),
                  levels = c("Wave 1 (Founder)", "Wave 2 (Lineage B.1)", "Wave 3 (Omicron)"))
  )
  
  # Compute smooth arched parabolic paths between hubs
  arc_list <- list()
  for (i in 1:nrow(routes)) {
    r <- routes[i, ]
    t_seq <- seq(0, 1, length.out = 45)
    
    # Linear interpolation
    arc_lon <- r$from_lon + t_seq * (r$to_lon - r$from_lon)
    # Parabolic elevation based on euclidean distance for 3D flight trajectory appearance
    dist <- sqrt((r$to_lon - r$from_lon)^2 + (r$to_lat - r$from_lat)^2)
    arc_lat <- r$from_lat + t_seq * (r$to_lat - r$from_lat) + (dist * 0.15) * sin(pi * t_seq)
    
    arc_list[[i]] <- data.frame(
      lon = arc_lon,
      lat = arc_lat,
      wave = r$wave,
      flow_id = i
    )
  }
  df_arcs <- do.call(rbind, arc_list)
  
  p <- ggplot() +
    # Global continental landmass base polygons
    geom_polygon(data = world_map, aes(x = long, y = lat, group = group), 
                 fill = "#F1F5F9", color = "#CBD5E1", linewidth = 0.25) +
    # Curved phylogeographic transmission arcs
    geom_path(data = df_arcs, aes(x = lon, y = lat, group = flow_id, color = wave), 
              linewidth = 1.1, alpha = 0.85) +
    # Outbreak epicenter nodes
    geom_point(data = hubs, aes(x = lon, y = lat, size = isolates), 
               shape = 21, fill = "#DC2626", color = "#FFFFFF", stroke = 1.0, alpha = 0.92) +
    geom_text(data = hubs, aes(x = lon, y = lat - 5.5, label = city), 
              fontface = "bold", size = 2.6, color = "#0F172A") +
    scale_color_manual(
      values = c(
        "Wave 1 (Founder)"      = "#2563EB",
        "Wave 2 (Lineage B.1)"  = "#D97706",
        "Wave 3 (Omicron)"      = "#7C3AED"
      ),
      name = "Epidemiological Dispersal Wave:"
    ) +
    scale_size_continuous(range = c(2.5, 7.5), name = "Sequenced Viral Isolates:") +
    scale_x_continuous(limits = c(-135, 175), expand = c(0, 0)) +
    scale_y_continuous(limits = c(-52, 78), expand = c(0, 0)) +
    coord_fixed(1.3) +
    labs(
      title = "Phylogeographic Transmission Arc Map: Global Genomic Pathogen Dispersal",
      subtitle = "Spatially explicit reconstruction of viral transmission lineages across international hubs; color indexes lineage emergence waves",
      caption = "Source: Simulated Bayesian discrete phylogeographic diffusion model (BEAST / TreeTime / Nextstrain architecture)"
    ) +
    theme_void() +
    theme(
      plot.title = element_text(face = "bold", size = 12.5, color = "#0F172A", margin = margin(b = 4)),
      plot.subtitle = element_text(size = 9.2, color = "#475569", margin = margin(b = 10)),
      plot.caption = element_text(size = 8, color = "#94A3B8", hjust = 0),
      legend.position = "top",
      legend.direction = "horizontal",
      legend.box = "horizontal",
      legend.title = element_text(face = "bold", size = 8.5),
      legend.text = element_text(size = 8.0),
      plot.margin = margin(14, 16, 14, 14)
    )
  p
}

explain <- function() {
  list(
    title = "Viral Phylogeographic Transmission Arc Map",
    how_to_read = "Global geographic coordinates establish surveillance epicenters. Curved parabolic arcs illustrate directional migration pathways inferred from Bayesian discrete phylogeography, with line color classifying evolutionary lineage waves (Founder, Variant of Concern, Omicron). Node circle radii scale with sequenced viral genomes.",
    when_to_use = "The international standard in real-time genomic epidemiology (Nextstrain, WHO, CDC) to visualize viral introductions, cross-border dispersal kinetics, and regional seeding events.",
    pitfalls = "Severe sequencing capacity disparities between countries bias ancestral state reconstruction; sample sets should be downsampled or regularized to prevent false super-spreader inferences.",
    journal_archetype = "Science, Nature, and Cell pandemic genomic epidemiology tracking.",
    data_recipe = "Requires geographic coordinates (lat/lon) of transmission hubs and Markov transition jump counts from ancestral phylogenetic trees."
  )
}
