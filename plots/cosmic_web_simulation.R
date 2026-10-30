# Large-Scale Cosmic Web Simulation (IllustrisTNG / Millennium Standard)
# Tier 4 (Mind-Blowing) - Astronomy

make_plot <- function(data_dir = "data") {
  library(ggplot2)
  if (file.exists("theme_journal.R")) source("theme_journal.R", local = TRUE)
  
  # Simulate 100 Megaparsec cosmological slice of dark matter cosmic web (IllustrisTNG standard)
  set.seed(42)
  n_pts <- 1400
  
  # Major cosmic web cluster nodes (vertices of dark matter filaments)
  nodes <- data.frame(
    x = c(25, 75, 45, 80, 20),
    y = c(30, 25, 70, 75, 80)
  )
  
  # Generate filamentary streams connecting cluster nodes
  filament_list <- list()
  edges <- list(c(1, 2), c(1, 3), c(2, 4), c(3, 4), c(3, 5), c(1, 5))
  for (e in edges) {
    p_from <- nodes[e[1], ]
    p_to <- nodes[e[2], ]
    # Distribute particles along filament with gaussian perpendicular scatter
    n_fil <- sample(120:200, 1)
    t_val <- runif(n_fil, 0, 1)
    # Filament path with slight curvature
    fx <- p_from$x + t_val * (p_to$x - p_from$x) + rnorm(n_fil, 0, 2.2)
    fy <- p_from$y + t_val * (p_to$y - p_from$y) + rnorm(n_fil, 0, 2.2)
    density_val <- rnorm(n_fil, mean = 6.5, sd = 1.0)
    filament_list[[length(filament_list) + 1]] <- data.frame(x = fx, y = fy, density = density_val)
  }
  
  # Add cosmic voids (sparse background particles)
  n_void <- 300
  void_pts <- data.frame(
    x = runif(n_void, 5, 95),
    y = runif(n_void, 5, 95),
    density = runif(n_void, 0.5, 2.5)
  )
  
  # Add dense galaxy cluster cores
  cluster_list <- list()
  for (i in 1:nrow(nodes)) {
    cx <- nodes$x[i]; cy <- nodes$y[i]
    cluster_list[[i]] <- data.frame(
      x = rnorm(60, cx, 1.2),
      y = rnorm(60, cy, 1.2),
      density = rnorm(60, 9.5, 0.8)
    )
  }
  
  df_cosmic <- rbind(do.call(rbind, filament_list), void_pts, do.call(rbind, cluster_list))
  
  p <- ggplot(df_cosmic, aes(x = x, y = y)) +
    # Density contours simulating gas/dark matter potential
    stat_density_2d(aes(fill = after_stat(level)), geom = "polygon", alpha = 0.35, bins = 8) +
    # Galaxy / dark matter particles
    geom_point(aes(color = density, size = density), alpha = 0.72) +
    # Annotate cosmic voids and clusters
    annotate("text", x = 45, y = 42, label = "Cosmic Void\n(Underdense Region)", 
             fontface = "bold", size = 3.3, color = "#64748B") +
    annotate("text", x = 25, y = 33, label = "Supercluster Node", 
             fontface = "bold", size = 3.1, color = "#F59E0B") +
    annotate("text", x = 60, y = 74, label = "Baryonic Gas Filament", 
             fontface = "italic", size = 3.0, color = "#93C5FD") +
    scale_fill_viridis_c(option = "magma", guide = "none") +
    scale_color_viridis_c(option = "inferno", name = "Dark Matter Log Density:") +
    scale_size_continuous(range = c(0.8, 3.2), guide = "none") +
    scale_x_continuous(breaks = seq(20, 80, by = 20), labels = paste0(seq(20, 80, by = 20), " Mpc"), limits = c(5, 95)) +
    scale_y_continuous(breaks = seq(20, 80, by = 20), labels = paste0(seq(20, 80, by = 20), " Mpc"), limits = c(5, 95)) +
    coord_fixed() +
    labs(
      title = "Large-Scale Cosmic Web: Dark Matter & Galaxy Filamentary Structure",
      subtitle = "N-body cosmological hydrodynamical simulation; filamentary bridges connect massive galaxy clusters",
      x = "Comoving Spatial Scale X (Megaparsecs, Mpc)",
      y = "Comoving Spatial Scale Y (Megaparsecs, Mpc)",
      caption = "Source: Simulated IllustrisTNG / Millennium N-body cosmological dark matter simulation"
    )
  
  if (exists("theme_topjournal")) {
    p <- p + theme_topjournal(grid = "none") +
      theme(
        axis.text = element_text(size = rel(0.85), color = "#94A3B8"),
        plot.background = element_rect(fill = "#0B0F19", color = NA),
        panel.background = element_rect(fill = "#0B0F19", color = NA),
        plot.title = element_text(color = "#F8FAFC"),
        plot.subtitle = element_text(color = "#94A3B8"),
        plot.caption = element_text(color = "#64748B"),
        axis.title = element_text(color = "#CBD5E1"),
        legend.position = "top",
        legend.direction = "horizontal",
        legend.title = element_text(face = "bold", size = 8.5, color = "#F8FAFC"),
        legend.text = element_text(size = 8.0, color = "#CBD5E1"),
        legend.key.width = unit(1.8, "cm"),
        legend.key.height = unit(0.35, "cm")
      )
  } else {
    p <- p + theme_dark()
  }
  p
}

explain <- function() {
  list(
    title = "Large-Scale Cosmic Web Cosmological Simulation",
    how_to_read = "A 100-Megaparsec spatial slice of the universe revealing its sponge-like large-scale architecture. Intersecting nodes represent supermassive galaxy clusters, connected by gravitational dark matter filaments that channel baryonic gas, surrounding vast underdense cosmic voids spanning dozens of Megaparsecs.",
    when_to_use = "The international benchmark in computational astrophysics and cosmological N-body simulations (IllustrisTNG, Millennium Simulation) to study galaxy formation, dark matter halos, and cosmic evolution.",
    pitfalls = "2D projections of thick 3D simulation slices can create artificial filament overlaps; slice thickness must be appropriately calibrated to distinguish true physical filaments from projection artifacts.",
    journal_archetype = "Nature, Science, and Monthly Notices of the Royal Astronomical Society (MNRAS) cosmological simulation releases.",
    data_recipe = "Requires N-body simulation particle coordinates and gravitational potential density grids."
  )
}
