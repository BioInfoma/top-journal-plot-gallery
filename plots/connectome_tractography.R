# Diffusion MRI Connectome Tractography Streamlines (HCP Style)
# Tier 4 (Mind-Blowing) - Neuroscience

make_plot <- function(data_dir = "data") {
  library(ggplot2)
  if (file.exists("theme_journal.R")) source("theme_journal.R", local = TRUE)
  
  # Simulate 2D projection of brain white-matter axonal fiber bundles
  # Standard directional color coding:
  # Red = Left-Right (Commissural fibers / Corpus callosum)
  # Green = Anterior-Posterior (Association fibers / Cingulum / SLF)
  # Blue = Superior-Inferior (Projection fibers / Corticospinal tract)
  set.seed(42)
  
  # Brain silhouette outline
  theta_brain <- seq(0, 2*pi, length.out = 120)
  brain_outline <- data.frame(
    x = 1.35 * 38 * cos(theta_brain) + 5 * cos(2 * theta_brain),
    y = 32 * sin(theta_brain) + 4 * sin(theta_brain)
  )
  
  # 1. Corpus Callosum fibers (Left-Right commissural - Red)
  t_seq <- seq(-25, 25, length.out = 30)
  callosum_list <- list()
  for (i in 1:12) {
    y_off <- runif(1, 4, 16)
    curve_arch <- y_off + 8 * (1 - (t_seq / 26)^2)
    callosum_list[[i]] <- data.frame(
      x = t_seq,
      y = curve_arch + rnorm(length(t_seq), 0, 0.3),
      tract = "Commissural (Corpus Callosum)",
      tract_id = paste0("cc_", i)
    )
  }
  
  # 2. Corticospinal Projection fibers (Superior-Inferior - Blue)
  cst_list <- list()
  for (i in 1:12) {
    x_off <- runif(1, -12, 12)
    y_path <- seq(22, -24, length.out = 30)
    x_path <- x_off + (y_path / 24) * 4 + rnorm(30, 0, 0.4)
    cst_list[[i]] <- data.frame(
      x = x_path,
      y = y_path,
      tract = "Projection (Corticospinal)",
      tract_id = paste0("cst_", i)
    )
  }
  
  # 3. Association bundles (Anterior-Posterior - Green)
  assoc_list <- list()
  for (i in 1:12) {
    x_path <- seq(-34, 34, length.out = 30)
    y_path <- -8 + 12 * sin(x_path / 18) + rnorm(30, 0, 0.4)
    assoc_list[[i]] <- data.frame(
      x = x_path,
      y = y_path,
      tract = "Association (Longitudinal Fasciculus)",
      tract_id = paste0("assoc_", i)
    )
  }
  
  df_tracts <- rbind(do.call(rbind, callosum_list), do.call(rbind, cst_list), do.call(rbind, assoc_list))
  df_tracts$tract <- factor(df_tracts$tract, levels = c("Commissural (Corpus Callosum)", "Projection (Corticospinal)", "Association (Longitudinal Fasciculus)"))
  
  tract_cols <- c(
    "Commissural (Corpus Callosum)" = "#DC2626", # Red (Left-Right)
    "Projection (Corticospinal)" = "#2563EB",     # Blue (Superior-Inferior)
    "Association (Longitudinal Fasciculus)" = "#059669" # Green (Anterior-Posterior)
  )
  
  p <- ggplot() +
    # Brain cranial silhouette
    geom_polygon(data = brain_outline, aes(x = x, y = y), fill = "#0F172A", color = "#334155", linewidth = 1.0) +
    # White-matter streamline tractography fibers
    geom_path(data = df_tracts, aes(x = x, y = y, group = tract_id, color = tract), 
              linewidth = 0.85, alpha = 0.85) +
    scale_color_manual(values = tract_cols, name = "Directional Tract Orientation:") +
    coord_fixed() +
    labs(
      title = "Human Connectome White-Matter Tractography Streamlines",
      subtitle = "Diffusion tensor imaging (DTI) directional tract reconstruction (Human Connectome Project standard)",
      caption = "Source: Simulated high-angular-resolution diffusion MRI tractography (DSI Studio / MRtrix3 standard)"
    ) +
    theme_void() +
    theme(
      plot.title = element_text(face = "bold", size = 13, color = "#0F172A", margin = margin(b = 4)),
      plot.subtitle = element_text(size = 9.5, color = "#475569", margin = margin(b = 10)),
      plot.caption = element_text(size = 8, color = "#94A3B8", hjust = 0),
      legend.position = "top",
      legend.direction = "horizontal",
      legend.title = element_text(face = "bold", size = 8.5),
      legend.text = element_text(size = 8.5),
      plot.margin = margin(12, 14, 12, 12)
    )
  p
}

explain <- function() {
  list(
    title = "Diffusion MRI Connectome Tractography Streamlines",
    how_to_read = "Tracks axonal white-matter fibers connecting cortical and subcortical brain areas. Directional color-coding follows international neuroimaging conventions: red = left-right commissural fibers (corpus callosum), green = anterior-posterior association bundles, blue = superior-inferior projection pathways (corticospinal tract).",
    when_to_use = "Essential in clinical neurology, cognitive neuroscience, and neurosurgery (Human Connectome Project, MRtrix3) to map neural structural connectivity and pre-surgical tumor resection boundaries.",
    pitfalls = "Tractography streamlines reflect mathematical models of water diffusion, NOT direct biological axons; crossing fibers in voxel bottlenecks can produce false-positive anatomical connections.",
    journal_archetype = "NeuroImage, Nature Neuroscience, and Science human connectome maps.",
    data_recipe = "Requires diffusion-weighted MRI (DWI/DTI) volumes and fiber streamline tracking coordinate files."
  )
}
