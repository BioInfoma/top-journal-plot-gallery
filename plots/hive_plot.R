# Deterministic Network Hive Plot
# Tier 3 (Advanced) - Network

make_plot <- function(data_dir = "data") {
  library(ggplot2)
  if (file.exists("theme_journal.R")) source("theme_journal.R", local = TRUE)
  
  # Three radial axes at 120-degree intervals (0, 120, 240 degrees)
  angles <- c(pi/2, pi/2 + 2*pi/3, pi/2 + 4*pi/3) # 90, 210, 330 degrees
  axis_names <- c("Axis 1 (Regulators)", "Axis 2 (Kinases)", "Axis 3 (Effectors)")
  
  # Simulate 15 nodes per axis along radial distances r = 1 to 5
  set.seed(42)
  n_per_axis <- 12
  node_list <- list()
  for (a in 1:3) {
    r_vals <- seq(1.2, 5.0, length.out = n_per_axis)
    node_list[[a]] <- data.frame(
      axis = a,
      axis_name = axis_names[a],
      r = r_vals,
      x = r_vals * cos(angles[a]),
      y = r_vals * sin(angles[a]),
      node_id = paste0("A", a, "_", 1:n_per_axis)
    )
  }
  df_nodes <- do.call(rbind, node_list)
  
  # Simulate curved edges bridging adjacent axes (Axis 1 -> Axis 2, Axis 2 -> Axis 3, Axis 3 -> Axis 1)
  edge_list <- list()
  for (i in 1:16) {
    # Pick adjacent axes
    a_from <- sample(1:3, 1)
    a_to <- ifelse(a_from == 3, 1, a_from + 1)
    
    node_from <- df_nodes[df_nodes$axis == a_from, ][sample(n_per_axis, 1), ]
    node_to <- df_nodes[df_nodes$axis == a_to, ][sample(n_per_axis, 1), ]
    
    # Quadratic bezier curve via center pull
    t_seq <- seq(0, 1, length.out = 30)
    # Control point pulled slightly toward origin
    cx <- (node_from$x + node_to$x) * 0.35
    cy <- (node_from$y + node_to$y) * 0.35
    
    bx <- (1 - t_seq)^2 * node_from$x + 2 * (1 - t_seq) * t_seq * cx + t_seq^2 * node_to$x
    by <- (1 - t_seq)^2 * node_from$y + 2 * (1 - t_seq) * t_seq * cy + t_seq^2 * node_to$y
    
    edge_list[[i]] <- data.frame(
      x = bx,
      y = by,
      edge_id = i,
      from_axis = axis_names[a_from]
    )
  }
  df_edges <- do.call(rbind, edge_list)
  
  # Axis ray line segments
  df_rays <- data.frame(
    x = 0, y = 0,
    xend = 5.4 * cos(angles),
    yend = 5.4 * sin(angles),
    axis_name = axis_names
  )
  
  p <- ggplot() +
    # Three coordinate radial axis rays
    geom_segment(data = df_rays, aes(x = x, y = y, xend = xend, yend = yend), 
                 color = "#475569", linewidth = 1.2) +
    # Curved inter-axis connection edges
    geom_path(data = df_edges, aes(x = x, y = y, group = edge_id, color = from_axis), 
              linewidth = 0.95, alpha = 0.75) +
    # Nodes along axes
    geom_point(data = df_nodes, aes(x = x, y = y, fill = axis_name), 
               shape = 21, size = 3.4, color = "#0F172A", stroke = 0.6) +
    # Axis label text
    annotate("text", x = 6.2 * cos(angles[1]), y = 6.2 * sin(angles[1]), label = axis_names[1], fontface = "bold", size = 3.3, color = "#1E293B") +
    annotate("text", x = 6.4 * cos(angles[2]), y = 6.4 * sin(angles[2]), label = axis_names[2], fontface = "bold", size = 3.3, color = "#1E293B") +
    annotate("text", x = 6.4 * cos(angles[3]), y = 6.4 * sin(angles[3]), label = axis_names[3], fontface = "bold", size = 3.3, color = "#1E293B") +
    scale_fill_manual(values = c("#2563EB", "#059669", "#D97706"), name = "Network Layer:") +
    scale_color_manual(values = c("#2563EB", "#059669", "#D97706"), name = "Source Layer:") +
    coord_fixed() +
    labs(
      title = "Deterministic Hive Plot: Eliminating Network Hairballs",
      subtitle = "Nodes arranged along radial axes by functional class; curved bezier paths connect interacting nodes",
      caption = "Source: Simulated multi-tier signaling cascade network topology (Krzywinski standard)"
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
    title = "Deterministic Network Hive Plot (Krzywinski Standard)",
    how_to_read = "Nodes are mapped deterministically along coordinate radial rays according to categorical rules (e.g. node type, subcellular localization) and ordered by quantitative metrics (e.g. degree, expression). Curved edges connect nodes between adjacent rays.",
    when_to_use = "Designed specifically to overcome the uninterpretable 'hairball' artifacts of force-directed spring layouts when comparing structural properties across large biological networks.",
    pitfalls = "Edges are only drawn between adjacent axes (not across non-adjacent rays), so multi-tier signaling networks require careful axis assignment.",
    journal_archetype = "Briefings in Bioinformatics and PLoS ONE network comparison diagrams.",
    data_recipe = "Requires node layer/axis classifications and an interaction edge list."
  )
}
