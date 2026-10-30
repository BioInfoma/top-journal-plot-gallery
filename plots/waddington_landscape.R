# C.H. Waddington Epigenetic Developmental Landscape
# Tier 4 (Mind-Blowing) - Single-cell

make_plot <- function(data_dir = "data") {
  library(ggplot2)
  if (file.exists("theme_journal.R")) source("theme_journal.R", local = TRUE)
  
  # Simulate 2D Waddington epigenetic landscape potential surface
  # Valleys branch as developmental time increases from top (y = 10, Stem) to bottom (y = 0, Differentiated)
  set.seed(42)
  grid_pts <- expand.grid(x = seq(-8, 8, length.out = 80), y = seq(0, 10, length.out = 80))
  
  # Valley branching potential U(x, y) = x^4 - a(y)*x^2 + b(y)*x
  # At y = 10 (Stem cell), single central valley at x = 0
  # At y = 5, bifurcation into 2 valleys
  # At y = 0, terminal trifurcation into 3 distinct valleys
  a_y <- 1.2 * (10 - grid_pts$y) # Ridge height between valleys
  potential <- 0.08 * (grid_pts$x^4) - 0.25 * a_y * (grid_pts$x^2) + 0.3 * grid_pts$y
  grid_pts$potential <- potential
  
  # Cell balls rolling down valleys
  cell_balls <- data.frame(
    x = c(0, -2.4, 2.4, -4.5, 4.2),
    y = c(9.2, 5.0, 4.8, 1.2, 1.4),
    state = c("Pluripotent Stem Ball", "Mesoderm Intermediate", "Endoderm Intermediate", "Terminal Fate A", "Terminal Fate B")
  )
  
  p <- ggplot(grid_pts, aes(x = x, y = y)) +
    geom_contour_filled(aes(z = potential), bins = 9) +
    geom_contour(aes(z = potential), color = "#FFFFFF", linewidth = 0.25, alpha = 0.45) +
    # Ball trajectories rolling down the valley floor
    geom_path(data = data.frame(x = c(0, -1.2, -2.4, -3.5, -4.5), y = c(9.2, 7.0, 5.0, 3.0, 1.2)),
              aes(x = x, y = y), color = "#FFFFFF", linetype = "dashed", linewidth = 0.95) +
    geom_path(data = data.frame(x = c(0, 1.2, 2.4, 3.2, 4.2), y = c(9.2, 7.0, 4.8, 3.0, 1.4)),
              aes(x = x, y = y), color = "#FFFFFF", linetype = "dashed", linewidth = 0.95) +
    # Pluripotent rolling cell spheres
    geom_point(data = cell_balls, aes(x = x, y = y), 
               shape = 21, size = 4.8, fill = "#F59E0B", color = "#0F172A", stroke = 1.2) +
    annotate("text", x = 0, y = 9.8, label = "Totipotent / Pluripotent State", fontface = "bold", size = 3.4, color = "#FFFFFF") +
    annotate("text", x = -4.5, y = 0.4, label = "Lineage Fate 1", fontface = "bold", size = 3.2, color = "#FFFFFF") +
    annotate("text", x = 4.2, y = 0.4, label = "Lineage Fate 2", fontface = "bold", size = 3.2, color = "#FFFFFF") +
    scale_fill_viridis_d(option = "cividis", name = "Epigenetic Potential U(x):") +
    scale_x_continuous(breaks = seq(-8, 8, by = 4), expand = c(0, 0)) +
    scale_y_continuous(breaks = seq(0, 10, by = 2), labels = c("Terminal Differentiation", "Late Lineage", "Bifurcation Point", "Early Commitment", "Progenitor", "Pluripotent Root"), expand = c(0, 0)) +
    labs(
      title = "C.H. Waddington Epigenetic Developmental Landscape",
      subtitle = "Quasi-potential energy surface where rolling cell spheres bifurcate into canalized lineage valleys",
      x = "Cellular State Expression Coordinate X",
      y = "Developmental Timeline (Waddington Gravity Axis)",
      caption = "Source: Conceptual and quantitative Waddington epigenetic landscape (Hopfield / Wang et al. standard)"
    )
  
  if (exists("theme_topjournal")) {
    p <- p + theme_topjournal(grid = "none") +
      theme(
        axis.text = element_text(size = rel(0.85), face = "bold"),
        legend.position = "top",
        legend.direction = "horizontal",
        legend.title = element_text(face = "bold", size = 8.5)
      )
  } else {
    p <- p + theme_minimal()
  }
  p
}

explain <- function() {
  list(
    title = "C.H. Waddington Epigenetic Developmental Landscape",
    how_to_read = "A topographical quasi-potential energy surface representing cellular fate commitment. The vertical axis represents developmental progression. A rolling ball represents a cell descending through branching valleys (attractor basins); ridges between valleys symbolize epigenetic barriers that canalize development.",
    when_to_use = "The foundational conceptual and mathematical framework in stem cell biology, reprogramming (iPSCs), and cancer transdifferentiation.",
    pitfalls = "In biological reality, cell states can cross barriers through artificial transcription factor reprogramming (Yamanaka factors), demonstrating that Waddington ridges are permeable under non-equilibrium driving forces.",
    journal_archetype = "Cell Stem Cell, Nature Cell Biology, and PNAS quantitative developmental dynamics.",
    data_recipe = "Requires single-cell transcriptomic velocity potential energy field calculations."
  )
}
