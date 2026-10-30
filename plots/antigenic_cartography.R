# Viral Antigenic Cartography (Smith et al. Influenza / SARS-CoV-2 Style)
# Tier 4 (Mind-Blowing) - Virology

make_plot <- function(data_dir = "data") {
  library(ggplot2)
  if (file.exists("theme_journal.R")) source("theme_journal.R", local = TRUE)
  
  # Simulate longitudinal viral antigenic drift trajectory across seasons
  # Antigenic distance: 1 grid unit = 2-fold change in neutralization / HI titer
  set.seed(42)
  years <- 2010:2024
  n_yr <- length(years)
  
  # Antigenic drift path (smooth trajectory in 2D antigenic space)
  x_drift <- cumsum(c(0, rnorm(n_yr - 1, mean = 1.2, sd = 0.4)))
  y_drift <- cumsum(c(0, rnorm(n_yr - 1, mean = 0.8, sd = 0.5)))
  
  df_strains <- data.frame(
    year = years,
    x = x_drift,
    y = y_drift,
    variant = paste0("Strain-", years)
  )
  
  # Reference antisera serum points
  df_sera <- data.frame(
    x = x_drift[seq(1, n_yr, by = 3)] + rnorm(5, 0, 0.6),
    y = y_drift[seq(1, n_yr, by = 3)] + rnorm(5, 0, 0.6),
    serum = paste0("Antiserum-", years[seq(1, n_yr, by = 3)])
  )
  
  p <- ggplot() +
    # Grid lines at 1 antigenic unit (2-fold dilution)
    geom_vline(xintercept = seq(-2, 20, by = 2), color = "#F1F5F9", linewidth = 0.5) +
    geom_hline(yintercept = seq(-2, 16, by = 2), color = "#F1F5F9", linewidth = 0.5) +
    # Temporal drift trajectory curve
    geom_path(data = df_strains, aes(x = x, y = y), color = "#64748B", linewidth = 1.0, linetype = "dotted") +
    # Antisera markers (open squares)
    geom_point(data = df_sera, aes(x = x, y = y), shape = 22, size = 4.2, fill = "#FFFFFF", color = "#1E293B", stroke = 1.1) +
    # Viral strain isolates colored by year
    geom_point(data = df_strains, aes(x = x, y = y, fill = year), shape = 21, size = 3.6, color = "#0F172A", stroke = 0.6) +
    geom_text(data = df_strains, aes(x = x, y = y + 0.65, label = year), fontface = "bold", size = 2.8, color = "#1E293B") +
    scale_fill_viridis_c(option = "viridis", name = "Isolation Season:") +
    scale_x_continuous(breaks = seq(0, 18, by = 2)) +
    scale_y_continuous(breaks = seq(0, 14, by = 2)) +
    coord_fixed() +
    annotate("label", x = 3, y = 11, 
             label = "Antigenic Scale:\n1 Grid Unit = 2-fold dilution\nSquares = Reference Antisera\nCircles = Viral Strains", 
             fontface = "bold", size = 3.2, color = "#0F172A", fill = "#FFFFFF") +
    labs(
      title = "Viral Antigenic Cartography: Longitudinal Antigenic Drift Map",
      subtitle = "Multi-dimensional scaling of neutralization antibody cross-reactivity titers over consecutive seasons",
      x = "Antigenic Dimension 1 (Antigenic Distance Units, AU)",
      y = "Antigenic Dimension 2 (Antigenic Distance Units, AU)",
      caption = "Source: Simulated hemagglutination inhibition / pseudovirus neutralization matrix (Smith et al. standard)"
    )
  
  if (exists("theme_topjournal")) {
    p <- p + theme_topjournal(grid = "none") +
      theme(
        axis.text = element_text(size = rel(0.85)),
        legend.position = "top",
        legend.direction = "horizontal",
        legend.title = element_text(face = "bold", size = 8.5),
        legend.key.width = unit(1.6, "cm"),
        legend.key.height = unit(0.35, "cm")
      )
  } else {
    p <- p + theme_classic()
  }
  p
}

explain <- function() {
  list(
    title = "Viral Antigenic Cartography (Smith et al. Standard)",
    how_to_read = "A geometric coordinate map derived from multidimensional scaling of hemagglutination inhibition (HI) or serum neutralization titers. Distance between a virus (circle) and antiserum (square) directly corresponds to antigenic difference (1 unit = 2-fold drop in antibody neutralization). Connected paths trace historical antigenic drift.",
    when_to_use = "The official WHO standard for selecting annual seasonal influenza and COVID-19 vaccine update strains (Smith et al., Science).",
    pitfalls = "High-dimensional neutralization matrices must be tested for dimensionality (typically 2D or 3D stress values); forcing high-dimensional antibody heterogeneity into 2D can distort relative distances.",
    journal_archetype = "Science, Nature, and Cell viral immune escape and vaccine strain selection figures.",
    data_recipe = "Requires pairwise viral strain by post-infection animal antiserum neutralization titer matrix."
  )
}
