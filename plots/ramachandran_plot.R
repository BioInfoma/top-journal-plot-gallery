# Protein Backbone Conformation (Ramachandran Plot)
# Tier 4 (Mind-Blowing) - Structure

make_plot <- function(data_dir = "data") {
  library(ggplot2)
  if (file.exists("theme_journal.R")) source("theme_journal.R", local = TRUE)
  
  # Simulate 700 amino acid backbone dihedral angles (phi, psi)
  set.seed(42)
  # 1. Alpha-helix cluster: phi ~ -60, psi ~ -45
  n_helix <- 340
  phi_helix <- rnorm(n_helix, mean = -62, sd = 12)
  psi_helix <- rnorm(n_helix, mean = -43, sd = 14)
  
  # 2. Beta-sheet cluster: phi ~ -125, psi ~ +135
  n_sheet <- 280
  phi_sheet <- rnorm(n_sheet, mean = -125, sd = 18)
  psi_sheet <- rnorm(n_sheet, mean = 135, sd = 19)
  
  # 3. Left-handed helix / turn: phi ~ +58, psi ~ +38
  n_turn <- 55
  phi_turn <- rnorm(n_turn, mean = 58, sd = 14)
  psi_turn <- rnorm(n_turn, mean = 38, sd = 16)
  
  # 4. Outliers / disallowed residues
  n_out <- 25
  phi_out <- runif(n_out, -180, 180)
  psi_out <- runif(n_out, -180, 180)
  
  df_rama <- data.frame(
    phi = c(phi_helix, phi_sheet, phi_turn, phi_out),
    psi = c(psi_helix, psi_sheet, psi_turn, psi_out),
    region = factor(c(rep("α-helix", n_helix),
                      rep("β-sheet", n_sheet),
                      rep("Left-handed α", n_turn),
                      rep("Outlier (Disallowed)", n_out)),
                    levels = c("α-helix", "β-sheet", "Left-handed α", "Outlier (Disallowed)"))
  )
  
  p <- ggplot(df_rama, aes(x = phi, y = psi)) +
    # Favorable conformational core zone rectangles with soft borders
    annotate("rect", xmin = -170, xmax = -60, ymin = 90, ymax = 175, fill = "#3B82F6", alpha = 0.14) +
    annotate("rect", xmin = -100, xmax = -35, ymin = -75, ymax = -15, fill = "#10B981", alpha = 0.14) +
    annotate("rect", xmin = 35, xmax = 90, ymin = 10, ymax = 70, fill = "#F59E0B", alpha = 0.14) +
    # Cross reference gridlines at 0 degrees
    geom_vline(xintercept = 0, color = "#94A3B8", linetype = "dashed", linewidth = 0.45) +
    geom_hline(yintercept = 0, color = "#94A3B8", linetype = "dashed", linewidth = 0.45) +
    # Backbone points
    geom_point(aes(color = region, shape = region), alpha = 0.75, size = 2.0) +
    scale_color_manual(values = c(
      "α-helix" = "#059669",
      "β-sheet" = "#2563EB",
      "Left-handed α" = "#D97706",
      "Outlier (Disallowed)" = "#DC2626"
    ), name = "Conformation:") +
    scale_shape_manual(values = c(
      "α-helix" = 16,
      "β-sheet" = 15,
      "Left-handed α" = 17,
      "Outlier (Disallowed)" = 4
    ), name = "Conformation:") +
    scale_x_continuous(breaks = seq(-180, 180, by = 60), limits = c(-180, 180), expand = c(0, 0)) +
    scale_y_continuous(breaks = seq(-180, 180, by = 60), limits = c(-180, 180), expand = c(0, 0)) +
    coord_fixed(ratio = 1) +
    annotate("text", x = -115, y = 160, label = "β-sheets", fontface = "bold", color = "#1D4ED8", size = 3.6) +
    annotate("text", x = -65, y = -65, label = "Right-handed α", fontface = "bold", color = "#047857", size = 3.6) +
    annotate("text", x = 65, y = 50, label = "Left-handed α", fontface = "bold", color = "#B45309", size = 3.3) +
    labs(
      title = "Ramachandran Plot: Protein Backbone Torsion Angle Validation",
      subtitle = "Phi (φ) vs Psi (ψ) dihedral angles; shaded regions represent sterically favored conformations",
      x = expression(bold("Phi (") * bolditalic(phi) * bold(") Dihedral Angle [Degrees]")),
      y = expression(bold("Psi (") * bolditalic(psi) * bold(") Dihedral Angle [Degrees]")),
      caption = "Source: Simulated macromolecular structure coordinates (N = 700 residues)"
    )
  
  if (exists("theme_topjournal")) {
    p <- p + theme_topjournal(grid = "xy") +
      theme(
        legend.position = "top",
        legend.direction = "horizontal",
        legend.title = element_text(face = "bold", size = 9),
        legend.text = element_text(size = 8.5)
      )
  } else {
    p <- p + theme_classic()
  }
  p
}

explain <- function() {
  list(
    title = "Protein Structure Ramachandran Validation Plot",
    how_to_read = "Plots backbone dihedral angles: phi (N-Calpha rotation) on the horizontal axis against psi (Calpha-C rotation) on the vertical axis, both ranging from -180 to +180 degrees. Distinct clusters identify sterically allowed regular secondary structures: beta-sheets in the upper-left and alpha-helices in the lower-left.",
    when_to_use = "Essential validation standard for every experimentally solved or predicted protein 3D structure (PDB deposits, AlphaFold, cryo-EM models) to verify steric geometry.",
    pitfalls = "Non-glycine residues falling outside the favored regions represent steric clashes or modeling errors. Glycine has no side chain and is the only residue naturally permitted in broader conformational zones.",
    journal_archetype = "Nature Structural & Molecular Biology and Structure macromolecular validation figures.",
    data_recipe = "Requires per-residue phi and psi dihedral angles from 3D atomic coordinates."
  )
}
