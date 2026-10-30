# Publication-Grade Multi-Panel Composite Figure (Panels A-D)
# Tier 4 (Mind-Blowing) - Composite

make_plot <- function(data_dir = "data") {
  library(ggplot2)
  if (file.exists("theme_journal.R")) source("theme_journal.R", local = TRUE)
  
  # Load available datasets
  xy <- read.csv(file.path(data_dir, "xy.csv"))
  grp <- read.csv(file.path(data_dir, "grp.csv"))
  
  # Panel A: Volcano / Feature Screening
  p_a <- ggplot(xy, aes(x = x, y = y, color = group)) +
    geom_point(alpha = 0.7, size = 1.8) +
    scale_color_manual(values = c("A" = "#E64B35", "B" = "#3C5488", "C" = "#00A087")) +
    labs(title = "Cohort Bivariate Screening", x = "Predictor (X)", y = "Response (Y)") +
    theme_classic(base_size = 9) +
    theme(legend.position = "none", plot.title = element_text(face = "bold"))
  
  # Panel B: Box and Point Distribution
  grp$group <- factor(grp$group, levels = c("Control", "DrugA", "DrugB", "DrugC"))
  p_b <- ggplot(grp, aes(x = group, y = value, fill = group)) +
    geom_boxplot(width = 0.5, outlier.shape = NA, alpha = 0.75, linewidth = 0.35) +
    geom_jitter(width = 0.15, size = 1.1, alpha = 0.5) +
    scale_fill_manual(values = c("Control" = "#64748B", "DrugA" = "#E64B35", "DrugB" = "#3C5488", "DrugC" = "#00A087")) +
    labs(title = "Efficacy by Experimental Arm", x = "Cohort", y = "Activity Index") +
    theme_classic(base_size = 9) +
    theme(legend.position = "none", plot.title = element_text(face = "bold"))
  
  # Panel C: Longitudinal Kinetic Trend
  ts <- read.csv(file.path(data_dir, "ts.csv"))
  ts$date <- as.Date(ts$date)
  p_c <- ggplot(head(ts, 90), aes(x = date, y = value, color = group)) +
    geom_line(linewidth = 0.8) +
    scale_color_manual(values = c("Site1" = "#E64B35", "Site2" = "#3C5488", "Site3" = "#00A087")) +
    labs(title = "Longitudinal Biomarker Kinetics", x = "Date", y = "Concentration (AU)") +
    theme_classic(base_size = 9) +
    theme(legend.position = "none", plot.title = element_text(face = "bold"))
  
  # Panel D: Dose-Response Model
  dose_x <- 10^seq(-2, 2, length.out = 100)
  dose_y <- 5 + 90 / (1 + (dose_x / 1.0)^(-1.3))
  dose_df <- data.frame(dose = dose_x, response = dose_y)
  p_d <- ggplot(dose_df, aes(x = dose, y = response)) +
    geom_line(color = "#1E40AF", linewidth = 1.0) +
    scale_x_log10() +
    labs(title = "Sigmoidal 4PL Potency Curve", x = "Concentration [μM]", y = "Inhibition (%)") +
    theme_classic(base_size = 9) +
    theme(plot.title = element_text(face = "bold"))
  
  if (requireNamespace("patchwork", quietly = TRUE)) {
    composite <- (p_a + p_b) / (p_c + p_d) +
      patchwork::plot_annotation(
        title = "Figure 1 | Integrated Experimental Profiling & Multi-Cohort Characterization",
        subtitle = "Multi-panel journal figure combining screening (A), cohort response (B), kinetics (C), and potency (D)",
        caption = "Nature / Science composite layout standard | Tagged with bold uppercase panel labels A-D",
        tag_levels = "A"
      ) &
      theme(
        plot.tag = element_text(face = "bold", size = 12, color = "#0F172A"),
        plot.title = element_text(face = "bold", size = 12, color = "#0F172A"),
        plot.subtitle = element_text(size = 9.5, color = "#475569")
      )
    return(composite)
  } else {
    p_a
  }
}

explain <- function() {
  list(
    title = "Multi-Panel Journal Composite Figure (Panels A–D)",
    how_to_read = "Integrates multiple complementary experiments into a single cohesive story labeled alphabetically (Panels A, B, C, D). Visual alignment, shared color semantics, and synchronized typography allow immediate cross-panel synthesis.",
    when_to_use = "The indispensable format for main-text figures in Nature, Science, Cell, and The Lancet, where space is strictly constrained and each figure must deliver a self-contained scientific milestone.",
    pitfalls = "Ensure identical font sizes, consistent color coding across panels (e.g. Drug A must not be red in Panel A and blue in Panel B), and proper alignment of axes.",
    journal_archetype = "Nature, Science, and Cell main-text multi-panel Figures 1 through 6.",
    data_recipe = "Requires multiple complementary experimental outputs combined via patchwork or cowplot."
  )
}
