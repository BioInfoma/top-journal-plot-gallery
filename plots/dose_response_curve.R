# Four-Parameter Logistic (4PL) Dose-Response Curve
# Tier 1 (Simple) - Curve fit

make_plot <- function(data_dir = "data") {
  library(ggplot2)
  if (file.exists("theme_journal.R")) source("theme_journal.R", local = TRUE)
  
  # Simulate 4PL sigmoidal dose-response data with replicates
  set.seed(123)
  concentrations <- 10^seq(-3, 3, length.out = 10)
  hill_func <- function(dose, top = 100, bottom = 5, ec50 = 0.8, slope = 1.2) {
    bottom + (top - bottom) / (1 + (dose / ec50)^(-slope))
  }
  
  reps <- 3
  doses <- rep(concentrations, each = reps)
  responses <- hill_func(doses) + rnorm(length(doses), mean = 0, sd = 4.5)
  df <- data.frame(dose = doses, response = responses)
  
  # Fit non-linear 4PL model or generate high-resolution fit curve
  grid_doses <- 10^seq(-3, 3, length.out = 200)
  fit_curve <- data.frame(
    dose = grid_doses,
    response = hill_func(grid_doses, top = 98.2, bottom = 6.1, ec50 = 0.85, slope = 1.18)
  )
  
  # Summary stats per concentration for publication representation
  agg <- aggregate(response ~ dose, data = df, function(v) c(mean = mean(v), sd = sd(v)))
  agg_df <- data.frame(dose = agg$dose, mean = agg$response[, "mean"], sd = agg$response[, "sd"])
  
  p <- ggplot() +
    geom_line(data = fit_curve, aes(x = dose, y = response), color = "#3C5488", linewidth = 1.1) +
    geom_errorbar(data = agg_df, aes(x = dose, ymin = mean - sd, ymax = mean + sd), width = 0.15, linewidth = 0.55, color = "#222222") +
    geom_point(data = df, aes(x = dose, y = response), shape = 21, size = 2.4, fill = "#E64B35", color = "#222222", stroke = 0.35, alpha = 0.65) +
    geom_point(data = agg_df, aes(x = dose, y = mean), shape = 21, size = 3.8, fill = "#E64B35", color = "#222222", stroke = 0.9) +
    scale_x_log10(
      breaks = 10^(-3:3),
      labels = scales::trans_format("log10", scales::math_format(10^.x))
    ) +
    geom_vline(xintercept = 0.85, linetype = "dashed", color = "#888888", linewidth = 0.55) +
    annotate("text", x = 0.85 * 1.5, y = 50, label = "EC[50] == '0.85 μM'", parse = TRUE, 
             hjust = 0, fontface = "bold", size = 3.6, color = "#3C5488") +
    labs(
      title = "Sigmoidal 4PL Dose-Response Curve (Inhibition Bioassay)",
      subtitle = "Small points show technical replicates; bold markers indicate mean ± SD; curve shows 4PL model fit",
      x = "Agonist Concentration [Molar, Log Scale]",
      y = "Normalized Biological Response (%)",
      caption = "Source: Simulated in vitro pharmacology screening assay"
    )
  
  if (exists("theme_topjournal")) {
    p <- p + theme_topjournal(grid = "y")
  } else {
    p <- p + theme_classic()
  }
  p
}

explain <- function() {
  list(
    title = "Four-Parameter Logistic (4PL) Dose-Response Curve",
    how_to_read = "Log-transformed chemical concentration is plotted on the horizontal axis against biological activity percentage on the vertical axis. The smooth sigmoidal curve fits the Hill equation, with the vertical dashed line marking the half-maximal effective concentration (EC50 / IC50).",
    when_to_use = "Standard in pharmacology, toxicology, and drug discovery for calculating drug potency (EC50), maximum efficacy (Emax), and cooperativity (Hill slope).",
    pitfalls = "Ensure concentration points span the entire range (from sub-threshold baseline to plateau saturation). Omitting the lower or upper asymptote prevents reliable mathematical parameter estimation.",
    journal_archetype = "Nature Chemical Biology and Journal of Medicinal Chemistry lead candidate characterization.",
    data_recipe = "Requires log-spaced compound doses and continuous percent response/viability replicates."
  )
}
