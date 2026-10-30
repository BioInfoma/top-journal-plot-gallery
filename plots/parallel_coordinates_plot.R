# Multivariate Parallel Coordinates Plot
# Tier 3 (Advanced) - Multivariate

make_plot <- function(data_dir = "data") {
  library(ggplot2)
  if (file.exists("theme_journal.R")) source("theme_journal.R", local = TRUE)
  
  mat_raw <- read.csv(file.path(data_dir, "mat.csv"), row.names = 1, check.names = FALSE)
  meta <- read.csv(file.path(data_dir, "mat_meta.csv"))
  rownames(meta) <- meta$sample
  
  # Select 6 representative continuous features across samples
  sub_mat <- as.matrix(mat_raw[1:6, ])
  
  # Min-max scale each feature to [0, 1] for unified vertical parallel axes
  scaled_mat <- apply(sub_mat, 1, function(x) (x - min(x)) / (max(x) - min(x)))
  
  df_long <- expand.grid(sample = rownames(scaled_mat), Feature = colnames(scaled_mat), stringsAsFactors = FALSE)
  df_long$Value <- as.vector(scaled_mat)
  df_long$Condition <- meta[df_long$sample, "condition"]
  df_long$Feature <- factor(df_long$Feature, levels = colnames(scaled_mat))
  
  p <- ggplot(df_long, aes(x = Feature, y = Value, group = sample, color = Condition)) +
    geom_line(alpha = 0.65, linewidth = 0.85) +
    geom_point(size = 1.8, alpha = 0.8) +
    scale_color_manual(values = c("Ctrl" = "#00A087", "Treat1" = "#E64B35", "Treat2" = "#3C5488"), name = "Condition:") +
    scale_y_continuous(breaks = seq(0, 1, by = 0.25), labels = c("0%", "25%", "50%", "75%", "100%")) +
    labs(
      title = "Multivariate Parallel Coordinates Profile",
      subtitle = "Each polyline traces an individual sample across six normalized high-dimensional biomarker axes",
      x = "Measured Phenotypic / Transcriptomic Feature",
      y = "Normalized Relative Scale (0 to 100%)",
      caption = "Source: Simulated multi-parameter expression matrix (30 samples x 6 features)"
    )
  
  if (exists("theme_topjournal")) {
    p <- p + theme_topjournal(grid = "xy") +
      theme(
        axis.text.x = element_text(face = "bold", size = rel(0.95)),
        legend.position = "top",
        legend.direction = "horizontal",
        legend.title = element_text(face = "bold", size = 8.5)
      )
  } else {
    p <- p + theme_classic()
  }
  p
}

explain <- function() {
  list(
    title = "Multivariate Parallel Coordinates Plot",
    how_to_read = "A series of parallel vertical axes represent distinct continuous dimensions. Each continuous polyline traces an individual subject across all variables simultaneously, with bundle convergence revealing shared sub-phenotypes or multi-feature signatures.",
    when_to_use = "Valuable for high-dimensional profiling, patient phenotyping, and anomaly detection where pairwise scatterplots would require dozens of separate panels.",
    pitfalls = "Horizontal ordering of vertical axes determines which pairwise interactions are adjacent; permute axis order or compute correlation-based ordering to reveal hidden associations.",
    journal_archetype = "Nature Methods, IEEE Transactions on Visualization, and Bioinformatics multivariate profiling.",
    data_recipe = "Requires multiple continuous numerical columns across matched observational samples."
  )
}
