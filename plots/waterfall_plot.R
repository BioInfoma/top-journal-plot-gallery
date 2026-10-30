# Clinical Oncology RECIST Waterfall Plot
# Tier 2 (Intermediate) - Clinical

make_plot <- function(data_dir = "data") {
  library(ggplot2)
  if (file.exists("theme_journal.R")) source("theme_journal.R", local = TRUE)
  
  # Simulate RECIST 1.1 tumor burden change data (percent change in sum of target lesion diameters)
  set.seed(101)
  n_pts <- 55
  pct_change <- c(
    runif(8, 20, 85),     # Progressive disease
    runif(15, -29, 19),   # Stable disease
    runif(25, -95, -30),  # Partial response
    runif(7, -100, -100)  # Complete response
  )
  pct_change <- sort(pct_change, decreasing = TRUE)
  
  # Assign response category
  recist_class <- ifelse(pct_change >= 20, "Progressive Disease (PD)",
                  ifelse(pct_change <= -100, "Complete Response (CR)",
                  ifelse(pct_change <= -30, "Partial Response (PR)", "Stable Disease (SD)")))
  
  df_wf <- data.frame(
    patient_id = factor(1:n_pts, levels = 1:n_pts),
    pct_change = pct_change,
    class = factor(recist_class, levels = c("Progressive Disease (PD)", "Stable Disease (SD)", "Partial Response (PR)", "Complete Response (CR)"))
  )
  
  recist_colors <- c(
    "Progressive Disease (PD)" = "#DC2626", # Red
    "Stable Disease (SD)" = "#64748B",     # Slate
    "Partial Response (PR)" = "#2563EB",   # Blue
    "Complete Response (CR)" = "#059669"   # Green
  )
  
  p <- ggplot(df_wf, aes(x = patient_id, y = pct_change, fill = class)) +
    geom_col(width = 0.78, color = "#222222", linewidth = 0.25) +
    geom_hline(yintercept = 20, linetype = "dashed", color = "#DC2626", linewidth = 0.55) +
    geom_hline(yintercept = -30, linetype = "dashed", color = "#2563EB", linewidth = 0.55) +
    geom_hline(yintercept = 0, color = "#1E293B", linewidth = 0.6) +
    scale_fill_manual(values = recist_colors) +
    scale_y_continuous(labels = function(x) paste0(x, "%"), limits = c(-105, 95)) +
    annotate("text", x = 2, y = 23, label = "RECIST +20% (Progression)", hjust = 0, size = 3.2, fontface = "bold", color = "#DC2626") +
    annotate("text", x = 2, y = -33, label = "RECIST -30% (Partial Response)", hjust = 0, size = 3.2, fontface = "bold", color = "#2563EB") +
    labs(
      title = "Clinical Oncology Waterfall Plot: Best Tumor Response",
      subtitle = "Maximum percent reduction from baseline in target lesion burden per patient under novel therapy",
      x = "Individual Clinical Trial Patient (Ranked)",
      y = "Best % Change in Target Lesion Sum",
      fill = "RECIST 1.1 Category",
      caption = "Source: Simulated Phase II oncology clinical trial cohort (N = 55 patients)"
    )
  
  if (exists("theme_topjournal")) {
    p <- p + theme_topjournal(grid = "y") +
      theme(
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank()
      )
  } else {
    p <- p + theme_classic()
  }
  p
}

explain <- function() {
  list(
    title = "Clinical Oncology RECIST Waterfall Plot",
    how_to_read = "Each vertical bar represents an individual patient, ordered from poorest response (tumor progression on the left) to greatest response (tumor shrinkage/remission on the right). Horizontal dashed lines indicate standardized RECIST criteria: +20% threshold for Progressive Disease (PD) and -30% for Partial Response (PR).",
    when_to_use = "The universal gold standard in clinical oncology Phase I/II/III trials to visualize individual patient therapeutic benefit across an entire cohort.",
    pitfalls = "Waterfall plots depict the single 'best' measurement over time and do NOT reflect how long the response lasted. A dramatic 80% shrinkage is incomplete information if the patient progressed shortly afterward; always pair with a Swimmer plot or Kaplan-Meier curve.",
    journal_archetype = "New England Journal of Medicine and Journal of Clinical Oncology primary trial figures.",
    data_recipe = "Requires per-patient best percentage change in tumor burden and clinical response classifications."
  )
}
