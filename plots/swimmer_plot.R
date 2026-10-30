# Clinical Trial Swimmer Plot for Longitudinal Patient Trajectories
# Tier 2 (Intermediate) - Clinical

make_plot <- function(data_dir = "data") {
  library(ggplot2)
  if (file.exists("theme_journal.R")) source("theme_journal.R", local = TRUE)
  
  # Simulate clinical trial timeline data for 16 cancer patients
  set.seed(42)
  n <- 16
  pt_ids <- paste0("Pt-", sprintf("%02d", 1:n))
  duration <- c(3.5, 4.8, 6.2, 8.0, 9.5, 11.2, 12.8, 14.5, 16.0, 18.2, 20.5, 23.0, 25.5, 28.0, 31.5, 35.0)
  stage <- sample(c("Stage III", "Stage IV"), n, replace = TRUE, prob = c(0.4, 0.6))
  ongoing <- c(FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, TRUE, FALSE, TRUE, FALSE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE)
  
  df_swimmer <- data.frame(
    id = factor(pt_ids, levels = pt_ids),
    duration = duration,
    stage = stage,
    ongoing = ongoing
  )
  
  # Simulate discrete clinical landmark events (response onset, progression, adverse events)
  events <- data.frame(
    id = factor(c("Pt-04", "Pt-07", "Pt-09", "Pt-11", "Pt-12", "Pt-13", "Pt-14", "Pt-15", "Pt-16",
                  "Pt-03", "Pt-05", "Pt-08", "Pt-10"), levels = pt_ids),
    time = c(2.1, 2.5, 3.0, 2.8, 3.2, 4.0, 3.5, 2.6, 2.4,
             5.8, 8.5, 12.8, 16.0),
    event_type = factor(c(rep("Partial/Complete Response", 9), rep("Disease Progression", 4)),
                        levels = c("Partial/Complete Response", "Disease Progression"))
  )
  
  p <- ggplot(df_swimmer, aes(x = duration, y = id)) +
    # The Swimmer Lane: Bar representing total follow-up duration
    geom_col(aes(fill = stage), width = 0.58, alpha = 0.85, color = "#334155", linewidth = 0.35, orientation = "y") +
    # Milestone clinical markers
    geom_point(data = events, aes(x = time, y = id, shape = event_type, color = event_type), size = 3.2, stroke = 0.8) +
    # Ongoing treatment arrow for active patients
    geom_segment(data = df_swimmer[df_swimmer$ongoing, ], 
                 aes(x = duration, xend = duration + 1.8, y = id, yend = id),
                 arrow = arrow(length = unit(0.18, "cm"), type = "closed"), 
                 color = "#047857", linewidth = 1.05) +
    scale_fill_manual(values = c("Stage III" = "#3C5488", "Stage IV" = "#8491B4"), name = "Baseline:") +
    scale_color_manual(values = c("Partial/Complete Response" = "#059669", "Disease Progression" = "#DC2626"), name = "Milestones:") +
    scale_shape_manual(values = c("Partial/Complete Response" = 16, "Disease Progression" = 17), name = "Milestones:") +
    scale_x_continuous(breaks = seq(0, 40, by = 6), expand = expansion(mult = c(0, 0.08))) +
    labs(
      title = "Clinical Swimmer Plot: Patient Treatment Duration and Milestones",
      subtitle = "Horizontal bars show time on protocol; embedded markers show events; arrowheads denote ongoing therapy",
      x = "Follow-up Duration on Protocol (Months)",
      y = "Subject Identifier",
      caption = "Source: Simulated Phase II targeted immunotherapy trial cohort (N = 16)"
    )
  
  if (exists("theme_topjournal")) {
    p <- p + theme_topjournal(grid = "x") +
      theme(
        axis.text.y = element_text(size = rel(0.9), face = "bold"),
        legend.position = "top",
        legend.direction = "horizontal",
        legend.box = "horizontal",
        legend.title = element_text(face = "bold", size = 8.5),
        legend.text = element_text(size = 8.5)
      )
  } else {
    p <- p + theme_classic()
  }
  p
}

explain <- function() {
  list(
    title = "Clinical Trial Swimmer Plot",
    how_to_read = "Each horizontal 'lane' tracks an individual patient over longitudinal follow-up. The bar length represents total time on study. Embedded symbols mark key milestone events (onset of tumor response, toxicity, or disease progression). Arrowheads at the end of bars designate patients still on active therapy with durable responses.",
    when_to_use = "The premier plot in oncology and rare-disease drug trials to communicate durability of response and clinical benefit over time across heterogeneous patient courses.",
    pitfalls = "Order patients logically (by response duration or baseline risk) rather than arbitrary subject ID numbers to prevent a chaotic, illegible plot.",
    journal_archetype = "The Lancet Oncology and Journal of Clinical Oncology immunotherapy trials.",
    data_recipe = "Requires per-patient study duration, ongoing therapy indicators, and timestamped milestone event logs."
  )
}
