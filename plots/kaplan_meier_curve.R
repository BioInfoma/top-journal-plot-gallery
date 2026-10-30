# Kaplan-Meier Survival Analysis with Censoring Marks
# Tier 1 (Simple) - Survival

make_plot <- function(data_dir = "data") {
  library(ggplot2)
  library(survival)
  if (file.exists("theme_journal.R")) source("theme_journal.R", local = TRUE)
  
  df <- read.csv(file.path(data_dir, "surv.csv"))
  
  # Fit Kaplan-Meier survival model
  fit <- survfit(Surv(time, event) ~ arm, data = df)
  
  # Extract tidy step data
  surv_df <- data.frame(
    time = fit$time,
    surv = fit$surv,
    n.risk = fit$n.risk,
    n.event = fit$n.event,
    n.censor = fit$n.censor,
    strata = rep(names(fit$strata), fit$strata)
  )
  surv_df$strata <- gsub("arm=", "", surv_df$strata)
  
  # Prepend baseline t=0
  init_df <- data.frame(
    time = c(0, 0),
    surv = c(1, 1),
    n.risk = c(75, 75),
    n.event = c(0, 0),
    n.censor = c(0, 0),
    strata = c("Placebo", "Treatment")
  )
  surv_df <- rbind(init_df, surv_df)
  censor_df <- surv_df[surv_df$n.censor > 0, ]
  
  # Log-rank test p-value
  sdiff <- survdiff(Surv(time, event) ~ arm, data = df)
  pval <- 1 - pchisq(sdiff$chisq, length(sdiff$n) - 1)
  pval_text <- if (pval < 0.001) "p < 0.001" else paste0("p = ", round(pval, 3))
  
  p <- ggplot(surv_df, aes(x = time, y = surv, color = strata)) +
    geom_step(linewidth = 1.05) +
    geom_point(data = censor_df, aes(x = time, y = surv), shape = 3, size = 2.5, stroke = 1.2, show.legend = FALSE) +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0, 1.02)) +
    annotate("text", x = max(surv_df$time) * 0.75, y = 0.85, 
             label = paste0("Log-rank test:\n", pval_text), 
             hjust = 0, size = 3.6, fontface = "bold", color = "#222222") +
    labs(
      title = "Kaplan-Meier Overall Survival Estimates",
      subtitle = "Step lines show survival probabilities; tick marks (+) indicate right-censored subjects",
      x = "Follow-up Duration (Months)",
      y = "Overall Survival Probability (%)",
      color = "Treatment Arm",
      caption = "Source: Simulated randomized clinical trial survival registry (N = 150)"
    )
  
  if (exists("theme_topjournal")) {
    p <- p + theme_topjournal(grid = "y") + scale_color_topjournal()
  } else {
    p <- p + theme_classic()
  }
  p
}

explain <- function() {
  list(
    title = "Kaplan-Meier Survival Curve",
    how_to_read = "The vertical step drops occur at each documented clinical endpoint event (death, progression). Plus-shaped tick marks (+) indicate censored individuals whose event status was unobserved at their last follow-up.",
    when_to_use = "Universal gold standard for time-to-event clinical trials, oncology disease-free survival (DFS), and patient outcome monitoring.",
    pitfalls = "Never truncate the x-axis arbitrarily, and always scrutinize the tail: when very few patients remain at risk, individual late events produce misleadingly huge drops in survival probability.",
    journal_archetype = "New England Journal of Medicine and Lancet Oncology pivotal Phase III trial endpoints.",
    data_recipe = "Requires time-to-event duration (continuous), binary event status (1 = event, 0 = censored), and arm identifier."
  )
}
