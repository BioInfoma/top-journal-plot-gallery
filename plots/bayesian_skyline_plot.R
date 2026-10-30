# Bayesian Skyline Plot (BSP) for Historical Population Demography
# Tier 3 (Advanced) - Phylodynamics

make_plot <- function(data_dir = "data") {
  library(ggplot2)
  if (file.exists("theme_journal.R")) source("theme_journal.R", local = TRUE)
  
  # Simulate Bayesian Skyline demographic reconstruction of effective population size Ne*tau
  # Tracking an epidemic expansion and subsequent intervention contraction over 25 years
  years_ago <- seq(0, 30, length.out = 100) # 0 = Present, 30 = Years in the past
  
  # True population trajectory (logistic expansion followed by plateau/decline)
  ne_median <- 10^(2.2 + 2.3 / (1 + exp(0.35 * (years_ago - 12))))
  # 95% HPD credible bounds (wider in the distant past due to fewer coalescent events)
  hpd_factor <- 1 + 0.045 * years_ago
  ne_lower <- ne_median / (1.8 * hpd_factor)
  ne_upper <- ne_median * (1.8 * hpd_factor)
  
  df_bsp <- data.frame(
    time = -years_ago, # Negative for calendar backward timeline
    median = ne_median,
    lower = ne_lower,
    upper = ne_upper
  )
  
  p <- ggplot(df_bsp, aes(x = time)) +
    geom_ribbon(aes(ymin = lower, ymax = upper), fill = "#3B82F6", alpha = 0.22) +
    geom_line(aes(y = median), color = "#1D4ED8", linewidth = 1.15) +
    scale_y_log10(
      breaks = 10^(1:5),
      labels = scales::trans_format("log10", scales::math_format(10^.x)),
      limits = c(10, 2e5)
    ) +
    scale_x_continuous(
      breaks = seq(-30, 0, by = 5),
      labels = c("30 yr ago", "25 yr ago", "20 yr ago", "15 yr ago", "10 yr ago", "5 yr ago", "Present")
    ) +
    annotate("text", x = -14, y = 3.5e4, label = "Epidemic Expansion Phase", 
             fontface = "bold", color = "#1D4ED8", size = 3.5) +
    labs(
      title = "Bayesian Skyline Plot: Effective Population Dynamics (Ne × τ)",
      subtitle = "Coalescent demographic inference over time; blue line shows median Ne; envelope marks 95% HPD interval",
      x = "Historical Time Horizon",
      y = expression(bold("Effective Population Size (") * bolditalic(N[e] * tau) * bold(", Log Scale)")),
      caption = "Source: Simulated coalescent molecular clock genealogy (BEAST Skyline model)"
    )
  
  if (exists("theme_topjournal")) {
    p <- p + theme_topjournal(grid = "xy")
  } else {
    p <- p + theme_classic()
  }
  p
}

explain <- function() {
  list(
    title = "Bayesian Skyline Plot (BSP)",
    how_to_read = "Chronological time extends from the past (left) to the present day (right). The vertical logarithmic axis reflects the effective population size multiplied by generation time (Ne*tau). The solid line marks the posterior median estimate, surrounded by a 95% HPD credible interval ribbon.",
    when_to_use = "Indispensable in viral phylodynamics and historical conservation genetics to track demographic booms, bottlenecks, and the impact of public health interventions over time.",
    pitfalls = "In the deep past, coalescent events become rare, causing 95% HPD intervals to flare widely; never over-interpret demographic changes near the root where data is sparse.",
    journal_archetype = "Science, PNAS, and Nature Ecology & Evolution viral demographic reconstructions.",
    data_recipe = "Requires longitudinal genetic sequence alignment with BEAST coalescent Skyline analysis."
  )
}
