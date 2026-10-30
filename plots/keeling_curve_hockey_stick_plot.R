# Atmospheric Carbon Dioxide Keeling Curve
# Tier 4 (Mind-Blowing) - Climate

make_plot <- function(data_dir = "data") {
  library(ggplot2)
  if (file.exists("theme_journal.R")) source("theme_journal.R", local = TRUE)
  
  # Simulate monthly Mauna Loa atmospheric CO2 concentration (1958 - 2024)
  months_seq <- seq(as.Date("1958-03-01"), as.Date("2024-12-01"), by = "month")
  n_m <- length(months_seq)
  t_yrs <- (1:n_m) / 12
  
  # Base 315 ppm in 1958 rising quadratically to 425 ppm in 2024
  trend <- 315 + 0.7 * t_yrs + 0.0145 * (t_yrs^2)
  # Annual seasonal drawdown cycle (peak in May, trough in October due to Northern hemisphere vegetation)
  cycle <- 3.2 * sin(2 * pi * (t_yrs - 0.2))
  co2_conc <- trend + cycle + rnorm(n_m, 0, 0.25)
  
  df_keeling <- data.frame(
    date = months_seq,
    co2 = co2_conc,
    trend = trend
  )
  
  p <- ggplot(df_keeling, aes(x = date)) +
    geom_ribbon(aes(ymin = 300, ymax = co2), fill = "#DC2626", alpha = 0.08) +
    geom_line(aes(y = trend), color = "#DC2626", linewidth = 0.95, alpha = 0.7) +
    geom_line(aes(y = co2), color = "#1E293B", linewidth = 0.45) +
    scale_y_continuous(limits = c(305, 435), breaks = seq(320, 420, by = 20), expand = c(0, 0)) +
    scale_x_date(date_breaks = "10 years", date_labels = "%Y") +
    annotate("label", x = as.Date("1965-01-01"), y = 405, 
             label = "1958: 315 ppm\n2024: 425 ppm (+35% increase)\nSawtooth = Northern forest seasonal respiration", 
             hjust = 0, size = 3.4, fontface = "bold", color = "#1E293B", fill = "#FFFFFF") +
    labs(
      title = "The Keeling Curve: Atmospheric Carbon Dioxide Record",
      subtitle = "Continuous monthly in situ measurements at Mauna Loa Observatory showing seasonal cycle and exponential rise",
      x = "Year of Observation (1958 – Present)",
      y = expression(bold("Atmospheric ") * bold(CO[2]) * bold(" Concentration [Parts Per Million, ppm]")),
      caption = "Source: Simulated Mauna Loa Observatory / Scripps Institution of Oceanography time series"
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
    title = "The Atmospheric CO2 Keeling Curve",
    how_to_read = "Chronological observation year is on the horizontal axis; atmospheric carbon dioxide concentration in parts per million (ppm) is on the vertical axis. The black line traces monthly readings exhibiting a seasonal sawtooth oscillation (vegetation drawing down CO2 during Northern spring/summer), while the red smooth curve tracks the accelerating multi-decade anthropogenic trend.",
    when_to_use = "The foundational graphic of modern climate science, documenting the unprecedented rise in greenhouse gases since the industrial revolution.",
    pitfalls = "Do not truncate the seasonal cycle by annual averaging when discussing biosphere dynamics, as the amplitude of the seasonal sawtooth itself provides critical data about biosphere carbon flux.",
    journal_archetype = "Tellus, Science, and Nature landmark atmospheric chemistry papers (Charles David Keeling).",
    data_recipe = "Requires longitudinal monthly date timestamps and continuous CO2 concentration measurements."
  )
}
