# Spatial Epidemiological Choropleth Map (CDC / Lancet Standard)
# Tier 3 (Advanced) - Maps

make_plot <- function(data_dir = "data") {
  library(ggplot2)
  if (requireNamespace("maps", quietly = TRUE)) library(maps)
  if (file.exists("theme_journal.R")) source("theme_journal.R", local = TRUE)
  
  # Load base US state coordinates with foolproof fallback
  coord_file <- file.path(data_dir, "us_states_coords.csv")
  if (file.exists(coord_file)) {
    us_states <- read.csv(coord_file)
  } else if (requireNamespace("maps", quietly = TRUE)) {
    us_states <- ggplot2::map_data("state")
  } else {
    stop("Geographic state boundary dataset not found. Please provide data/us_states_coords.csv")
  }
  
  # Deterministic epidemiological incidence rates per 100k population
  set.seed(101)
  unique_states <- unique(us_states$region)
  
  # Epidemiological disease incidence simulation (e.g. Arbovirus / Lyme / Influenza)
  # Regional gradients: higher in Northeast and Upper Midwest
  rates <- sapply(unique_states, function(st) {
    base <- 45
    if (st %in% c("new york", "massachusetts", "pennsylvania", "new jersey", "connecticut")) base <- 82
    if (st %in% c("california", "texas", "florida", "georgia")) base <- 64
    if (st %in% c("montana", "wyoming", "north dakota", "south dakota", "idaho")) base <- 18
    rnorm(1, mean = base, sd = 9)
  })
  
  df_rates <- data.frame(
    region = unique_states,
    incidence = round(pmax(10, rates), 1)
  )
  
  # Categorize into 5 discrete public health surveillance tiers
  df_rates$tier <- cut(
    df_rates$incidence,
    breaks = c(0, 25, 45, 65, 85, Inf),
    labels = c("< 25 (Baseline)", "25 - 45 (Moderate)", "45 - 65 (Substantial)", "65 - 85 (High)", "> 85 (Epidemic Alert)"),
    include.lowest = TRUE
  )
  
  map_df <- merge(us_states, df_rates, by = "region")
  map_df <- map_df[order(map_df$order), ]
  
  # State centroids for top surveillance labels
  hotspot_labels <- data.frame(
    region = c("new york", "texas", "california", "florida", "illinois", "washington"),
    code = c("NY: 88.4", "TX: 67.2", "CA: 61.5", "FL: 71.0", "IL: 48.3", "WA: 38.1"),
    long = c(-75.5, -99.0, -119.5, -82.5, -89.2, -120.5),
    lat = c(43.0, 31.5, 37.2, 28.5, 40.0, 47.4)
  )
  
  p <- ggplot() +
    # State polygon fills
    geom_polygon(data = map_df, aes(x = long, y = lat, group = group, fill = tier), 
                 color = "#FFFFFF", linewidth = 0.35) +
    # Hotspot annotation points & labels
    geom_point(data = hotspot_labels, aes(x = long, y = lat), size = 1.8, color = "#0F172A", alpha = 0.75) +
    geom_text(data = hotspot_labels, aes(x = long, y = lat + 0.8, label = code), 
              fontface = "bold", size = 2.6, color = "#0F172A") +
    scale_fill_manual(
      values = c(
        "< 25 (Baseline)"        = "#EFF6FF",
        "25 - 45 (Moderate)"     = "#93C5FD",
        "45 - 65 (Substantial)"  = "#3B82F6",
        "65 - 85 (High)"         = "#1D4ED8",
        "> 85 (Epidemic Alert)"  = "#7F1D1D"
      ),
      name = "Surveillance Incidence Rate (per 100,000 population):"
    ) +
    coord_fixed(1.3) +
    labs(
      title = "Epidemiological Surveillance Choropleth Map: Continental Disease Burden",
      subtitle = "Standardized cumulative incidence rate per 100,000 population across US jurisdictions; five-tier CDC public health stratification",
      caption = "Source: Simulated national notifiable infectious disease surveillance registry (NNDSS / CDC MMWR standard)"
    ) +
    theme_void() +
    theme(
      plot.title = element_text(face = "bold", size = 12.5, color = "#0F172A", margin = margin(b = 4)),
      plot.subtitle = element_text(size = 9.2, color = "#475569", margin = margin(b = 10)),
      plot.caption = element_text(size = 8, color = "#94A3B8", hjust = 0),
      legend.position = "top",
      legend.direction = "horizontal",
      legend.title = element_text(face = "bold", size = 8.5),
      legend.text = element_text(size = 8.0),
      legend.key.width = unit(1.2, "cm"),
      legend.key.height = unit(0.35, "cm"),
      plot.margin = margin(14, 16, 14, 14)
    )
  p
}

explain <- function() {
  list(
    title = "Epidemiological Surveillance Choropleth Map",
    how_to_read = "Geographic administrative units (states or provinces) are shaded according to discrete statistical incidence bins (e.g. cases per 100,000 population). Deep crimson hues mark jurisdictions exceeding the epidemic alert threshold (>85 / 100k), while light blue tones represent baseline endemic rates.",
    when_to_use = "The international reporting benchmark in spatial epidemiology, global public health (WHO, CDC MMWR, The Lancet), and health policy to communicate geographical burden disparities and guide resource allocation.",
    pitfalls = "Avoid raw case counts, which merely mirror population density; always report population-standardized incidence rates (per 100,000 or per 1,000,000 person-years).",
    journal_archetype = "The Lancet, CDC MMWR, and New England Journal of Medicine spatial health maps.",
    data_recipe = "Requires geographic boundary polygons joined with standardized regional statistical metrics."
  )
}
