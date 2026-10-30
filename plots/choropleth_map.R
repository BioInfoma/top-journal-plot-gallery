# Spatial Epidemiological Choropleth Map
# Tier 3 (Advanced) - Maps

make_plot <- function(data_dir = "data") {
  library(ggplot2)
  if (file.exists("theme_journal.R")) source("theme_journal.R", local = TRUE)
  
  # Continental US state level simulated epidemiological incidence map
  us_states <- map_data("state")
  
  # Simulate disease burden per state (Incidence per 100,000 population)
  set.seed(42)
  unique_states <- unique(us_states$region)
  df_rates <- data.frame(
    region = unique_states,
    incidence = rnorm(length(unique_states), mean = 45, sd = 18)
  )
  df_rates$incidence <- pmax(12, df_rates$incidence)
  
  map_df <- merge(us_states, df_rates, by = "region")
  map_df <- map_df[order(map_df$order), ]
  
  p <- ggplot(map_df, aes(x = long, y = lat, group = group, fill = incidence)) +
    geom_polygon(color = "#FFFFFF", linewidth = 0.3) +
    scale_fill_gradientn(
      colors = c("#EFF6FF", "#93C5FD", "#3B82F6", "#1D4ED8", "#1E3A8A"),
      name = "Incidence / 100k:"
    ) +
    coord_fixed(1.3) +
    labs(
      title = "Epidemiological Surveillance Choropleth Map",
      subtitle = "Standardized infectious disease incidence rate per 100,000 population across geographic jurisdictions",
      caption = "Source: Simulated public health epidemiological registry (CDC surveillance standard)"
    ) +
    theme_void() +
    theme(
      plot.title = element_text(face = "bold", size = 13, color = "#0F172A", margin = margin(b = 4)),
      plot.subtitle = element_text(size = 9.5, color = "#475569", margin = margin(b = 10)),
      plot.caption = element_text(size = 8, color = "#94A3B8", hjust = 0),
      legend.position = "top",
      legend.direction = "horizontal",
      legend.title = element_text(face = "bold", size = 8.5),
      legend.key.width = unit(1.6, "cm"),
      legend.key.height = unit(0.35, "cm"),
      plot.margin = margin(12, 14, 12, 12)
    )
  p
}

explain <- function() {
  list(
    title = "Epidemiological Surveillance Choropleth Map",
    how_to_read = "Geographical administrative boundaries (states, provinces, districts) are shaded according to continuous statistical measurements (e.g. disease incidence, vaccination coverage, mortality rates). Darker hues signify higher disease burden.",
    when_to_use = "Essential in global public health, spatial epidemiology, and health policy to communicate geographical disparities, healthcare access deficits, and regional transmission hot spots.",
    pitfalls = "Land area does not equal population; large, sparsely populated regions visually dominate choropleth maps. When conveying patient volume rather than per-capita incidence, consider population-weighted cartograms.",
    journal_archetype = "The Lancet, CDC MMWR, and New England Journal of Medicine spatial health maps.",
    data_recipe = "Requires geographic boundary GIS polygons joined with standardized regional statistical metrics."
  )
}
