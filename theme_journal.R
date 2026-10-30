# Publication-grade theme and palette definitions for Top-Journal Plot Gallery
library(ggplot2)

# NPG / Nature-style 10-color categorical palette
pal_topjournal_colors <- c(
  "#E64B35", # Deep Coral / Vermilion
  "#4DBBD5", # Ocean Teal
  "#00A087", # Jade Green
  "#3C5488", # Slate Navy
  "#F39B7F", # Warm Peach
  "#8491B4", # Heather Purple
  "#91D1C2", # Soft Mint
  "#DC0000", # Crimson
  "#7E6148", # Muted Ochre
  "#B09C85"  # Warm Stone
)

scale_color_topjournal <- function(...) {
  scale_color_manual(values = pal_topjournal_colors, ...)
}

scale_fill_topjournal <- function(...) {
  scale_fill_manual(values = pal_topjournal_colors, ...)
}

# Continuous gradients: Diverging (Navy to Coral) and Sequential (Mako/Teal)
scale_fill_topjournal_div <- function(low = "#3C5488", mid = "#F7F7F7", high = "#E64B35", midpoint = 0, ...) {
  scale_fill_gradient2(low = low, mid = mid, high = high, midpoint = midpoint, ...)
}

scale_color_topjournal_div <- function(low = "#3C5488", mid = "#F7F7F7", high = "#E64B35", midpoint = 0, ...) {
  scale_color_gradient2(low = low, mid = mid, high = high, midpoint = midpoint, ...)
}

#' Nature / Science Publication Theme
theme_topjournal <- function(base_size = 11, base_family = "sans", grid = "y") {
  thm <- theme_classic(base_size = base_size, base_family = base_family) +
    theme(
      plot.title = element_text(face = "bold", size = rel(1.18), color = "#1A1A1A", margin = margin(b = 4)),
      plot.subtitle = element_text(size = rel(0.92), color = "#555555", margin = margin(b = 10)),
      plot.caption = element_text(size = rel(0.75), color = "#777777", margin = margin(t = 8), hjust = 0),
      axis.title.x = element_text(face = "bold", size = rel(0.92), color = "#222222", margin = margin(t = 6)),
      axis.title.y = element_text(face = "bold", size = rel(0.92), color = "#222222", margin = margin(r = 6)),
      axis.text = element_text(size = rel(0.85), color = "#333333"),
      axis.line = element_line(color = "#333333", linewidth = 0.45),
      axis.ticks = element_line(color = "#333333", linewidth = 0.45),
      legend.position = "top",
      legend.justification = "left",
      legend.direction = "horizontal",
      legend.title = element_text(face = "bold", size = rel(0.85), color = "#222222"),
      legend.text = element_text(size = rel(0.82), color = "#333333"),
      legend.background = element_blank(),
      legend.key = element_blank(),
      strip.background = element_rect(fill = "#F4F5F7", color = NA),
      strip.text = element_text(face = "bold", size = rel(0.85), color = "#222222", margin = margin(t = 4, b = 4)),
      panel.background = element_rect(fill = "#FFFFFF", color = NA),
      plot.background = element_rect(fill = "#FFFFFF", color = NA),
      plot.margin = margin(12, 14, 12, 12)
    )
  
  if (grid == "y") {
    thm <- thm + theme(
      panel.grid.major.y = element_line(color = "#EEEEEE", linewidth = 0.35),
      panel.grid.major.x = element_blank()
    )
  } else if (grid == "x") {
    thm <- thm + theme(
      panel.grid.major.x = element_line(color = "#EEEEEE", linewidth = 0.35),
      panel.grid.major.y = element_blank()
    )
  } else if (grid == "xy") {
    thm <- thm + theme(
      panel.grid.major = element_line(color = "#EEEEEE", linewidth = 0.35)
    )
  } else {
    thm <- thm + theme(panel.grid.major = element_blank())
  }
  thm
}
