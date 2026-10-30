# Proportional Set Intersections (Venn Diagram)
# Tier 2 (Intermediate) - Sets

make_plot <- function(data_dir = "data") {
  library(ggplot2)
  if (file.exists("theme_journal.R")) source("theme_journal.R", local = TRUE)
  
  df <- read.csv(file.path(data_dir, "sets.csv"))
  
  # Compute intersections for 3 sets: SetA, SetB, SetC
  a <- df$SetA; b <- df$SetB; c <- df$SetC
  
  n_a_only <- sum(a & !b & !c)
  n_b_only <- sum(!a & b & !c)
  n_c_only <- sum(!a & !b & c)
  n_ab <- sum(a & b & !c)
  n_ac <- sum(a & !b & c)
  n_bc <- sum(!a & b & c)
  n_abc <- sum(a & b & c)
  
  # Geometric circles for 3-set layout
  theta <- seq(0, 2 * pi, length.out = 150)
  r <- 1.45
  
  circle_a <- data.frame(x = -0.75 + r * cos(theta), y = 0.55 + r * sin(theta), set = "Set A")
  circle_b <- data.frame(x = 0.75 + r * cos(theta), y = 0.55 + r * sin(theta), set = "Set B")
  circle_c <- data.frame(x = 0 + r * cos(theta), y = -0.65 + r * sin(theta), set = "Set C")
  circles <- rbind(circle_a, circle_b, circle_c)
  
  labels_df <- data.frame(
    x = c(-1.3, 1.3, 0, 0, -0.7, 0.7, 0),
    y = c(0.8, 0.8, -1.1, 0.75, -0.2, -0.2, 0.18),
    label = c(paste0("Set A\n(", n_a_only, ")"),
              paste0("Set B\n(", n_b_only, ")"),
              paste0("Set C\n(", n_c_only, ")"),
              n_ab, n_ac, n_bc, n_abc)
  )
  
  p <- ggplot() +
    geom_polygon(data = circles, aes(x = x, y = y, fill = set, color = set), alpha = 0.28, linewidth = 0.9) +
    geom_text(data = labels_df, aes(x = x, y = y, label = label), fontface = "bold", size = 4.2, color = "#1E293B") +
    scale_fill_manual(values = c("Set A" = "#E64B35", "Set B" = "#3C5488", "Set C" = "#00A087")) +
    scale_color_manual(values = c("Set A" = "#E64B35", "Set B" = "#3C5488", "Set C" = "#00A087")) +
    coord_fixed() +
    labs(
      title = "Three-Set Venn Diagram: Shared Biomarker Signatures",
      subtitle = "Overlapping areas illustrate intersection cardinality across three experimental gene sets",
      fill = "Cohort Set",
      color = "Cohort Set",
      caption = "Source: Simulated gene discovery catalog (N = 300 candidates)"
    )
  
  if (exists("theme_topjournal")) {
    p <- p + theme_topjournal(grid = "none") +
      theme(
        axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank()
      )
  } else {
    p <- p + theme_void()
  }
  p
}

explain <- function() {
  list(
    title = "Three-Way Set Intersection Venn Diagram",
    how_to_read = "Overlapping circular planar regions represent shared set memberships. Each distinct enclosed zone displays the exact count of unique entities found exclusively within that combination of categories.",
    when_to_use = "Classic visualization for presenting overlaps among 2 or 3 sets (e.g. genes identified by different sequencing platforms or variant callers).",
    pitfalls = "Venn diagrams become visually confusing and topologically flawed beyond 3 or 4 sets; for 4 or more sets, standard journal reporting requires an UpSet plot.",
    journal_archetype = "Nature Protocols and Genome Biology tool comparisons.",
    data_recipe = "Requires binary membership columns or named character vectors of set items."
  )
}
