# Scalable Set Intersections (UpSet Plot)
# Tier 2 (Intermediate) - Sets

make_plot <- function(data_dir = "data") {
  library(ggplot2)
  if (file.exists("theme_journal.R")) source("theme_journal.R", local = TRUE)
  
  df <- read.csv(file.path(data_dir, "sets.csv"))
  sets <- c("SetA", "SetB", "SetC", "SetD")
  
  # Compute all non-empty combinations
  comb_strings <- apply(df[, sets], 1, function(r) paste(as.integer(r), collapse = ""))
  comb_table <- sort(table(comb_strings), decreasing = TRUE)
  # Take top 10 most frequent intersection patterns
  top_combs <- head(comb_table[names(comb_table) != "0000"], 10)
  
  comb_df <- data.frame(
    comb_id = 1:length(top_combs),
    pattern = names(top_combs),
    size = as.numeric(top_combs)
  )
  
  # Matrix dot grid data
  grid_rows <- list()
  for (i in 1:nrow(comb_df)) {
    chars <- strsplit(comb_df$pattern[i], "")[[1]]
    for (s_idx in 1:length(sets)) {
      grid_rows[[length(grid_rows) + 1]] <- data.frame(
        comb_id = i,
        set_name = sets[s_idx],
        set_idx = s_idx,
        is_active = chars[s_idx] == "1"
      )
    }
  }
  grid_df <- do.call(rbind, grid_rows)
  grid_df$set_name <- factor(grid_df$set_name, levels = rev(sets))
  
  # Connecting line segments between active dots
  line_segments <- do.call(rbind, lapply(1:nrow(comb_df), function(i) {
    sub_g <- grid_df[grid_df$comb_id == i & grid_df$is_active, ]
    if (nrow(sub_g) > 1) {
      data.frame(
        comb_id = i,
        y_min = min(as.numeric(sub_g$set_name)),
        y_max = max(as.numeric(sub_g$set_name))
      )
    } else NULL
  }))
  
  # Upper Bar Chart (Intersection Size)
  p1 <- ggplot(comb_df, aes(x = factor(comb_id), y = size)) +
    geom_col(fill = "#1E40AF", width = 0.65, alpha = 0.9) +
    geom_text(aes(label = size), vjust = -0.5, size = 3.2, fontface = "bold", color = "#1E293B") +
    scale_y_continuous(expand = expansion(mult = c(0, 0.18))) +
    labs(
      title = "UpSet Plot: Scalable High-Order Set Intersections",
      subtitle = "Upper bars show intersection cardinality; matrix dots indicate participating sets",
      y = "Intersection Size"
    ) +
    theme_classic() +
    theme(
      axis.title.x = element_blank(),
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank(),
      plot.title = element_text(face = "bold", size = 13, color = "#0F172A"),
      plot.subtitle = element_text(size = 10, color = "#475569")
    )
  
  # Lower Dot Matrix
  p2 <- ggplot(grid_df, aes(x = factor(comb_id), y = set_name)) +
    geom_point(color = "#E2E8F0", size = 4.2) +
    (if (!is.null(line_segments)) geom_segment(data = line_segments, aes(x = factor(comb_id), xend = factor(comb_id), y = y_min, yend = y_max), color = "#1E40AF", linewidth = 1.05) else NULL) +
    geom_point(data = grid_df[grid_df$is_active, ], color = "#1E40AF", size = 4.4) +
    labs(x = "Intersection Combination Rank", y = "Set") +
    theme_classic() +
    theme(
      axis.line = element_blank(),
      axis.ticks = element_blank(),
      axis.text.y = element_text(face = "bold", color = "#1E293B", size = 10),
      panel.grid.major.y = element_line(color = "#F1F5F9", linewidth = 0.5)
    )
  
  if (requireNamespace("patchwork", quietly = TRUE)) {
    return(p1 / p2 + patchwork::plot_layout(heights = c(2.2, 1.2)))
  } else {
    p1
  }
}

explain <- function() {
  list(
    title = "UpSet Plot for High-Order Set Intersections",
    how_to_read = "The bottom matrix rows represent sets, where filled dots connected by vertical lines indicate a specific combination of overlapping sets. The top vertical bar chart displays the exact cardinality of each intersection.",
    when_to_use = "The modern peer-reviewed replacement for Venn diagrams when comparing 4 or more sets (e.g. shared orthologs, differential gene lists, mutation catalogs).",
    pitfalls = "Order intersections logically (by descending frequency or degree of overlap) to keep the matrix interpretable.",
    journal_archetype = "Nature Methods and Nature Genetics multi-cohort and multi-pipeline comparisons.",
    data_recipe = "Requires binary membership matrix or item-set list."
  )
}
