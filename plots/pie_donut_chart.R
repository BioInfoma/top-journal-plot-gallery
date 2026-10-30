# Clean Donut Chart for Categorical Shares
# Tier 1 (Simple) - Composition

make_plot <- function(data_dir = "data") {
  library(ggplot2)
  if (file.exists("theme_journal.R")) source("theme_journal.R", local = TRUE)
  
  df <- read.csv(file.path(data_dir, "grp.csv"))
  tbl <- as.data.frame(table(df$group))
  colnames(tbl) <- c("category", "count")
  tbl$fraction <- tbl$count / sum(tbl$count)
  tbl$ymax <- cumsum(tbl$fraction)
  tbl$ymin <- c(0, head(tbl$ymax, n = -1))
  tbl$label_pos <- (tbl$ymax + tbl$ymin) / 2
  tbl$pct_label <- paste0(round(tbl$fraction * 100, 1), "%")
  
  p <- ggplot(tbl, aes(ymax = ymax, ymin = ymin, xmax = 4, xmin = 2.4, fill = category)) +
    geom_rect(color = "#FFFFFF", linewidth = 1.2) +
    coord_polar(theta = "y") +
    xlim(c(1, 4)) +
    geom_text(aes(x = 3.2, y = label_pos, label = pct_label), color = "#FFFFFF", fontface = "bold", size = 3.6) +
    labs(
      title = "Cohort Allocation Proportions",
      subtitle = "Donut ring showing relative distribution across experimental strata",
      fill = "Cohort",
      caption = "Source: Simulated multi-arm participant registry"
    )
  
  if (exists("theme_topjournal")) {
    p <- p + theme_topjournal(grid = "none") + scale_fill_topjournal() +
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
    title = "Donut Composition Chart",
    how_to_read = "The circular circumference represents 100% of the sample. Arc length and area correspond proportionally to each cohort's share of total observations.",
    when_to_use = "Appropriate only for presenting high-level proportional shares among a very small number of distinct categories (ideally 2 to 5 classes).",
    pitfalls = "Human visual perception estimates linear lengths far better than polar angles or curved areas. For more than 4-5 categories or subtle percentage differences, always prefer a horizontal bar or lollipop chart.",
    journal_archetype = "Lancet executive summary demographic breakdowns.",
    data_recipe = "Requires 1 categorical factor with 2–5 distinct levels."
  )
}
