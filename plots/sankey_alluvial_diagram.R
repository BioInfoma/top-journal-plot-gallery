# Three-Stage Sankey / Alluvial Patient Treatment Trajectory
# Tier 3 (Advanced) - Flow

make_plot <- function(data_dir = "data") {
  library(ggplot2)
  if (file.exists("theme_journal.R")) source("theme_journal.R", local = TRUE)
  
  # Multi-stage longitudinal patient cohort (N = 300)
  # Stage 1 (Diagnosis) -> Stage 2 (1st Line Therapy) -> Stage 3 (2-Year Outcome)
  flows <- data.frame(
    diag = c(
      rep("Luminal A", 3),
      rep("Luminal B", 3),
      rep("HER2+", 2),
      rep("Triple-Negative", 2)
    ),
    therapy = c(
      "Endocrine", "Endocrine", "Chemo+Endo",
      "Chemo+Endo", "Chemo+Endo", "Chemotherapy",
      "Targeted anti-HER2", "Targeted anti-HER2",
      "Chemotherapy", "Chemo + IO"
    ),
    outcome = c(
      "Complete Remission", "Stable Disease", "Complete Remission",
      "Complete Remission", "Stable Disease", "Relapse / Progression",
      "Complete Remission", "Relapse / Progression",
      "Relapse / Progression", "Complete Remission"
    ),
    n = c(
      75, 25, 20,
      40, 30, 20,
      45, 15,
      18, 12
    ),
    stringsAsFactors = FALSE
  )
  
  # Helper to compute y positions for strata
  calc_strata <- function(df, col_name, x_pos) {
    agg <- aggregate(n ~ get(col_name), data = df, sum)
    colnames(agg)[1] <- "name"
    agg <- agg[order(-agg$n), ]
    agg$cum_top <- cumsum(agg$n)
    agg$cum_bot <- c(0, head(agg$cum_top, -1))
    agg$x <- x_pos
    agg
  }
  
  s1 <- calc_strata(flows, "diag", 1)
  s2 <- calc_strata(flows, "therapy", 2)
  s3 <- calc_strata(flows, "outcome", 3)
  
  # Smooth sigmoidal ribbons between stages
  make_ribbons <- function(df, from_col, to_col, s_from, s_to, x1, x2, color_col) {
    agg_flow <- aggregate(n ~ get(from_col) + get(to_col) + get(color_col), data = df, sum)
    colnames(agg_flow) <- c("from", "to", "color_val", "n")
    
    # Track current offsets
    s_from$curr_bot <- s_from$cum_bot
    s_to$curr_bot <- s_to$cum_bot
    
    ribbons <- list()
    for (i in 1:nrow(agg_flow)) {
      row_i <- agg_flow[i, ]
      idx_f <- which(s_from$name == row_i$from)
      idx_t <- which(s_to$name == row_i$to)
      
      y0_bot <- s_from$curr_bot[idx_f]
      y0_top <- y0_bot + row_i$n
      s_from$curr_bot[idx_f] <- y0_top
      
      y1_bot <- s_to$curr_bot[idx_t]
      y1_top <- y1_bot + row_i$n
      s_to$curr_bot[idx_t] <- y1_top
      
      # Sigmoid interpolation
      t_seq <- seq(0, 1, length.out = 30)
      sig <- 1 / (1 + exp(-10 * (t_seq - 0.5)))
      x_coords <- x1 + t_seq * (x2 - x1)
      y_bot <- y0_bot + (y1_bot - y0_bot) * sig
      y_top <- y0_top + (y1_top - y0_top) * sig
      
      ribbons[[i]] <- data.frame(
        x = c(x_coords, rev(x_coords)),
        y = c(y_bot, rev(y_top)),
        color_val = row_i$color_val,
        flow_id = paste0(x1, "_", i)
      )
    }
    do.call(rbind, ribbons)
  }
  
  ribbons_1_2 <- make_ribbons(flows, "diag", "therapy", s1, s2, 1.08, 1.92, "diag")
  ribbons_2_3 <- make_ribbons(flows, "therapy", "outcome", s2, s3, 2.08, 2.92, "diag")
  all_ribbons <- rbind(ribbons_1_2, ribbons_2_3)
  
  # Strata blocks for plotting
  all_strata <- rbind(
    data.frame(xmin = 0.92, xmax = 1.08, ymin = s1$cum_bot, ymax = s1$cum_top, label = s1$name, n = s1$n),
    data.frame(xmin = 1.92, xmax = 2.08, ymin = s2$cum_bot, ymax = s2$cum_top, label = s2$name, n = s2$n),
    data.frame(xmin = 2.92, xmax = 3.08, ymin = s3$cum_bot, ymax = s3$cum_top, label = s3$name, n = s3$n)
  )
  all_strata$pct <- paste0(round(100 * all_strata$n / 300), "%")
  all_strata$text_label <- paste0(all_strata$label, "\n(n=", all_strata$n, ", ", all_strata$pct, ")")
  
  diag_colors <- c(
    "Luminal A" = "#00A087",
    "Luminal B" = "#3C5488",
    "HER2+" = "#E64B35",
    "Triple-Negative" = "#DC0000"
  )
  
  p <- ggplot() +
    # Smooth continuous alluvial ribbons
    geom_polygon(data = all_ribbons, aes(x = x, y = y, group = flow_id, fill = color_val), alpha = 0.45) +
    # Vertical strata nodes
    geom_rect(data = all_strata, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax), 
              fill = "#1E293B", color = "#FFFFFF", linewidth = 0.5) +
    geom_text(data = all_strata, aes(x = (xmin + xmax) / 2, y = (ymin + ymax) / 2, label = text_label), 
              color = "#FFFFFF", fontface = "bold", size = 2.7, lineheight = 0.9) +
    scale_fill_manual(values = diag_colors, name = "Diagnosis Subtype:") +
    scale_x_continuous(
      breaks = c(1, 2, 3),
      labels = c("1. Molecular Diagnosis", "2. First-Line Intervention", "3. Two-Year Clinical Outcome"),
      limits = c(0.65, 3.35)
    ) +
    scale_y_continuous(expand = c(0.01, 0)) +
    labs(
      title = "Multi-Stage Alluvial Flow: Oncology Patient Care Pathways",
      subtitle = "Longitudinal patient flow across diagnosis, systemic therapy, and 2-year therapeutic response",
      y = "Total Cohort Volume (N = 300 Patients)",
      caption = "Source: Simulated multi-center oncology registry tracking 300 patients across 3 landmark stages"
    )
  
  if (exists("theme_topjournal")) {
    p <- p + theme_topjournal(grid = "none") +
      theme(
        axis.text.x = element_text(face = "bold", size = rel(1.05), color = "#0F172A"),
        axis.text.y = element_text(size = rel(0.85)),
        axis.line = element_blank(),
        axis.ticks = element_blank(),
        legend.position = "top",
        legend.direction = "horizontal",
        legend.title = element_text(face = "bold", size = 9),
        legend.text = element_text(size = 8.5)
      )
  } else {
    p <- p + theme_classic()
  }
  p
}

explain <- function() {
  list(
    title = "Multi-Stage Alluvial Flow Diagram",
    how_to_read = "Vertical rectangular blocks represent discrete disease, treatment, or outcome states across successive clinical milestones. Fluid sigmoidal bands link strata, with band thickness proportional to the number and percentage of patients transitioning along that pathway.",
    when_to_use = "The premier graphic in clinical oncology, epidemiology, and health systems research to visualize complex multi-stage care pathways, line-of-therapy attrition, and treatment efficacy transitions.",
    pitfalls = "Maintain identical color coding for patient cohorts across all transitions. Avoid excessive strata or crisscrossing ribbons that obscure dominant flows.",
    journal_archetype = "The Lancet and Nature Medicine real-world evidence and therapeutic sequencing diagrams.",
    data_recipe = "Requires multi-column longitudinal patient categorical transitions and volume counts."
  )
}
