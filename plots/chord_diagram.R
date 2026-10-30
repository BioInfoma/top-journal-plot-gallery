# Circular Chord Diagram of Directional Flows
# Tier 3 (Advanced) - Flow

make_plot <- function(data_dir = "data") {
  library(ggplot2)
  if (file.exists("theme_journal.R")) source("theme_journal.R", local = TRUE)
  
  df <- read.csv(file.path(data_dir, "flow.csv"))
  
  if (requireNamespace("circlize", quietly = TRUE)) {
    draw_chord <- function() {
      circlize::circos.clear()
      circlize::circos.par(start.degree = 90, gap.degree = 5)
      
      # Directional flow matrix
      grid_cols <- c(
        "Stage I" = "#10B981", "Stage II" = "#3B82F6", "Stage III" = "#F59E0B",
        "Responder" = "#059669", "Stable" = "#6366F1", "Progressed" = "#DC2626"
      )
      
      circlize::chordDiagram(
        df,
        grid.col = grid_cols,
        transparency = 0.35,
        annotationTrack = c("name", "grid"),
        annotationTrackHeight = c(0.04, 0.06),
        directional = 1,
        direction.type = c("diffHeight", "arrows"),
        link.arr.type = "big.arrow"
      )
      
      title("Circular Chord Diagram: Patient Stage Transition Dynamics", 
            cex.main = 1.05, font.main = 2, col.main = "#0F172A", line = 1)
    }
    
    if (requireNamespace("ggplotify", quietly = TRUE)) {
      return(ggplotify::as.ggplot(draw_chord))
    } else {
      draw_chord()
    }
  } else {
    plot.new()
    text(0.5, 0.5, "Requires 'circlize' package")
  }
}

explain <- function() {
  list(
    title = "Circular Chord Transition Diagram",
    how_to_read = "Categories occupy circular perimeter arcs, with area proportional to total flow volume. Flow ribbons connect source and destination categories across the circle; arrowheads and unequal arc widths at endpoints show the magnitude and net directionality of transitions.",
    when_to_use = "Spectacular for presenting migration patterns, cell differentiation transitions, economic trade flows, or pairwise immune cell repertoire interactions.",
    pitfalls = "Ribbons crossing the circular center can become visually chaotic if there are too many fine-grained categories; aggregate minor flows into larger groups.",
    journal_archetype = "Nature and Science international trade, human migration, and cell-cell interaction networks.",
    data_recipe = "Requires pairwise flow contingency table or transition edgelist with volume counts."
  )
}
