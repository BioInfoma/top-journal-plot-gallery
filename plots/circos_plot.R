# Circos Genomic Architecture & Translocation Ribbons
# Tier 3 (Advanced) - Genomics

make_plot <- function(data_dir = "data") {
  library(ggplot2)
  if (file.exists("theme_journal.R")) source("theme_journal.R", local = TRUE)
  
  if (requireNamespace("circlize", quietly = TRUE)) {
    # If circlize is available, draw full publication circos plot
    draw_circos <- function() {
      circlize::circos.clear()
      circlize::circos.par(start.degree = 90, gap.degree = 4, track.margin = c(0.01, 0.01))
      
      # Chromosomes 1 through 12
      chr_names <- paste0("Chr", 1:12)
      chr_lens <- c(240, 220, 190, 180, 170, 160, 150, 140, 130, 125, 120, 115)
      df_chrs <- data.frame(
        chr = factor(chr_names, levels = chr_names),
        start = 0,
        end = chr_lens
      )
      
      circlize::circos.initialize(factors = df_chrs$chr, xlim = cbind(df_chrs$start, df_chrs$end))
      
      # Track 1: Chromosome ideogram ring
      chr_colors <- rep(c("#1E3A8A", "#3B82F6", "#0D9488", "#10B981"), length.out = 12)
      circlize::circos.track(ylim = c(0, 1), bg.col = chr_colors, track.height = 0.08,
                             panel.fun = function(x, y) {
                               sector_name <- circlize::get.cell.meta.data("sector.index")
                               circlize::circos.text(circlize::CELL_META$xcenter, 0.5, sector_name, 
                                                     facing = "clockwise", niceFacing = TRUE, col = "#FFFFFF", cex = 0.65, font = 2)
                             })
      
      # Track 2: Copy Number Variation (CNV) scatter
      set.seed(42)
      circlize::circos.track(ylim = c(-2, 2), track.height = 0.12, bg.col = "#F8FAFC",
                             panel.fun = function(x, y) {
                               x_pts <- runif(15, 0, circlize::get.cell.meta.data("xlim")[2])
                               y_pts <- rnorm(15, 0, 0.7)
                               cols <- ifelse(y_pts > 0.5, "#DC2626", ifelse(y_pts < -0.5, "#2563EB", "#94A3B8"))
                               circlize::circos.points(x_pts, y_pts, pch = 16, cex = 0.45, col = cols)
                             })
      
      # Central Translocation Links
      circlize::circos.link("Chr1", c(30, 70), "Chr7", c(40, 90), col = "#DC262680", border = "#DC2626")
      circlize::circos.link("Chr3", c(110, 150), "Chr9", c(20, 60), col = "#2563EB80", border = "#2563EB")
      circlize::circos.link("Chr4", c(50, 90), "Chr11", c(60, 100), col = "#05966980", border = "#059669")
      circlize::circos.link("Chr2", c(80, 120), "Chr8", c(30, 70), col = "#D9770680", border = "#D97706")
      
      title("Circos Pan-Genomic Architecture: Chromosomal Rearrangements", 
            cex.main = 1.05, font.main = 2, col.main = "#0F172A", line = 1)
    }
    
    if (requireNamespace("ggplotify", quietly = TRUE)) {
      return(ggplotify::as.ggplot(draw_circos))
    } else {
      draw_circos()
    }
  } else {
    plot.new()
    text(0.5, 0.5, "Requires 'circlize' package")
  }
}

explain <- function() {
  list(
    title = "Circos Whole-Genome Structural Variation Map",
    how_to_read = "Chromosomes form a circular perimeter ring (ideograms). Concentric annular tracks display localized genomic features (somatic mutations, copy number amplifications/deletions). Curved ribbons crossing the central interior represent inter-chromosomal structural rearrangements (translocations, gene fusions).",
    when_to_use = "The iconic visual flagship of cancer whole-genome sequencing (TCGA, ICGC) and structural genomics to present complex karyotypes and chromothripsis events in a single figure.",
    pitfalls = "High ribbon density can turn the center into an impenetrable knot; filter links strictly to validated high-confidence fusion events or recurrent driver rearrangements.",
    journal_archetype = "Nature, Science, and Cell cancer genome structural variation landscapes.",
    data_recipe = "Requires chromosome coordinate boundaries, regional CNV/mutation values, and paired translocation breakends."
  )
}
