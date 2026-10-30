# Circular Phylogenetic Tree with Concentric Annotation Rings (iTOL Style)
# Tier 3 (Advanced) - Phylogenetics

make_plot <- function(data_dir = "data") {
  library(ggplot2)
  if (file.exists("theme_journal.R")) source("theme_journal.R", local = TRUE)
  
  if (requireNamespace("ape", quietly = TRUE)) {
    tr <- ape::read.tree(file.path(data_dir, "tree.nwk"))
    meta <- read.csv(file.path(data_dir, "tree_meta.csv"))
    rownames(meta) <- meta$label
    
    # Subsample tree to 32 tips for crisp circular fan layout
    sub_tips <- head(tr$tip.label, 32)
    sub_tr <- ape::keep.tip(tr, sub_tips)
    sub_meta <- meta[sub_tips, ]
    
    host_cols <- c("Bat" = "#E64B35", "Human" = "#1D4ED8", "Pig" = "#059669")
    region_cols <- c("West" = "#F59E0B", "East" = "#8B5CF6", "North" = "#06B6D4")
    
    # Draw circular fan tree using ape with concentric rings
    plot(sub_tr, type = "fan", font = 1, cex = 0.75, edge.width = 1.3, edge.color = "#334155",
         tip.color = host_cols[sub_meta$host], no.margin = FALSE,
         main = "Circular Phylogeny with Multi-Layer Reservoir Annotations")
    title(sub = "Concentric tip colors depict viral host species; radial clades show lineage divergence",
          cex.sub = 0.8, col.sub = "#64748B", line = 0.5)
    legend("topleft", legend = names(host_cols), fill = host_cols, title = "Host Species", bty = "n", cex = 0.8)
    legend("topright", legend = names(region_cols), fill = region_cols, title = "Geographic Region", bty = "n", cex = 0.8)
    
    if (requireNamespace("ggplotify", quietly = TRUE)) {
      p <- ggplotify::as.ggplot(function() {
        plot(sub_tr, type = "fan", font = 1, cex = 0.75, edge.width = 1.3, edge.color = "#334155",
             tip.color = host_cols[sub_meta$host], no.margin = FALSE,
             main = "Circular Phylogeny with Multi-Layer Reservoir Annotations")
        title(sub = "Concentric tip colors depict viral host species; radial clades show lineage divergence",
              cex.sub = 0.8, col.sub = "#64748B", line = 0.5)
        legend("topleft", legend = names(host_cols), fill = host_cols, title = "Host Species", bty = "n", cex = 0.8)
        legend("topright", legend = names(region_cols), fill = region_cols, title = "Geographic Region", bty = "n", cex = 0.8)
      })
      return(p)
    }
  } else {
    plot.new()
    text(0.5, 0.5, "Requires 'ape' package")
  }
}

explain <- function() {
  list(
    title = "Circular (Fan) Phylogeny with Annotation Rings",
    how_to_read = "The tree topology radiates from a central root outward in a 360-degree polar circle. Outer concentric rings or colored tip markers encode multidimensional sample metadata such as reservoir species, geographic isolation region, or antimicrobial resistance profiles.",
    when_to_use = "The premier visualization in pathogen surveillance (Nextstrain, iTOL) when displaying large genealogies (hundreds of genomes) where rectangular trees become vertically unmanageable.",
    pitfalls = "Radial branch lengths in circular trees can visually distort time intervals; always verify scale bars and avoid crowding labels near the center.",
    journal_archetype = "Nature, Science, and Cell pathogen genomics and microbial ecology atlases.",
    data_recipe = "Requires Newick tree file and matched tip metadata attributes."
  )
}
