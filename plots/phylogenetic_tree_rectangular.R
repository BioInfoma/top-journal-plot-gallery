# Rectangular Phylogenetic Tree with Host Metadata
# Tier 3 (Advanced) - Phylogenetics

make_plot <- function(data_dir = "data") {
  library(ggplot2)
  if (file.exists("theme_journal.R")) source("theme_journal.R", local = TRUE)
  
  if (requireNamespace("ape", quietly = TRUE)) {
    tr <- ape::read.tree(file.path(data_dir, "tree.nwk"))
    meta <- read.csv(file.path(data_dir, "tree_meta.csv"))
    rownames(meta) <- meta$label
    
    # Prune tree to 22 tips for clear, spacious publication rendering
    keep_tips <- head(tr$tip.label, 22)
    sub_tr <- ape::keep.tip(tr, keep_tips)
    sub_meta <- meta[keep_tips, ]
    
    # Use ape's plot with metadata tip colors
    host_cols <- c("Bat" = "#E64B35", "Human" = "#3C5488", "Pig" = "#00A087")
    tip_col <- host_cols[sub_meta$host]
    
    # We can render using base R plot wrapped in ggplotify or base device
    plot(sub_tr, font = 1, cex = 0.85, edge.width = 1.6, edge.color = "#334155", 
         tip.color = tip_col, no.margin = FALSE, main = "Phylogenetic Tree of Viral Isolates across Host Reservoirs")
    title(sub = "Simulated coalescent tree; tip colors denote reservoir host species", 
          cex.sub = 0.8, col.sub = "#64748B", line = 0.5)
    legend("topleft", legend = names(host_cols), fill = host_cols, title = "Host Species", 
           bty = "n", cex = 0.85)
    ape::add.scale.bar(length = 0.1, lcol = "#334155", lwd = 1.5)
    
    # If ggplotify is available, return as grob so Shiny renders seamlessly
    if (requireNamespace("ggplotify", quietly = TRUE)) {
      p <- ggplotify::as.ggplot(function() {
        plot(sub_tr, font = 1, cex = 0.85, edge.width = 1.6, edge.color = "#334155", 
             tip.color = tip_col, no.margin = FALSE, main = "Phylogenetic Tree of Viral Isolates across Host Reservoirs")
        title(sub = "Simulated coalescent tree; tip colors denote reservoir host species", 
              cex.sub = 0.8, col.sub = "#64748B", line = 0.5)
        legend("topleft", legend = names(host_cols), fill = host_cols, title = "Host Species", 
               bty = "n", cex = 0.85)
        ape::add.scale.bar(length = 0.1, lcol = "#334155", lwd = 1.5)
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
    title = "Rectangular Phylogenetic Tree with Host Annotations",
    how_to_read = "The branching hierarchy depicts evolutionary divergence from a common root to extant taxa at the tips. Horizontal branch lengths are proportional to genetic divergence or evolutionary time. Colored tip labels correspond to host species or geographic isolation reservoirs.",
    when_to_use = "Essential for evolutionary biology, viral molecular epidemiology, pathogen surveillance, and comparative genomics.",
    pitfalls = "Clade orientation around nodes can be flipped without changing tree topology; never assume taxa adjacent in vertical order are closest evolutionary relatives without following branch nodes.",
    journal_archetype = "Nature and Science outbreak phylogenetics (e.g. Nextstrain, Ebola, SARS-CoV-2).",
    data_recipe = "Requires Newick/Nexus phylogenetic tree and matched tip metadata."
  )
}
