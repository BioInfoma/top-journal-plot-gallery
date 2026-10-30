# DensiTree: Posterior Distribution Density of Phylogenetic Trees
# Tier 3 (Advanced) - Phylogenetics

make_plot <- function(data_dir = "data") {
  library(ggplot2)
  if (file.exists("theme_journal.R")) source("theme_journal.R", local = TRUE)
  
  # Simulate posterior tree sample overlay (100 MCMC trees with topological uncertainty)
  set.seed(42)
  taxa <- paste0("Taxon_", LETTERS[1:10])
  n_taxa <- length(taxa)
  n_trees <- 40
  
  # Base coordinates for consensus cladogram
  branch_list <- list()
  for (t in 1:n_trees) {
    # Add slight branch length and node height jitter representing posterior variance
    jitter_factor <- rnorm(n_taxa, mean = 0, sd = 0.04)
    y_coords <- 1:n_taxa + rnorm(n_taxa, 0, 0.02)
    
    # Simple coalescent branch coordinates
    for (i in 1:(n_taxa - 1)) {
      x0 <- runif(1, 0, 0.2)
      x1 <- runif(1, 0.4, 0.8)
      branch_list[[length(branch_list) + 1]] <- data.frame(
        x = c(x0, x1, x1),
        y = c(i, i, i + 1),
        tree_id = t,
        clade = ifelse(t > 30, "Minor Topology (ILS)", "Consensus Topology")
      )
    }
  }
  df_branches <- do.call(rbind, branch_list)
  
  p <- ggplot(df_branches, aes(x = x, y = y, group = interaction(tree_id, clade), color = clade)) +
    geom_path(alpha = 0.18, linewidth = 0.75) +
    scale_color_manual(values = c("Consensus Topology" = "#1D4ED8", "Minor Topology (ILS)" = "#DC2626"), name = "Posterior State:") +
    scale_y_continuous(breaks = 1:n_taxa, labels = taxa, limits = c(0.5, n_taxa + 0.5)) +
    scale_x_continuous(breaks = seq(0, 1, by = 0.2), limits = c(-0.05, 1.15)) +
    labs(
      title = "DensiTree: Transparent Bayesian Posterior Tree Overlay",
      subtitle = "Multi-tree overlay reveals topological concordance, node dating variance, and reticulation/ILS",
      x = "Divergence Time Depth (Root to Tips)",
      y = "Taxon Tip Label",
      caption = "Source: Simulated Bayesian MCMC posterior tree sample (40 independent topologies)"
    )
  
  if (exists("theme_topjournal")) {
    p <- p + theme_topjournal(grid = "y") +
      theme(
        axis.text.y = element_text(face = "bold", size = rel(0.9)),
        legend.position = "top",
        legend.direction = "horizontal",
        legend.title = element_text(face = "bold", size = 8.5)
      )
  } else {
    p <- p + theme_classic()
  }
  p
}

explain <- function() {
  list(
    title = "DensiTree Posterior Tree Density Overlay",
    how_to_read = "Hundreds of trees sampled from a Bayesian MCMC posterior distribution are plotted directly on top of each other with low opacity (alpha transparency). Dense solid bundles reflect strong topological consensus; diffuse webbing reveals phylogenetic conflict, incomplete lineage sorting (ILS), or hybridization.",
    when_to_use = "Essential companion in phylogenomics to evaluate genome-wide gene tree incongruence and distinguish true evolutionary consensus from stochastic noise.",
    pitfalls = "Single consensus trees (like majority-rule trees) discard valuable uncertainty; DensiTree prevents misleading readers by making phylogenetic ambiguity visible.",
    journal_archetype = "Systematic Biology and Molecular Biology and Evolution phylogenomic discordance figures.",
    data_recipe = "Requires a multi-tree Newick or Nexus file containing posterior MCMC trees."
  )
}
