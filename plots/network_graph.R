# Protein-Protein / Gene Regulatory Network Graph
# Tier 3 (Advanced) - Network

make_plot <- function(data_dir = "data") {
  library(ggplot2)
  if (file.exists("theme_journal.R")) source("theme_journal.R", local = TRUE)
  
  df <- read.csv(file.path(data_dir, "net.csv"))
  
  # Select top 30 most connected nodes for clean, legible network layout
  nodes <- unique(c(df$from, df$to))
  degrees <- table(c(df$from, df$to))
  top_nodes <- names(sort(degrees, decreasing = TRUE)[1:25])
  sub_df <- df[df$from %in% top_nodes & df$to %in% top_nodes, ]
  
  if (requireNamespace("igraph", quietly = TRUE)) {
    g <- igraph::graph_from_data_frame(sub_df, directed = FALSE)
    set.seed(42)
    layout_mat <- igraph::layout_with_fr(g)
    
    # Extract tidy edge coordinates
    edge_list <- igraph::as_data_frame(g, what = "edges")
    edge_coords <- list()
    for (i in 1:nrow(edge_list)) {
      u_idx <- which(igraph::V(g)$name == edge_list$from[i])
      v_idx <- which(igraph::V(g)$name == edge_list$to[i])
      edge_coords[[i]] <- data.frame(
        x = layout_mat[u_idx, 1],
        y = layout_mat[u_idx, 2],
        xend = layout_mat[v_idx, 1],
        yend = layout_mat[v_idx, 2],
        weight = edge_list$weight[i]
      )
    }
    edges_df <- do.call(rbind, edge_coords)
    
    # Extract tidy node coordinates
    deg <- igraph::degree(g)
    nodes_df <- data.frame(
      name = igraph::V(g)$name,
      x = layout_mat[, 1],
      y = layout_mat[, 2],
      degree = deg,
      is_hub = deg >= quantile(deg, 0.75)
    )
    
    p <- ggplot() +
      geom_segment(data = edges_df, aes(x = x, y = y, xend = xend, yend = yend), 
                   color = "#CBD5E1", linewidth = 0.65, alpha = 0.6) +
      geom_point(data = nodes_df, aes(x = x, y = y, size = degree, fill = is_hub), 
                 shape = 21, color = "#1E293B", stroke = 0.6) +
      geom_text(data = nodes_df[nodes_df$is_hub, ], aes(x = x, y = y, label = name), 
                vjust = -1.2, fontface = "bold", size = 3.2, color = "#0F172A") +
      scale_size_continuous(range = c(3.5, 9.5), name = "Connectivity Degree") +
      scale_fill_manual(values = c("FALSE" = "#38BDF8", "TRUE" = "#EF4444"), name = "Hub Status", labels = c("Peripheral Node", "Hub Gene")) +
      labs(
        title = "Protein-Protein Interaction (PPI) Network Topology",
        subtitle = "Force-directed Fruchterman-Reingold layout; node area scales with degree centrality",
        caption = "Source: Simulated interactome network edge list"
      ) +
      theme_void() +
      theme(
        plot.title = element_text(face = "bold", size = 13, color = "#0F172A", margin = margin(b = 4)),
        plot.subtitle = element_text(size = 10, color = "#475569", margin = margin(b = 10)),
        plot.caption = element_text(size = 8, color = "#94A3B8", hjust = 0),
        legend.position = "right",
        plot.margin = margin(12, 14, 12, 12)
      )
    return(p)
  } else {
    plot.new()
    text(0.5, 0.5, "Requires 'igraph' package")
  }
}

explain <- function() {
  list(
    title = "Biological Interaction Network Graph",
    how_to_read = "Nodes represent biological entities (proteins, genes, metabolites), and edges indicate physical binding interactions, co-expression, or regulatory connections. Node size scales with degree connectivity, and prominent red nodes highlight critical network hubs.",
    when_to_use = "Essential for systems biology, interactome mapping, disease module discovery, and drug-target interaction screens.",
    pitfalls = "Force-directed 2D layouts can introduce artificial clustering or hairballs in dense graphs. Avoid over-interpreting spatial coordinates; focus on degree distribution, modularity, and shortest-path centrality.",
    journal_archetype = "Nature Reviews Genetics and Cell Systems interactome and hub-gene figures.",
    data_recipe = "Requires edge list with 'from', 'to', and optional interaction confidence weights."
  )
}
