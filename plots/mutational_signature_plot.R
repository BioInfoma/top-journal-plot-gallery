# COSMIC 96-Channel Single Base Substitution (SBS) Mutational Signature Profile
# Tier 3 (Advanced) - Genomics

make_plot <- function(data_dir = "data") {
  library(ggplot2)
  if (file.exists("theme_journal.R")) source("theme_journal.R", local = TRUE)
  
  # 6 substitution classes
  subs <- c("C>A", "C>G", "C>T", "T>A", "T>C", "T>G")
  bases <- c("A", "C", "G", "T")
  
  # 16 trinucleotide flanking contexts per substitution = 96 channels
  contexts <- expand.grid(upstream = bases, downstream = bases)
  contexts$label <- paste0(contexts$upstream, "·", contexts$downstream)
  
  sbs_rows <- list()
  set.seed(42)
  for (s in subs) {
    # SBS1 / Aging signature archetype (strong C>T at CpG sites: NCG contexts)
    # High peak at A[C>T]G, C[C>T]G, G[C>T]G, T[C>T]G
    sub_weights <- runif(16, 0.005, 0.02)
    if (s == "C>T") {
      # CpG context indices
      cpg_idx <- which(contexts$downstream == "G")
      sub_weights[cpg_idx] <- runif(length(cpg_idx), 0.12, 0.22)
    }
    sub_weights <- sub_weights / sum(sub_weights) * ifelse(s == "C>T", 0.48, 0.10)
    
    for (k in 1:16) {
      sbs_rows[[length(sbs_rows) + 1]] <- data.frame(
        substitution = s,
        context = contexts$label[k],
        rate = sub_weights[k]
      )
    }
  }
  df_sbs <- do.call(rbind, sbs_rows)
  df_sbs$substitution <- factor(df_sbs$substitution, levels = subs)
  
  sub_colors <- c(
    "C>A" = "#06B6D4", # Cyan
    "C>G" = "#0F172A", # Dark Slate
    "C>T" = "#DC2626", # Red (Deamination signature peak)
    "T>A" = "#CBD5E1", # Light Gray
    "T>C" = "#16A34A", # Green
    "T>G" = "#F97316"  # Orange
  )
  
  p <- ggplot(df_sbs, aes(x = context, y = rate, fill = substitution)) +
    geom_col(width = 0.72) +
    facet_grid(~ substitution, scales = "free_x") +
    scale_fill_manual(values = sub_colors) +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1), expand = expansion(mult = c(0, 0.15))) +
    labs(
      title = "COSMIC Mutational Signature SBS1: Spontaneous 5-Methylcytosine Deamination",
      subtitle = "96-channel trinucleotide mutation spectrum; prominent C>T spikes at NpCpG contexts indicate biological aging clock",
      x = "Trinucleotide Sequence Context (5' Flank · 3' Flank)",
      y = "Mutation Contribution Ratio (%)",
      caption = "Source: Simulated COSMIC v3 SBS1 canonical mutational signature spectrum (Alexandrov standard)"
    )
  
  if (exists("theme_topjournal")) {
    p <- p + theme_topjournal(grid = "y") +
      theme(
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, size = 6.0, family = "mono"),
        axis.text.y = element_text(size = rel(0.85)),
        strip.text = element_text(face = "bold", size = rel(0.95), color = "#FFFFFF"),
        strip.background = element_rect(fill = "#1E293B", color = NA),
        legend.position = "none"
      )
  } else {
    p <- p + theme_classic()
  }
  p
}

explain <- function() {
  list(
    title = "COSMIC 96-Channel Mutational Signature Profile",
    how_to_read = "Mutations are classified into 6 single-base substitution types (C>A, C>G, C>T, T>A, T>C, T>G) and further subdivided across 16 trinucleotide sequence contexts (flanking 5' and 3' bases), totaling 96 channels. Spikes indicate specific molecular mutational processes (e.g. C>T at CpG sites in SBS1 reflects endogenous 5-methylcytosine deamination; T>A in SBS4 reflects tobacco carcinogens).",
    when_to_use = "Universal standard in cancer genomics (Alexandrov / Stratton et al., COSMIC) to deconvolve endogenous enzymatic damage, defective DNA mismatch repair, and environmental mutagen exposure.",
    pitfalls = "Deconvoluting signatures with non-negative matrix factorization (NMF) requires sufficient mutation counts (at least ~100 SNVs per genome) to avoid overfitting artifactual signatures.",
    journal_archetype = "Nature, Science, and Cell cancer mutational signature landmark publications.",
    data_recipe = "Requires 96-channel single base substitution counts from whole-exome or whole-genome sequencing."
  )
}
