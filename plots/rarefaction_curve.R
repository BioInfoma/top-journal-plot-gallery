# Ecological / Microbiome Rarefaction Curves
# Tier 2 (Intermediate) - Ecology

make_plot <- function(data_dir = "data") {
  library(ggplot2)
  if (file.exists("theme_journal.R")) source("theme_journal.R", local = TRUE)
  
  abund_raw <- read.csv(file.path(data_dir, "abund.csv"), row.names = 1, check.names = FALSE)
  meta <- read.csv(file.path(data_dir, "abund_meta.csv"))
  rownames(meta) <- meta$sample
  
  # Compute analytical rarefaction curves across sampling depth
  # Using vegan if available, or exact hypergeometric sampling formula
  sample_names <- head(colnames(abund_raw), 16)
  depths <- seq(50, 1500, by = 100)
  
  rarefy_curve <- function(counts, sub_n) {
    total_n <- sum(counts)
    if (total_n < sub_n) return(NA)
    # Expected species = sum(1 - choose(total_n - counts, sub_n) / choose(total_n, sub_n))
    s_obs <- sum(sapply(counts[counts > 0], function(c_i) {
      if (total_n - c_i < sub_n) 1
      else 1 - exp(lchoose(total_n - c_i, sub_n) - lchoose(total_n, sub_n))
    }))
    s_obs
  }
  
  curve_list <- list()
  for (smp in sample_names) {
    col_counts <- abund_raw[, smp]
    grp <- meta[smp, "group"]
    for (d in depths) {
      richness <- rarefy_curve(col_counts, d)
      if (!is.na(richness)) {
        curve_list[[length(curve_list) + 1]] <- data.frame(
          sample = smp,
          group = grp,
          depth = d,
          richness = richness
        )
      }
    }
  }
  df_rare <- do.call(rbind, curve_list)
  
  p <- ggplot(df_rare, aes(x = depth, y = richness, group = sample, color = group)) +
    geom_line(linewidth = 0.85, alpha = 0.75) +
    scale_color_manual(values = c("Healthy" = "#00A087", "Disease" = "#E64B35"), name = "Cohort") +
    labs(
      title = "Microbiome Rarefaction Curves: Alpha Diversity Saturation",
      subtitle = "Observed taxonomic richness as a function of sequencing depth per biological specimen",
      x = "Sequencing Depth (Subsampled Reads / Library Size)",
      y = "Observed Taxonomic Richness (ASVs / Taxa)",
      caption = "Source: Simulated 16S rRNA amplicon sequencing survey (40 samples x 30 taxa)"
    )
  
  if (exists("theme_topjournal")) {
    p <- p + theme_topjournal(grid = "xy")
  } else {
    p <- p + theme_classic()
  }
  p
}

explain <- function() {
  list(
    title = "Microbiome Rarefaction Curve",
    how_to_read = "Each curve traces observed species or taxonomic richness as sequencing depth (subsampled read counts) increases. Curves that plateau to a horizontal asymptote demonstrate that the microbial community was sampled to saturation.",
    when_to_use = "Standard quality control in metagenomics, 16S microbiome profiling, and macro-ecology to account for library size differences before comparing alpha diversity.",
    pitfalls = "Never compare raw unrarefied richness counts between samples with unequal sequencing depths, which creates false differences driven purely by library yield.",
    journal_archetype = "ISME Journal and Microbiome alpha-diversity benchmarking.",
    data_recipe = "Requires OTU/ASV abundance matrix (taxa × samples) and sample metadata."
  )
}
