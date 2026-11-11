# Publication-Grade Multi-Panel Composite Figure (Nature Pangenome Standard)
# Tier 4 (Mind-Blowing) - Composite

make_plot <- function(data_dir = "data") {
  library(ggplot2)
  if (file.exists("theme_journal.R")) source("theme_journal.R", local = TRUE)
  
  # -------------------------------------------------------------
  # Panel A: Population Ancestry Structure (PCA + Demographic Grouping)
  # -------------------------------------------------------------
  set.seed(42)
  n_samples <- 470
  # Simulate 1KG / HPRC continental ancestry clusters
  pop_groups <- c("African (AFR)", "American (AMR)", "East Asian (EAS)", "European (EUR)", "South Asian (SAS)")
  pop_cols <- c(
    "African (AFR)"    = "#D97706",
    "American (AMR)"   = "#7C3AED",
    "East Asian (EAS)" = "#0284C7",
    "European (EUR)"   = "#059669",
    "South Asian (SAS)" = "#DC2626"
  )
  
  df_pca <- data.frame(
    PC1 = c(rnorm(180, -0.06, 0.02), rnorm(100, 0.04, 0.02), rnorm(80, 0.08, 0.015), rnorm(60, 0.02, 0.015), rnorm(50, 0.05, 0.018)),
    PC2 = c(rnorm(180, -0.04, 0.03), rnorm(100, 0.05, 0.02), rnorm(80, 0.07, 0.02), rnorm(60, -0.05, 0.02), rnorm(50, -0.02, 0.02)),
    Ancestry = factor(rep(pop_groups, c(180, 100, 80, 60, 50)), levels = pop_groups),
    is_HPRC = sample(c(TRUE, FALSE), n_samples, replace = TRUE, prob = c(0.15, 0.85))
  )
  
  p_a <- ggplot(df_pca, aes(x = PC1, y = PC2, color = Ancestry)) +
    geom_point(aes(shape = is_HPRC, size = is_HPRC), alpha = 0.78) +
    scale_shape_manual(values = c("FALSE" = 16, "TRUE" = 17), labels = c("1KG Cohort", "HPRC Assembly"), name = "Sample:") +
    scale_size_manual(values = c("FALSE" = 1.6, "TRUE" = 2.8), labels = c("1KG Cohort", "HPRC Assembly"), name = "Sample:") +
    scale_color_manual(values = pop_cols, name = "Continental Ancestry:") +
    labs(
      title = "Pangenome Cohort Ancestry Structure",
      subtitle = "PCA of 1KG samples highlighting diverse diploid assembly donors",
      x = "Principal Component 1 (18.4%)",
      y = "Principal Component 2 (11.2%)"
    ) +
    theme_classic(base_size = 9) +
    theme(
      plot.title = element_text(face = "bold", size = 10, color = "#0F172A"),
      plot.subtitle = element_text(size = 8, color = "#475569"),
      legend.position = "right",
      legend.key.size = unit(0.35, "cm"),
      legend.title = element_text(face = "bold", size = 7.5),
      legend.text = element_text(size = 7.0),
      axis.title = element_text(size = 8, color = "#1E293B"),
      axis.text = element_text(size = 7.5)
    )
  
  # -------------------------------------------------------------
  # Panel B: Assembly Contiguity NGx Curves (Benchmark Comparison)
  # -------------------------------------------------------------
  x_cov <- seq(0, 3.1, length.out = 100) # Cumulative coverage in Gb
  
  # Simulate NGx step curves
  # T2T-CHM13 benchmark (near complete telomere-to-telomere)
  y_chm13 <- pmax(0, 240 * (1 - (x_cov / 3.05)^0.35))
  # GRCh38 reference (fragmented with N-gaps)
  y_grch38 <- pmax(0, 145 * (1 - (x_cov / 2.95)^0.55))
  # 47 HPRC PacBio HiFi + Pore-C Assemblies (Range)
  y_hprc_mean <- pmax(0, 185 * (1 - (x_cov / 3.02)^0.45))
  y_hprc_lower <- pmax(0, 155 * (1 - (x_cov / 2.98)^0.50))
  y_hprc_upper <- pmax(0, 215 * (1 - (x_cov / 3.04)^0.40))
  
  df_ngx <- data.frame(
    cov = x_cov,
    CHM13 = y_chm13,
    GRCh38 = y_grch38,
    HPRC_Mean = y_hprc_mean,
    HPRC_Min = y_hprc_lower,
    HPRC_Max = y_hprc_upper
  )
  
  p_b <- ggplot(df_ngx, aes(x = cov)) +
    geom_ribbon(aes(ymin = HPRC_Min, ymax = HPRC_Max), fill = "#93C5FD", alpha = 0.35) +
    geom_line(aes(y = HPRC_Mean, color = "HPRC 47 Assemblies (Mean)"), linewidth = 0.9) +
    geom_line(aes(y = CHM13, color = "T2T-CHM13 v2.0 Benchmark"), linewidth = 0.9, linetype = "dashed") +
    geom_line(aes(y = GRCh38, color = "GRCh38 Reference (N-Split)"), linewidth = 0.85, linetype = "dotted") +
    geom_hline(yintercept = 40, linetype = "longdash", color = "#64748B", linewidth = 0.5) +
    annotate("text", x = 2.4, y = 48, label = "Average NG50: 40 Mb", size = 2.6, fontface = "bold", color = "#334155") +
    scale_color_manual(
      values = c(
        "HPRC 47 Assemblies (Mean)"  = "#1D4ED8",
        "T2T-CHM13 v2.0 Benchmark"    = "#0F172A",
        "GRCh38 Reference (N-Split)" = "#DC2626"
      ),
      name = "Assembly Standard:"
    ) +
    scale_x_continuous(breaks = seq(0, 3, by = 0.5), expand = c(0, 0)) +
    scale_y_continuous(breaks = seq(0, 250, by = 50), expand = c(0, 0)) +
    labs(
      title = "Assembly Contiguity NGx Curves",
      subtitle = "Cumulative sequence coverage vs contig length across diploid assemblies",
      x = "Cumulative Genome Coverage (Gb)",
      y = "Contig Length NG(x) [Mb]"
    ) +
    theme_classic(base_size = 9) +
    theme(
      plot.title = element_text(face = "bold", size = 10, color = "#0F172A"),
      plot.subtitle = element_text(size = 8, color = "#475569"),
      legend.position = "right",
      legend.key.size = unit(0.35, "cm"),
      legend.title = element_text(face = "bold", size = 7.5),
      legend.text = element_text(size = 7.0),
      axis.title = element_text(size = 8, color = "#1E293B"),
      axis.text = element_text(size = 7.5)
    )
  
  # -------------------------------------------------------------
  # Panel C: Phasing Accuracy (Switch vs Hamming Error Rate)
  # -------------------------------------------------------------
  set.seed(88)
  n_phased <- 94 # 47 Maternal + 47 Paternal
  switch_err <- c(rbeta(47, shape1 = 2, shape2 = 4) * 1.5, rbeta(47, shape1 = 2.2, shape2 = 4.2) * 1.6)
  hamming_err <- switch_err * 0.9 + rnorm(94, 0.4, 0.25)
  haplotype <- rep(c("Maternal Haplotype", "Paternal Haplotype"), each = 47)
  
  df_phasing <- data.frame(
    switch = switch_err,
    hamming = pmax(0.05, hamming_err),
    haplotype = haplotype
  )
  
  p_c <- ggplot(df_phasing, aes(x = switch, y = hamming, color = haplotype, fill = haplotype)) +
    geom_point(size = 2.2, alpha = 0.85, shape = 21, stroke = 0.5) +
    scale_color_manual(values = c("Maternal Haplotype" = "#E11D48", "Paternal Haplotype" = "#2563EB"), name = "Phased Lineage:") +
    scale_fill_manual(values = c("Maternal Haplotype" = "#FDA4AF", "Paternal Haplotype" = "#93C5FD"), name = "Phased Lineage:") +
    geom_smooth(method = "lm", se = FALSE, linewidth = 0.65, linetype = "dashed") +
    annotate("label", x = 1.1, y = 2.2, label = "Trio Phasing Accuracy > 99.2%", 
             fill = "#F0FDF4", color = "#166534", size = 2.6, fontface = "bold") +
    labs(
      title = "Haplotype Phasing Accuracy",
      subtitle = "Switch error vs Hamming error rate across 47 diploid assemblies",
      x = "Switch Error Rate (%)",
      y = "Hamming Error Rate (%)"
    ) +
    theme_classic(base_size = 9) +
    theme(
      plot.title = element_text(face = "bold", size = 10, color = "#0F172A"),
      plot.subtitle = element_text(size = 8, color = "#475569"),
      legend.position = "right",
      legend.key.size = unit(0.35, "cm"),
      legend.title = element_text(face = "bold", size = 7.5),
      legend.text = element_text(size = 7.0),
      axis.title = element_text(size = 8, color = "#1E293B"),
      axis.text = element_text(size = 7.5)
    )
  
  # -------------------------------------------------------------
  # Panel D: Total Assembled Sequence Length & Quality (Haplotype Boxplot)
  # -------------------------------------------------------------
  set.seed(99)
  n_per_arm <- 47
  hap_categories <- c("Maternal Female", "Maternal Male", "Paternal Female", "Paternal Male")
  lens <- c(
    rnorm(n_per_arm, 3.038, 0.012),
    rnorm(n_per_arm, 3.031, 0.011),
    rnorm(n_per_arm, 3.042, 0.013),
    rnorm(n_per_arm, 2.932, 0.018) # Paternal male without X chromosome
  )
  
  df_len <- data.frame(
    category = factor(rep(hap_categories, each = n_per_arm), levels = hap_categories),
    length_gb = lens
  )
  
  p_d <- ggplot(df_len, aes(x = category, y = length_gb, fill = category)) +
    geom_boxplot(width = 0.45, outlier.shape = NA, alpha = 0.85, linewidth = 0.4, color = "#0F172A") +
    geom_jitter(width = 0.15, size = 1.2, alpha = 0.55, shape = 21, fill = "#1E293B", color = "#FFFFFF", stroke = 0.3) +
    geom_hline(yintercept = 3.05, linetype = "dashed", color = "#15803D", linewidth = 0.5) +
    annotate("text", x = 1.3, y = 3.062, label = "T2T Expected (3.05 Gb)", size = 2.6, fontface = "bold", color = "#15803D") +
    scale_fill_manual(
      values = c(
        "Maternal Female" = "#991B1B",
        "Maternal Male"   = "#DC2626",
        "Paternal Female" = "#1E40AF",
        "Paternal Male"   = "#3B82F6"
      ),
      guide = "none"
    ) +
    scale_y_continuous(breaks = seq(2.90, 3.08, by = 0.04), limits = c(2.89, 3.08)) +
    labs(
      title = "Total Sequence Length per Phased Haplotype",
      subtitle = "Disomic assembly completeness; paternal male reflects hemizygous Y (2.93 Gb)",
      x = "Haplotype & Donor Sex",
      y = "Total Sequence (x 10^9 bp)"
    ) +
    theme_classic(base_size = 9) +
    theme(
      plot.title = element_text(face = "bold", size = 10, color = "#0F172A"),
      plot.subtitle = element_text(size = 8, color = "#475569"),
      axis.title = element_text(size = 8, color = "#1E293B"),
      axis.text = element_text(size = 7.5),
      axis.text.x = element_text(angle = 15, hjust = 1)
    )
  
  # Combine into a Nature-standard 2x2 multi-panel layout
  if (requireNamespace("patchwork", quietly = TRUE)) {
    composite <- (p_a + p_b) / (p_c + p_d) +
      patchwork::plot_annotation(
        title = "Figure 1 | Presenting 47 Accurate & Diverse Diploid Human Pangenome Assemblies",
        subtitle = "Multi-panel genomic milestone integrating cohort ancestry (a), contiguity NGx (b), phasing accuracy (c), and haplotype lengths (d)",
        caption = "Nature 2023 Human Pangenome Reference Consortium standard | Tagged with bold lowercase panel labels a-d",
        tag_levels = "a"
      ) &
      theme(
        plot.tag = element_text(face = "bold", size = 13, color = "#0F172A"),
        plot.title = element_text(face = "bold", size = 12.5, color = "#0F172A", margin = margin(b = 3)),
        plot.subtitle = element_text(size = 9.2, color = "#475569", margin = margin(b = 8)),
        plot.caption = element_text(size = 8, color = "#94A3B8", hjust = 0)
      )
    return(composite)
  } else {
    p_a
  }
}

explain <- function() {
  list(
    title = "Nature Multi-Panel Pangenome Landmark Figure (Panels a–d)",
    how_to_read = "Synthesizes a flagship genomic study into four coordinated panels: (a) 1000 Genomes cohort PCA highlighting diverse assembly donors; (b) Cumulative coverage vs contig length NGx curves benchmarked against T2T-CHM13 and GRCh38; (c) Switch vs Hamming error rates evaluating trio phasing fidelity; (d) Total sequence length per haploid assembly confirming completeness.",
    when_to_use = "The international pinnacle of scientific publishing: the main-text Figure 1 of landmark genomic consortia (Human Pangenome Reference Consortium, Vertebrate Genomes Project, Telomere-to-Telomere Consortium).",
    pitfalls = "Ensure identical typography across panels, unified color semantics for donor populations and haplotypes, clear uppercase/lowercase panel tags, and benchmark reference baselines.",
    journal_archetype = "Nature 2023 Human Pangenome Reference Consortium (Nature 617, 312–324).",
    data_recipe = "Combines population PCA coordinates, assembly contig length vectors, trio phasing error logs, and chromosome assembly summary tables."
  )
}
