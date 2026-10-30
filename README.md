# Top-Journal Plot Gallery

[![R Version](https://img.shields.io/badge/R-4.5%2B-blue.svg)](https://www.r-project.org/)
[![Shiny](https://img.shields.io/badge/Shiny-1.12%2B-brightgreen.svg)](https://shiny.posit.co/)
[![bslib](https://img.shields.io/badge/Bootstrap%205-bslib-blueviolet.svg)](https://rstudio.github.io/bslib/)
[![Plots](https://img.shields.io/badge/Catalog-100%20Plots%20Complete-success.svg)](#catalog-tiers)

An interactive, searchable visual encyclopedia of **100 publication-grade scientific and statistical visualizations** commonly featured in top-tier journals (*Nature*, *Science*, *Cell*, *The Lancet*).

Built in **R / Shiny** with a modern **Bootstrap 5 (`bslib`)** architecture, this reference platform enables researchers, data scientists, and computational biologists to look up plot archetypes, inspect self-contained reproducible R code, read plain-language statistical guides and pitfalls, preview underlying datasets, and download publication-ready 300 DPI figures with one click.

---

## 🌟 Key Features

- **100 Curated Visualizations**: Spanning 4 visual sophistication tiers across 38 scientific domains (genomics, clinical trials, single-cell, machine learning, physics, astronomy, and ecology).
- **Publication-Ready Aesthetic**: Unified `theme_topjournal()` design system enforcing *Nature* / *Science* graphic standards, high data-to-ink ratio, and Nature Publishing Group (NPG) color palettes.
- **Immediate-Render Figure Canvas**: Spacious, responsive 540px viewstage that renders instantly on page load with top-aligned horizontal legends.
- **Deep Educational Guide**:
  - 🔍 **How to Read**: Clear deconstruction of visual encodings, coordinate mappings, and intervals.
  - 🎯 **When to Use**: Specific biological, clinical, and physical experimental contexts.
  - ⚠️ **Pitfalls & Traps**: Common statistical traps, unadjusted p-values, overplotting, and normalization artifacts.
  - 🏛️ **Journal Archetype**: Real-world literature benchmarks from landmark publications.
- **1-Click Reproducibility**:
  - 💻 **Copy Code**: One-click clipboard copy of clean, self-contained R scripts.
  - 📷 **High-Res Export**: 300 DPI PNG and vector PDF download buttons.
  - 📊 **Dataset Download**: One-click download of the sample CSV dataset used in each figure.
- **Navigation Controls**: Real-time keyword search, Tier filter, and `❮ Previous` / `Next ❯` / `🎲 Random` browsing controls.

---

## 📊 Catalog Tiers

| Tier | Complexity | Plots | Canonical Examples |
| :--- | :--- | :--- | :--- |
| **Tier 1: Simple** | Foundational | 20 / 20 | Scatter with regression, Tukey Boxplot with raw jitter, Violin with embedded IQR box, Kaplan-Meier with right-censoring, 4PL Dose-Response curve, Bland-Altman method agreement. |
| **Tier 2: Intermediate** | Specialized | 25 / 25 | Volcano plot with FDR thresholds, GWAS Manhattan & Q-Q plots ($\lambda_{\text{GC}}$), PCA biplot, UMAP & t-SNE embeddings, Bi-clustered Heatmap with dendrograms, ROC & PR curves, Clinical RECIST Waterfall & Swimmer plots, UpSet plot, Sequence Logo. |
| **Tier 3: Advanced** | High-Dimensional | 30 / 30 | 100% Stacked Taxonomic Abundance, Multi-Stage Alluvial Flow, Circular & Time-Scaled MCC Phylogeny with 95% HPD node bars, Hi-C Chromatin contact matrix, Multi-Omic Genome Browser tracks, Oncoplot, RNA-seq Sashimi plot, Single-cell Pseudotime & RNA velocity streamlines, SHAP beeswarm explainability, Spatial Transcriptomics tissue maps. |
| **Tier 4: Mind-Blowing** | Conceptual / 3D | 25 / 25 | Ramachandran plot, Cryo-EM 2.4 Å density map with FSC 0.143 curve, Macromolecular Free-Energy Landscape (PMF), Deep Mutational Scanning (DMS) heatmap, Wrightian Fitness Landscape, Viral Antigenic Cartography, Pangenome Variation Graph, Waddington Epigenetic Landscape, Fractal Tree of Life (OneZoom), Connectome Tractography, Lorenz Strange Attractor, Deep Learning 3D Loss Landscape & Transformer Attention Saliency, CMB Angular Power Spectrum, LIGO Gravitational Wave Chirp Spectrogram, Cosmic Web N-body simulation. |

---

## 🚀 Running Locally

### 1. Prerequisites
Ensure you have **R (>= 4.2)** installed on your machine.

### 2. Clone the Repository
```bash
git clone https://github.com/Bioinfoma/plot_gallery_pack.git
cd plot_gallery_pack
```

### 3. Generate Sample Data
Run the generator script to create the synthetic datasets in `data/`:
```r
Rscript make_dummy_data.R
```

### 4. Launch the Shiny App
In R or your terminal:
```r
shiny::runApp(port = 3838, launch.browser = TRUE)
```

---

## ☁️ Deploying to shinyapps.io

To host this gallery on your free or paid [shinyapps.io](https://www.shinyapps.io/) account:

1. Install `rsconnect` in R:
   ```r
   install.packages("rsconnect")
   ```
2. Configure your shinyapps.io account token (from your shinyapps.io dashboard under **Account → Tokens**):
   ```r
   rsconnect::setAccountInfo(
     name = "<YOUR_ACCOUNT_NAME>",
     token = "<YOUR_TOKEN>",
     secret = "<YOUR_SECRET>"
   )
   ```
3. Deploy the application:
   ```r
   rsconnect::deployApp(appName = "top-journal-plot-gallery")
   ```

---

## 📁 Repository Structure

```
plot_gallery_pack/
├── app.R                  # Main Shiny application (UI & Server)
├── plot_catalog.csv       # Master catalog database of all 100 plots
├── theme_journal.R        # Publication theme (theme_topjournal) & NPG color palettes
├── make_dummy_data.R      # Generates synthetic datasets into data/
├── data/                  # 15 canonical datasets (xy, ts, grp, de, gwas, mat, surv...)
├── plots/                 # 100 self-contained, reproducible plot scripts (*.R)
│   ├── scatter_plot.R
│   ├── volcano_plot.R
│   ├── sankey_alluvial_diagram.R
│   ├── ramachandran_plot.R
│   └── ... (100 plot modules)
└── www/                   # Web assets & styling
```

---

## 📜 License
MIT License. Created for the scientific and open-source data visualization community.
