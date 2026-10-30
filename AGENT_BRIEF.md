# Brief: Build the "Top-Journal Plot Gallery" Shiny app

## Goal
A Shiny app that works as a searchable database of 100 plot types seen in top journals (Nature, Science, Cell, Lancet), ordered simple to advanced. Each entry shows: a live example plot, a plain-language explanation, and how it can be used.

## Resources in this pack
- `plot_catalog.csv`: the 100 plots. Columns: id, tier (1 simple ... 4 mind-blowing), name, category, what_it_shows, typical_use, r_packages, dummy_dataset, build_difficulty, slug.
- `make_dummy_data.R`: generates shared datasets into `data/` (xy, ts, grp, de, gwas, mat, surv, clf, meta, sets, net, flow, abund, seq, tree). Rows with `dummy_dataset = special` need a dataset the agent simulates inside the plot's own file.
- `plots/`: one R file per plot, named `<slug>.R`, each defining `make_plot(data_dir = "data")` that returns a ggplot/plotly/base-plot-producing function result. Three examples are included; replicate the pattern.
- `app.R`: skeleton app (filters by tier/category/search, detail panel, live example, code viewer).

## Build rules
1. Run `Rscript make_dummy_data.R` first.
2. Build plots in order of `build_difficulty` (easy, medium, hard). Ship easy ones first; mark unfinished entries "Example coming soon" rather than leaving broken.
3. Each plot file must also define `explain()` returning a list(title, how_to_read, when_to_use, pitfalls) with 2-3 short sentences each.
4. Every example must run without internet and use only dummy data. Wrap in tryCatch so one failure never breaks the app.
5. Missing optional packages: detect with `requireNamespace`, show an install hint, do not crash.
6. Show the plot's R code in a "Code" tab (read the file text).
7. For the Python-only or heavy 3D entries, use plotly or rgl-free alternatives where possible.
8. Test every plot file with `source()` in a clean session before registering it.

## Suggested app features
Tier filter, category filter, free-text search, "random plot" button, download PNG, download dummy CSV, favourites list.
