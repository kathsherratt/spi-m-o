# spi-m-o

context

- notes on evaluating COVID-19 SPI-M-O work
- background / overall idea presented [here]

all work is exploratory / experimental: not production ready

## publishing

Site publishes to GitHub Pages via `.github/workflows/publish.yml` on push to `main`.

Uses Quarto freeze (`_quarto.yml`: `execute: freeze: true`) so CI never installs R or executes code - a project render always assembles the site from cached results in `_freeze/`, no matter which page changed. This keeps runs fast (~30s vs ~4-5min with a full R install).

After changing R code or data that feeds a `.qmd` page (e.g. `notebook/pages/explore-r-gr.qmd`, `R/process-r-gr.R`):

1. Render that page directly to re-execute it and refresh its cache: `quarto render notebook/pages/explore-r-gr.qmd` (a project-level `quarto render` won't re-execute under `freeze: true`)
2. Commit the updated `_freeze/` alongside your changes
3. Push

If you forget, CI has no R to fall back on and the render step fails loud rather than publishing stale content.
