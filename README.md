<!-- badges: start -->
[![Codecov test coverage](https://codecov.io/gh/Food-Systems-Research-Institute/SMdocs/graph/badge.svg)](https://app.codecov.io/gh/Food-Systems-Research-Institute/SMdocs)
<!-- badges: end -->

# Sustainability Metrics Docs

## About

The Sustainability Metrics project is a collaborative effort to measure food system sustainability in New England led by the University of Vermont (UVM) Food Systems Research Institute (FSRI). We work with five teams of investigators conducting primary research in vital aspects of local and regional food systems. To support this work, we are also exploring secondary data sources. 

This repository houses a Quarto project which describes the framework and contains information on the metrics as well as preliminary exploratory analyses. You can visit the [Quarto website here](https://fsrc.w3.uvm.edu/sustainability_metrics/pages/index.html). Data for this repository are pulled from the [SMdata](https://www.github.com/Food-Systems-Research-Institute/SMdata) package. 


## Project Structure

- `pages/`: Quarto `.qmd` files that make up the website, one per page, including graphs and analyses.
- `outputs/`: Graphs and tables produced in `pages` are saved here.
- `_assets/`: Contains images, CSS, and `.bib` files used in the project.
- `_preso/`: Reveal.js presentations building on SMdocs analyses
- `_site/`: Rendered site as HTML
- `R/`: Functions used in the package.
- `man/`: Documentation of functions and datasets used in the package.
- `tests/`: Unit testing of package functions.
- `dev/`: Functions and scripts to support analyses. Bit of a mess and needs some pruning.


### Data Inventory Manuscript

- Wrangling and analysis scripts located in `pages/` directory as `dp_*`.
- Table processing scripts also found in `dev/`:
  - `dev/excel_processing.R` takes Excel with the framework of dimensions, indices, indicators, and metrics for the manuscript and creates datasets used in scripts. It also processes the literature justifications for indicators.
  - `dev/giant_tables.R` makes two upsettingly large tables for the manuscript - one with summary stats and sources, the other with supplementary material including literature justifications. 
