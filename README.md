<!-- badges: start -->
[![Codecov test coverage](https://codecov.io/gh/Food-Systems-Research-Center/sm-docs/graph/badge.svg)](https://app.codecov.io/gh/Food-Systems-Research-Center/sm-docs)
<!-- badges: end -->

# Sustainability Metrics Docs

## About

> [!NOTE]
> This project is development. The framework, metrics, and analyses are all preliminary.
 
The Sustainability Metrics project is a collaborative effort to measure food system sustainability in New England led by the University of Vermont (UVM) UVM Food Systems Research Center (FSRC). We work with five teams of investigators conducting primary research in vital aspects of local and regional food systems. To support this work, we are also exploring secondary data sources. 

This repository houses a Quarto project which describes the framework and contains information on the metrics as well as preliminary exploratory analyses. You can visit the [Quarto website here](https://fsrc.w3.uvm.edu/sustainability_metrics/pages/index.html).

## Project Structure

- `data`: Clean datasets imported from [SMdata](https://www.github.com/food-systems-research-center/sm-data), including:
  - `sm_data.rds`: metrics, metadata, fips keys, and more as a list R object
  - `sm_spatial.rds`: polygons for Northeast states, counties, and more as a list R object
  - `metrics_and_metadata.zip`: Zip file containing three `.csv`s containing metrics, metadata, and a fips key. Thrown in here to make it easier to pull data from Python directly. Rather than handing off from R.
- `pages`: Quarto `.qmd` files that make up the website, including graphs and analyses. Quarto also plays nicely with `.ipynb` files.
- `images`: Figures and photos used in Quarto doc.
- `dev`: Functions and scripts to support analyses. Bit of a mess and needs some pruning.
- `temp`: Various temporary scripts for testing.     
