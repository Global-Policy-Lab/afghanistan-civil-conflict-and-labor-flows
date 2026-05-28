# afghanistan-civil-conflict-and-labor-flows

This repository contains code and data necessary to replicate the findings of the paper:

Xiao Hui Tai, Suraj R. Nair, Shikhar Mehra, and Joshua E. Blumenstock. Satellite and Mobile Phone Data Reveal How Violence Affects Seasonal Migration in Afghanistan.

This work is licensed under the Apache License, Version 2.0.

# Instructions
One of the main datasets for the paper contains detailed information on over 20 billion mobile phone transactions in Afghanistan. These data contain proprietary and confidential information belonging to a major telecommunications operator, which we do not have permission to release.

However, all the code required to process the data and produce the results are in the following folders:

- `displacement-metrics`: code to compute migration metrics from the raw call detail records
- `home_location_detector`: helper code (code for the `migration_detector` package (Chi et al. 2020))
- `estimation`: code to create the analysis data set and produce all results

# Full estimation pipeline

Users with access to the full CDR and satellite datasets can run the complete estimation pipeline in `code/estimation/`. Before running any script, edit the four root path variables at the top of `code/estimation/config.R` to match your environment:

```r
WORK_ROOT <- "/path/to/your/working/directory"
CDR_ROOT  <- "/path/to/cdr/data"
SAT_ROOT  <- "/path/to/satellite/data"
TMP_ROOT  <- "/path/to/scratch/directory"
```

All scripts call `source("config.R")` and derive every other path from these four variables. See `code/estimation/README.md` for the full execution order and a description of each script's inputs and outputs.

# Demo
The `demo` folder is a self-contained folder that users can run to produce all the figures and tables in the paper. Sample data sets are provided where original data cannot be released, and are indicated as such in the code.

To run the demo scripts, set your working directory to the `code/` folder and run the R script corresponding to the figure or table number:

```r
setwd("path/to/repo/code")
source("demo/fig2.R")
```

Output PDFs are written to `code/demo/output/`.

# System requirements

## R packages

The table below lists all R packages used across the demo scripts and the full estimation pipeline.

| Package | Used in | Purpose |
|---|---|---|
| `dplyr` | Demo + estimation | Data manipulation |
| `ggplot2` | Demo + estimation | Figures |
| `gridExtra` | Demo + estimation | Multi-panel figure layout |
| `scales` | Demo + estimation | Axis formatting |
| `tidyr` | Demo + estimation | Data reshaping |
| `lmtest` | Demo + estimation | Clustered standard errors |
| `sandwich` | Demo + estimation | Variance-covariance matrices |
| `stargazer` | Demo + estimation | Regression tables |
| `sf` | Demo + estimation | Spatial data |
| `viridis` | Demo + estimation | Color palettes |
| `colorspace` | Demo + estimation | Color palettes |
| `knitr` | Demo + estimation | Table formatting |
| `lubridate` | Estimation only | Date arithmetic |

Install all packages at once:

```r
install.packages(c("dplyr", "ggplot2", "gridExtra", "scales", "tidyr",
                   "lmtest", "sandwich", "stargazer", "sf", "viridis",
                   "colorspace", "knitr", "lubridate"))
```

## Python packages

The MODIS–Landsat validation notebook (`code/demo/figS3.ipynb`) requires Python 3 and a Google Earth Engine account. Install the required packages with:

```bash
pip install earthengine-api geopandas numpy pandas rasterio rioxarray shapely
```

| Package | Purpose |
|---|---|
| `earthengine-api` | Google Earth Engine Python API (`ee`) |
| `geopandas` | Spatial dataframes |
| `numpy` | Array operations |
| `pandas` | Tabular data |
| `rasterio` | GeoTIFF I/O and raster merging |
| `rioxarray` | Raster I/O via xarray (`rxr`) |
| `shapely` | Geometry objects (`Point`) |

After installing `earthengine-api`, authenticate once with:

```bash
earthengine authenticate
```

## Software and hardware

- R version 4.0 or higher (tested on R 4.5)
- Python 3.8 or higher (for the MODIS–Landsat notebook only)
- No non-standard hardware required
