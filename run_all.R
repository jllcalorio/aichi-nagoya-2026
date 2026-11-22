# Aichi-Nagoya 2026 (20th Asian Games) — full pipeline.
# Run from the project root:  Rscript run_all.R
# Step 1 is slow on first run (~1.5 h at 1 request/second) but is cached in data/raw/,
# so re-runs only fetch what is missing.

source("R/01_scrape.R")             # raw JSON  -> data/raw/
source("R/02_country_covariates.R") # World Bank population & GDP -> data/country_covariates.csv
source("R/03_build_tables.R")       # tidy CSVs -> data/
source("R/04_metrics.R")            # metrics   -> output/*.csv
source("R/05_figures.R")            # 600 DPI charts -> output/figures/ + zip in output/downloads/
source("R/06_downloads.R")          # Excel workbook (data + all tables) -> output/downloads/
source("R/07_render_report.R")      # self-contained Quarto report -> output/ (needs the Quarto CLI)
