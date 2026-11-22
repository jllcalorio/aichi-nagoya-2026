# Beyond the Medal Table: Aichi-Nagoya 2026 (20th Asian Games)

A fully reproducible R pipeline that scrapes every published result from the Aichi-Nagoya 2026 Asian Games, builds tidy datasets, and compares all countries with metrics that go beyond the gold-first medal table, with a focus on the Philippines.

**Main output:** a self-contained HTML report (`output/ag2026_philippines_report.html`) with a table of contents, numbered tables and figures, callouts, and built-in download buttons for the figures (ZIP, 600 DPI) and for the data plus every table (Excel, one worksheet per table).

---

## Why this project exists

The official medal table ranks countries by gold, then silver, then bronze. That ignores team size, national resources, and results just below the podium. This project asks: *how do countries compare when those things are taken into account, and was the Philippines under-credited?*

## Key findings (Philippines)

| | |
|---|---|
| Official medals | 4 gold, 9 silver, 16 bronze (29), official rank #17 (#14 by total medals) |
| Rank on the Comparable Performance Index | #24 of 38 (range #17 to #24 across four index versions) |
| Medal points (3/2/1) vs. expectation from population and GDP per person | 46 won, about 53 expected (ratio 0.86) |
| Ratio when team size is also allowed for | 0.54 |
| Average field beaten | 47.4% |
| Head-to-head record | 174 wins, 228 losses (43.3%) |

- The data **does not support** the idea that the Philippines was broadly under-credited. It is mid-table on every version of the index.
- Gold-first ranking costs it three places (#17 by gold, #14 by total medals).
- Relative to population and wealth it did better than Indonesia, Vietnam and Singapore. Thailand and Malaysia did better.
- Strong sports: taekwondo, artistic gymnastics, pole vault, 3x3 basketball, surfing. Weak spots: swimming, shooting, fencing, wrestling, judo.

The metrics were defined before looking at Philippine numbers and were not tuned toward any result.

## Data

Scraped from the official results site, `results.asiangames2026.org` (a Vue single-page app), through the JSON API behind it. Coverage: 10 September to 4 October 2026.

| File | Rows | Description |
|---|---|---|
| `data/ag2026_results.csv` | 18,224 | One row per competitor per competition unit (match, heat, round) |
| `data/ag2026_final_rankings.csv` | 7,639 | Final place, result and medal for each entrant in all 469 events |
| `data/ag2026_medal_table.csv` | 40 | Official medal standings |
| `data/ag2026_entries.csv` | 1,148 | Athletes entered per country per sport |
| `data/country_covariates.csv` | | Population and GDP (World Bank; Chinese Taipei entered manually) |
| `data/ag2026.rds` | | All of the above as R objects |
| `output/country_metrics.csv` | 45 | Every metric for every country |
| `output/index_sensitivity.csv` | | Index and rank under four index versions |
| `output/phi_*.csv` | | Philippines-only extracts |

**Results CSV column groups** (in order): *when* (date, start time in Japan time), *what* (sport, event, gender, phase, unit, venue, medal/head-to-head flags), *who* (country, competitor, team members, bib), *opponent* (country and name, head-to-head units only), *outcome* (score, period scores, result detail, rank, win/loss, status such as DNF/DSQ, qualification, record, medal).

Medal counts reconcile with the official medal table for every country.

## Metrics

All metrics are computed identically for every country.

- **Entry**: one competitor (athlete, pair or team) in one event. "Did not start" entries are dropped.
- **Medal points**: 3 gold, 2 silver, 1 bronze.
- **Medals per entry / per 100 athletes**: conversion rates that remove team-size effects.
- **Average field beaten**: for each entry, (field size − final place) ÷ (field size − 1), averaged. A winner scores 100%, last place 0%.
- **Top-8 rate** and **near misses** (4th or 5th without a medal).
- **Head-to-head strength**: a Bradley–Terry rating fitted to every head-to-head contest, with a light ridge penalty so small samples do not give extreme values.
- **Actual ÷ expected medal points**: quasi-Poisson regression of medal points on log population, log GDP per person and a host flag, with and without log athletes sent.
- **Small-team shrinkage**: rate metrics are pulled toward the all-country average by adding 25 pseudo-entries.
- **Comparable Performance Index**: mean percentile rank of five components (field beaten, medals per entry, top-8 rate, head-to-head strength, actual ÷ expected with team size), for the 38 countries with at least 25 entries. Four index variants test sensitivity.

## Repository structure

```
run_all.R                  # runs the whole pipeline
R/
  00_helpers.R             # API access, decoding, caching, utilities
  01_scrape.R              # download raw JSON (cached in data/raw/)
  02_country_covariates.R  # World Bank population and GDP
  03_build_tables.R        # tidy tables and CSVs
  04_metrics.R             # metrics, models, index, sensitivity
  figures_lib.R            # figure definitions (shared by report and export)
  report_tables.R          # table definitions (shared by report and Excel)
  05_figures.R             # 600 DPI PNGs + ZIP
  06_downloads.R           # Excel workbook (data + all tables)
  07_render_report.R       # render the Quarto report
report/
  ag2026_philippines_report.qmd
  download_bar.html
data/    
output/
```

## Getting started

**Requirements:** R 4.x, the Quarto CLI (bundled with Positron and RStudio), and the R packages loaded in `R/00_helpers.R` and the scripts (including dplyr, ggplot2, ggrepel, gt, openxlsx, zip, base64enc).

```r
# from the project root
source("run_all.R")
# or from a shell
# Rscript run_all.R
```

The first scrape is slow (about 1.5 hours at 1 request per second, roughly 6,000 files, about 190 MB). Responses are cached in `data/raw/`, so re-runs are quick. To skip scraping, run scripts 03 to 07 on an existing cache. Consider adding `data/raw/` to `.gitignore`.

## Technical notes on the scraper

- The site returns 404 to plain requests without a browser User-Agent.
- API responses are zlib-compressed and then UTF-8-encoded as latin1 code points. `R/00_helpers.R` reverses this at the byte level.
- Requests are rate-limited, cached, and retried on 429/5xx.

## Limitations

- Head-to-head counts every match equally, from group games to finals. Rubbers inside team ties are not counted separately.
- Field beaten uses each sport's official classification; some sports give all quarter-final losers 5th place.
- DPR Korea has no World Bank GDP, so it has no actual ÷ expected value. Chinese Taipei's population and GDP are entered manually.
- Three provisional units (one equestrian dressage test, one surfing heat, one sailing race) returned no data. One medal event (men's modern pentathlon individual) is missing from the site's daily lists but is present in final rankings.
- Data reflects the site at the time of scraping.

## Data source and terms

All competition data belongs to its publishers (Aichi-Nagoya 2026 organisers / Olympic Council of Asia). This repository contains an independent analysis. Check the results site's terms of use before redistributing the raw data.