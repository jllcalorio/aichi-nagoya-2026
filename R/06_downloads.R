# Step 6: Excel workbook with the scraped data and every result table, one table per
# worksheet -> output/downloads/ag2026_data_and_tables.xlsx
# (The 600 DPI figure zip is written by R/05_figures.R.)

source("R/00_helpers.R")
source("R/report_tables.R")
suppressPackageStartupMessages(library(openxlsx))
dir.create("output/downloads", recursive = TRUE, showWarnings = FALSE)

d  <- readRDS("data/ag2026.rds")
m  <- readRDS("output/metrics.rds")
cn <- m$country
cov <- read_csv("data/country_covariates.csv", show_col_types = FALSE)

# snake_case -> readable column names (no underscores)
nice_names <- function(df) {
  map <- c(h2h = "head-to-head", afb = "average field beaten", irm = "status (DNF/DSQ/DNS)",
           noc = "country", gdp = "GDP", usd = "US$", pc = "per person", pop = "population",
           adj = "adjusted", pts = "points", pos = "position", phi = "Philippines", iso3 = "ISO3",
           sea = "Southeast Asia", perf = "performance", vs = "vs.", w = "wins", l = "losses",
           "10m" = "10 million", "100b" = "100 billion", ath = "athletes")
  nm <- vapply(strsplit(names(df), "_"), function(parts) {
    parts <- ifelse(tolower(parts) %in% names(map), map[tolower(parts)], parts)
    s <- paste(parts, collapse = " ")
    paste0(toupper(substr(s, 1, 1)), substr(s, 2, nchar(s)))
  }, character(1))
  names(df) <- make.unique(nm, sep = " ")
  df
}
prep <- function(df) {
  df <- as.data.frame(df)
  for (c in names(df)) if (inherits(df[[c]], "POSIXct")) df[[c]] <- format(df[[c]], "%Y-%m-%d %H:%M", tz = "Asia/Tokyo")
  df
}

sheets <- list(
  list("Results (scraped)", nice_names(d$results), "One row per competitor per competition unit (match, heat, round). Start time is Japan time."),
  list("Final rankings", nice_names(d$final_rankings), "One row per competitor per event: final place, result and medal."),
  list("Medal table", nice_names(d$medal_table), "Official medal standings."),
  list("Athletes by sport", nice_names(d$entries), "Athletes entered by each country in each sport."),
  list("Schedule units", nice_names(d$units), "Every competition unit on the schedule, with venue and status."),
  list("Country covariates", nice_names(cov), "Population and GDP (World Bank; Chinese Taipei from official statistics)."),
  list("Table 1 PHI summary", tbl_phi_summary(cn), "Report Table 1: Philippine headline measures and rank."),
  list("Table 2 Comparable ranking", tbl_ranking(cn), "Report Table 2: every eligible country on the comparable metrics."),
  list("Table 3 PHI medals", tbl_phi_medals(m), "Report Table 3: every Philippine medal."),
  list("Table 4 PHI near misses", tbl_phi_near(m), "Report Table 4: Philippine 4th and 5th places."),
  list("Country metrics (all)", nice_names(cn), "Every metric for every country (data behind Figures 1, 3, 4, 5 and 8)."),
  list("Index sensitivity", nice_names(m$sensitivity), "Index and rank under four index versions (data behind Figure 2)."),
  list("PHI all entries", nice_names(m$phi_entries), "Every Philippine event entry with final place and share of field beaten."),
  list("PHI by sport", nice_names(m$phi_by_sport), "Philippine results summarised by sport (data behind Figure 6)."),
  list("PHI H2H by opponent", nice_names(m$phi_h2h_by_opp), "Philippine head-to-head record by opponent (data behind Figure 7)."),
  list("Model coefficients", tbl_models(m), "Quasi-Poisson models of medal points used for the expected-medal-points measures.")
)

wb <- createWorkbook()
hs <- createStyle(textDecoration = "bold", fgFill = "#e8eef7", border = "bottom", wrapText = TRUE)
addWorksheet(wb, "About")
about <- data.frame(Worksheet = vapply(sheets, `[[`, "", 1),
                    Rows = vapply(sheets, \(s) nrow(s[[2]]), 0L),
                    Contents = vapply(sheets, `[[`, "", 3))
writeData(wb, "About", "Aichi-Nagoya 2026 (20th Asian Games): scraped results and analysis tables", startRow = 1)
writeData(wb, "About", paste("Source: results.asiangames2026.org, scraped", format(Sys.Date(), "%d %B %Y")), startRow = 2)
writeData(wb, "About", about, startRow = 4, headerStyle = hs)
addStyle(wb, "About", createStyle(textDecoration = "bold", fontSize = 13), rows = 1, cols = 1)
setColWidths(wb, "About", 1:3, c(28, 8, 95))

for (s in sheets) {
  addWorksheet(wb, s[[1]])
  df <- prep(s[[2]])
  writeData(wb, s[[1]], df, headerStyle = hs, withFilter = TRUE)
  freezePane(wb, s[[1]], firstRow = TRUE)
  setColWidths(wb, s[[1]], seq_along(df), pmin(45, pmax(10, nchar(names(df)) + 2)))
}
xlsx_path <- "output/downloads/ag2026_data_and_tables.xlsx"
saveWorkbook(wb, xlsx_path, overwrite = TRUE)
message("Excel workbook: ", xlsx_path, " (", round(file.size(xlsx_path) / 1e6, 1), " MB)")
