# Step 2: population and GDP per NOC (World Bank WDI), for size-adjusted metrics.
# Output: data/country_covariates.csv

source("R/00_helpers.R")

noc_iso3 <- tribble(
  ~noc,  ~iso3,
  "AFG","AFG", "BRN","BHR", "BAN","BGD", "BHU","BTN", "BRU","BRN", "CAM","KHM",
  "CHN","CHN", "PRK","PRK", "HKG","HKG", "IND","IND", "INA","IDN", "IRI","IRN",
  "IRQ","IRQ", "JPN","JPN", "JOR","JOR", "KAZ","KAZ", "KOR","KOR", "KUW","KWT",
  "KGZ","KGZ", "LAO","LAO", "LBN","LBN", "MAC","MAC", "MAS","MYS", "MDV","MDV",
  "MGL","MNG", "MYA","MMR", "NEP","NPL", "OMA","OMN", "PAK","PAK", "PLE","PSE",
  "PHI","PHL", "QAT","QAT", "KSA","SAU", "SGP","SGP", "SRI","LKA", "SYR","SYR",
  "TPE",NA,    "TJK","TJK", "THA","THA", "TLS","TLS", "TKM","TKM", "UAE","ARE",
  "UZB","UZB", "VIE","VNM", "YEM","YEM"
)

wb_indicator <- function(ind) {
  isos <- paste(na.omit(noc_iso3$iso3), collapse = ";")
  url <- sprintf("https://api.worldbank.org/v2/country/%s/indicator/%s?format=json&date=2015:2025&per_page=2000",
                 isos, ind)
  x <- request(url) |> req_retry(max_tries = 4) |> req_perform() |> resp_body_json()
  map_dfr(x[[2]], \(r) tibble(iso3 = r$countryiso3code, year = as.integer(r$date),
                              value = as.numeric(r$value %||% NA)))
}
latest <- function(df, name) {
  df |> filter(!is.na(value)) |> group_by(iso3) |> slice_max(year, n = 1) |> ungroup() |>
    rename("{name}" := value, "{name}_year" := year)
}

pop <- latest(wb_indicator("SP.POP.TOTL"), "population")
gdp <- latest(wb_indicator("NY.GDP.MKTP.CD"), "gdp_usd")

cov <- noc_iso3 |>
  left_join(pop, by = "iso3") |>
  left_join(gdp, by = "iso3") |>
  mutate(source = if_else(is.na(iso3), NA_character_, "World Bank WDI"))

# Chinese Taipei is not covered by the World Bank. Manual figures:
# population ~23.4 million (Ministry of the Interior, end-2024);
# GDP ~US$ 781.6 billion (DGBAS, 2024).
cov <- cov |>
  mutate(population      = if_else(noc == "TPE", 23.40e6, population),
         population_year = if_else(noc == "TPE", 2024L, population_year),
         gdp_usd         = if_else(noc == "TPE", 781.6e9, gdp_usd),
         gdp_usd_year    = if_else(noc == "TPE", 2024L, gdp_usd_year),
         source          = if_else(noc == "TPE", "Manual (Taiwan official statistics)", source),
         gdp_per_capita  = gdp_usd / population)

write_csv(cov, "data/country_covariates.csv", na = "")
cat("Missing population:", paste(cov$noc[is.na(cov$population)], collapse = ", "), "\n")
cat("Missing GDP:", paste(cov$noc[is.na(cov$gdp_usd)], collapse = ", "), "\n")
print(cov |> filter(gdp_usd_year < 2023 | population_year < 2023) |> select(noc, population_year, gdp_usd_year))
