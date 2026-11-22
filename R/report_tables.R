# Shared table definitions with plain-English column names. Used by the Quarto report
# (formatted with gt) and by R/06_downloads.R (one Excel worksheet per table).
# Values stay numeric so Excel keeps them as numbers.

suppressPackageStartupMessages({ library(dplyr); library(tidyr) })

rank_among_eligible <- function(cn, col, code = "PHI") {
  v <- cn[[col]]; ok <- !is.na(v) & !is.na(cn$rank_comparable)
  r <- rank(-v[ok], ties.method = "min")
  r[cn$country_code[ok] == code]
}

# Table 1: Philippines headline measures
tbl_phi_summary <- function(cn) {
  p <- filter(cn, country_code == "PHI")
  n <- sum(!is.na(cn$rank_comparable))
  tibble(
    Measure = c("Medal points", "Medals per entry", "Average field beaten", "Top-8 rate",
                "Head-to-head win rate", "Head-to-head strength", "Actual ÷ expected medal points",
                "Actual ÷ expected medal points, allowing for team size", "Comparable Performance Index"),
    Philippines = c(as.character(p$medal_points),
                    sprintf("%.1f%%", 100 * c(p$medals_per_entry, p$avg_field_beaten, p$top8_rate)),
                    sprintf("%.1f%% (%d won, %d lost)", 100 * p$h2h_win_rate, p$h2h_wins, p$h2h_matches - p$h2h_wins),
                    sprintf("%.2f", c(p$h2h_strength, p$perf_vs_resources, p$perf_vs_resources_delegation)),
                    sprintf("%.1f", p$comparable_index)),
    Rank = c(sapply(c("medal_points", "medals_per_entry", "avg_field_beaten", "top8_rate", "h2h_win_rate",
                      "h2h_strength", "perf_vs_resources", "perf_vs_resources_delegation"),
                    \(c) rank_among_eligible(cn, c)), p$rank_comparable),
    `Out of` = n)
}

# Table 2: every eligible country on the comparable metrics
tbl_ranking <- function(cn) {
  cn |> filter(!is.na(rank_comparable)) |> arrange(rank_comparable) |>
    transmute(`Comparable rank` = rank_comparable, `Official rank` = rank_official,
              Country = country, Gold = gold, Silver = silver, Bronze = bronze,
              Athletes = athletes, Entries = entries,
              `Medals per entry` = medals_per_entry, `Average field beaten` = avg_field_beaten,
              `Top-8 rate` = top8_rate, `Head-to-head strength` = h2h_strength,
              `Actual ÷ expected` = perf_vs_resources_delegation,
              `Index` = comparable_index)
}

# Table 3: Philippine medals
tbl_phi_medals <- function(m) {
  m$phi_entries |> filter(!is.na(medal)) |>
    arrange(factor(medal, c("Gold", "Silver", "Bronze")), medal_date) |>
    transmute(Date = medal_date, Medal = medal, Sport = sport, Event = event,
              `Athlete or team` = coalesce(team_members, competitor_name), `Field size` = n_field)
}

# Table 4: Philippine near misses (4th and 5th, no medal)
tbl_phi_near <- function(m) {
  m$phi_entries |> filter(near_miss == 1) |> arrange(final_rank, sport, event) |>
    transmute(Place = final_rank, Sport = sport, Event = event,
              `Athlete or team` = coalesce(team_members, competitor_name), `Field size` = n_field)
}

# Model coefficients (Excel only)
tbl_models <- function(m) {
  lab <- c("(Intercept)" = "Intercept", "log(population)" = "Population (log)",
           "log(gdp_per_capita)" = "GDP per person (log)", "host" = "Host country",
           "log(athletes)" = "Athletes sent (log)")
  co <- function(mod, nm) { s <- summary(mod)$coefficients
    tibble(Model = nm, Term = unname(lab[rownames(s)]), Estimate = s[, 1], `Standard error` = s[, 2], `P value` = s[, 4]) }
  bind_rows(co(m$m_res, "Population, wealth and host"),
            co(m$m_del, "Population, wealth, host and team size"))
}
