# Step 4: comparable performance metrics for every NOC, plus a Philippines focus.
# Reads data/ag2026.rds and data/country_covariates.csv. Writes to output/.

source("R/00_helpers.R")
dir.create("output", showWarnings = FALSE)

d   <- readRDS("data/ag2026.rds")
fr  <- d$final_rankings
res <- d$results
cov <- read_csv("data/country_covariates.csv", show_col_types = FALSE)
HOST <- "JPN"
SEA  <- c("PHI", "THA", "INA", "MAS", "VIE", "SGP", "MYA", "CAM", "LAO", "BRU", "TLS")

# ---- entry-level scores ----------------------------------------------------------
# One "entry" = one competitor (athlete, pair or team) in one event.
# DNS entries are dropped (never competed); DNF/DSQ etc. count as beating nobody.
fr_scored <- fr |>
  filter(!(irm %in% c("DNS")), !is.na(country_code), country_code != "ART") |>
  group_by(sport_code, event_code) |>
  mutate(n_field = n()) |>
  ungroup() |>
  mutate(
    rank_used = if_else(is.na(final_rank), n_field, final_rank),
    # share of the other competitors this entry finished ahead of (1 = won, 0 = last)
    field_beaten = if_else(n_field > 1, pmax(0, (n_field - rank_used) / (n_field - 1)), NA_real_),
    top8         = as.integer(!is.na(final_rank) & final_rank <= 8),
    placing_pts  = if_else(!is.na(final_rank) & final_rank <= 8, 9L - final_rank, 0L),
    near_miss    = as.integer(medal_total == 0 & !is.na(final_rank) & final_rank %in% 4:5),
    medal_pts    = 3L * gold + 2L * silver + bronze,
    # medals an average entrant would expect in this event by chance
    medals_in_event = NA_real_
  ) |>
  group_by(sport_code, event_code) |>
  mutate(chance_medals = sum(medal_total) / n_field) |>
  ungroup()

# ---- head-to-head matches ----------------------------------------------------------
h2h <- res |>
  filter(is_h2h, !is.na(win_loss), win_loss %in% c("Win", "Loss"),
         !is.na(opponent_country_code), country_code != opponent_country_code,
         !country_code %in% "ART", !opponent_country_code %in% "ART")

h2h_summary <- h2h |>
  group_by(country_code) |>
  summarise(h2h_matches = n(), h2h_wins = sum(win_loss == "Win"), .groups = "drop") |>
  mutate(h2h_win_rate = h2h_wins / h2h_matches)

# Bradley-Terry: P(i beats j) = logistic(s_i - s_j), fitted with glm over every match
# (each match once). A small ridge (pseudo-match vs an average opponent) stabilises
# countries that played few matches.
bt_fit <- function(m) {
  m <- m |> filter(win_loss == "Win") |> transmute(winner = country_code, loser = opponent_country_code)
  teams <- sort(unique(c(m$winner, m$loser)))
  X <- matrix(0, nrow(m), length(teams), dimnames = list(NULL, teams))
  X[cbind(seq_len(nrow(m)), match(m$winner, teams))] <- 1
  X[cbind(seq_len(nrow(m)), match(m$loser, teams))]  <- -1
  # ridge: each team gets one win and one loss against a zero-strength "average" opponent
  R <- diag(length(teams)); colnames(R) <- teams
  Xall <- rbind(X, R, -R)
  y <- c(rep(1, nrow(X)), rep(1, length(teams)), rep(1, length(teams)))
  fit <- glm(y ~ 0 + Xall, family = binomial())  # y=1 always: row direction encodes winner
  s <- coef(fit); names(s) <- teams; s[is.na(s)] <- 0
  tibble(country_code = teams, h2h_strength = as.numeric(s - mean(s)))
}
bt <- bt_fit(h2h)

# ---- country table -------------------------------------------------------------------
athletes <- d$entries |> group_by(country_code) |>
  summarise(athletes = sum(athletes_total), sports_entered_official = n_distinct(sport_code), .groups = "drop")

country <- fr_scored |>
  group_by(country_code) |>
  summarise(
    country        = first(country),
    events_entered = n_distinct(paste(sport_code, event_code)),
    entries        = n(),
    sports_entered = n_distinct(sport_code),
    gold = sum(gold), silver = sum(silver), bronze = sum(bronze), medals = sum(medal_total),
    medal_points   = sum(medal_pts),
    top8_finishes  = sum(top8),
    placing_points = sum(placing_pts),
    near_misses    = sum(near_miss),
    avg_field_beaten = mean(field_beaten, na.rm = TRUE),
    expected_medals_by_chance = sum(chance_medals),
    .groups = "drop") |>
  left_join(athletes, by = "country_code") |>
  left_join(h2h_summary, by = "country_code") |>
  left_join(bt, by = "country_code") |>
  left_join(cov |> select(noc, population, gdp_usd, gdp_per_capita), by = c("country_code" = "noc")) |>
  mutate(
    medals_per_100_athletes = 100 * medals / athletes,
    medals_per_entry        = medals / entries,
    top8_rate               = top8_finishes / entries,
    medal_points_per_10m_pop = medal_points / (population / 1e7),
    medal_points_per_100b_gdp = medal_points / (gdp_usd / 1e11),
    medals_vs_chance        = medals / expected_medals_by_chance,
    host = as.integer(country_code == HOST)
  )

# ---- resource-based expectation (Bernard & Busse style) -------------------------------
# medal_points ~ log(population) + log(GDP per capita) + host, quasi-Poisson.
# A ratio > 1 means a country won more than its population and wealth predict.
mdat <- country |> filter(!is.na(population), !is.na(gdp_per_capita))
m_res <- glm(medal_points ~ log(population) + log(gdp_per_capita) + host,
             family = quasipoisson(), data = mdat)
# Second model also controls for delegation size (how many athletes were sent).
m_del <- glm(medal_points ~ log(population) + log(gdp_per_capita) + host + log(athletes),
             family = quasipoisson(), data = mdat)
country <- country |>
  left_join(mdat |> transmute(country_code,
                              expected_medal_points_resources  = predict(m_res, type = "response"),
                              expected_medal_points_resources_delegation = predict(m_del, type = "response")),
            by = "country_code") |>
  mutate(perf_vs_resources  = medal_points / expected_medal_points_resources,
         perf_vs_resources_delegation = medal_points / expected_medal_points_resources_delegation)

# ---- small-sample shrinkage ----------------------------------------------------------
# A country with 12 entries and 5 medals is not reliably "better" than one with 400
# entries and 100 medals. Empirical-Bayes style: add K pseudo-entries at the
# all-country average rate, so small delegations are pulled toward the average and
# large ones barely move.
K_PRIOR <- 25
p_medal <- sum(country$medals) / sum(country$entries)
p_top8  <- sum(country$top8_finishes) / sum(country$entries)
p_afb   <- weighted.mean(country$avg_field_beaten, country$entries, na.rm = TRUE)
country <- country |>
  mutate(medals_per_entry_adj = (medals + K_PRIOR * p_medal) / (entries + K_PRIOR),
         top8_rate_adj        = (top8_finishes + K_PRIOR * p_top8) / (entries + K_PRIOR),
         avg_field_beaten_adj = (avg_field_beaten * entries + K_PRIOR * p_afb) / (entries + K_PRIOR))

# ---- composite index: mean percentile across the comparable metrics ----------------
MIN_ENTRIES <- 25
pct <- function(x) percent_rank(x)
eligible <- country |> filter(entries >= MIN_ENTRIES)
idx <- eligible |>
  mutate(p_field   = pct(avg_field_beaten_adj),
         p_eff     = pct(medals_per_entry_adj),
         p_top8    = pct(top8_rate_adj),
         p_h2h     = pct(h2h_strength),
         p_res     = pct(perf_vs_resources_delegation)) |>
  rowwise() |>
  mutate(comparable_index = 100 * mean(c(p_field, p_eff, p_top8, p_h2h, p_res), na.rm = TRUE)) |>
  ungroup() |>
  select(country_code, comparable_index)

country <- country |>
  left_join(idx, by = "country_code") |>
  mutate(rank_official   = rank(-(gold * 1e6 + silver * 1e3 + bronze), ties.method = "min"),
         rank_comparable = if_else(is.na(comparable_index), NA_integer_,
                                   as.integer(rank(-comparable_index, ties.method = "min", na.last = "keep"))),
         region_sea = country_code %in% SEA) |>
  relocate(rank_official, rank_comparable, comparable_index, .after = country) |>
  arrange(rank_official)

write_csv(country, "output/country_metrics.csv", na = "")

# ---- sensitivity: does the ranking depend on how the index is built? -----------------
index_variants <- list(
  `Baseline (population, wealth and team size)` = c("avg_field_beaten_adj", "medals_per_entry_adj", "top8_rate_adj",
                                         "h2h_strength", "perf_vs_resources_delegation"),
  `Population and wealth only (no team-size control)` = c("avg_field_beaten_adj", "medals_per_entry_adj", "top8_rate_adj",
                                              "h2h_strength", "perf_vs_resources"),
  `No population or wealth adjustment` = c("avg_field_beaten_adj", "medals_per_entry_adj", "top8_rate_adj", "h2h_strength"),
  `Competitive quality only (field beaten and head-to-head)` = c("avg_field_beaten_adj", "h2h_strength"))
el <- filter(country, !is.na(rank_comparable))
sensitivity <- imap_dfr(index_variants, function(cols, nm) {
  s <- rowMeans(sapply(cols, \(c) percent_rank(el[[c]])), na.rm = TRUE) * 100
  tibble(variant = nm, country_code = el$country_code, country = el$country, index = s,
         rank = as.integer(rank(-s, ties.method = "min")))
})
write_csv(sensitivity, "output/index_sensitivity.csv", na = "")

# ---- Philippines focus -------------------------------------------------------------
phi_entries <- fr_scored |>
  filter(country_code == "PHI") |>
  select(medal_date, sport, event, gender, competitor_name, team_members, final_rank, rank_tied,
         n_field, n_countries_event, field_beaten, final_result, irm, medal, top8, near_miss) |>
  arrange(sport, event, final_rank)

phi_by_sport <- fr_scored |>
  filter(country_code == "PHI") |>
  group_by(sport) |>
  summarise(entries = n(), gold = sum(gold), silver = sum(silver), bronze = sum(bronze),
            medals = sum(medal_total), top8 = sum(top8), near_misses = sum(near_miss),
            avg_field_beaten = mean(field_beaten, na.rm = TRUE), .groups = "drop") |>
  left_join(h2h |> filter(country_code == "PHI") |> group_by(sport) |>
              summarise(h2h_w = sum(win_loss == "Win"), h2h_l = sum(win_loss == "Loss"), .groups = "drop"),
            by = "sport") |>
  arrange(desc(medals), desc(avg_field_beaten))

phi_h2h_by_opp <- h2h |>
  filter(country_code == "PHI") |>
  group_by(opponent_country_code, opponent_country) |>
  summarise(matches = n(), wins = sum(win_loss == "Win"), losses = sum(win_loss == "Loss"), .groups = "drop") |>
  mutate(win_rate = wins / matches) |>
  arrange(desc(matches))

phi_results <- res |> filter(country_code == "PHI")

write_csv(phi_entries,    "output/phi_event_entries.csv", na = "")
write_csv(phi_by_sport,   "output/phi_by_sport.csv", na = "")
write_csv(phi_h2h_by_opp, "output/phi_h2h_by_opponent.csv", na = "")
write_csv(phi_results,    "output/phi_all_results.csv", na = "")

saveRDS(list(country = country, sensitivity = sensitivity, fr_scored = fr_scored, h2h = h2h, m_res = m_res, m_del = m_del,
             phi_entries = phi_entries, phi_by_sport = phi_by_sport, phi_h2h_by_opp = phi_h2h_by_opp),
        "output/metrics.rds")

cat("\nModel (resources):\n"); print(summary(m_res)$coefficients)
cat("\nModel (resources + delegation):\n"); print(summary(m_del)$coefficients)
show_cols <- c("country_code", "rank_official", "rank_comparable", "comparable_index", "gold", "silver",
               "bronze", "medals", "athletes", "avg_field_beaten", "medals_per_entry", "h2h_win_rate",
               "h2h_strength", "perf_vs_resources", "perf_vs_resources_delegation")
cat("\nTop by comparable index:\n")
print(country |> arrange(rank_comparable) |> select(all_of(show_cols)) |> head(20), width = 250)
cat("\nPhilippines & SEA:\n")
print(country |> filter(region_sea) |> select(all_of(show_cols)), width = 250)
