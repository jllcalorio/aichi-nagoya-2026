# Step 3: turn cached JSON into tidy tables (reads data/raw only, no network).
# Outputs (data/):
#   ag2026_results.csv        one row per competitor per competition unit (match/heat/round)
#   ag2026_final_rankings.csv one row per competitor per event, with final place and medal
#   ag2026_medal_table.csv    official medal standings by NOC
#   ag2026_entries.csv        athletes entered per NOC per sport

source("R/00_helpers.R")
read_raw <- function(path) {
  f <- cache_path(path)
  if (file.exists(f) && file.size(f) > 0) fromJSON(f, simplifyVector = FALSE) else NULL
}

gender_from_event <- function(ev) {
  recode(substr(ev, 1, 1), M = "Men", W = "Women", X = "Mixed", O = "Open", .default = NA_character_)
}
medal_name <- function(m) recode(m %||% "", ME_GOLD = "Gold", ME_SILVER = "Silver",
                                 ME_BRONZE = "Bronze", .default = NA_character_)

orgs  <- read_raw("ALL/orgs/list")
org_name <- setNames(map_chr(orgs, \(o) chr(o$Desc)), map_chr(orgs, \(o) chr(o$Key)))
discs <- read_raw("ALL/disc/list")
disc_name <- setNames(map_chr(discs, \(d) chr(d$Desc)), map_chr(discs, \(d) chr(d$Key)))
dates <- unlist(read_raw("ALL/schedule/matrix")$dates)

# ---- 1. schedule: one row per competition unit --------------------------------
units <- map_dfr(dates, function(d) {
  map_dfr(read_raw(paste0("ALL/schedule/day/", d)), function(u) tibble(
    sport_code   = chr(u$Disc),
    unit_code    = chr(u$ResCode),
    start_time   = chr(u$DateTimeRaw),
    event_code   = chr(u$Event),
    event        = chr(u$EventDesc),
    phase        = chr(u$PhaseDescA),
    phase_order  = as.integer(u$PhaseOrder %||% NA),
    unit         = chr(u$UnitDesc),
    venue        = chr(u$VenueDesc),
    location     = chr(u$LocDesc),
    unit_status  = chr(u$Status),
    is_h2h       = isTRUE(u$isH2H),
    medal_unit_code = chr(u$Medal)
  ))
}) |>
  distinct(sport_code, unit_code, .keep_all = TRUE) |>
  mutate(start_time_local = as.POSIXct(substr(start_time, 1, 19), format = "%Y-%m-%dT%H:%M:%S", tz = "Asia/Tokyo"),
         date = as.Date(substr(start_time, 1, 10)),
         sport = unname(disc_name[sport_code]),
         gender = gender_from_event(event_code),
         is_medal_unit = !is.na(medal_unit_code) & !medal_unit_code %in% c("", "0")) |>
  select(-start_time)

# ---- 2. unit results: one row per competitor per unit -------------------------
member_string <- function(c) {
  m <- c$Members %||% list()
  m <- keep(m, \(x) is.null(x$Function) || identical(x$Function, "AA01") || isTRUE(x$FuncDesc == "Athlete"))
  if (!length(m)) return(list(names = NA_character_, n = NA_integer_))
  list(names = paste(map_chr(m, \(x) chr(x$Name)), collapse = "; "), n = length(m))
}

parse_unit <- function(sport_code, unit_code) {
  x <- read_raw(paste0(sport_code, "/results/", unit_code))
  comps <- x$Competitors %||% list()
  if (!length(comps)) return(NULL)
  res <- x$Results %||% list()
  periods <- res$Periods %||% list()
  # period-by-period score, from the perspective of the home (first) competitor
  p_home <- map_chr(periods, \(p) chr(p$ResHome)); p_away <- map_chr(periods, \(p) chr(p$ResAway))
  ok <- !is.na(p_home) & p_home != "" & !is.na(p_away) & p_away != ""
  p_home <- p_home[ok]; p_away <- p_away[ok]

  rows <- map_dfr(seq_along(comps), function(i) {
    c <- comps[[i]]; mem <- member_string(c)
    tibble(
      sport_code = sport_code, unit_code = unit_code, comp_index = i,
      competitor_type = chr(c$Type %||% x$Info$Type),
      country_code    = chr(c$Org),
      competitor_id   = chr(c$Reg),
      competitor_name = chr(c$Name),
      team_members    = mem$names,
      n_members       = mem$n,
      bib             = chr(c$Bib),
      start_order     = suppressWarnings(as.integer(chr(c$StartOrder))),
      result          = chr(c$Result),
      result_detail   = na_if(chr(c$ResDetail), ""),
      result_info     = na_if(chr(c$ResInfo), ""),
      diff_to_leader  = na_if(chr(c$Diff), ""),
      rank            = suppressWarnings(as.integer(chr(c$Rk))),
      rank_tied       = isTRUE(c$RkEq),
      wlt             = na_if(chr(c$WLT), ""),
      irm             = na_if(chr(c$IRM), ""),
      qualified       = na_if(chr(c$Qualified), ""),
      record          = na_if(chr(c$RecordInd), ""),
      unit_medal      = medal_name(c$Medal),
      unit_score_detail = na_if(chr(res$ResDetail), "")
    )
  })
  rows$period_scores <- NA_character_
  if (length(p_home) && nrow(rows) == 2) {
    rows$period_scores[1] <- paste(paste0(p_home, "-", p_away), collapse = ", ")
    rows$period_scores[2] <- paste(paste0(p_away, "-", p_home), collapse = ", ")
  }
  rows
}

todo <- filter(units, unit_status != "CANCELED")
message("Parsing ", nrow(todo), " units ...")
comp_rows <- map2_dfr(todo$sport_code, todo$unit_code, \(s, u) tryCatch(parse_unit(s, u), error = function(e) {
  message("  parse error ", s, " ", u, ": ", conditionMessage(e)); NULL }))
message("Units with results on disk: ", n_distinct(paste(comp_rows$sport_code, comp_rows$unit_code)))

# Opponent columns for head-to-head units (exactly two sides)
h2h_keys <- units |> filter(is_h2h) |> select(sport_code, unit_code)
opp <- comp_rows |>
  semi_join(h2h_keys, by = c("sport_code", "unit_code")) |>
  group_by(sport_code, unit_code) |> filter(n() == 2) |>
  mutate(opp_i = rev(comp_index)) |> ungroup()
opp <- opp |>
  select(sport_code, unit_code, comp_index, opp_i) |>
  left_join(comp_rows |> select(sport_code, unit_code, opp_i = comp_index,
                                opponent_country_code = country_code, opponent_name = competitor_name,
                                opponent_result = result),
            by = c("sport_code", "unit_code", "opp_i")) |>
  select(-opp_i)

results <- comp_rows |>
  left_join(opp, by = c("sport_code", "unit_code", "comp_index")) |>
  left_join(units, by = c("sport_code", "unit_code")) |>
  mutate(
    country          = unname(org_name[country_code]),
    opponent_country = unname(org_name[opponent_country_code]),
    # W / L / T. For H2H use the WLT flag; fall back to comparing numeric scores.
    win_loss = case_when(
      !is_h2h ~ NA_character_,
      wlt %in% c("W", "L", "T") ~ wlt,
      suppressWarnings(as.numeric(result) > as.numeric(opponent_result)) ~ "W",
      suppressWarnings(as.numeric(result) < as.numeric(opponent_result)) ~ "L",
      TRUE ~ NA_character_),
    win_loss = recode(win_loss, W = "Win", L = "Loss", T = "Tie")
  ) |>
  select(
    # when
    date, start_time_local,
    # what
    sport_code, sport, event_code, event, gender, phase_order, phase, unit_code, unit,
    venue, location, unit_status, is_medal_unit, is_h2h,
    # who
    country_code, country, competitor_type, competitor_id, competitor_name, team_members, n_members,
    bib, start_order,
    # against whom
    opponent_country_code, opponent_country, opponent_name,
    # outcome
    result, opponent_result, period_scores, unit_score_detail, result_detail, result_info,
    diff_to_leader, rank, rank_tied, win_loss, irm, qualified, record, unit_medal
  ) |>
  arrange(date, start_time_local, sport, event, phase_order, unit_code, rank, start_order) |>
  rename(unit_score_detail_home_view = unit_score_detail)

# ---- 3. final rankings per event ----------------------------------------------
events <- units |>
  group_by(sport_code, sport, event_code, event, gender) |>
  summarise(event_start = min(date), event_end = max(date),
            medal_date = suppressWarnings(max(date[is_medal_unit])), .groups = "drop") |>
  mutate(medal_date = if_else(is.infinite(medal_date), event_end, medal_date))

# Medal events with no units of their own in the schedule (e.g. gymnastics all-around)
medal_daily <- map_dfr(dates, \(d) map_dfr(read_raw(paste0("ALL/medals/daily/", d)), \(m) tibble(
  sport_code = chr(m$Disc), event_code = substr(chr(m$Event), 1, 20), event = chr(m$EventDesc),
  medal_date = as.Date(substr(chr(m$DateRaw), 1, 10)))))
extra_events <- medal_daily |>
  distinct(sport_code, event_code, .keep_all = TRUE) |>
  anti_join(events, by = c("sport_code", "event_code")) |>
  mutate(sport = unname(disc_name[sport_code]), gender = gender_from_event(event_code),
         event_start = medal_date, event_end = medal_date)
events <- bind_rows(events, extra_events)

final_rank <- pmap_dfr(events |> select(sport_code, event_code), function(sport_code, event_code) {
  x <- read_raw(paste0(sport_code, "/final-rank/", event_code))
  comps <- x$Competitors %||% list()
  if (!length(comps)) return(NULL)
  map_dfr(comps, function(c) {
    mem <- member_string(c)
    tibble(sport_code = sport_code, event_code = event_code,
           country_code = chr(c$Org), competitor_type = chr(c$Type),
           competitor_id = chr(c$Reg), competitor_name = chr(c$Name),
           team_members = mem$names, n_members = mem$n,
           final_rank = suppressWarnings(as.integer(chr(c$Rk))),
           final_rank_pos = suppressWarnings(as.integer(c$RkPo %||% NA)),
           rank_tied = isTRUE(c$RkEq),
           final_result = na_if(chr(c$Result), ""),
           irm = na_if(chr(c$IRM), ""),
           medal = medal_name(c$Medal))
  })
})

final_rankings <- final_rank |>
  left_join(events, by = c("sport_code", "event_code")) |>
  group_by(sport_code, event_code) |>
  mutate(n_entries_event   = n(),
         n_countries_event = n_distinct(country_code),
         n_ranked_event    = sum(!is.na(final_rank))) |>
  ungroup() |>
  mutate(country = unname(org_name[country_code]),
         gold = as.integer(medal %in% "Gold"), silver = as.integer(medal %in% "Silver"),
         bronze = as.integer(medal %in% "Bronze"), medal_total = gold + silver + bronze) |>
  select(medal_date, event_start, event_end, sport_code, sport, event_code, event, gender,
         n_entries_event, n_countries_event, n_ranked_event,
         country_code, country, competitor_type, competitor_id, competitor_name, team_members, n_members,
         final_rank, final_rank_pos, rank_tied, final_result, irm, medal, gold, silver, bronze, medal_total) |>
  arrange(medal_date, sport, event, final_rank_pos, final_rank)

# ---- 4. official medal table & entries ----------------------------------------
st <- read_raw("ALL/medals/standings")
medal_table <- map_dfr(st, \(s) tibble(
  rank_gold = suppressWarnings(as.integer(chr(s$Rk))),
  rank_total = suppressWarnings(as.integer(chr(s$RkTotal))),
  country_code = chr(s$Org), country = unname(org_name[chr(s$Org)]),
  gold = s$Count$ME_GOLD$total %||% 0L, silver = s$Count$ME_SILVER$total %||% 0L,
  bronze = s$Count$ME_BRONZE$total %||% 0L, total = s$Count$total$total %||% 0L,
  gold_men = s$Count$ME_GOLD$M %||% 0L, gold_women = s$Count$ME_GOLD$W %||% 0L,
  gold_mixed = s$Count$ME_GOLD$X %||% 0L)) |>
  arrange(rank_gold, desc(silver), desc(bronze))

ent <- read_raw("ALL/entries/orgs/info")
entries <- map_dfr(ent, function(o) map_dfr(o$Totals$Count %||% list(), \(d) tibble(
  country_code = chr(o$Key), country = chr(o$Desc), sport_code = chr(d$Disc), sport = chr(d$DiscDesc),
  athletes_men = d$M %||% 0L, athletes_women = d$W %||% 0L, athletes_total = d$Total %||% 0L))) |>
  arrange(country, sport)

# ---- write --------------------------------------------------------------------
# write times as Japan local time text (write_csv would convert POSIXct to UTC)
write_csv(mutate(results, start_time_local = format(start_time_local, "%Y-%m-%d %H:%M", tz = "Asia/Tokyo")),
          "data/ag2026_results.csv", na = "")
write_csv(final_rankings, "data/ag2026_final_rankings.csv", na = "")
write_csv(medal_table,    "data/ag2026_medal_table.csv", na = "")
write_csv(entries,        "data/ag2026_entries.csv", na = "")
saveRDS(list(results = results, final_rankings = final_rankings, medal_table = medal_table,
             entries = entries, units = units), "data/ag2026.rds")

message("results rows: ", nrow(results), " | final_rankings rows: ", nrow(final_rankings),
        " | events with final ranks: ", n_distinct(paste(final_rankings$sport_code, final_rankings$event_code)))
