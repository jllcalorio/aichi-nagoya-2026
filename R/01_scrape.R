# Step 1: download every raw JSON document we need into data/raw/ (cached).
# Safe to re-run: already-downloaded paths are read from disk, not re-fetched.
# Run from the project root:  Rscript R/01_scrape.R

source("R/00_helpers.R")
log_msg <- function(...) cat(format(Sys.time(), "%H:%M:%S"), ..., "\n")

# --- reference data -----------------------------------------------------------
ref_paths <- c("ALL/schedule/matrix", "ALL/disc/list", "ALL/orgs/list",
               "ALL/medals/standings", "ALL/entries/orgs/info",
               "ALL/medals/multi-medallists")
walk(ref_paths, ag_get)
dates <- unlist(ag_get("ALL/schedule/matrix")$dates)
log_msg("dates:", min(dates), "to", max(dates), "(", length(dates), ")")

# --- daily schedules (all sports) + daily medal lists ------------------------
day_units <- map(dates, \(d) ag_get(paste0("ALL/schedule/day/", d)))
walk(dates, \(d) ag_get(paste0("ALL/medals/daily/", d)))

units <- map2_dfr(dates, day_units, function(d, x) {
  map_dfr(x, \(u) tibble(Disc = chr(u$Disc), ResCode = chr(u$ResCode),
                         Event = chr(u$Event), Status = chr(u$Status)))
}) |> distinct(Disc, ResCode, .keep_all = TRUE)
# Some medal events (e.g. gymnastics all-around) have no units of their own in the
# daily schedule; pick them up from the official daily medal lists.
medal_events <- map_dfr(dates, \(d) map_dfr(ag_get(paste0("ALL/medals/daily/", d)),
  \(m) tibble(Disc = chr(m$Disc), Event = substr(chr(m$Event), 1, 20))))
events <- bind_rows(distinct(units, Disc, Event), medal_events) |> distinct()
log_msg("units:", nrow(units), " events:", nrow(events))

# --- final rankings per event -------------------------------------------------
for (i in seq_len(nrow(events))) {
  ag_get(paste0(events$Disc[i], "/final-rank/", events$Event[i]))
  if (i %% 50 == 0) log_msg("final-rank", i, "/", nrow(events))
}

# --- unit-level results (the big one) ----------------------------------------
todo <- filter(units, Status != "CANCELED")
for (i in seq_len(nrow(todo))) {
  ag_get(paste0(todo$Disc[i], "/results/", todo$ResCode[i]))
  if (i %% 100 == 0) log_msg("results", i, "/", nrow(todo))
}
log_msg("DONE. cached files:", length(list.files(RAW_DIR)))
