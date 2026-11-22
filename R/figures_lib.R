# Shared figure definitions. Used by R/05_figures.R (600 DPI PNG export) and by the
# Quarto report, so both show exactly the same charts.
# make_figures(m) returns a named list; each element has: plot, title, w, h (inches).
# Plots carry no title: the report puts the title in the numbered figure caption, and
# the PNG export adds it back with labs(title = ...).

suppressPackageStartupMessages({ library(ggplot2); library(scales); library(dplyr); library(tidyr) })

COL_PHI <- "#2a78d6"; COL_SEA <- "#eb6834"; COL_REST <- "#c3c2b7"
INK <- "#0b0b0b"; INK2 <- "#52514e"; MUTED <- "#898781"; GRID <- "#e1e0d9"; SURF <- "#fcfcfb"
SEA <- c("PHI", "THA", "INA", "MAS", "VIE", "SGP", "MYA", "CAM", "LAO", "BRU", "TLS")
FIG_SOURCE <- "Source: results.asiangames2026.org (scraped). Analysis in R."

theme_ag <- function() {
  theme_minimal(base_size = 12, base_family = "sans") +
    theme(plot.background = element_rect(fill = SURF, colour = NA),
          panel.grid.major = element_line(colour = GRID, linewidth = 0.3),
          panel.grid.minor = element_blank(),
          axis.text = element_text(colour = MUTED), axis.title = element_text(colour = INK2),
          plot.title = element_text(colour = INK, face = "bold", size = 14),
          plot.subtitle = element_text(colour = INK2, size = 10.5),
          plot.caption = element_text(colour = MUTED, size = 8.5, hjust = 0),
          strip.text = element_text(colour = INK, face = "bold", hjust = 0),
          legend.position = "top", legend.title = element_blank(),
          legend.text = element_text(colour = INK2),
          plot.title.position = "plot", plot.caption.position = "plot")
}
grp <- function(code) factor(case_when(code == "PHI" ~ "Philippines",
                                       code %in% SEA ~ "Other Southeast Asia",
                                       TRUE ~ "Other countries"),
                             levels = c("Philippines", "Other Southeast Asia", "Other countries"))
grp_cols <- c("Philippines" = COL_PHI, "Other Southeast Asia" = COL_SEA, "Other countries" = COL_REST)

make_figures <- function(m) {
  country <- m$country
  cc <- country |> filter(!is.na(rank_comparable)) |> mutate(g = grp(country_code))
  n_el <- nrow(cc)
  figs <- list()

  # 1. Official vs comparable rank ------------------------------------------------------
  p <- cc |>
    mutate(country = reorder(country, -rank_comparable)) |>
    ggplot(aes(y = country)) +
    geom_segment(aes(x = rank_official, xend = rank_comparable, yend = country), colour = GRID, linewidth = 1) +
    geom_point(aes(x = rank_official, shape = "Official medal-table rank (gold first)"),
               fill = SURF, colour = MUTED, size = 2.6, stroke = 1) +
    geom_point(aes(x = rank_comparable, fill = g), shape = 21, colour = SURF, size = 3.4, stroke = 0.6) +
    scale_shape_manual(values = 21) +
    scale_fill_manual(values = grp_cols, breaks = names(grp_cols)) +
    scale_x_reverse(breaks = c(1, 5, 10, 15, 20, 25, 30, 35, 40)) +
    guides(fill = guide_legend(order = 1, override.aes = list(size = 3.4)), shape = guide_legend(order = 2)) +
    labs(subtitle = "Filled dot = rank on the Comparable Performance Index. Hollow dot = official rank. Further right = better.",
         x = "Rank (1 = best)", y = NULL, caption = FIG_SOURCE) +
    theme_ag() + theme(axis.text.y = element_text(size = 8.5, colour = INK2), legend.box = "vertical")
  figs$rank_official_vs_comparable <- list(plot = p, w = 9, h = 9.5,
    title = "Official medal-table rank vs. comparable-performance rank")

  # 2. Sensitivity of the index ---------------------------------------------------------
  sens <- m$sensitivity |>
    group_by(country_code, country) |>
    summarise(best = min(rank), worst = max(rank), .groups = "drop") |>
    left_join(select(cc, country_code, rank_comparable, rank_official, g), by = "country_code") |>
    mutate(country = reorder(country, -rank_comparable))
  p <- ggplot(sens, aes(y = country)) +
    geom_segment(aes(x = best, xend = worst, yend = country, colour = g), linewidth = 2.4, lineend = "round", alpha = 0.55) +
    geom_point(aes(x = rank_comparable, colour = g), size = 2.8) +
    scale_colour_manual(values = grp_cols, breaks = names(grp_cols)) +
    scale_x_reverse(breaks = c(1, 5, 10, 15, 20, 25, 30, 35)) +
    labs(subtitle = paste0("Bar = best-to-worst rank across the four index versions. Dot = baseline index rank. ",
                           n_el, " countries; further right = better."),
         x = "Rank (1 = best)", y = NULL, caption = FIG_SOURCE) +
    theme_ag() + theme(axis.text.y = element_text(size = 8.5, colour = INK2), panel.grid.major.y = element_blank())
  figs$index_sensitivity <- list(plot = p, w = 9, h = 9.5,
    title = "How much each country's rank moves when the index is built differently")

  # 3. Field beaten vs conversion -------------------------------------------------------
  lab <- cc |> filter(country_code %in% c(SEA, "CHN", "JPN", "KOR", "IND", "IRI", "KAZ", "UZB", "TPE", "HKG"))
  p <- ggplot(cc, aes(avg_field_beaten, medals_per_entry)) +
    geom_point(data = filter(cc, g == "Other countries"), aes(colour = g), size = 2.6) +
    geom_point(data = filter(cc, g != "Other countries"), aes(colour = g), size = 3.4) +
    ggrepel::geom_text_repel(data = lab, aes(label = country), colour = INK2, size = 3,
                             min.segment.length = 0.2, segment.colour = MUTED, box.padding = 0.4, seed = 1) +
    scale_colour_manual(values = grp_cols, breaks = names(grp_cols)) +
    scale_x_continuous(labels = percent) + scale_y_continuous(labels = percent) +
    labs(subtitle = "Horizontal: average share of each event's field that a country's entries finished ahead of.\nVertical: share of entries that won a medal.",
         x = "Average field beaten", y = "Medals per entry", caption = FIG_SOURCE) +
    theme_ag()
  figs$field_beaten_vs_conversion <- list(plot = p, w = 9, h = 6.5,
    title = "How deep into the field each country finished, and how often that became a medal")

  # 4. Actual vs expected ---------------------------------------------------------------
  ce <- country |> filter(!is.na(expected_medal_points_resources), medal_points > 0) |> mutate(g = grp(country_code))
  p <- ggplot(ce, aes(expected_medal_points_resources, medal_points)) +
    geom_abline(slope = 1, intercept = 0, colour = MUTED, linetype = "dashed", linewidth = 0.4) +
    geom_point(data = filter(ce, g == "Other countries"), aes(colour = g), size = 2.6) +
    geom_point(data = filter(ce, g != "Other countries"), aes(colour = g), size = 3.4) +
    ggrepel::geom_text_repel(data = filter(ce, country_code %in% c(SEA, "CHN", "JPN", "KOR", "IND", "IRI", "KAZ", "UZB", "TPE")),
              aes(label = country), colour = INK2, size = 3, min.segment.length = 0.2,
              segment.colour = MUTED, box.padding = 0.4, seed = 1) +
    scale_colour_manual(values = grp_cols, breaks = names(grp_cols)) +
    scale_x_log10() + scale_y_log10() +
    labs(subtitle = "Medal points = 3 per gold, 2 per silver, 1 per bronze. Above the dashed line = won more than expected. Log scales.",
         x = "Expected medal points (from population, GDP per person and host status)", y = "Actual medal points",
         caption = FIG_SOURCE) +
    theme_ag()
  figs$actual_vs_expected <- list(plot = p, w = 9, h = 6.5,
    title = "Medal points won vs. what population and wealth predict")

  # 5. Southeast Asia small multiples ---------------------------------------------------
  sea <- country |> filter(country_code %in% SEA, entries > 0) |>
    transmute(country, country_code, rank_official,
              `Medals per 100 athletes` = medals_per_100_athletes,
              `Average field beaten` = avg_field_beaten * 100,
              `Head-to-head win rate` = h2h_win_rate * 100,
              `Actual ÷ expected medal points` = perf_vs_resources) |>
    pivot_longer(-c(country, country_code, rank_official), names_to = "metric") |>
    filter(!is.na(value)) |>
    mutate(metric = factor(metric, c("Medals per 100 athletes", "Average field beaten",
                                     "Head-to-head win rate", "Actual ÷ expected medal points")),
           g = grp(country_code),
           country = reorder(country, -rank_official),
           lab = case_when(metric == "Actual ÷ expected medal points" ~ formatC(value, format = "f", digits = 2),
                           metric == "Medals per 100 athletes" ~ formatC(value, format = "f", digits = 1),
                           TRUE ~ paste0(round(value), "%")))
  p <- ggplot(sea, aes(value, country, fill = g)) +
    geom_col(width = 0.68) +
    geom_text(aes(label = lab), hjust = -0.15, size = 3, colour = INK2) +
    facet_wrap(~metric, scales = "free_x", nrow = 1) +
    scale_x_continuous(expand = expansion(mult = c(0, 0.32))) +
    scale_fill_manual(values = grp_cols, breaks = names(grp_cols)[1:2]) +
    labs(subtitle = "Countries ordered by official medal-table rank (best at top). Percentages for field beaten and head-to-head win rate.",
         x = NULL, y = NULL, caption = FIG_SOURCE) +
    theme_ag() + theme(axis.text.y = element_text(colour = INK2), axis.text.x = element_blank(),
                       panel.grid.major = element_blank())
  figs$southeast_asia <- list(plot = p, w = 11, h = 5,
    title = "Southeast Asia: four ways to compare the region's teams")

  # 6. Philippines by sport -------------------------------------------------------------
  pbs <- m$phi_by_sport |> filter(entries > 0) |> mutate(sport = reorder(sport, avg_field_beaten))
  p <- ggplot(pbs, aes(avg_field_beaten, sport)) +
    geom_col(fill = COL_PHI, width = 0.65) +
    geom_text(aes(label = paste0(medals, " medal", if_else(medals == 1, "", "s"), " · ", top8, " top-8 · ",
                                 entries, if_else(entries == 1, " entry", " entries"))),
              hjust = -0.05, size = 2.9, colour = INK2) +
    scale_x_continuous(labels = percent, limits = c(0, 1.3), breaks = seq(0, 1, .25)) +
    labs(subtitle = "Each bar = mean share of the field beaten over all Philippine entries in that sport (100% = won every event entered).",
         x = "Average field beaten", y = NULL, caption = FIG_SOURCE) +
    theme_ag() + theme(axis.text.y = element_text(colour = INK2, size = 9), panel.grid.major.y = element_blank())
  figs$phi_by_sport <- list(plot = p, w = 9, h = max(5, 0.27 * nrow(pbs) + 1.6),
    title = "The Philippines by sport: how far into the field its entries finished")

  # 7. Philippines head-to-head record by opponent ---------------------------------------
  opp <- m$phi_h2h_by_opp |> filter(matches >= 3) |>
    mutate(opponent_country = reorder(opponent_country, matches)) |>
    transmute(opponent_country, Wins = wins, Losses = -losses, win_rate) |>
    pivot_longer(c(Wins, Losses), names_to = "res") |>
    mutate(res = factor(res, c("Wins", "Losses")))
  p <- ggplot(opp, aes(value, opponent_country, fill = res)) +
    geom_col(width = 0.7) +
    geom_vline(xintercept = 0, colour = INK2, linewidth = 0.3) +
    geom_text(data = filter(opp, value != 0), aes(label = abs(value), hjust = if_else(value > 0, -0.3, 1.3)),
              size = 2.9, colour = INK2) +
    scale_fill_manual(values = c(Wins = COL_PHI, Losses = COL_REST)) +
    scale_x_continuous(labels = \(x) abs(x), expand = expansion(mult = 0.12)) +
    labs(subtitle = "Matches, bouts and games against each opponent country (3 or more meetings). Ordered by number of meetings.",
         x = "Losses  ←  number of matches  →  Wins", y = NULL, caption = FIG_SOURCE) +
    theme_ag() + theme(axis.text.y = element_text(colour = INK2, size = 9), panel.grid.major.y = element_blank())
  figs$phi_h2h_by_opponent <- list(plot = p, w = 9, h = max(5, 0.26 * n_distinct(opp$opponent_country) + 1.6),
    title = "The Philippines' head-to-head record, by opponent")

  # 8. Head-to-head strength ------------------------------------------------------------
  hb <- country |> filter(!is.na(h2h_strength), h2h_matches >= 20) |>
    mutate(g = grp(country_code), country = reorder(country, h2h_strength))
  p <- ggplot(hb, aes(h2h_strength, country, fill = g)) +
    geom_col(width = 0.7) +
    geom_vline(xintercept = 0, colour = INK2, linewidth = 0.3) +
    scale_fill_manual(values = grp_cols, breaks = names(grp_cols)) +
    labs(subtitle = "Bradley-Terry rating from every match, bout and game. 0 = an average opponent. Countries with at least 20 matches.",
         x = "Rating (log-odds of beating an average opponent)", y = NULL, caption = FIG_SOURCE) +
    theme_ag() + theme(axis.text.y = element_text(size = 8.5, colour = INK2), panel.grid.major.y = element_blank())
  figs$h2h_strength <- list(plot = p, w = 9, h = 8.5,
    title = "Head-to-head strength, adjusted for the opponents each country faced")

  figs
}
