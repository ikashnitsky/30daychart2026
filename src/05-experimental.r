#===============================================================================
# #30DayChartChallenge 2026 — Day 5: Experimental
# Data export for D3 interactive slope chart
# Life expectancy 1960 → 2020 | Data: {owidapi} R package
#===============================================================================

library(tidyverse)
library(owidapi)
library(countrycode)
library(jsonlite)
library(ggflags)  # needed for iso2 flag filter

# ── Data prep (identical to Day 4) ───────────────────────────────────────────
owid_data <- owid_get(chart_id = "life-expectancy")

df <- owid_data |>
  filter(year %in% c(1960, 2020)) |>
  rename(country = entity_name, lifeExp = life_expectancy_0) |>
  filter(country != "Antarctica") |>
  mutate(
    iso2      = countrycode(country, "country.name", "iso2c", warn = FALSE) |> tolower(),
    continent = countrycode(country, "country.name", "continent", warn = FALSE) |> as.character()
  ) |>
  drop_na(iso2, continent) |>
  filter(iso2 %in% names(ggflags::lflags))

gain_df <- df |>
  pivot_wider(
    id_cols     = c(country, continent, iso2),
    names_from  = year,
    values_from = lifeExp,
    names_prefix = "y"
  ) |>
  drop_na(y1960, y2020) |>
  mutate(gain = y2020 - y1960) |>
  mutate(across(c(y1960, y2020, gain), \(x) round(x, 2)))

n <- nrow(gain_df)

# Spread label positions evenly (mirrors ggplot logic from Day 4)
gain_df <- gain_df |>
  arrange(desc(y1960)) |>
  mutate(yPosLeft = seq(87, 10, length.out = n))

right_pos <- gain_df |>
  arrange(desc(y2020)) |>
  mutate(yPosRight = seq(87, 10, length.out = n)) |>
  select(country, yPosRight)

gain_df <- gain_df |>
  left_join(right_pos, by = "country") |>
  mutate(across(c(yPosLeft, yPosRight), \(x) round(x, 4)))

# ── Export JSON ───────────────────────────────────────────────────────────────
dir.create("data", showWarnings = FALSE)

gain_df |>
  toJSON(pretty = TRUE) |>
  write_lines("data/05-life-expectancy.json")

cat("✓ Exported", nrow(gain_df), "countries → data/05-life-expectancy.json\n")
cat("  Then open 05-experimental.html in a browser\n")
