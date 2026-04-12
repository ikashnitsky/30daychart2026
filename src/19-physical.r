# ..........................................................
# 2026-04-11 -- 30DayChartChallenge
# Day 11: Physical | IIHF World Championship player physique 2001–2024 -----------
# Ilya Kashnitsky, ilya.kashnitsky@gmail.com
# ..........................................................

library(tidyverse)
library(rvest)
library(httr2)
library(lubridate)
library(glue)
library(janitor)


# devtools::source_gist("653e1040a07364ae82b1bb312501a184")  # theme_ik (Step 2)

# ============================================================================
# HELPER — scrape one archived roster page
# ============================================================================
parse_iihf_archived_page <- function(wb_url, year, country, verbose = TRUE) {
  if (verbose) {
    message("  Fetching: ", country, " ", year)
  }

  resp <- tryCatch(
    request(wb_url) |>
      req_user_agent("Mozilla/5.0 (compatible; R-research/1.0)") |>
      req_timeout(30) |>
      req_retry(max_tries = 3, backoff = ~5) |>
      req_perform(),
    error = function(e) {
      message("    ERROR: ", e$message)
      NULL
    }
  )
  if (is.null(resp) || resp_status(resp) != 200) {
    return(NULL)
  }

  html <- resp |> resp_body_string()

  # ── Extract s-statistic-wrapper blocks ──────────────────────────────────
  # Each block = one player expanded profile card
  wrappers <- str_extract_all(
    html,
    r'((?s)<div class="s-statistic-wrapper">.*?(?=<div class="s-statistic-wrapper">|</tbody>))' # FIXED: was \\" (double backslash)
  )[[1]]

  if (length(wrappers) == 0) {
    return(NULL)
  }

  parse_field <- function(block, label) {
    pattern <- glue(
      '<span class="s-title">{label}:</span>\\s*',
      '<span class="s-stat">(.*?)</span>'
    )
    m <- str_match(block, pattern)
    if (is.na(m[1, 2])) {
      return(NA_character_)
    }
    str_trim(m[1, 2])
  }

  map_dfr(wrappers, function(w) {
    name_m <- str_match(w, '<div class="s-name">(.*?)</div>')
    name <- if (!is.na(name_m[1, 2])) str_trim(name_m[1, 2]) else NA_character_
    pos <- parse_field(w, "Position")
    ht_raw <- parse_field(w, "Height \\(cm\\)")
    wt_raw <- parse_field(w, "Weight \\(kg\\)")
    dob_raw <- parse_field(w, "Date of Birth")

    # Height stored as metres in IIHF website (e.g. "1.80")
    ht_cm <- suppressWarnings(
      if (!is.na(ht_raw)) round(as.numeric(ht_raw) * 100) else NA_real_
    )
    wt_kg <- suppressWarnings(as.numeric(wt_raw))

    # ── CHANGED: parse full calendar date, derive birth_year from it ──────
    # IIHF pages use formats like "Jan 15, 1990", "15/01/1990", "1990-01-15"
    dob_parsed <- suppressWarnings(
      as.Date(
        parse_date_time(
          dob_raw,
          orders = c("mdy", "dmy", "ymd", "BdY", "bdY", "dbY"),
          quiet = TRUE
        )
      )
    )
    birth_year <- if (!is.na(dob_parsed)) {
      as.integer(year(dob_parsed))
    } else {
      NA_integer_
    }

    tibble(
      year = as.integer(year),
      country = country,
      name = name,
      position = pos,
      height_cm = ht_cm,
      weight_kg = wt_kg,
      date_of_birth = dob_parsed, # ← NEW: full Date preserved
      birth_year = birth_year # derived from date_of_birth
    )
  })
}

# ============================================================================
# PART A — Discover all archived roster URLs via Wayback CDX API
# ============================================================================
years_new <- c(2017, 2018, 2019, 2021, 2022, 2023, 2024)

message("=== Step 1: CDX discovery ===")

cdx_results <- map_dfr(years_new, function(yr) {
  Sys.sleep(0.8)
  cdx_url <- glue(
    "http://web.archive.org/cdx/search/cdx",
    "?url=www.iihf.com/en/events/{yr}/wm/teams/roster/*",
    "&output=json&fl=original,timestamp&filter=statuscode:200",
    "&collapse=original"
  )
  resp <- tryCatch(
    request(cdx_url) |> req_timeout(25) |> req_perform(),
    error = function(e) NULL
  )
  if (is.null(resp) || resp_status(resp) != 200) {
    message("  CDX failed for ", yr)
    return(tibble())
  }
  raw <- resp |> resp_body_json()
  if (length(raw) < 2) {
    return(tibble())
  }

  bind_rows(lapply(raw[-1], function(r) {
    tibble(year = as.integer(yr), url = r[[1]], timestamp = r[[2]])
  }))
})

message("CDX: ", nrow(cdx_results), " total archived URLs found")

# ============================================================================
# PART B — Deduplicate to one URL per (year × country)
# ============================================================================
cdx_clean <- cdx_results |>
  mutate(
    url_clean = str_remove(url, "[?#].*"),
    team_id = as.integer(str_extract(url_clean, "(?<=/roster/)\\d+")),
    country_raw = str_extract(url_clean, "(?<=/roster/\\d{1,8}/)([a-z_-]+)"),
    country_slug = country_raw |>
      str_replace_all("-", "_") |>
      str_replace("czech_republic|czechia", "czech") |>
      str_replace("united_states|united-states", "usa") |>
      str_replace("great_britain|great-britain", "gbr") |>
      str_replace("roc", "russia")
  ) |>
  filter(!is.na(team_id), !is.na(country_slug)) |>
  filter(!(year == 2019 & team_id == 19313)) |>
  filter(!(year == 2021 & team_id == 19315)) |>
  group_by(year, country_slug) |>
  slice_max(team_id, n = 1, with_ties = FALSE) |>
  ungroup() |>
  mutate(
    wayback_url = glue("https://web.archive.org/web/{timestamp}/{url_clean}")
  )

message("After dedup: ", nrow(cdx_clean), " unique (year × country) pages")
count(cdx_clean, year) |> print()

# ============================================================================
# PART C — Fetch and parse all archived team pages
# ============================================================================
message("\n=== Step 2: Fetching archived roster pages ===")

df_new <- map_dfr(seq_len(nrow(cdx_clean)), function(i) {
  row <- cdx_clean[i, ]
  Sys.sleep(0.6)
  parse_iihf_archived_page(
    wb_url = row$wayback_url,
    year = row$year,
    country = row$country_slug,
    verbose = TRUE
  )
})

message("\nNew data: ", nrow(df_new), " raw player-records")

# ============================================================================
# PART D — Load Ilya's original 2001–2016 dataset
# ============================================================================

df_old_raw <- read_csv("https://ndownloader.figshare.com/files/5303173")

message("Original columns: ", paste(names(df_old_raw), collapse = ", "))
glimpse(df_old_raw)

# ── Standardise old dataset ─────────────────────────────────────────────────
# ── CHANGED: detect any DOB-style column; add NA_Date_ stub when absent ───
df_old_renamed <- df_old_raw |>
  rename_with(str_to_lower) |>
  rename_with(
    ~ case_when(
      . == "height" ~ "height_cm",
      . == "weight" ~ "weight_kg",
      . %in% c("cohort", "byr") ~ "birth_year",
      str_detect(., "dob|date.*birth|born") ~ "date_of_birth",
      TRUE ~ .
    )
  )

# Add date_of_birth if the old export doesn't carry it (graceful fallback)
if (!"date_of_birth" %in% names(df_old_renamed)) {
  df_old_renamed <- df_old_renamed |>
    mutate(date_of_birth = NA_Date_)
} else {
  df_old_renamed <- df_old_renamed |>
    mutate(
      date_of_birth = as.Date(
        parse_date_time(
          date_of_birth,
          orders = c("mdy", "dmy", "ymd", "BdY", "bdY", "dbY"),
          quiet = TRUE
        )
      )
    )
}

# Derive birth_year from date_of_birth when only DOB column was present
if (!"birth_year" %in% names(df_old_renamed)) {
  df_old_renamed <- df_old_renamed |>
    mutate(birth_year = as.integer(year(date_of_birth)))
}

df_old <- df_old_renamed |>
  transmute(
    year = as.integer(year),
    country = as.character(country) |>
      str_to_lower() |>
      str_replace_all(" ", "_"),
    name = as.character(name),
    position = as.character(position),
    height_cm = as.numeric(height_cm),
    weight_kg = as.numeric(weight_kg),
    date_of_birth = date_of_birth, # ← NEW: Date or NA_Date_
    birth_year = as.integer(birth_year)
  )

# ============================================================================
# PART E — Standardise new data and combine
# ============================================================================
df_new_std <- df_new |>
  mutate(
    position = case_when(
      str_detect(position, regex("^Goalk", ignore_case = TRUE)) ~ "Goalie",
      str_detect(position, regex("^Def", ignore_case = TRUE)) ~ "Defender",
      str_detect(position, regex("^For", ignore_case = TRUE)) ~ "Forward",
      TRUE ~ NA_character_
    ),
    country = country |> coalesce(country)
  ) |>
  select(
    year,
    country,
    name,
    position,
    height_cm,
    weight_kg,
    date_of_birth,
    birth_year
  ) # ← NEW: date_of_birth added to select()

# Combine
df_iihf <- bind_rows(df_old, df_new_std) |>

  filter(
    between(height_cm, 155, 220),
    between(weight_kg, 55, 130),
    !is.na(position),
    !is.na(birth_year),
    year >= 2001,
    year <= 2024
  ) |>

  distinct(year, country, name, .keep_all = TRUE) |>

  mutate(
    bmi = weight_kg / (height_cm / 100)^2,
    position = factor(position, levels = c("Goalie", "Defender", "Forward"))
  )

# ============================================================================
# DIAGNOSTICS
# ============================================================================
message("\n=== Final dataset ===")
message(
  nrow(df_iihf),
  " player-records across ",
  n_distinct(df_iihf$year),
  " IIHF World Championships"
)
cat("\nRecords per year:\n")
print(count(df_iihf, year), n = 30)
cat("\nRecords per position:\n")
print(count(df_iihf, position))
cat("\nHeight summary:\n")
print(summary(df_iihf$height_cm))
cat("\nDate-of-birth coverage:\n") # ← NEW diagnostic
cat(
  "  Full DOB available:  ",
  sum(!is.na(df_iihf$date_of_birth)),
  "\n",
  "  Birth year only:     ",
  sum(is.na(df_iihf$date_of_birth) & !is.na(df_iihf$birth_year)),
  "\n"
)

write_csv(df_iihf, "dat/iihf_players_2001_2024.csv")
message("\n✅  Saved: dat/iihf_players_2001_2024.csv")
