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
library(countrycode)
library(ggtext)
library(ggflags)


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
    "(?s)<div class=\"s-statistic-wrapper\">.*?(?=<div class=\"s-statistic-wrapper\">|</tbody>)"
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
    birth_year <- suppressWarnings(
      if (!is.na(dob_raw)) {
        as.integer(str_extract(dob_raw, "\\d{4}"))
      } else {
        NA_integer_
      }
    )

    tibble(
      year = as.integer(year),
      country = country,
      name = name,
      position = pos,
      height_cm = ht_cm,
      weight_kg = wt_kg,
      birth_year = birth_year
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
  # Parse team_id and country slug from URL path
  mutate(
    url_clean = str_remove(url, "[?#].*"), # strip query strings
    team_id = as.integer(str_extract(url_clean, "(?<=/roster/)\\d+")),
    country_raw = str_extract(url_clean, "(?<=/roster/\\d{1,8}/)([a-z_-]+)"),
    # Normalise country slug
    country_slug = country_raw |>
      str_replace_all("-", "_") |>
      str_replace("czech_republic|czechia", "czech") |>
      str_replace("united_states|united-states", "usa") |>
      str_replace("great_britain|great-britain", "gbr") |>
      str_replace("roc", "russia") # ROC = Russia 2021/2022
  ) |>
  filter(!is.na(team_id), !is.na(country_slug)) |>

  # For 2019: Finland has ID 3695 (correct) and 19313 (Finland's 2021 ID)
  filter(!(year == 2019 & team_id == 19313)) |>

  # For 2021: Germany has placeholder 19315 and correct 3696
  filter(!(year == 2021 & team_id == 19315)) |>

  # For 2023: IIHF migrated IDs mid-year (26xxx → 36xxx); keep highest
  group_by(year, country_slug) |>
  slice_max(team_id, n = 1, with_ties = FALSE) |>
  ungroup() |>

  # Build Wayback Machine fetch URL
  mutate(
    wayback_url = glue(
      "https://web.archive.org/web/{timestamp}/{url_clean}"
    )
  )

message("After dedup: ", nrow(cdx_clean), " unique (year × country) pages")
count(cdx_clean, year) |> print()

# ============================================================================
# PART C — Fetch and parse all archived team pages
# ============================================================================
message("\n=== Step 2: Fetching archived roster pages ===")

df_new <- map_dfr(seq_len(nrow(cdx_clean)), function(i) {
  row <- cdx_clean[i, ]
  Sys.sleep(0.6) # polite delay
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
# Assumes column names: year, country, name, position, height, weight, byr
# Adjust the case_when() below if your CSV differs
df_old <- df_old_raw |>
  rename_with(str_to_lower) |>
  rename_with(
    ~ case_when(
      . == "height" ~ "height_cm",
      . == "weight" ~ "weight_kg",
      . == "cohort" ~ "birth_year",
      TRUE ~ .
    )
  ) |>
  transmute(
    year = as.integer(year),
    country = as.character(country) |>
      str_to_lower() |>
      str_replace_all(" ", "_"),
    name = as.character(name),
    position = as.character(position),
    height_cm = as.numeric(height_cm),
    weight_kg = as.numeric(weight_kg),
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
    country = country |>
      coalesce(country) |>
      str_replace("czech", "czechia") |>
      countrycode::countrycode(
        origin = "country.name",
        destination = "iso3c"
      ) |>
      str_to_lower()
  ) |>
  select(year, country, name, position, height_cm, weight_kg, birth_year)


# Combine
df_iihf <- bind_rows(df_old, df_new_std) |>

  # Plausibility filter
  filter(
    between(height_cm, 155, 220),
    between(weight_kg, 55, 130),
    !is.na(position),
    !is.na(birth_year),
    year >= 2001,
    year <= 2024
  ) |>

  # Drop exact duplicates (same year + country + name)
  distinct(year, country, name, .keep_all = TRUE) |>

  # Derived columns
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

write_csv(df_iihf, "dat/iihf_players_2001_2024.csv")
message("\n✅  Saved: dat/iihf_players_2001_2024.csv")


# VISUALIZE -- replicate figure 6 from the blog post ---------------------

devtools::source_gist("653e1040a07364ae82b1bb312501a184")
sysfonts::font_add_google("Atkinson Hyperlegible", family = "ah")
showtext::showtext_auto()
theme_set(theme_ik(base_family = "ah", base_size = 14))

# ============================================================
# Day 11 · Physical
# Replicate + update Figure 6 from the blog post
# ============================================================

# ---- 1) READ YOUR UPDATED HOCKEY DATA ----------------------

df_iihf <- readr::read_csv(
  "dat/iihf_players_2001_2024.csv",
  show_col_types = FALSE
)

# calculate hockey players' cohort height averages for each country
df_hoc <- df_iihf |>
  group_by(name, country, birth_year) |>
  summarise(ind_height_cm = mean(height_cm, na.rm = TRUE), .groups = "drop") |>
  group_by(country, cohort = birth_year) |>
  summarise(
    height_avg = mean(ind_height_cm, na.rm = TRUE),
    n_players = n(),
    .groups = "drop"
  )


# ---- 2) LOAD ADDITIONAL POPULATION DATA --------------------
# Hatton, T. J., & Bray, B. E. (2010), copied to figshare in the original post

df_hb <- read.csv("https://ndownloader.figshare.com/files/5303878") |>
  gather("country", "h_pop", 2:16) |>
  mutate(period = paste(period)) |>
  separate(period, c("t1", "t2"), sep = "/") |>
  transmute(
    cohort = (as.numeric(t1) + as.numeric(t2)) / 2,
    country = country |> str_to_lower(),
    height_pop = h_pop
  )


# ---- 3) KEEP ONLY COUNTRIES IN BOTH DATASETS ---------------
both_cnt <- intersect(sort(unique(df_hb$country)), sort(unique(df_hoc$country)))

pop_plot <- df_hb |>
  filter(country %in% both_cnt) |>
  mutate(
    country = country |>
      str_replace("den", "dnk") |>
      str_replace("ger", "deu") |>
      countrycode(
        origin = "iso3c",
        destination = "iso2c"
      ) |>
      str_to_lower()
  )

hoc_plot <- df_hoc |>
  filter(country %in% both_cnt) |>
  mutate(
    country = country |>
      str_replace("den", "dnk") |>
      str_replace("ger", "deu") |>
      countrycode(
        origin = "iso3c",
        destination = "iso2c"
      ) |>
      str_to_lower()
  )


# Shared y-range
y_min <- floor(
  min(c(pop_plot$height_pop, hoc_plot$height_avg), na.rm = TRUE) / 5
) *
  5
y_max <- ceiling(
  max(c(pop_plot$height_pop, hoc_plot$height_avg), na.rm = TRUE) / 5
) *
  5

# ---- 4) PLOT ------------------------------------------------
col_pop_line <- "#0B57A4"
col_pop_point <- "#243365"
col_hoc_point <- "#87CF3E"
col_hoc_line <- "#34BE5B"

p <- ggplot() +
  # population baseline: visible but quieter
  geom_line(
    data = pop_plot,
    aes(x = cohort, y = height_pop, group = country),
    color = col_pop_line,
    linewidth = 0.7,
    alpha = 0.9
  ) +
  geom_point(
    data = pop_plot,
    aes(x = cohort, y = height_pop),
    color = col_pop_point,
    size = 1.8
  ) +
  geom_point(
    data = pop_plot,
    aes(x = cohort, y = height_pop),
    color = "white",
    size = 0.9
  ) +

  # hockey cohort means: saturated focal series
  geom_point(
    data = hoc_plot,
    aes(x = cohort, y = height_avg),
    color = col_hoc_point,
    size = 2,
    shape = 18
  ) +
  geom_smooth(
    data = hoc_plot,
    aes(x = cohort, y = height_avg),
    method = "lm",
    se = FALSE,
    color = col_hoc_line,
    linewidth = 0.9
  ) +
  geom_flag(
    data = hoc_plot,
    aes(country = country),
    size = 12,
    x = 1880,
    y = 190
  ) +

  facet_wrap(~country, ncol = 4) +
  coord_cartesian(ylim = c(y_min, y_max)) +
  scale_x_continuous(
    breaks = seq(1850, 2000, 50)
  ) +
  labs(
    title = "Height of <span style='color:#87CF3E'>ice hockey players</span> vs <span style='color:#243365'>male population</span>",
    x = "Birth cohort",
    y = "Height (cm)",
    caption = "Data: IIHF data on ice hockey players; Hatton & Bray (2010) male population data\n#30DayChartChallenge 2026 · Day 11 · Physical · Ilya Kashnitsky @ikashnitsky.phd\n"
  ) +
  theme(
    plot.caption.position = "plot",
    plot.title = ggtext::element_markdown(size = 22),
    plot.subtitle = ggtext::element_markdown(),
    legend.position = "none",
    strip.text = element_blank(),
    strip.background = element_blank(),
    panel.grid.minor = element_blank()
  )

# ---- 5) EXPORT ---------------------------------------------

ggsave(
  "out/11-physical.pdf",
  p,
  width = 8,
  height = 8,
  dpi = 300,
  bg = "#eeffff"
)
