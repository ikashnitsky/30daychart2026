# ==============================================================================
# Day 11 · Physical | #30DayChartChallenge 2026
# Option B — "The Shrinking Pool, The Rising Bar" (timeline 2001–2024)
# Two geomtextpath lines per birth half (H1 = Jan–Jun / H2 = Jul–Dec):
#   • left  y-axis: share of H2 players (RAE trend)
#   • right y-axis: mean within-cohort height deviation  H2 − H1 (selection Δ)
# Dual-axis via sec_axis; lines coloured H1 vs H2.
# ==============================================================================

library(tidyverse)
library(geomtextpath)
library(ggtext)
library(showtext)

devtools::source_gist("653e1040a07364ae82b1bb312501a184")
sysfonts::font_add_google("Atkinson Hyperlegible", family = "ah")
showtext::showtext_auto()
theme_set(theme_ik(base_family = "ah"))

# ── 1. DATA ────────────────────────────────────────────────────────────────────
dat_raw <- read_csv("nhl_rae_players.csv")   # ← adjust filename

dat <- dat_raw |>
  mutate(
    birth_half = if_else(birth_quarter %in% c("Q1", "Q2"), "H1 (Jan–Jun)", "H2 (Jul–Dec)") |>
                   factor(levels = c("H1 (Jan–Jun)", "H2 (Jul–Dec)")),
    position   = factor(position, levels = c("Goalie", "Defender", "Forward"))
  ) |>
  group_by(birth_year, position) |>
  mutate(height_dev = height_cm - mean(height_cm, na.rm = TRUE)) |>
  ungroup()

# ── 2. SEASON-LEVEL AGGREGATION ───────────────────────────────────────────────
# Share of each half per season (across all positions)
share_ts <- dat |>
  count(season, birth_half) |>
  group_by(season) |>
  mutate(share = n / sum(n) * 100) |>
  ungroup()

# Mean height deviation per half × season
hdev_ts <- dat |>
  group_by(season, birth_half) |>
  summarise(mean_hdev = mean(height_dev, na.rm = TRUE), .groups = "drop")

# Delta: H2 height advantage over H1 within each season
hdev_delta <- hdev_ts |>
  pivot_wider(names_from = birth_half, values_from = mean_hdev,
              names_repair = "universal") |>
  rename(H1 = `H1 (Jan–Jun)`, H2 = `H2 (Jul–Dec)`) |>
  mutate(delta = H2 - H1) |>
  select(season, delta)

# Join for dual-axis approach: rescale delta onto share scale
# We'll overlay delta as a second set of lines using a secondary axis transform
# scale factor: map delta range onto share range
share_range <- range(share_ts$share, na.rm = TRUE)
delta_range  <- range(hdev_delta$delta, na.rm = TRUE)

rescale_delta <- function(x) {
  (x - delta_range[1]) / diff(delta_range) * diff(share_range) + share_range[1]
}
inverse_rescale <- function(x) {
  (x - share_range[1]) / diff(share_range) * diff(delta_range) + delta_range[1]
}

hdev_delta <- hdev_delta |>
  mutate(delta_scaled = rescale_delta(delta))

# Colour palette: viridis-safe H1 / H2
h_colors <- c("H1 (Jan–Jun)" = "#2172B4", "H2 (Jul–Dec)" = "#D94F01")
delta_color <- "#5B4A72"

# ── 3. PLOT ───────────────────────────────────────────────────────────────────
p <- ggplot() +
  # Share lines — H1 and H2
  geomtextpath::geom_textsmooth(
    data    = share_ts,
    aes(x = season, y = share, color = birth_half, label = birth_half),
    method  = "loess", span = 0.55,
    se      = FALSE, linewidth = 1.1,
    size    = 3.6, family = "ah", fontface = "bold",
    hjust   = 0.18, text_smoothing = 35
  ) +
  # Delta line (H2 height advantage, rescaled to left axis)
  geomtextpath::geom_textsmooth(
    data    = hdev_delta,
    aes(x = season, y = delta_scaled,
        label = "H2 height advantage over H1 (cm, right axis)"),
    color   = delta_color, method = "loess", span = 0.55,
    linetype = "longdash", linewidth = 0.9, se = FALSE,
    size    = 3.0, family = "ah", hjust = 0.65, text_smoothing = 30
  ) +
  scale_color_manual(values = h_colors, guide = "none") +
  scale_x_continuous(breaks = seq(2002, 2024, 4)) +
  scale_y_continuous(
    name   = "Share of roster by birth half (%)",
    labels = scales::label_percent(scale = 1, suffix = " %"),
    sec.axis = sec_axis(
      transform = inverse_rescale,
      name      = "Mean height deviation H2 − H1 (cm)",
      labels    = scales::label_number(suffix = " cm", style_positive = "plus")
    )
  ) +
  labs(
    title    = "**The Shrinking Pool, The Rising Bar**",
    subtitle = "Day 11 · Physical | As H2 players shrink as a share of NHL rosters, those who make it grow taller relative to peers",
    x        = "NHL season",
    caption  = "Data: Hockey-Reference NHL rosters 2001–2024\n#30DayChartChallenge 2026 · Ilya Kashnitsky @ikashnitsky.phd\n"
  ) +
  theme(
    plot.title             = ggtext::element_markdown(size = 15),
    plot.subtitle          = ggtext::element_markdown(size = 10, color = "grey45"),
    plot.caption.position  = "plot",
    axis.title.y.right     = element_text(color = delta_color, size = 11),
    axis.text.y.right      = element_text(color = delta_color),
    axis.ticks.y.right     = element_line(color = delta_color)
  )

# ── 4. EXPORT ─────────────────────────────────────────────────────────────────
dir.create("out", showWarnings = FALSE)
ggsave("out/11B-shrinking-pool.pdf",
       p, width = 10, height = 6.5, dpi = 300, bg = "#eeffff")
ggsave("out/11B-shrinking-pool.png",
       p, width = 10, height = 6.5, dpi = 300, bg = "#eeffff")
message("✔ Saved out/11B-shrinking-pool.pdf / .png")
