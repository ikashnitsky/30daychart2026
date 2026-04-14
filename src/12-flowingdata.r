# ..........................................................
# 2026-04-12 -- 30DayChartChallenge
# Day 12: Flowing Data -----------
# Ilya Kashnitsky, ilya.kashnitsky@gmail.com
# ..........................................................

library(tidyverse)
library(babynames)
library(ggstream)
library(ggtext)
library(patchwork)
library(showtext)
library(sysfonts)
library(devtools)

# ── Mandatory setup ────────────────────────────────────────────────────────
devtools::source_gist("653e1040a07364ae82b1bb312501a184")
sysfonts::font_add_google("Atkinson Hyperlegible", family = "ah")
showtext::showtext_auto()
theme_set(theme_ik(base_size = 14, base_family = "ah"))

# ── Data ───────────────────────────────────────────────────────────────────
focus_years <- 1950:2022

# Top 10 names per sex by total births in the focus window
get_top <- function(s, n = 10) {
  babynames |>
    filter(sex == s, year %in% focus_years) |>
    group_by(name) |>
    summarise(total = sum(n), .groups = "drop") |>
    slice_max(total, n = n, with_ties = FALSE) |>
    pull(name)
}

top_f <- get_top("F")
top_m <- get_top("M")

# Peak year per name → drives color ordering (early peak = one end of spectrum)
get_peak <- function(s, nms) {
  babynames |>
    filter(sex == s, year %in% focus_years, name %in% nms) |>
    group_by(name) |>
    slice_max(n, n = 1, with_ties = FALSE) |>
    ungroup() |>
    select(name, peak_year = year)
}

peak_f <- get_peak("F", top_f)
peak_m <- get_peak("M", top_m)

# Build stream-ready data: complete year×name grid, fill 0s, order by peak year
build_stream <- function(s, nms, peaks) {
  babynames |>
    filter(sex == s, year %in% focus_years, name %in% nms) |>
    select(year, name, n) |>
    complete(year = focus_years, name = nms, fill = list(n = 0)) |>
    left_join(peaks, by = "name") |>
    # factor ordered by peak_year so viridis color = temporal arc
    mutate(name = fct_reorder(name, peak_year))
}

df_f <- build_stream("F", top_f, peak_f)
df_m <- build_stream("M", top_m, peak_m)

# ── Theme tweak for stream panels ─────────────────────────────────────────
stream_theme <- theme(
  legend.position = "none",
  axis.text.y = element_blank(),
  axis.ticks.y = element_blank(),
  panel.grid.major.y = element_blank(),
  panel.grid.minor = element_blank(),
  panel.grid.major.x = element_line(color = "white", linewidth = 0.4),
  plot.title = element_text(
    size = 16,
    face = "bold",
    family = "ah",
    margin = margin(b = 2)
  )
)

x_scale <- scale_x_continuous(
  breaks = seq(1950, 2020, 10),
  labels = c("1950", "'60", "'70", "'80", "'90", "'00", "'10", "'20"),
  expand = expansion(add = 0.5)
)

# ── Girls panel: plasma palette (yellow→orange→pink→purple = old→new) ─────
p_f <- ggplot(df_f, aes(year, n, fill = name, label = name)) +
  geom_stream(
    type = "mirror",
    bw = 0.80,
    extra_span = 0.15,
    color = NA,
    alpha = 0.92
  ) +
  geom_stream_label(
    type = "mirror",
    bw = 0.80,
    extra_span = 0.15,
    n_grid = 1000,
    size = 3.8,
    family = "ah",
    fontface = "bold",
    color = "white"
  ) +
  scale_fill_viridis_d(
    option = "C",
    direction = -1, # plasma, warm→cool
    begin = 0.1,
    end = 0.92
  ) +
  x_scale +
  labs(title = "Girls", x = NULL, y = NULL) +
  stream_theme

# ── Boys panel: viridis palette (purple→teal→green = old→new) ─────────────
p_m <- ggplot(df_m, aes(year, n, fill = name, label = name)) +
  geom_stream(
    type = "mirror",
    bw = 0.80,
    extra_span = 0.15,
    color = NA,
    alpha = 0.92
  ) +
  geom_stream_label(
    type = "mirror",
    bw = 0.80,
    extra_span = 0.15,
    n_grid = 1000,
    size = 3.8,
    family = "ah",
    fontface = "bold",
    color = "white"
  ) +
  scale_fill_viridis_d(
    option = "D",
    direction = -1, # viridis, purple→yellow
    begin = 0.05,
    end = 0.90
  ) +
  x_scale +
  labs(title = "Boys", x = NULL, y = NULL) +
  stream_theme

# ── Combine ────────────────────────────────────────────────────────────────
p_final <- p_f /
  p_m +
  plot_annotation(
    title = "**Name Waves: the Ebb and Flow of American Baby Names**",
    subtitle = "Stream width reflects total births; top 10 names per sex, USA 1950–2022.\nColor follows each name's era: early-peak names are warm, recent-peak names cool",
    caption = "Data: US Social Security Administration via {babynames}\n#30DayChartChallenge 2026 · Day 12 · FlowingData · Ilya Kashnitsky @ikashnitsky.phd\n",
    theme = theme(
      plot.title = ggtext::element_markdown(size = 24, family = "ah"),
      plot.subtitle = element_text(
        size = 16,
        family = "ah",
        color = "#269292",
        lineheight = 1
      ),
      plot.caption = element_text(
        size = 9,
        family = "ah",
        color = "#269292",
        lineheight = 1
      ),
      plot.caption.position = "plot",
      plot.margin = margin(16, 16, 10, 16)
    )
  )

ggsave(
  "out/12-flowingdata.pdf",
  p_final,
  width = 10,
  height = 10,
  dpi = 300,
  bg = "#eeffff"
)
