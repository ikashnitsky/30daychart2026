# ══════════════════════════════════════════════════════════════════════════════
# #30DayChartChallenge 2026 · Day 7 · Multiscale
# World map: Life expectancy × Total fertility rate (biscale)
# Custom legend with data-density tertile cutoffs + annotations
# ══════════════════════════════════════════════════════════════════════════════

# ── packages ----

library(tidyverse)
library(countrycode)
library(biscale)
library(rnaturalearth)
library(rnaturalearthdata)
library(sf)
library(rmapshaper)
library(patchwork)
library(ggtext)
library(wpp2024)

# ── mandatory setup ────────────────────────────────────────────────────────────
devtools::source_gist("653e1040a07364ae82b1bb312501a184")
sysfonts::font_add_google("Atkinson Hyperlegible", family = "ah")
showtext::showtext_auto()
theme_set(theme_ik(base_family = "ah"))

# ── 1. fetch WPP data ─────────────────────────────────────────────────────────
data(e01dt) # life expectancy at birth
data(tfr1dt) # total fertility rate

df <- left_join(
  e01dt |>
    filter(year == 2023) |>
    select(country_code, name, e0 = e0B),
  tfr1dt |>
    filter(year == 2023) |>
    select(country_code, name, tfr)
) |>
  mutate(
    iso3c = country_code |>
      countrycode(origin = "iso3n", destination = "iso3c")
  ) |>
  drop_na(iso3c)

tfr_cuts <- quantile(df$tfr, probs = c(0.33, 0.66))
e0_cuts <- quantile(df$e0, probs = c(0.33, 0.66))

# categorize e0 and TFR in terciles
df_bi <- df |>
  mutate(
    tfr_grp = case_when(
      tfr <= tfr_cuts[1] ~ 1L,
      tfr <= tfr_cuts[2] ~ 2L,
      TRUE ~ 3L
    ),
    e0_grp = case_when(
      e0 <= e0_cuts[1] ~ 1L,
      e0 <= e0_cuts[2] ~ 2L,
      TRUE ~ 3L
    ),
    bi_class = paste0(tfr_grp, "-", e0_grp) # biscale "x-y" convention
  )

# ── 3. palette ─────────────────────────────────────────────────────────────────
PAL <- "PurpleOr"
pal_colors <- bi_pal(PAL, dim = 3, preview = FALSE)

# bi_pal("PurpleOr", dim = 3)

# ── 4. world map ───────────────────────────────────────────────────────────────
world <- ne_countries(scale = "medium", returnclass = "sf") |>
  filter(iso_a3 != "ATA") |> # drop Antarctica
  left_join(select(df_bi, bi_class, iso3c), by = c("iso_a3_eh" = "iso3c"))

map_plt <- ggplot(world) +
  geom_sf(
    aes(fill = bi_class),
    color = NA,
    show.legend = FALSE
  ) +
  geom_sf(
    data = world |> ms_innerlines(),
    color = "#ccffff",
    linewidth = 0.5,
    show.legend = FALSE
  ) +
  scale_fill_manual(values = pal_colors, na.value = "grey82", drop = FALSE) +
  coord_sf(
    crs = "+proj=robin", # Robinson projection
    xlim = c(-180, 180),
    ylim = c(-60, 85),
    expand = FALSE,
    default_crs = sf::st_crs(4326) # limits in WGS84 degrees
  ) +
  labs(
    title = "Humanity in transition, <span style = 'color: #F06292;'>Demographic Transition</span>",
    subtitle = "Life expectancy (y) × Total fertility rate (x), 2023",
    caption = "Data: World Population Prospects via {wpp2024}\nDemographic Transition is the dominant theory in demography, for details check Our World in Data\n#30DayChartChallenge 2026 · Day 7 · Multiscale · Ilya Kashnitsky @ikashnitsky.phd\n"
  ) +
  theme_ik(base_family = "ah", base_size = 14) +
  theme(
    plot.title = ggtext::element_markdown(),
    plot.subtitle = ggtext::element_markdown(size = 16),
    plot.caption.position = "plot",
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank(),
    panel.background = element_rect(fill = "#c8dff0", color = NA) # ocean blue
  )

# ── 5. custom biscale legend (built from scratch, not using bi_legend()) ───────

# helper: perceived luminance to decide text color
compute_lum <- function(hex) {
  v <- col2rgb(hex) / 255
  0.299 * v[1] + 0.587 * v[2] + 0.114 * v[3]
}

bi_pcts <- df_bi |>
  count(bi_class, tfr_grp, e0_grp) |>
  mutate(
    pct = n / sum(n) * 100,
    pct_label = sprintf("%.1f%%", pct),
    fill_col = pal_colors[bi_class],
    lum = sapply(fill_col, compute_lum),
    # dark text on light tiles, white text on dark tiles
    text_col = ifelse(lum > 0.45, "grey20", "white")
  )

# axis tick labels with actual data-value cutoffs
x_labs <- c(
  sprintf("≤%.1f", tfr_cuts[1]),
  sprintf("%.1f–%.1f", tfr_cuts[1], tfr_cuts[2]),
  sprintf(">%.1f", tfr_cuts[2])
)
y_labs <- c(
  sprintf("≤%d yr", round(e0_cuts[1])),
  sprintf("%d–%d yr", round(e0_cuts[1]), round(e0_cuts[2])),
  sprintf(">%d yr", round(e0_cuts[2]))
)

legend_plt <- ggplot(
  bi_pcts,
  aes(x = tfr_grp, y = e0_grp)
) +
  geom_tile(aes(fill = bi_class), color = NA, show.legend = FALSE) +
  scale_fill_manual(values = pal_colors) +
  coord_equal() +
  # dashed white lines marking the data-density cut boundaries
  geom_vline(
    xintercept = c(1.5, 2.5),
    color = "white",
    linewidth = 0.7,
    linetype = "dashed"
  ) +
  geom_hline(
    yintercept = c(1.5, 2.5),
    color = "white",
    linewidth = 0.7,
    linetype = "dashed"
  ) +
  # percentage of countries in each of the 9 biscale cells
  geom_text(
    aes(label = pct_label, color = text_col),
    size = 3.2,
    fontface = "bold",
    family = "ah"
  ) +
  scale_color_identity() +
  scale_x_continuous(
    breaks = 1:3,
    labels = x_labs,
    limits = c(0.5, 3.5),
    expand = c(0, 0)
  ) +
  scale_y_continuous(
    breaks = 1:3,
    labels = y_labs,
    limits = c(0.5, 3.5),
    expand = c(0, 0)
  ) +
  labs(
    x = "Total fertility rate -->",
    y = "Life expectancy at birth -->",
    title = "Biscale legend\n(% countries per cell)"
  ) +
  theme_ik(base_family = "ah", base_size = 8) +
  theme(
    plot.title = element_text(size = 7.5, lineheight = 1.2),
    axis.text = element_text(size = 6.5),
    axis.title = element_text(size = 7, face = "bold"),
    panel.background = element_blank(),
    plot.background = element_blank(),
    panel.grid = element_blank()
  )

# ── 6. assemble: inset legend into map, bottom-left corner ────────────────────
final <- map_plt +
  inset_element(
    legend_plt,
    left = -0.12,
    bottom = -0.01,
    right = 0.33,
    top = 0.49
  ) &
  theme(plot.background = element_rect(fill = "#eeffff", color = NA))

ggsave(
  "out/07-multiscale.pdf",
  final,
  width = 12,
  height = 7,
  dpi = 300,
  bg = "#eeffff"
)
