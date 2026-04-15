# ============================================================
# #30DayChartChallenge 2026 · Day 14 · Trade
# "Land for Sale" — territorial cash purchases between nations
# Data: Wikipedia – List of territory purchased by a sovereign
#       nation from another sovereign nation
# ============================================================

library(tidyverse)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(ggrepel)
library(showtext)
library(ggtext)
library(scales)

# ── Mandatory setup ────────────────────────────────────────
devtools::source_gist("653e1040a07364ae82b1bb312501a184")
sysfonts::font_add_google("Atkinson Hyperlegible", family = "ah")
showtext::showtext_auto()
theme_set(theme_ik(base_family = "ah"))

# ── Data ───────────────────────────────────────────────────
# Source: https://en.wikipedia.org/wiki/List_of_territory_purchased_by_a_sovereign_nation_from_another_sovereign_nation
territories <- tribble(
  ~territory,                              ~buyer,           ~seller,       ~year, ~area_km2,  ~lon,   ~lat,
  "Isle of Man\n& Hebrides",               "Scotland",       "Norway",      1266,      8000,   -4.5,  56.0,
  "Dunkirk",                               "France",         "England",     1662,        44,    2.4,  51.0,
  "Estonia, Livonia,\nIngria & SE Finland","Russia",         "Sweden",      1721,    100000,   24.0,  59.0,
  "Saint Croix",                           "Denmark-Norway", "France",      1733,       210,  -64.7,  17.7,
  "Louisiana Territory",                   "USA",            "France",      1803,   2140000,  -92.0,  33.0,
  "Florida",                               "USA",            "Spain",       1819,    200000,  -81.0,  28.0,
  "Singapore",                             "UK",             "Johor",       1824,       728,  103.8,   1.3,
  "Danish India\n(Tranquebar etc.)",        "UK",             "Denmark",     1845,        44,   79.8,  11.0,
  "Mexican Cession",                       "USA",            "Mexico",      1848,   1360000, -119.0,  37.0,
  "Danish Gold Coast",                     "UK",             "Denmark",     1850,     12000,   -1.0,   5.0,
  "Gadsden Purchase",                      "USA",            "Mexico",      1853,     76800, -111.0,  32.5,
  "Saxe-Lauenburg",                        "Prussia",        "Austria",     1865,      1000,   10.5,  53.5,
  "Alaska",                                "USA",            "Russia",      1867,   1717856, -153.0,  64.0,
  "Dutch Gold Coast",                      "UK",             "Netherlands", 1872,      3000,   -2.0,   6.5,
  "Saint Barthélemy",                      "France",         "Sweden",      1878,        21,  -62.8,  17.9,
  "Philippines",                           "USA",            "Spain",       1898,    300000,  123.0,  12.0,
  "Caroline Islands",                      "Germany",        "Spain",       1899,      1160,  151.0,   6.5,
  "Palau",                                 "Germany",        "Spain",       1899,       488,  134.5,   7.5,
  "Mariana Islands",                       "Germany",        "Spain",       1899,      1000,  146.0,  15.0,
  "Cagayan Sulu\n& Sibutu",               "USA",            "Spain",       1900,       467,  118.0,   7.0,
  "Acre State",                            "Brazil",         "Bolivia",     1903,    142800,  -70.0,  -9.0,
  "Danish West Indies\n(US Virgin Is.)",  "USA",            "Denmark",     1916,       400,  -64.9,  18.3,
  "Jäniskoski-Niskakoski",                "USSR",           "Finland",     1947,       176,   28.5,  69.0,
  "Gwadar",                                "Pakistan",       "Oman",        1958,     15210,   62.3,  25.1,
  "Selfkant, Elten\n& Suderwick",         "W. Germany",     "Netherlands", 1963,        69,    6.2,  51.5
) %>%
  mutate(
    buyer_group = case_when(
      buyer %in% c("USA")                              ~ "United States",
      buyer %in% c("UK")                               ~ "United Kingdom",
      buyer %in% c("Russia", "USSR")                   ~ "Russia / USSR",
      buyer %in% c("Germany", "Prussia", "W. Germany") ~ "Germany / Prussia",
      buyer == "France"                                ~ "France",
      TRUE                                             ~ "Other"
    ),
    # label notable / large territories
    show_label = area_km2 > 70000 |
      territory %in% c(
        "Singapore", "Dunkirk", "Gwadar", "Saxe-Lauenburg",
        "Jäniskoski-Niskakoski", "Saint Barthélemy", "Saint Croix",
        "Danish West Indies\n(US Virgin Is.)", "Isle of Man\n& Hebrides"
      )
  )

# Okabe-Ito colorblind-safe categorical palette
buyer_pal <- c(
  "United States"     = "#0072B2",  # dark blue
  "United Kingdom"    = "#D55E00",  # vermillion
  "Russia / USSR"     = "#CC79A7",  # pink-purple
  "Germany / Prussia" = "#56B4E9",  # sky blue
  "France"            = "#009E73",  # bluish green
  "Other"             = "#E69F00"   # orange
)

# ── Spatial ────────────────────────────────────────────────
world   <- ne_countries(scale = "medium", returnclass = "sf")
rob_crs <- "+proj=robin +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m"

world_rob <- st_transform(world, rob_crs)

pts <- territories %>%
  st_as_sf(coords = c("lon", "lat"), crs = 4326) %>%
  st_transform(rob_crs)

# Helper: get Robinson coordinates for label_repel
get_rob_coords <- function(df) {
  df %>%
    st_as_sf(coords = c("lon", "lat"), crs = 4326) %>%
    st_transform(rob_crs) %>%
    mutate(
      rx = st_coordinates(.)[, 1],
      ry = st_coordinates(.)[, 2]
    ) %>%
    st_drop_geometry()
}

label_df <- territories %>%
  filter(show_label) %>%
  get_rob_coords()

# ── Visual constants ────────────────────────────────────────
bg         <- "#eeffff"   # theme_ik default bg
ocean_col  <- "#b8d4e5"
land_col   <- "#dbd6ce"
border_col <- "#c0bab3"
grid_col   <- alpha("white", 0.4)

# ── Plot ───────────────────────────────────────────────────
p <- ggplot() +
  geom_sf(data = world_rob, fill = land_col, color = border_col, linewidth = 0.2) +
  # shadow ring for depth
  geom_sf(
    data  = pts,
    aes(size = area_km2),
    shape = 21, fill = "grey20", color = NA, alpha = 0.15
  ) +
  # main bubbles
  geom_sf(
    data  = pts,
    aes(size = area_km2, fill = buyer_group),
    shape = 21, color = "white", alpha = 0.90, stroke = 0.5
  ) +
  # labels
  geom_label_repel(
    data = label_df,
    aes(
      x      = rx,
      y      = ry,
      label  = paste0(territory, "\n", year),
      color  = buyer_group
    ),
    size              = 3.0,
    family            = "ah",
    fontface          = "bold",
    fill              = alpha(bg, 0.85),
    label.size        = 0.2,
    label.r           = unit(0.12, "lines"),
    label.padding     = unit(0.20, "lines"),
    box.padding       = unit(0.50, "lines"),
    min.segment.length = 0.2,
    max.overlaps      = 30,
    seed              = 99
  ) +
  # ── Scales ────────────────────────────────────────────────
  scale_fill_manual(values = buyer_pal,  name = "Buyer") +
  scale_color_manual(values = buyer_pal, guide = "none") +
  scale_size_area(
    max_size = 24,
    breaks   = c(1000, 76800, 300000, 1717856),
    labels   = c("1K km²", "77K km²", "300K km²", "1.7M km²"),
    name     = "Territory area"
  ) +
  coord_sf(crs = rob_crs, expand = FALSE) +
  labs(
    title    = "**Land for Sale**: Territories purchased between sovereign nations",
    subtitle = "Day 14 · Trade | 25 cash transactions, 1266–1963 · Robinson projection",
    caption  = "Data: Wikipedia – List of territory purchased by a sovereign nation from another sovereign nation\n#30DayChartChallenge 2026 · Ilya Kashnitsky @ikashnitsky.phd\n"
  ) +
  theme_ik(base_family = "ah", base_size = 14, bg_color = bg) +
  theme(
    # override only what the map needs
    panel.background       = element_rect(fill = ocean_col, color = NA),
    panel.grid.major       = element_line(color = grid_col, linewidth = 0.3),
    plot.title             = ggtext::element_markdown(size = rel(1.4)),
    plot.subtitle          = ggtext::element_markdown(color = "grey35"),
    plot.caption.position  = "plot",
    plot.caption           = element_text(hjust = 0, color = "grey45", size = rel(0.72)),
    axis.text              = element_blank(),
    axis.ticks             = element_blank(),
    legend.position        = "bottom",
    legend.box             = "horizontal",
    legend.title           = element_text(face = "bold", size = rel(0.85)),
    legend.text            = element_text(size = rel(0.82)),
    legend.margin          = margin(t = 2, b = 2),
    plot.margin            = margin(10, 14, 8, 14)
  ) +
  guides(
    fill = guide_legend(
      override.aes = list(size = 5, alpha = 1),
      nrow = 1,
      order = 1
    ),
    size = guide_legend(nrow = 1, order = 2)
  )

# ── Export ─────────────────────────────────────────────────
dir.create("out", showWarnings = FALSE)
ggsave(
  "out/14-trade.pdf",
  plot   = p,
  width  = 14,
  height = 9,
  dpi    = 300,
  bg     = bg         # must match theme_ik bg_color
)

message("✓ Saved  out/14-trade.pdf")

# ── Variation idea ─────────────────────────────────────────
# Next: facet_wrap(~buyer_group) — one panel per buyer nation,
# showing only *their* acquisitions on a small-multiples world map.
# Or: geom_bump()-style timeline encoding year on x-axis,
# with territory area on y-axis and buyer as color.
