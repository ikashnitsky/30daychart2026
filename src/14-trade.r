# ..........................................................
# 2026-04-14 -- 30DayChartChallenge
# Day 14: Trade | From diesel-dominant to EV-first in one decade -----------
# Ilya Kashnitsky, ilya.kashnitsky@gmail.com
# ..........................................................

library(tidyverse)
library(dkstat)
library(ggstream)
library(ggtext)
library(showtext)
library(patchwork)
library(scales)

devtools::source_gist("653e1040a07364ae82b1bb312501a184")
sysfonts::font_add_google("Atkinson Hyperlegible", family = "ah")
showtext::showtext_auto()
theme_set(theme_ik(base_size = 14, base_family = "ah"))

# ── 1. BIL51 · New registrations by propellant ───────────────────────────────
bil51_raw <- dst_get_data(
  table = "BIL51",
  EJER = "Total",
  DRIV = c("Petrol", "Diesel", "Electricity", "Hybrid/petrol", "Hybrid/diesel"),
  Tid = "*",
  lang = "en"
)

df_stream <- bil51_raw |>
  mutate(
    year = lubridate::year(TID),
    fuel = case_when(
      str_detect(DRIV, "Electricity") ~ "Battery Electric",
      str_detect(DRIV, "Petrol") ~ "Petrol",
      str_detect(DRIV, "Diesel") ~ "Diesel",
      str_detect(DRIV, "Hybrid/petrol") ~ "Plug-in Hybrid",
      str_detect(DRIV, "Hybrid/diesel") ~ "Plug-in Hybrid"
    )
  ) |>
  group_by(year, fuel) |>
  summarise(n = sum(value, na.rm = TRUE), .groups = "drop") |>
  filter(year >= 2011, year <= 2025) |>
  mutate(
    fuel = fct_relevel(
      fuel,
      "Diesel",
      "Petrol",
      "Plug-in Hybrid",
      "Battery Electric"
    )
  )

# ── 2. KN8Y · Find EV CN codes dynamically, then pull import values ──────────
meta_kn8 <- dst_meta("KN8Y", lang = "en")

# Locate 8703.80 / 8703.90 electric passenger-car codes in the VARE variable
vare_vals <- meta_kn8$variables |>
  filter(id == "VARE") |>
  pull(values) |>
  pluck(1) # first (only) list element

ev_codes <- vare_vals |>
  filter(
    str_detect(id, "^8703(80|90)") &
      str_detect(tolower(text), "electric|only electric")
  ) |>
  pull(id)

message("EV CN codes found: ", paste(ev_codes, collapse = ", "))

# Pull import values DKK by country
kn8_raw <- dst_get_data(
  table = "KN8Y",
  INDUD = "Imports",
  VARE = ev_codes, # all matching BEV codes
  LAND = "*",
  ENHED = "DKK",
  Tid = as.character(2019:2024),
  lang = "en"
)

df_countries <- kn8_raw |>
  filter(LAND != "Total") |>
  group_by(LAND) |>
  summarise(total_dkk = sum(value, na.rm = TRUE), .groups = "drop") |>
  filter(total_dkk > 0) |>
  slice_max(total_dkk, n = 12) |>
  mutate(
    LAND = fct_reorder(LAND, total_dkk),
    label = paste0(round(total_dkk / 1e9, 1), " B")
  )

# ── Palette (colorblind-safe, viridis-derived) ────────────────────────────────
fuel_pal <- c(
  "Battery Electric" = "#1b7837",
  "Plug-in Hybrid" = "#7fbf7b",
  "Petrol" = "#d6604d",
  "Diesel" = "#bdbdbd"
)

# ── Panel A · Proportional stream chart ──────────────────────────────────────
p_stream <- df_stream |>
  ggplot(aes(year, n, fill = fuel, color = fuel)) +
  ggstream::geom_stream(type = "proportional", bw = 0.75, alpha = 0.88) +
  # label the dominant zones at endpoints
  annotate(
    "text",
    x = 2025.3,
    y = 0.10,
    label = "BEV",
    color = "#1b7837",
    fontface = "bold",
    size = 4,
    hjust = 0,
    family = "ah"
  ) +
  annotate(
    "text",
    x = 2025.3,
    y = 0.30,
    label = "Hybrid",
    color = "#5aae61",
    size = 3.5,
    hjust = 0,
    family = "ah"
  ) +
  annotate(
    "text",
    x = 2012,
    y = 0.35,
    label = "Petrol",
    color = "#d6604d",
    size = 3.5,
    family = "ah"
  ) +
  annotate(
    "text",
    x = 2012,
    y = 0.78,
    label = "Diesel",
    color = "#888888",
    size = 3.5,
    family = "ah"
  ) +
  scale_fill_manual(values = fuel_pal, guide = "none") +
  scale_color_manual(values = fuel_pal, guide = "none") +
  scale_x_continuous(breaks = seq(2011, 2025, 2), expand = c(0.02, 0)) +
  scale_y_continuous(labels = percent_format(), expand = c(0, 0)) +
  labs(
    title = "**A** · Share of new registrations by fuel type, 2011–2025",
    subtitle = "Denmark manufactures no passenger cars — every new registration is an import",
    x = NULL,
    y = NULL
  ) +
  theme(
    plot.title = ggtext::element_markdown(),
    plot.subtitle = element_text(color = "grey40"),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    panel.grid.major.y = element_blank()
  )

# ── Panel B · Country-origin dotplot ─────────────────────────────────────────
p_countries <- df_countries |>
  ggplot(aes(total_dkk / 1e9, LAND)) +
  geom_segment(
    aes(x = 0, xend = total_dkk / 1e9, y = LAND, yend = LAND),
    color = "#cccccc",
    linewidth = 0.8
  ) +
  geom_point(color = "#1b7837", size = 3.5) +
  geom_text(
    aes(label = label),
    hjust = -0.25,
    size = 3.2,
    color = "#444444",
    family = "ah"
  ) +
  scale_x_continuous(
    labels = \(x) paste0(x, " B DKK"),
    expand = c(0.02, 0, 0.15, 0)
  ) +
  labs(
    title = "**B** · Where did Denmark import electric cars from? (2019–2024)",
    subtitle = "Cumulative import value, CN codes 8703.80 / 8703.90",
    x = NULL,
    y = NULL
  ) +
  theme(
    plot.title = ggtext::element_markdown(),
    plot.subtitle = element_text(color = "grey40"),
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_line(linetype = "dashed", color = "#dddddd")
  )

# ── Combine panels ────────────────────────────────────────────────────────────
p_final <- p_stream /
  p_countries +
  plot_layout(heights = c(1, 1.2)) +
  plot_annotation(
    title = "Denmark's electric car **import** revolution",
    subtitle = "Day 14 · Trade | From diesel-dominant to EV-first in one decade",
    caption = "Data: Statistics Denmark, Tables BIL51 & KN8Y · api.statbank.dk\n#30DayChartChallenge 2026 · Ilya Kashnitsky @ikashnitsky.phd\n",
    theme = theme(
      plot.title = ggtext::element_markdown(size = 22, family = "ah"),
      plot.subtitle = element_text(size = 13, color = "grey30", family = "ah"),
      plot.caption = element_text(
        size = 9,
        color = "grey50",
        family = "ah",
        hjust = 0
      ),
      plot.caption.position = "plot",
      plot.margin = margin(12, 12, 8, 12)
    )
  )

dir.create("out", showWarnings = FALSE)
ggsave(
  "out/14-trade.pdf",
  p_final,
  width = 10,
  height = 12,
  dpi = 300,
  bg = "#eeffff"
)
