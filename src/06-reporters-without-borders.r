# ..........................................................
# 2026-04-06 -- 30DayChartChallenge
# Day 6: Reporters Without Borders                -----------
# Ilya Kashnitsky, ilya.kashnitsky@gmail.com
# ..........................................................

# ─── libraries ────────────────────────────────────────────────────────────────
library(tidyverse)
library(magrittr)
library(RKaggle)
library(countrycode)
library(ggbeeswarm) # geom_quasirandom()
library(ggtext)
library(showtext)

# ─── mandatory setup ──────────────────────────────────────────────────────────
devtools::source_gist("653e1040a07364ae82b1bb312501a184")
sysfonts::font_add_google("Atkinson Hyperlegible", family = "ah")
showtext::showtext_auto()
# theme_set(theme_ik(base_family = "ah"))

# get and transform the data ---------------------------------------------

rsf <- get_dataset(
  "vladyslavhubanov/summary-data-from-reporter-without-the-borders"
) |>
  extract2(2)

df <- rsf |>
  # filter out problematic non-harmonized years
  filter(year |> is_greater_than(2012)) |>
  transmute(
    year,
    iso,
    country,
    rank,
    score,
    continent = coalesce(
      countrycode(iso, "iso3c", "continent", warn = FALSE),
      countrycode(iso, "iso2c", "continent", warn = FALSE),
      countrycode(iso, "country.name", "continent", warn = FALSE)
    )
  ) |>
  drop_na(continent)

# ─── year-level average for box fill ─────────────────────────────────────────
year_avg <- df |>
  group_by(year) |>
  summarise(
    avg_score = mean(score, na.rm = TRUE),
    .groups = "drop"
  )

df_plot <- df |>
  left_join(year_avg, by = "year") |>
  mutate(
    year = year |> as_factor() |> fct_rev(),
  )

# ─── gapminder-inspired bright continent palette (for dark bg) ────────────────
continent_pal <- c(
  "Africa" = "#FF9500", # vivid amber
  "Americas" = "#00E676", # neon green
  "Asia" = "#FF1744", # bright red
  "Europe" = "#40C4FF", # sky blue
  "Oceania" = "#D500F9" # electric purple
)

# ─── plot ─────────────────────────────────────────────────────────────────────
p <- ggplot(df_plot, aes(y = year, x = score)) +

  # layer 1: boxes — fill encodes yearly avg press freedom; no outlier marks
  geom_boxplot(
    aes(fill = avg_score),
    notch = TRUE,
    notchwidth = 0.5,
    outlier.shape = NA,
    alpha = 0.35,
    color = "grey75",
    linewidth = 0.4
  ) +

  # layer 2: raw country points — color encodes continent
  geom_quasirandom(
    aes(color = continent),
    size = 1.0,
    alpha = 0.72,
    bandwidth = 0.7
  ) +

  scale_fill_viridis_c(
    name = "Global average score",
    option = "H",
    begin = .3,
    end = .8,
    direction = -1,
    guide = guide_colorbar(
      barwidth = 25,
      barheight = 1,
      title.position = "top",
      title.hjust = 0.5
    )
  ) +

  scale_color_manual(
    values = continent_pal,
    name = NULL,
    guide = guide_legend(
      override.aes = list(size = 3.5, alpha = 1),
      nrow = 1,
      position = "top"
    )
  ) +

  scale_x_continuous(expand = expansion(mult = 0.02), position = "top") +

  theme_minimal(base_size = 14) +
  # manually recreating gark theme
  theme(
    text = element_text(family = "ah", color = "grey85"),
    plot.background = element_rect(fill = "#1a1a1a", color = NA),
    panel.background = element_rect(fill = "#1a1a1a", color = NA),
    panel.grid.major.x = element_line(color = "grey22", linewidth = 0.25),
    panel.grid.major.y = element_line(color = "grey22", linewidth = 0.15),
    panel.grid.minor = element_blank(),
    axis.text.y = element_text(size = 9.5, color = "grey70"),
    axis.text.x = element_text(size = 9, color = "grey70"),
    axis.title = element_text(color = "grey85"),
    plot.title = ggtext::element_markdown(
      size = 24,
      lineheight = 1.2,
      margin = margin(b = 4),
      color = "grey95"
    ),
    plot.subtitle = ggtext::element_markdown(
      size = 14,
      color = "grey60",
      lineheight = 1.35,
      margin = margin(b = 10)
    ),
    plot.caption = element_text(size = 9, color = "grey45", lineheight = 1.2),
    plot.caption.position = "plot",
    legend.position = "bottom",
    legend.box = "vertical",
    legend.spacing.y = unit(0.3, "cm"),
    legend.margin = margin(t = 5),
    legend.background = element_rect(fill = "#1a1a1a", color = NA),
    legend.text = element_text(color = "grey70"),
    legend.title = element_text(color = "grey85"),
    plot.margin = margin(20, 25, 15, 15)
  ) +

  labs(
    title = "**Press Freedom** is declining globally",
    subtitle = "Day 06 · Reporters Without Borders | RSF World Press Freedom Index",
    caption = "Data: RSF World Press Freedom Index via Kaggle (vladyslavhubanov)\n#30DayChartChallenge 2026 · Ilya Kashnitsky @ikashnitsky.phd\n",
    y = NULL,
    x = "Press Freedom Score"
  )

# ─── save ─────────────────────────────────────────────────────────────────────
ggsave(
  "out/06-rsf-press-freedom.pdf",
  p,
  width = 8,
  height = 8,
  dpi = 300,
  bg = "#1a1a1a"
)
