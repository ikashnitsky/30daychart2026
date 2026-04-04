#===============================================================================
# #30DayChartChallenge 2026 — Day 4: Slope
# Life expectancy 1960 → 2020 by country
# Data: {owidapi} R package
#===============================================================================

library(tidyverse)
library(owidapi)
library(ggflags)
library(cowplot)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(countrycode)
library(showtext)
library(ggtext)

# ── Mandatory setup ────────────────────────────────────────────────────────────
devtools::source_gist("653e1040a07364ae82b1bb312501a184")
sysfonts::font_add_google("Atkinson Hyperlegible", family = "ah")
showtext::showtext_auto()
theme_set(theme_ik(base_size = 14, base_family = "ah"))

# ── Palette — gapminder.org canonical region colors ───────────────────────────
gap_colors <- c(
  "Africa" = "#ff5872",
  "Americas" = "#00d5e9",
  "Asia" = "#7feb02",
  "Europe" = "#3B90FF",
  "Oceania" = "#E37900"
)

# ── Data prep ─────────────────────────────────────────────────────────────────
# Get data from Our World in Data, use 1960 and 2020
owid_data <- owid_get(chart_id = "life-expectancy")

df <- owid_data %>%
  filter(year %in% c(1960, 2020)) %>%
  rename(country = entity_name, lifeExp = life_expectancy_0) %>%
  filter(country != "Antarctica") %>%
  mutate(
    iso2 = countrycode(country, "country.name", "iso2c", warn = FALSE) %>%
      tolower(),
    continent = countrycode(
      country,
      "country.name",
      "continent",
      warn = FALSE
    ) %>%
      as.character()
  ) %>%
  drop_na(iso2, continent) %>%
  filter(iso2 %in% names(ggflags::lflags))

# compute gain for ordering highlight labels and ensure data in both years
gain_df <- df %>%
  pivot_wider(
    id_cols = c(country, continent, iso2),
    names_from = year,
    values_from = lifeExp,
    names_prefix = "y"
  ) %>%
  drop_na(y1960, y2020) %>%
  mutate(gain = y2020 - y1960)

df <- df %>% inner_join(gain_df %>% select(country, gain), by = "country")

df_1960 <- df %>%
  filter(year == 1960) %>%
  arrange(desc(lifeExp)) %>%
  mutate(y_pos = seq(87, 10, length.out = n()))
df_2020 <- df %>%
  filter(year == 2020) %>%
  arrange(desc(lifeExp)) %>%
  mutate(y_pos = seq(87, 10, length.out = n()))

# ── Main slope chart ───────────────────────────────────────────────────────────
main <- df %>%
  ggplot(aes(
    x = factor(year, levels = c(1960, 2020)),
    y = lifeExp,
    group = country,
    color = continent
  )) +
  geom_hline(
    yintercept = seq(10, 85, 5),
    color = "#ccffff",
    linewidth = 2.8,
    alpha = .75
  ) +
  geom_line(linewidth = .35, alpha = .5) +
  geom_flag(
    data = df_1960,
    aes(country = iso2),
    size = 3.5,
    position = position_jitter(width = 0.08, height = 0.6, seed = 42)
  ) +
  geom_text(
    data = df_1960,
    aes(x = 0.38, y = y_pos, label = str_to_lower(country)),
    hjust = 0,
    size = 2.1,
    family = "ah",
    fontface = "bold",
    alpha = 3 / 4
  ) +
  geom_flag(
    data = df_2020,
    aes(country = iso2),
    size = 3.5,
    position = position_jitter(width = 0.08, height = 0.6, seed = 42)
  ) +
  geom_text(
    data = df_2020,
    aes(x = 2.62, y = y_pos, label = str_to_lower(country)),
    hjust = 1,
    size = 2.1,
    family = "ah",
    fontface = "bold",
    alpha = 3 / 4
  ) +
  scale_color_manual(values = gap_colors) +
  scale_y_continuous(
    limits = c(10, 87),
    breaks = seq(30, 85, 10),
    minor_breaks = NULL,
    expand = expansion(add = 0)
  ) +
  scale_x_discrete(
    position = "top",
    expand = expansion(add = .62),
    labels = c("1960", "2020")
  ) +
  coord_cartesian(clip = "off") +
  theme(
    legend.position = "none",
    panel.grid = element_blank(),
    axis.text.x = element_text(face = "bold", size = 18, family = "ah"),
    axis.title.y = element_text(size = 11, color = "#006666"),
    plot.title = ggtext::element_markdown(),
    plot.subtitle = ggtext::element_markdown(),
    plot.caption.position = "plot",
    plot.margin = margin(10, 20, 10, 15)
  ) +
  labs(
    title = "**Life expectancy increased in every country**",
    subtitle = "#30DayChartChallenge Day 4 · Slope | 1960 --> 2020 · Lines colored by world region",
    caption = "Data: {owidapi} R package\n#30DayChartChallenge 2026 · Ilya Kashnitsky @ikashnitsky.phd\n",
    x = NULL,
    y = "Life expectancy at birth (years)"
  )

# ── Inset world map ───────────────────────────────────────────────────────────
world <- ne_countries(scale = "medium", returnclass = "sf") %>%
  filter(continent != "Antarctica") %>%
  mutate(
    continent_gap = case_when(
      continent %in% c("North America", "South America") ~ "Americas",
      continent == "Africa" ~ "Africa",
      continent == "Asia" ~ "Asia",
      continent == "Europe" ~ "Europe",
      continent == "Oceania" ~ "Oceania",
      TRUE ~ NA_character_
    )
  )

inset <- world %>%
  ggplot() +
  geom_sf(aes(fill = continent_gap), color = NA) +
  scale_fill_manual(values = gap_colors, na.value = "#00444455") +
  coord_sf(crs = st_crs("+proj=robin")) +
  theme_void() +
  theme(legend.position = "none")

# ── Assemble with cowplot ──────────────────────────────────────────────────────
out <- ggdraw(main) +
  draw_plot(inset, x = .18, y = .01, width = .60, height = .26)

ggsave(
  "out/04-slope.pdf",
  plot = out,
  width = 9,
  height = 12,
  dpi = 300,
  bg = "#eeffff"
)
