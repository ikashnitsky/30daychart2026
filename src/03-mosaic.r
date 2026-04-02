# ..........................................................
# 2026-04-03 -- 30DayChartChallenge 2026 Edition
# Day 3 -- mosaic                -----------
# Ilya Kashnitsky, ilya.kashnitsky@gmail.com
# ..........................................................

library(tidyverse)
library(tidytuesdayR)
# pak::pak("haleyjeppson/ggmosaic")
library(ggmosaic)
library(showtext)
library(ggtext)
library(sysfonts)

devtools::source_gist("653e1040a07364ae82b1bb312501a184")
sysfonts::font_add_google("Atkinson Hyperlegible", family = "ah")
showtext::showtext_auto()
theme_set(theme_ik(base_size = 16, base_family = "ah"))

# ── Data ──────────────────────────────────────────────────────────────────────
tuesdata <- tidytuesdayR::tt_load('2026-01-13')
africa <- tuesdata$africa

# Country → 5-region lookup (exhaustive for ~55 African countries)
region_map <- c(
  # North
  "Algeria" = "North",
  "Egypt" = "North",
  "Libya" = "North",
  "Morocco" = "North",
  "Tunisia" = "North",
  "Sudan" = "North",
  "Western Sahara" = "North",
  "Mauritania" = "North",
  # West
  "Nigeria" = "West",
  "Ghana" = "West",
  "Senegal" = "West",
  "Mali" = "West",
  "Burkina Faso" = "West",
  "Niger" = "West",
  "Guinea" = "West",
  "Côte d'Ivoire" = "West",
  "Ivory Coast" = "West",
  "Sierra Leone" = "West",
  "Liberia" = "West",
  "Togo" = "West",
  "Benin" = "West",
  "Gambia" = "West",
  "The Gambia" = "West",
  "Guinea-Bissau" = "West",
  "Cape Verde" = "West",
  # Central
  "Democratic Republic of the Congo" = "Central",
  "Republic of the Congo" = "Central",
  "Cameroon" = "Central",
  "Central African Republic" = "Central",
  "Chad" = "Central",
  "Gabon" = "Central",
  "Equatorial Guinea" = "Central",
  "São Tomé and Príncipe" = "Central",
  "Angola" = "Central",
  # East
  "Ethiopia" = "East",
  "Kenya" = "East",
  "Tanzania" = "East",
  "Uganda" = "East",
  "Rwanda" = "East",
  "Burundi" = "East",
  "Somalia" = "East",
  "Eritrea" = "East",
  "Djibouti" = "East",
  "South Sudan" = "East",
  # Southern
  "South Africa" = "Southern",
  "Zimbabwe" = "Southern",
  "Zambia" = "Southern",
  "Mozambique" = "Southern",
  "Malawi" = "Southern",
  "Namibia" = "Southern",
  "Botswana" = "Southern",
  "Lesotho" = "Southern",
  "Eswatini" = "Southern",
  "Swaziland" = "Southern",
  "Madagascar" = "Southern",
  "Comoros" = "Southern"
)

df <- africa |>
  mutate(
    region = dplyr::recode(country, !!!region_map, .default = NA_character_),
    # fct_rev ensures the top-to-bottom visual order matches our logical vector when coordinates are flipped
    region = factor(
      region,
      levels = rev(c("North", "West", "Central", "East", "Southern"))
    ),
    # Keep top 6 families by row count; fold the tail into "Other families"
    family_lumped = fct_lump_n(
      family,
      n = 6,
      other_level = "Other families"
    )
  ) |>
  filter(!is.na(region)) |>
  # Order by overall prevalence so the legend reads top→bottom = most→least common
  mutate(
    family_lumped = fct_infreq(family_lumped) |> fct_rev()
  ) |>
  # fix typographic issue
  mutate(
    family_lumped = family_lumped |>
      str_replace("Kxʼa", "Kx'a")
  )

# ── Plot ──────────────────────────────────────────────────────────────────────
p <- ggplot(data = df) +
  geom_mosaic(
    aes(x = product(region), fill = family_lumped),
    na.rm = TRUE,
    color = "#eeffff", # matches bg → clean grout lines
    linewidth = 0.7
  ) +
  # Percentage labels only on tiles large enough to read
  geom_mosaic_text(
    aes(x = product(region), fill = family_lumped),
    na.rm = TRUE,
    size = 3.2,
    color = "white",
    fontface = "bold",
    min.label.size = 0.03 # suppress labels on tiny slivers
  ) +
  coord_flip() + # Rule 1: Flips the plot so region labels are horizontal on the Y-axis
  scale_fill_viridis_d(
    option = "D",
    direction = -1,
    name = NULL
  ) +
  labs(
    title = "**Africa's Linguistic Mosaic**",
    # Subtitle updated to reflect the flipped geometry
    subtitle = "Share of language family by region" |>
      str_wrap(50),
    caption = "Data: Languages of Africa · TidyTuesday 2026-01-13\nRow width represents the share of language-country pairs\n#30DayChartChallenge 2026 · Ilya Kashnitsky @ikashnitsky.phd\n"
  ) +
  theme_ik(base_size = 16, base_family = "ah") +
  theme(
    plot.title = ggtext::element_markdown(size = 40),
    plot.subtitle = ggtext::element_markdown(size = 26),
    plot.caption.position = "plot",
    legend.position = "bottom",
    legend.key.size = unit(0.55, "cm"),
    # Switch axis stripping: keep Y-axis text for regions, remove X-axis
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.title = element_blank(),
    axis.text.y = element_text(hjust = 1) # Guarantees clean, right-aligned horizontal text
  ) +
  guides(fill = guide_legend(nrow = 2, reverse = TRUE))

# ── Export ────────────────────────────────────────────────────────────────────
ggsave(
  "out/03-mosaic.pdf",
  plot = p,
  width = 9,
  height = 9,
  dpi = 300,
  bg = "#eeffff"
)
