# ..........................................................
# 2026-04-01 -- 30DayChartChallenge 2026 Edition
# Day 1 -- part-to-whole                -----------
# Ilya Kashnitsky, ilya.kashnitsky@gmail.com
# ..........................................................

# AI helper chat: https://www.perplexity.ai/search/day-1-part-to-whole-let-s-play-9r2UB4azS3iaj5urON4OXg

library(tidyverse)
library(treemapify)
library(ggtext)
library(showtext)
library(scales)

devtools::source_gist("653e1040a07364ae82b1bb312501a184")
sysfonts::font_add_google("Atkinson Hyperlegible", family = "ah")
showtext::showtext_auto()
theme_set(theme_ik(base_family = "ah"))

# ── Data ──────────────────────────────────────────────────────────────────────
data("Titanic")

df_tm <- as.data.frame(Titanic) |>
  group_by(Sex, Class) |>
  summarise(
    total = sum(Freq),
    survived = sum(Freq[Survived == "Yes"]),
    .groups = "drop"
  ) |>
  mutate(
    surv_rate = survived / total,
    Class = factor(Class, levels = c("1st", "2nd", "3rd", "Crew")),
    tile_label = paste0(
      Class,
      "\n",
      percent(surv_rate, accuracy = 1),
      " survived",
      "\n(n = ",
      total,
      ")"
    )
  )

# ── Plot ──────────────────────────────────────────────────────────────────────
p <- ggplot(
  df_tm,
  aes(
    area = total,
    fill = surv_rate,
    label = tile_label,
    subgroup = Sex
  )
) +
  geom_treemap(color = "#eeffff", size = 2) +
  geom_treemap_subgroup_border(color = "#eeffff", size = 6) +
  # Large sex label — ghosted so tile text stays legible
  geom_treemap_subgroup_text(
    place = "bottom",
    fontface = "bold",
    colour = "white",
    alpha = 0.55,
    grow = TRUE,
    family = "ah"
  ) +
  # Per-tile: class + survival rate + count
  geom_treemap_text(
    colour = "white",
    place = "centre",
    size = 11,
    reflow = TRUE,
    family = "ah"
  ) +
  scale_fill_viridis_c(
    option = "mako",
    begin = 0.15,
    end = 0.92,
    labels = percent_format(accuracy = 1),
    name = "Survival rate",
    guide = guide_colorbar(
      barwidth = 30,
      barheight = 1,
      title.position = "top",
      title.hjust = 0.5
    )
  ) +
  labs(
    title = "**Women & 1st class first** — how the Titanic's lifeboats were filled",
    subtitle = "Day 1 · Part to Whole | Tile area is proportional to the number of passengers aboard; color = survival rate",
    caption = "#30DayChartChallenge 2026 · Ilya Kashnitsky @ikashnitsky · ikashnitsky.phd\nData: built-in R {datasets} — Titanic (2201 passengers & crew)"
  ) +
  theme(
    plot.title = ggtext::element_markdown(size = 18),
    plot.subtitle = ggtext::element_markdown(),
    plot.caption.position = "plot",
    legend.position = "bottom"
  )

# ── Export ────────────────────────────────────────────────────────────────────
ggsave(
  "out/01-part-to-whole.pdf",
  plot = p,
  width = 8,
  height = 8,
  dpi = 300,
  bg = "#eeffff"
)
