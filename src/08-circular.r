# ..........................................................
# 2026-04-08 -- 30DayChartChallenge
# Day 8: circulr                -----------
# Ilya Kashnitsky, ilya.kashnitsky@gmail.com
# ..........................................................

# GitHub out of hours commits and ai brain burn

library(tidyverse)
library(ggtext)
library(showtext)

devtools::source_gist("653e1040a07364ae82b1bb312501a184")
sysfonts::font_add_google("Atkinson Hyperlegible", family = "ah")
showtext::showtext_auto()
theme_set(theme_ik(base_size = 14, base_family = "ah"))

# ── Data: GitHub punch_card API, 8 major repos, last 6 days pooled ──────────────
# Repos: microsoft/vscode · torvalds/linux · facebook/react · vercel/next.js
#        tensorflow/tensorflow · rust-lang/rust · golang/go · kubernetes/kubernetes
# Fetched April 2026; API returns the last 6 days.
# Data processed in "src/08-circular-data-fetching.py" and saved as "dat/github_punch_card_data.csv"

df <- read_csv("dat/github_punch_card_data.csv")

hourly <- df |>
  group_by(hour) |>
  summarise(total = sum(commits, na.rm = TRUE)) |>
  ungroup() |>
  mutate(pct = total / sum(total) * 100) |>
  mutate(
    zone = case_when(
      hour >= 9 & hour <= 17 ~ "Working hours (9–17h)",
      hour >= 6 & hour <= 8 ~ "Twilight (6–8h & 18–20h)",
      hour >= 18 & hour <= 20 ~ "Twilight (6–8h & 18–20h)",
      TRUE ~ "After hours (0–5h & 21–23h)"
    ) |>
      factor(
        levels = c(
          "Working hours (9–17h)",
          "Twilight (6–8h & 18–20h)",
          "After hours (0–5h & 21–23h)"
        )
      )
  )

# ── Color palette (colorblind-safe) ───────────────────────────────────────────
zone_cols <- c(
  "Working hours (9–17h)" = "#f9c74f", # amber
  "Twilight (6–8h & 18–20h)" = "#f3722c", # orange
  "After hours (0–5h & 21–23h)" = "#480ca8" # deep violet
)

# ── Clock-face labels: place just outside bar tips ────────────────────────────
clock_labs <- tibble(
  hour = c(0, 3, 6, 9, 12, 15, 18, 21),
  label = c(
    "0 h\nmidnight",
    "3 h",
    "6 h",
    "9 h",
    "12 h\nnoon",
    "15 h",
    "18 h",
    "21 h"
  ),
  r = 9.9
)

# ── Build plot ────────────────────────────────────────────────────────────────
p <- ggplot(hourly, aes(x = hour, y = pct)) +

  # Circular reference grid
  geom_hline(
    yintercept = c(2, 4, 6, 8),
    color = "grey72",
    linewidth = 0.22,
    linetype = "dotted"
  ) +

  # Bars  (counterclockwise from midnight at top — intentional 24h-clock read)
  geom_col(aes(fill = zone), width = 0.88, color = NA) +

  # Hour tick marks at the base ring
  geom_segment(
    data = tibble(hour = 0:23),
    aes(x = hour, xend = hour, y = -0.18, yend = 0.05),
    color = "grey55",
    linewidth = 0.35,
    inherit.aes = FALSE
  ) +

  # Clock-face labels outside the bars
  geom_text(
    data = clock_labs,
    aes(x = hour, y = r, label = label),
    size = 3.4,
    family = "ah",
    color = "grey30",
    lineheight = 0.82,
    inherit.aes = FALSE
  ) +

  # Centre annotation — placed at very low radius (hollow centre)
  annotate(
    "text",
    x = 0,
    y = -3.5,
    label = "39.6%\ncommits outside\nworking hours",
    size = 4,
    family = "ah",
    color = "grey20",
    fontface = "bold",
    hjust = 0.5,
    vjust = 0.5,
    lineheight = 1.15
  ) +

  # Thin separator arc between hollow centre and bars (cosmetic)
  geom_hline(yintercept = 0, color = "grey60", linewidth = 0.35) +

  # ── Scales ──────────────────────────────────────────────────────────────────
  scale_x_continuous(limits = c(-0.5, 23.5), breaks = NULL) +
  scale_y_continuous(limits = c(-4.8, 9.9), breaks = NULL) +
  scale_fill_manual(
    values = zone_cols,
    name = NULL,
    guide = guide_legend(
      nrow = 1,
      override.aes = list(size = 5, shape = 15)
    )
  ) +

  # coord_polar: start = -pi/2 puts hour 0 (midnight) at the top
  coord_polar(start = 0) +

  # ── Labels ──────────────────────────────────────────────────────────────────
  labs(
    title = "<span style = 'color: #F06292;'>FOSS</span> — Free of Sleep Surrender",
    subtitle = paste0(
      "Percent of commits per hour · 8 major GitHub repos"
    ),
    caption = c(
      "Data: GitHub punch_card API — microsoft/vscode · torvalds/linux · facebook/react · vercel/next.js\ntensorflow/tensorflow · rust-lang/rust · golang/go · kubernetes/kubernetes\n#30DayChartChallenge 2026 · Day 8 · Circular · Ilya Kashnitsky @ikashnitsky.phd"
    )
  ) +

  # ── Theme ────────────────────────────────────────────────────────────────────
  theme(
    plot.title = ggtext::element_markdown(size = 24),
    plot.subtitle = ggtext::element_markdown(size = 16, lineheight = 1.45),
    plot.caption.position = "plot",
    axis.title = element_blank(),
    axis.text = element_blank(),
    panel.grid = element_blank(),
    legend.position = "bottom",
    legend.text = element_text(size = 10.5),
    legend.key.size = unit(0.5, "cm"),
    legend.spacing.x = unit(0.5, "cm"),
    plot.margin = margin(12, 12, 8, 12)
  )

# ── Export ────────────────────────────────────────────────────────────────────
ggsave(
  "out/08-circular.pdf",
  plot = p,
  width = 8,
  height = 8,
  dpi = 300,
  bg = "#eeffff"
)
