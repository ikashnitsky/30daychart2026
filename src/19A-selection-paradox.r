# ==============================================================================
# Day 11 · Physical | #30DayChartChallenge 2026
# Option A — "The Selection Paradox" (dual panel)
# Top:    lollipop count by birth quarter × era
# Bottom: ggdist::stat_halfeye of within-cohort height deviation × position
# ==============================================================================

library(tidyverse)
library(ggdist)
library(ggtext)
library(patchwork)
library(showtext)

devtools::source_gist("653e1040a07364ae82b1bb312501a184")
sysfonts::font_add_google("Atkinson Hyperlegible", family = "ah")
showtext::showtext_auto()
theme_set(theme_ik(base_family = "ah"))

# ── 1. DATA ────────────────────────────────────────────────────────────────────
# Expected columns (adjust path/name as needed):
#   season        <int>  NHL season year (2001–2024)
#   player        <chr>  player identifier (can be duplicated across seasons)
#   position      <chr>  "Goalie" / "Defender" / "Forward"
#   birth_quarter <chr>  "Q1" / "Q2" / "Q3" / "Q4"
#   birth_year    <int>  calendar year of birth
#   height_cm     <dbl>  height in centimetres

dat_raw <- read_csv("dat/iihf_players_2001_2024.csv") # ← adjust filename

dat <- dat_raw |>
  mutate(
    birth_quarter = factor(birth_quarter, levels = c("Q1", "Q2", "Q3", "Q4")),
    position = factor(position, levels = c("Goalie", "Defender", "Forward")),
    era = if_else(season <= 2010, "2001–2010", "2011–2024") |>
      factor(levels = c("2001–2010", "2011–2024"))
  ) |>
  group_by(birth_year, position) |>
  mutate(height_dev = height_cm - mean(height_cm, na.rm = TRUE)) |>
  ungroup()

# ── 2. TOP PANEL: lollipop — counts by birth quarter × era ────────────────────
counts_era <- dat |>
  count(era, birth_quarter) |>
  group_by(era) |>
  mutate(pct = n / sum(n) * 100) |>
  ungroup()

# Expected proportion under null (uniform): 25 %
null_pct <- 25

q_colors <- c(Q1 = "#BFD7EA", Q2 = "#6AABCF", Q3 = "#2172B4", Q4 = "#08306B")

p_top <- ggplot(
  counts_era,
  aes(x = birth_quarter, y = pct, color = birth_quarter)
) +
  geom_hline(
    yintercept = null_pct,
    color = "grey70",
    linewidth = 0.4,
    linetype = "dashed"
  ) +
  geom_segment(
    aes(xend = birth_quarter, y = null_pct, yend = pct),
    linewidth = 0.8,
    alpha = 0.7
  ) +
  geom_point(size = 4, alpha = 0.9) +
  annotate(
    "text",
    x = 0.6,
    y = null_pct + 0.8,
    label = "Uniform 25 %",
    color = "grey55",
    size = 3.2,
    hjust = 0,
    family = "ah",
    fontface = "italic"
  ) +
  facet_wrap(~era, nrow = 1) +
  scale_color_manual(values = q_colors, guide = "none") +
  scale_y_continuous(
    labels = scales::label_percent(scale = 1, suffix = " %"),
    limits = c(10, 40)
  ) +
  labs(
    x = NULL,
    y = "Share of roster (%)",
    title = "**The Selection Paradox**: born earlier, selected more — born later, selected harder",
    subtitle = "Day 11 · Physical | NHL 2001–2024 roster-level birth-quarter analysis"
  ) +
  theme(
    plot.title = ggtext::element_markdown(size = 15),
    plot.subtitle = ggtext::element_markdown(size = 10, color = "grey45"),
    strip.text = element_text(size = 12, face = "bold"),
    axis.text.x = element_text(size = 11, face = "bold"),
    panel.spacing = unit(1.2, "lines")
  )

# ── 3. BOTTOM PANEL: stat_halfeye — height deviation × position ───────────────
# N annotation
n_lab <- dat |>
  count(birth_quarter, position) |>
  mutate(label = paste0("*n* = ", scales::comma(n)))

p_bot <- ggplot(
  dat,
  aes(
    x = birth_quarter,
    y = height_dev,
    fill = birth_quarter,
    color = birth_quarter
  )
) +
  geom_hline(
    yintercept = 0,
    color = "grey60",
    linewidth = 0.4,
    linetype = "dashed"
  ) +
  stat_halfeye(
    aes(thickness = after_stat(pdf * n)),
    normalize = "panels",
    adjust = 1.2,
    point_interval = "mean_qi",
    .width = c(0.66, 0.95),
    alpha = 0.82,
    slab_linewidth = 0.25
  ) +
  geom_richtext(
    data = n_lab,
    aes(x = birth_quarter, y = -3.8, label = label),
    size = 2.9,
    color = "grey50",
    fill = NA,
    label.color = NA,
    family = "ah",
    inherit.aes = FALSE
  ) +
  facet_wrap(~position, nrow = 1) +
  scale_fill_manual(values = q_colors, guide = "none") +
  scale_color_manual(values = q_colors, guide = "none") +
  scale_y_continuous(
    labels = scales::label_number(suffix = " cm", style_positive = "plus"),
    breaks = seq(-4, 4, 2)
  ) +
  labs(
    x = "Birth quarter  (Q1 = Jan–Mar  ·  Q4 = Oct–Dec)",
    y = "Height deviation from\nbirth-year position mean (cm)",
    caption = "Data: Hockey-Reference NHL rosters 2001–2024\n#30DayChartChallenge 2026 · Ilya Kashnitsky @ikashnitsky.phd\n"
  ) +
  theme(
    plot.caption.position = "plot",
    strip.text = element_text(size = 12, face = "bold"),
    axis.text.x = element_text(size = 11, face = "bold"),
    panel.spacing = unit(1.2, "lines")
  )

# ── 4. PATCHWORK ──────────────────────────────────────────────────────────────
p_final <- p_top / p_bot + plot_layout(heights = c(1, 1.4))

# ── 5. EXPORT ─────────────────────────────────────────────────────────────────
dir.create("out", showWarnings = FALSE)
ggsave(
  "out/11A-selection-paradox.pdf",
  p_final,
  width = 10,
  height = 11,
  dpi = 300,
  bg = "#eeffff"
)
ggsave(
  "out/11A-selection-paradox.png",
  p_final,
  width = 10,
  height = 11,
  dpi = 300,
  bg = "#eeffff"
)
message("✔ Saved out/11A-selection-paradox.pdf / .png")
