# ..........................................................
# 2026-04-02 -- 30DayChartChallenge 2026 Edition
# Day 2 -- pictogram                -----------
# Ilya Kashnitsky, ilya.kashnitsky@gmail.com
# ..........................................................

# AI helper chat: https://www.perplexity.ai/search/day-2-pictogram-visualize-my-d-5.dmFgnySe.ZSfIqBHgSng

library(tidyverse)
library(patchwork)
library(ggtext)
library(showtext)
library(sysfonts)
library(ggforce)

# ---- mandatory setup ----
devtools::source_gist("653e1040a07364ae82b1bb312501a184")
sysfonts::font_add_google("Atkinson Hyperlegible", family = "ah")
showtext::showtext_auto()
theme_set(theme_ik(base_size = 14, base_family = "ah"))

bg <- "#eeffff"

# card palette (one per rule)
card_bg <- c("#e8f5e9", "#e3f2fd", "#fff8e1", "#f3e5f5", "#fce4ec", "#e0f7fa")
card_bd <- c("#66bb6a", "#42a5f5", "#ffca28", "#ab47bc", "#ef5350", "#26c6da")

# helper: outer card annotation
card_annot <- function(title_text, sub_text, n) {
  plot_annotation(
    title = title_text,
    subtitle = sub_text,
    theme = theme(
      plot.title = element_markdown(
        family = "ah",
        size = 12,
        face = "plain",
        margin = margin(b = 2)
      ),
      plot.subtitle = element_text(
        family = "ah",
        size = 8,
        color = "gray50",
        margin = margin(b = 6)
      ),
      plot.background = element_rect(
        fill = card_bg[n],
        color = card_bd[n],
        linewidth = 0.9
      ),
      plot.margin = margin(8, 8, 8, 8)
    )
  )
}

# helper: inner mini-panel theme
void_mini <- function(fill = "#f9f9f9") {
  theme_void(base_family = "ah") +
    theme(
      legend.position = "none",
      plot.background = element_rect(fill = fill, color = NA),
      plot.margin = margin(2, 4, 6, 4),
      plot.subtitle = element_text(
        family = "ah",
        size = 7,
        hjust = 0.5,
        color = "gray45",
        margin = margin(t = 3)
      )
    )
}

vcol <- viridis::viridis(2, option = "D", begin = 0.25, end = 0.72)

# ============================================================
# RULE 0 · Show distributions, never only summaries
# ============================================================
set.seed(42)
d0 <- tibble(
  grp = rep(c("A", "B"), each = 40),
  val = c(rnorm(40, 3, 0.9), rnorm(40, 7, 1.5))
)

r0w <- ggplot(d0, aes(grp, val, fill = grp)) +
  stat_summary(fun = mean, geom = "col", width = 0.5, alpha = 0.9) +
  scale_fill_manual(values = vcol) +
  labs(subtitle = "mean bars only  ✗") +
  void_mini("#fff5f5")

r0r <- ggplot(d0, aes(grp, val, color = grp)) +
  geom_jitter(width = 0.12, size = 0.9, alpha = 0.5) +
  stat_summary(fun = mean, geom = "point", shape = 18, size = 3.5) +
  scale_color_manual(values = vcol) +
  labs(subtitle = "show distribution  ✓") +
  void_mini("#f5fff5")

p0 <- (r0w | r0r) +
  card_annot(
    "**0** · Show the *distribution*",
    "Never only summaries · layer data · small multiples",
    1
  )

# ============================================================
# RULE 1 · Text is horizontal — flip charts
# ============================================================
d1 <- tibble(
  cat = c("Very Long Label", "Another Cat", "Third Grp", "Last One"),
  val = c(4.2, 3.1, 5.8, 2.7)
)

r1w <- ggplot(d1, aes(cat, val)) +
  geom_col(fill = vcol[1], alpha = 0.8, width = 0.6) +
  labs(subtitle = "rotated labels  ✗") +
  theme_void(base_family = "ah") +
  theme(
    axis.text.x = element_text(
      angle = 45,
      hjust = 1,
      size = 5.5,
      family = "ah"
    ),
    legend.position = "none",
    plot.background = element_rect(fill = "#fff5f5", color = NA),
    plot.margin = margin(2, 4, 18, 4),
    plot.subtitle = element_text(
      family = "ah",
      size = 7,
      hjust = 0.5,
      color = "gray45",
      margin = margin(t = 3)
    )
  )

r1r <- ggplot(d1, aes(val, fct_reorder(cat, val))) +
  geom_col(fill = vcol[2], alpha = 0.8, width = 0.6) +
  labs(subtitle = "flipped horizontal  ✓") +
  theme_void(base_family = "ah") +
  theme(
    axis.text.y = element_text(size = 5.5, hjust = 1, family = "ah"),
    legend.position = "none",
    plot.background = element_rect(fill = "#f5fff5", color = NA),
    plot.margin = margin(2, 4, 4, 4),
    plot.subtitle = element_text(
      family = "ah",
      size = 7,
      hjust = 0.5,
      color = "gray45",
      margin = margin(t = 3)
    )
  )

p1 <- (r1w | r1r) +
  card_annot(
    "**1** · Text is *horizontal*",
    "Flip charts · geomtextpath on lines · ggrepel for points",
    2
  )

# ============================================================
# RULE 2 · Readable base size  (≥14, ≥16 for social)
# ============================================================
# Annotate at three distinct sizes so the hierarchy is visible
p2 <- ggplot() +
  # dim stripe for "too small"
  annotate(
    "rect",
    xmin = -0.1,
    xmax = 3.1,
    ymin = 3.25,
    ymax = 3.75,
    fill = "#ffeeee",
    color = NA
  ) +
  annotate(
    "text",
    x = 0.1,
    y = 3.50,
    label = "8 pt  — too small  ✗",
    hjust = 0,
    family = "ah",
    size = 2.8,
    color = "#cc4444"
  ) +
  # stripe for 14 pt
  annotate(
    "rect",
    xmin = -0.1,
    xmax = 3.1,
    ymin = 2.55,
    ymax = 3.10,
    fill = "#eaf7ea",
    color = NA
  ) +
  annotate(
    "text",
    x = 0.1,
    y = 2.80,
    label = "14 pt  — minimum  ✓",
    hjust = 0,
    family = "ah",
    size = 14 * 0.353,
    color = "#2d6a4f"
  ) +
  # stripe for 16 pt
  annotate(
    "rect",
    xmin = -0.1,
    xmax = 3.1,
    ymin = 1.50,
    ymax = 2.25,
    fill = "#e8f2ff",
    color = NA
  ) +
  annotate(
    "text",
    x = 0.1,
    y = 1.85,
    label = "16 pt  — social  ✓✓",
    hjust = 0,
    family = "ah",
    size = 16 * 0.353,
    color = "#1a5276"
  ) +
  xlim(-0.1, 3.1) +
  ylim(1.15, 4.0) +
  labs(
    title = "**2** · *Readable* base size",
    subtitle = "≥ 14 for charts  ·  ≥ 16 for social"
  ) +
  theme_void(base_family = "ah") +
  theme(
    plot.title = element_markdown(
      family = "ah",
      size = 12,
      face = "plain",
      margin = margin(b = 2)
    ),
    plot.subtitle = element_text(
      family = "ah",
      size = 8,
      color = "gray50",
      margin = margin(b = 6)
    ),
    plot.background = element_rect(
      fill = card_bg[3],
      color = card_bd[3],
      linewidth = 0.9
    ),
    plot.margin = margin(8, 8, 8, 8),
    legend.position = "none"
  )

# ============================================================
# RULE 3 · Colorblind-safe palettes
# ============================================================
d3 <- tibble(
  x = c(1:6, 1:6),
  y = c(rep(1.5, 6), rep(0.5, 6)),
  col = c(
    viridis::viridis(6),
    c("#FF0000", "#FF8C00", "#FFFF00", "#00CC00", "#0000FF", "#8B00FF")
  )
)

p3 <- ggplot(d3, aes(x, y, fill = col)) +
  geom_tile(width = 0.88, height = 0.42) +
  scale_fill_identity() +
  annotate(
    "text",
    x = 3.5,
    y = 1.88,
    size = 3.3,
    family = "ah",
    label = "viridis  ✓  colorblind-safe",
    color = "#2d6a4f",
    fontface = "bold"
  ) +
  annotate(
    "text",
    x = 3.5,
    y = 0.85,
    size = 3.3,
    family = "ah",
    label = "rainbow  ✗  inaccessible",
    color = "#cc3333",
    fontface = "bold"
  ) +
  coord_fixed() +
  xlim(0.4, 6.6) +
  ylim(0.15, 2.2) +
  labs(
    title = "**3** · *Colorblind*-safe palettes",
    subtitle = "viridis default  ·  no rainbow / red-green"
  ) +
  theme_void(base_family = "ah") +
  theme(
    legend.position = "none",
    plot.title = element_markdown(
      family = "ah",
      size = 12,
      face = "plain",
      margin = margin(b = 2)
    ),
    plot.subtitle = element_text(
      family = "ah",
      size = 8,
      color = "gray50",
      margin = margin(b = 6)
    ),
    plot.background = element_rect(
      fill = card_bg[4],
      color = card_bd[4],
      linewidth = 0.9
    ),
    plot.margin = margin(8, 8, 8, 8)
  )

# ============================================================
# RULE 4 · Gray out background · saturate focal elements
# ============================================================
set.seed(12)
d4 <- tibble(x = rnorm(100), y = rnorm(100)) |>
  mutate(focal = x > 0.7 & y > 0.4)

fcol <- viridis::viridis(1, begin = 0.65)

p4 <- ggplot() +
  geom_point(
    data = filter(d4, !focal),
    aes(x, y),
    color = "gray83",
    size = 1.6
  ) +
  geom_point(data = filter(d4, focal), aes(x, y), color = fcol, size = 2.9) +
  ggforce::geom_mark_ellipse(
    data = filter(d4, focal),
    aes(x, y),
    color = fcol,
    fill = NA,
    expand = unit(3, "mm"),
    linewidth = 0.6,
    linetype = "dashed"
  ) +
  labs(
    title = "**4** · *Gray* out  ·  *saturate* focal",
    subtitle = "ggforce callouts  ·  color title words not legends"
  ) +
  theme_void(base_family = "ah") +
  theme(
    legend.position = "none",
    plot.title = element_markdown(
      family = "ah",
      size = 12,
      face = "plain",
      margin = margin(b = 2)
    ),
    plot.subtitle = element_text(
      family = "ah",
      size = 8,
      color = "gray50",
      margin = margin(b = 6)
    ),
    plot.background = element_rect(
      fill = card_bg[5],
      color = card_bd[5],
      linewidth = 0.9
    ),
    plot.margin = margin(8, 8, 8, 8)
  )

# ============================================================
# RULE 5 · Dotplots > bar charts · every mark earns its place
# ============================================================
d5 <- tibble(
  cat = c("Alpha", "Beta", "Gamma", "Delta", "Epsilon"),
  val = c(3.2, 5.1, 2.4, 6.8, 4.5)
) |>
  mutate(cat = fct_reorder(cat, val))

col5 <- viridis::viridis(1, begin = 0.45)

r5w <- ggplot(d5, aes(cat, val)) +
  geom_col(fill = col5, alpha = 0.78, width = 0.65) +
  labs(subtitle = "bar chart  ✗") +
  theme_void(base_family = "ah") +
  theme(
    axis.text.x = element_text(size = 5.5, family = "ah"),
    legend.position = "none",
    plot.background = element_rect(fill = "#fff5f5", color = NA),
    plot.margin = margin(2, 4, 10, 4),
    plot.subtitle = element_text(
      family = "ah",
      size = 7,
      hjust = 0.5,
      color = "gray45",
      margin = margin(t = 3)
    )
  )

r5r <- ggplot(d5, aes(x = val, y = cat)) +
  geom_segment(
    aes(x = 1.5, xend = val, y = cat, yend = cat),
    color = col5,
    alpha = 0.38,
    linewidth = 0.8
  ) +
  geom_point(color = col5, size = 3.2) +
  labs(subtitle = "dot plot  ✓") +
  theme_void(base_family = "ah") +
  theme(
    axis.text.y = element_text(size = 5.5, hjust = 1, family = "ah"),
    legend.position = "none",
    plot.background = element_rect(fill = "#f5fff5", color = NA),
    plot.margin = margin(2, 4, 4, 4),
    plot.subtitle = element_text(
      family = "ah",
      size = 7,
      hjust = 0.5,
      color = "gray45",
      margin = margin(t = 3)
    )
  )

p5 <- (r5w | r5r) +
  card_annot(
    "**5** · *Dotplots* > bar charts",
    "Faceted lines > stacked areas  ·  every mark earns its place",
    6
  )

# ============================================================
# FINAL ASSEMBLY  (3 × 2 patchwork poster)
# ============================================================
final_plot <- (p0 | p1 | p2) /
  (p3 | p4 | p5) +
  plot_annotation(
    title = "**The 6 Rules of Good Dataviz**",
    subtitle = "Day 2 · Pictogram | Each rule illustrated as a self-referential mini chart",
    caption = "#30DayChartChallenge 2026 · Ilya Kashnitsky @ikashnitsky.phd\nData: Author's personal dataviz principles",
    theme = theme(
      plot.title = element_markdown(
        family = "ah",
        size = 22,
        face = "plain",
        margin = margin(b = 4)
      ),
      plot.subtitle = element_text(
        family = "ah",
        size = 13,
        color = "gray40",
        margin = margin(b = 12)
      ),
      plot.caption = element_text(
        family = "ah",
        size = 9,
        color = "gray55",
        hjust = 0,
        lineheight = 1.3
      ),
      plot.caption.position = "plot",
      plot.background = element_rect(fill = bg, color = NA),
      plot.margin = margin(20, 20, 15, 20)
    )
  )

ggsave(
  "out/02-pictogram.pdf",
  final_plot,
  width = 12,
  height = 9,
  dpi = 300,
  bg = bg
)
