# ==============================================================================
# Day 11 · Physical | #30DayChartChallenge 2026
# Option C ⭐ — "The Physical Selection Funnel"
#
# ggdist::stat_halfeye — one slab per birth quarter, WITHIN each position facet
#   x-axis : birth quarter Q1 → Q4  (born earlier → later in the year)
#   y-axis : within-cohort height deviation (cm above/below birth-year mean)
#   slab width ∝ N — RAE scarcity is visible in the geometry itself
#   colour : single blue gradient, Q1 (pale) → Q4 (saturated)
#   facets : Goalie · Defender · Forward (3 columns)
#   annotation: italic n-labels below each slab
# ==============================================================================

library(tidyverse)
library(ggdist)
library(ggtext)
library(patchwork)
library(showtext)

devtools::source_gist("653e1040a07364ae82b1bb312501a184")
sysfonts::font_add_google("Atkinson Hyperlegible", family = "ah")
showtext::showtext_auto()
theme_set(theme_ik(base_family = "ah", base_size = 14))

# ── 1. DATA ────────────────────────────────────────────────────────────────────
# Expected columns (one row = one player-season appearance):
#   season        <int>   NHL season year (2001–2024)
#   position      <chr>   "Goalie" / "Defender" / "Forward"
#   birth_quarter <chr>   "Q1" / "Q2" / "Q3" / "Q4"
#   birth_year    <int>   calendar year of birth
#   height_cm     <dbl>   height in centimetres

dat_raw <- read_csv("nhl_rae_players.csv")   # ← adjust filename / path

dat <- dat_raw |>
  mutate(
    birth_quarter = factor(birth_quarter, levels = c("Q1", "Q2", "Q3", "Q4")),
    position      = factor(position,      levels = c("Goalie", "Defender", "Forward"))
  ) |>
  # within-cohort height deviation: height − mean height for same birth-year × position
  group_by(birth_year, position) |>
  mutate(height_dev = height_cm - mean(height_cm, na.rm = TRUE)) |>
  ungroup() |>
  filter(!is.na(height_dev), !is.na(birth_quarter), !is.na(position))

# ── 2. N ANNOTATIONS ─────────────────────────────────────────────────────────
n_lab <- dat |>
  count(birth_quarter, position) |>
  mutate(label = paste0("italic(n)==", scales::comma(n)))

# Y position for n labels: just below the data range
y_label <- min(dat$height_dev, na.rm = TRUE) - 0.4

# ── 3. COLOUR PALETTE ────────────────────────────────────────────────────────
# Single-hue sequential: Q1 pale → Q4 fully saturated
# Communicates that Q4 "survivors" are the darkest, most selected group
q_fills  <- c(Q1 = "#C6DBEF", Q2 = "#6BAED6", Q3 = "#2171B5", Q4 = "#08306B")
q_colors <- c(Q1 = "#9ECAE1", Q2 = "#4292C6", Q3 = "#08519C", Q4 = "#041E42")

# ── 4. PLOT ───────────────────────────────────────────────────────────────────
p <- ggplot(dat, aes(
  x     = birth_quarter,
  y     = height_dev,
  fill  = birth_quarter,
  color = birth_quarter
)) +

  # — cohort-mean reference line
  geom_hline(
    yintercept = 0,
    color      = "grey60",
    linewidth  = 0.45,
    linetype   = "dashed"
  ) +
  annotate(
    "text",
    x = 0.52, y = 0.12,
    label    = "Birth-year\nposition mean",
    color    = "grey55",
    size     = 3.0,
    hjust    = 0,
    family   = "ah",
    fontface = "italic"
  ) +

  # — main halfeye
  #   thickness = pdf × n  →  slab area ∝ N, so Q1 is fat, Q4 is thin
  #   normalize = "panels" rescales within each facet so tallest slab
  #   in the panel fills the available width (keeps facets comparable)
  stat_halfeye(
    aes(thickness = after_stat(pdf * n)),
    normalize      = "panels",
    adjust         = 1.2,        # slight smoothing
    point_interval = "mean_qi",
    .width         = c(0.66, 0.95),
    alpha          = 0.84,
    slab_linewidth  = 0.25,
    interval_color = "white",
    point_color    = "white",
    point_size     = 1.6,
    interval_size_range = c(0.5, 1.2)
  ) +

  # — italic n labels below each slab
  geom_text(
    data         = n_lab,
    aes(x = birth_quarter, y = y_label, label = label),
    parse        = TRUE,
    size         = 3.0,
    color        = "grey52",
    family       = "ah",
    inherit.aes  = FALSE
  ) +

  facet_wrap(~ position, nrow = 1) +

  scale_fill_manual(values  = q_fills,  guide = "none") +
  scale_color_manual(values = q_colors, guide = "none") +
  scale_y_continuous(
    labels = scales::label_number(suffix = " cm", style_positive = "plus"),
    breaks = seq(-6, 6, 2)
  ) +

  labs(
    title    = "**Born late, selected hard**",
    subtitle = "Day 11 · Physical | Q4 NHL players are *rarer* (narrower slab) yet *taller* than birth-year peers — compensatory physical selection",
    x        = "Birth quarter   (Q1 = Jan–Mar  ·  Q4 = Oct–Dec)",
    y        = "Height deviation from birth-year\nposition mean (cm)",
    caption  = "Data: Hockey-Reference NHL rosters 2001–2024\n#30DayChartChallenge 2026 · Ilya Kashnitsky @ikashnitsky.phd\n"
  ) +

  theme(
    plot.title             = ggtext::element_markdown(size = 17),
    plot.subtitle          = ggtext::element_markdown(size = 10.5, color = "grey40",
                                                       lineheight = 1.35),
    plot.caption.position  = "plot",
    strip.text             = element_text(size = 13, face = "bold"),
    panel.spacing          = unit(1.8, "lines"),
    axis.text.x            = element_text(size = 11, face = "bold"),
    axis.title             = element_text(size = 11),
    plot.margin            = margin(12, 14, 6, 14)
  )

# ── 5. EXPORT ─────────────────────────────────────────────────────────────────
dir.create("out", showWarnings = FALSE)
ggsave("out/11C-physical-funnel.pdf",
       p, width = 10, height = 7, dpi = 300, bg = "#eeffff")
ggsave("out/11C-physical-funnel.png",
       p, width = 10, height = 7, dpi = 300, bg = "#eeffff")
message("✔ Saved out/11C-physical-funnel.pdf / .png")

# ── VARIATION IDEAS ───────────────────────────────────────────────────────────
# • Split by era (2001-2010 vs 2011-2024) → 6 facets (position × era): does
#   the funnel narrow AND rise more steeply in the recent decade?
# • Use ggbeeswarm::geom_quasirandom() overlaid on the halfeye to show
#   individual player points, colour-coded by birth_year.
# • Day 12 idea: if the prompt allows, map the same RAE effect geographically
#   by player nationality using geofacet::facet_geo() on country grids.
