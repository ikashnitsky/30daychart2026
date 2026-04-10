# ..........................................................
# 2026-04-09 -- 30DayChartChallenge
# Day 9: wealth                -----------
# Ilya Kashnitsky, ilya.kashnitsky@gmail.com
# ..........................................................

library(tidyverse)
library(wid)
library(ggalt)
library(ggtext)
library(showtext)
library(scales)

# ── mandatory setup ───────────────────────────────────────────────────────────
devtools::source_gist("653e1040a07364ae82b1bb312501a184")
sysfonts::font_add_google("Atkinson Hyperlegible", family = "ah")
showtext::showtext_auto()
theme_set(theme_ik(base_family = "ah"))

# ── data ──────────────────────────────────────────────────────────────────────
countries <- c("US", "FR", "DE", "CN", "GB", "BR", "IN")
country_labels <- c(
    US = "United States",
    FR = "France",
    DE = "Germany",
    CN = "China",
    GB = "United Kingdom",
    BR = "Brazil",
    IN = "India"
)

# shweal = share of net personal wealth; j = equal-split adults; 992 = age 20+
dat_raw <- download_wid(
    indicators = "shweal",
    areas = countries,
    years = 1980:2023,
    perc = c("p0p50", "p50p90", "p99p100"),
    ages = 992,
    pop = "j",
    include_extrapolations = TRUE # needed for sparse historical series
)

# ── dumbbell endpoints ────────────────────────────────────────────────────────
# anchor: year closest to 1985 per country×percentile, and the latest year
dat_db <- dat_raw |>
    filter(!is.na(value)) |>
    group_by(country, percentile) |>
    summarise(
        yr_early = year[which.min(abs(year - 1985))],
        val_early = value[which.min(abs(year - 1985))],
        yr_late = max(year),
        val_late = value[year == max(year)][1],
        .groups = "drop"
    ) |>
    mutate(
        country_label = country_labels[country],
        group = case_when(
            percentile == "p0p50" ~ "Bottom 50%",
            percentile == "p50p90" ~ "Middle 40%",
            percentile == "p99p100" ~ "Top 1%"
        ) |>
            factor(levels = c("Top 1%", "Middle 40%", "Bottom 50%"))
    )

# order countries by size of Middle 40% change (most hollowed → top of plot)
country_order <- dat_db |>
    filter(group == "Middle 40%") |>
    arrange(desc(val_early - val_late)) |> # largest drop first = top
    pull(country_label)

dat_db <- mutate(
    dat_db,
    country_label = factor(country_label, levels = rev(country_order))
)

# ── colour palette ────────────────────────────────────────────────────────────
col_early <- "#8ecae6" # light blue = ~1985
col_late <- "#023047" # dark navy  = latest

# ── plot ──────────────────────────────────────────────────────────────────────
p <- ggplot(dat_db, aes(y = country_label, x = val_early, xend = val_late)) +

    ggalt::geom_dumbbell(
        size = 1.3,
        size_x = 3.5,
        size_xend = 4.5,
        colour = "grey75",
        colour_x = col_early,
        colour_xend = col_late
    ) +

    # annotate direction labels in each facet header area
    geom_text(
        data = dat_db |>
            filter(country_label == levels(dat_db$country_label)[1]),
        aes(x = val_early, y = Inf, label = "~1985"),
        vjust = -0.5,
        hjust = 0.5,
        size = 3.5,
        color = col_early,
        fontface = "bold",
        family = "ah"
    ) +
    geom_text(
        data = dat_db |>
            filter(country_label == levels(dat_db$country_label)[1]),
        aes(x = val_late, y = Inf, label = "latest"),
        vjust = -0.5,
        hjust = 0.5,
        size = 3.5,
        color = col_late,
        fontface = "bold",
        family = "ah"
    ) +

    facet_wrap(~group, nrow = 1) +

    scale_x_continuous(
        labels = percent_format(accuracy = 1),
        expand = expansion(mult = 0.15)
    ) +

    labs(
        title = "Only the richest keep accumulating wealth",
        subtitle = paste0(
            "Share of net personal wealth — ",
            "<span style='color:",
            col_early,
            "'>**~1985**</span>",
            " --> ",
            "<span style='color:",
            col_late,
            "'>**latest available year**</span>"
        ),
        caption = "Data: World Inequality Database (WID.world) via {wid}\n#30DayChartChallenge 2026 · Day 9 · Wealth · Ilya Kashnitsky @ikashnitsky.phd\n"
    ) +

    theme_ik(base_size = 16, base_family = "ah") +
    theme(
        plot.title = ggtext::element_markdown(size = 26),
        plot.subtitle = ggtext::element_markdown(size = 18),
        plot.caption.position = "plot",
        strip.text = element_text(face = "bold", size = 13),
        axis.title = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_text(size = 14, face = 2),
        panel.grid.major.y = element_line(color = "grey92", linewidth = 0.3),
        panel.grid.major.x = element_line(color = "grey88", linewidth = 0.3),
        panel.spacing = unit(1.8, "lines")
    )

ggsave(
    "out/09-wealth.pdf",
    p,
    width = 10,
    height = 7,
    dpi = 300,
    bg = "#eeffff"
)
