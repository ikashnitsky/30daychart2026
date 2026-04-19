# ..........................................................
# 2026-04-15 -- 30DayChartChallenge
# Day 15: correlation                -----------
# Ilya Kashnitsky, ilya.kashnitsky@gmail.com
# ..........................................................

library(tidyverse)
library(ggtext)
library(showtext)
library(ggrepel)
library(patchwork)

devtools::source_gist("653e1040a07364ae82b1bb312501a184")
sysfonts::font_add_google("Atkinson Hyperlegible", family = "ah")
showtext::showtext_auto()
theme_set(theme_ik(base_family = "ah"))

# Approximate data reconstructed from Kenny (2015) & Calling Bullshit case study
# Age at death (median) by genre, from the original chart
# Genre founding decade from musicology consensus
genres <- tribble(
  ~genre        , ~age_at_death_m , ~age_at_death_f , ~founded_decade ,
  "Gospel"      ,              74 ,              74 ,            1920 ,
  "Blues"       ,              71 ,              70 ,            1910 ,
  "Jazz"        ,              69 ,              71 ,            1910 ,
  "Country"     ,              67 ,              73 ,            1920 ,
  "R&B"         ,              62 ,              65 ,            1940 ,
  "Rock"        ,              56 ,              55 ,            1950 ,
  "New Age"     ,              55 ,              60 ,            1970 ,
  "Techno"      ,              48 ,              45 ,            1980 ,
  "Disco"       ,              47 ,              49 ,            1970 ,
  "Punk"        ,              41 ,              40 ,            1970 ,
  "Metal"       ,              38 , NA              ,            1970 ,
  "Rap/Hip-hop" ,              33 , NA              ,            1980
)

df <- genres |>
  pivot_longer(
    age_at_death_m:age_at_death_f,
    names_to = "sex",
    values_to = "age_at_death"
  ) |>
  mutate(
    sex = if_else(sex == "age_at_death_m", "Male", "Female"),
    genre = fct_reorder(genre, age_at_death, .fun = mean, na.rm = TRUE)
  )

# Color encodes genre founding decade — the confounding variable
decade_pal <- c(
  "1910" = "#2c7bb6",
  "1920" = "#abd9e9",
  "1940" = "#ffffbf",
  "1950" = "#fdae61",
  "1970" = "#d7191c",
  "1980" = "#7b0000"
)

p <- ggplot(
  df,
  aes(x = age_at_death, y = genre, color = factor(founded_decade))
) +
  geom_line(aes(group = genre), color = "grey75", linewidth = 1.2) +
  geom_point(aes(shape = sex), size = 4.5, alpha = 0.9) +
  geom_vline(
    xintercept = 70,
    linetype = "dashed",
    color = "grey50",
    linewidth = 0.6
  ) +
  annotate(
    "text",
    x = 71,
    y = 0.6,
    label = "US pop. avg.",
    hjust = 0,
    size = 4,
    family = "ah",
    color = "grey40"
  ) +
  scale_color_manual(
    values = decade_pal,
    name = "Genre founded"
  ) +
  scale_shape_manual(
    values = c("Male" = 18, "Female" = 20),
    name = "Sex"
  ) +
  scale_x_continuous(
    limits = c(25, 85),
    breaks = seq(30, 80, 10),
    labels = \(x) paste0(x, " yrs"),
    position = "top"
  ) +
  guides(
    color = guide_legend(order = 1, override.aes = list(shape = 15, size = 5)),
    shape = guide_legend(order = 2)
  ) +
  labs(
    title = "Do **rap musicians** really die 40 years younger than **jazz musicians**?",
    subtitle = "Age at death by genre — but genre age is the real confound (right-censoring)",
    caption = "Data: Kenny & Asher (2015), Med Probl Perform Art; Calling Bullshit case study (Bergstrom & West)\n#30DayChartChallenge 2026 · Day 15 · Correlation · Ilya Kashnitsky @ikashnitsky.phd\n",
    x = NULL,
    y = NULL
  ) +
  theme(
    plot.title = ggtext::element_markdown(size = 16),
    plot.subtitle = ggtext::element_markdown(size = 11),
    plot.caption.position = "plot",
    legend.position = "right",
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_line(color = "grey90"),
    axis.text.y = element_text(size = 14, face = 2)
  ) +
  # Annotation arrow pointing to hip-hop, explaining censoring
  annotate(
    "richtext",
    x = 41,
    y = 1.7,
    label = "Rap/Hip-hop **founded ~1980**<br>— most artists still alive!<br>Only early deaths recorded.",
    hjust = 0,
    vjust = 0.5,
    size = 3.5,
    family = "ah",
    fill = NA,
    label.color = NA,
    color = "#7b0000"
  ) +
  annotate(
    "segment",
    x = 40,
    xend = 34,
    y = 1.3,
    yend = 1.05,
    arrow = arrow(length = unit(0.2, "cm")),
    color = "#7b0000"
  )

ggsave(
  "out/15-correlation.pdf",
  p,
  width = 10,
  height = 7,
  dpi = 300,
  bg = "#eeffff"
)
