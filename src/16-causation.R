# ..........................................................
# 2026-04-16 -- 30DayChartChallenge
# Day 16: cuasation                -----------
# Ilya Kashnitsky, ilya.kashnitsky@gmail.com
# ..........................................................

library(tidyverse)
library(wpp2024)

# ── Mandatory setup ──────────────────────────────────────────────────────────
devtools::source_gist("653e1040a07364ae82b1bb312501a184")
sysfonts::font_add_google("Atkinson Hyperlegible", family = "ah")
showtext::showtext_auto()
theme_set(theme_ik(base_size = 14, base_family = "ah"))


# Load data
data(e0M1)
data(mxM1)
data(popM1)

# Extract Italy
italy_e0_wide <- e0M1 %>% filter(name == "Italy")
italy_mx_wide <- mxM1 %>% filter(name == "Italy")
italy_pop_wide <- popM1 %>% filter(name == "Italy")

# Convert e0 to long format
italy_e0 <- italy_e0_wide %>%
  pivot_longer(
    cols = matches("^[0-9]{4}$"),
    names_to = "year",
    values_to = "e0"
  ) %>%
  mutate(year = as.integer(year)) %>%
  select(year, e0)

# Convert rates and pop to long format
italy_mx_long <- italy_mx_wide %>%
  pivot_longer(
    cols = matches("^[0-9]{4}$"),
    names_to = "year",
    values_to = "mx"
  ) %>%
  mutate(year = as.integer(year))

italy_pop_long <- italy_pop_wide %>%
  pivot_longer(
    cols = matches("^[0-9]{4}$"),
    names_to = "year",
    values_to = "pop"
  ) %>%
  mutate(year = as.integer(year))

# Join and calculate total deaths
italy_deaths <- italy_mx_long %>%
  left_join(italy_pop_long, by = c("country_code", "name", "age", "year")) %>%
  mutate(deaths = pop * mx) %>%
  group_by(year) %>%
  summarise(deaths = sum(deaths, na.rm = TRUE), .groups = "drop")

# Merge
df <- italy_e0 %>%
  left_join(italy_deaths, by = "year") %>%
  arrange(year)

# Calculate log-differences
de0 <- df %>%
  mutate(
    le0 = log(e0),
    dle0 = le0 - lag(le0),
    l_death = log(deaths),
    dldeath = l_death - lag(l_death)
  ) %>%
  filter(!is.na(dle0) & !is.na(dldeath))

# Filter from 1970 onwards to match original code
de0_prd <- de0 %>%
  filter(year >= 1970)

# Regression model (down-weighting 2020 as original did)
de0_prd <- de0_prd %>%
  mutate(wgt = ifelse(year == 2020, 0, 1))

fit <- lm(dle0 ~ dldeath, data = de0_prd, weights = wgt)
de0_prd <- de0_prd %>%
  mutate(dle0_pred = predict(fit, newdata = de0_prd))

# Create the plot
p <- de0_prd %>%
  ggplot(aes(x = dldeath, y = dle0)) +
  geom_hline(yintercept = 0, linewidth = 1, color = "#269292") +
  geom_vline(xintercept = 0, linewidth = 1, color = "#269292") +
  geom_line(
    aes(y = dle0_pred),
    color = "#C2185B",
    linewidth = 2,
    alpha = .5
  ) +
  geom_point(aes(color = ifelse(year == 2020, TRUE, FALSE))) +
  scale_x_continuous(breaks = seq(-0.2, 0.2, 0.1)) +
  scale_y_continuous(breaks = seq(-0.2, 0.2, 0.02)) +
  scale_color_manual(values = c("#074444", "#D02090")) +
  annotate(
    "text",
    y = -0.012,
    x = 0.15,
    color = "#D02090",
    label = "2020",
    family = "ah",
    size = 4,
    fontface = "bold"
  ) +
  labs(
    x = expression(Delta ~ log ~ D),
    y = expression(Delta ~ log ~ e[0]),
    title = "A near-perfect linear link between changes in\nlife expectancy and total death counts",
    subtitle = "Year-on-year relative changes in life expectancy VS year-on-year relative changes in total death counts, Italy, males, 1950-2023",
    color = NULL,
    caption = "Data: UN World Population Prospects 2024 (wpp2024 R package)\n#30DayChartChallenge 2026 · Day 16 · causation · Ilya Kashnitsky @ikashnitsky.phd\n"
  ) +
  theme(
    plot.title = element_text(size = 23),
    plot.subtitle = ggtext::element_markdown(size = 8.4, face = 2),
    plot.caption.position = "plot",
    legend.position = "none"
  )

# ── Export ────────────────────────────────────────────────────────────────────
ggsave(
  "out/16-causation.pdf",
  p,
  width = 8,
  height = 8,
  dpi = 300,
  bg = "#eeffff"
)
