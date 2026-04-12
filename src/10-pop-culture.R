# ..........................................................
# 2026-04-10 -- 30DayChartChallenge
# Day 10: Pop Culture
#               -----------
# Ilya Kashnitsky, ilya.kashnitsky@gmail.com
# ..........................................................

library(tidyverse)
library(ggdist)
library(ggtext)
library(ggrepel)
library(showtext)
library(devtools)
library(lubridate)

# ── Mandatory setup ────────────────────────────────────────────────────────────
devtools::source_gist("653e1040a07364ae82b1bb312501a184")
sysfonts::font_add_google("Atkinson Hyperlegible", family = "ah")
showtext::showtext_auto()
theme_set(theme_ik(base_family = "ah"))

# ── Data: TidyTuesday Billboard Hot 100 (1958–2021) ───────────────────────────
billboard_raw <- readr::read_csv(
  "https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2021/2021-09-14/billboard.csv",
  show_col_types = FALSE
)

# First week at #1 per main performer (strip featured credits)
first_no1 <- billboard_raw |>
  filter(week_position == 1) |>
  mutate(
    week_date = mdy(week_id),
    main_artist = str_remove(
      performer,
      "(?i)\\s+(feat\\.?|ft\\.?|featuring|with ).*"
    ) |>
      str_trim()
  ) |>
  group_by(main_artist) |>
  summarise(
    first_hit_date = min(week_date),
    first_hit_year = year(min(week_date)),
    .groups = "drop"
  )

# ── Curated artist metadata: birth year + genre ────────────────────────────────
# For groups: birth year of lead/founding member
artist_meta <- tribble(
  ~main_artist                  , ~birth_year , ~genre    ,
  # ── Rock ──────────────────────────────────────────────────────────────────
  "Elvis Presley"               ,        1935 , "Rock"    ,
  "The Beatles"                 ,        1940 , "Rock"    , # John Lennon
  "The Rolling Stones"          ,        1943 , "Rock"    , # Mick Jagger
  "The Monkees"                 ,        1945 , "Rock"    , # Davy Jones
  "Paul McCartney"              ,        1942 , "Rock"    ,
  "Wings"                       ,        1942 , "Rock"    ,
  "Elton John"                  ,        1947 , "Rock"    ,
  "Rod Stewart"                 ,        1945 , "Rock"    ,
  "Billy Joel"                  ,        1949 , "Rock"    ,
  "John Denver"                 ,        1943 , "Rock"    ,
  "Eagles"                      ,        1947 , "Rock"    , # Glenn Frey
  "The Police"                  ,        1951 , "Rock"    , # Sting
  "Phil Collins"                ,        1951 , "Rock"    ,
  "Bruce Springsteen"           ,        1949 , "Rock"    ,
  "Tina Turner"                 ,        1939 , "Rock"    ,
  "Bon Jovi"                    ,        1962 , "Rock"    ,
  "Joan Jett & the Blackhearts" ,        1958 , "Rock"    ,
  "Santana"                     ,        1947 , "Rock"    ,
  # ── Pop ───────────────────────────────────────────────────────────────────
  "Michael Jackson"             ,        1958 , "Pop"     ,
  "Prince"                      ,        1958 , "Pop"     ,
  "Madonna"                     ,        1958 , "Pop"     ,
  "Whitney Houston"             ,        1963 , "Pop"     ,
  "Mariah Carey"                ,        1969 , "Pop"     ,
  "Celine Dion"                 ,        1968 , "Pop"     ,
  "Olivia Newton-John"          ,        1948 , "Pop"     ,
  "Diana Ross"                  ,        1944 , "Pop"     ,
  "Lionel Richie"               ,        1949 , "Pop"     ,
  "Cyndi Lauper"                ,        1953 , "Pop"     ,
  "Paula Abdul"                 ,        1962 , "Pop"     ,
  "Janet Jackson"               ,        1966 , "Pop"     ,
  "Bee Gees"                    ,        1946 , "Pop"     , # Barry Gibb
  "Andy Gibb"                   ,        1958 , "Pop"     ,
  "Donna Summer"                ,        1948 , "Pop"     ,
  "ABBA"                        ,        1945 , "Pop"     , # Agnetha Fältskog
  "Debbie Gibson"               ,        1970 , "Pop"     ,
  "Tiffany"                     ,        1971 , "Pop"     ,
  "New Kids on the Block"       ,        1970 , "Pop"     , # Jordan Knight
  "Ace of Base"                 ,        1966 , "Pop"     , # Jenny Berggren
  "Ricky Martin"                ,        1971 , "Pop"     ,
  "Backstreet Boys"             ,        1980 , "Pop"     , # Nick Carter
  "Destiny's Child"             ,        1981 , "Pop"     ,
  "Beyoncé"                     ,        1981 , "Pop"     ,
  "Britney Spears"              ,        1981 , "Pop"     ,
  "Christina Aguilera"          ,        1980 , "Pop"     ,
  "Kelly Clarkson"              ,        1982 , "Pop"     ,
  "Katy Perry"                  ,        1984 , "Pop"     ,
  "Lady Gaga"                   ,        1986 , "Pop"     ,
  "Taylor Swift"                ,        1989 , "Pop"     ,
  "Adele"                       ,        1988 , "Pop"     ,
  "Justin Bieber"               ,        1994 , "Pop"     ,
  "Ariana Grande"               ,        1993 , "Pop"     ,
  "Ed Sheeran"                  ,        1991 , "Pop"     ,
  "One Direction"               ,        1994 , "Pop"     , # Harry Styles
  "Dua Lipa"                    ,        1995 , "Pop"     ,
  "Billie Eilish"               ,        2001 , "Pop"     ,
  "Olivia Rodrigo"              ,        2003 , "Pop"     ,
  "Doja Cat"                    ,        1995 , "Pop"     ,
  "SZA"                         ,        1990 , "Pop"     ,
  "Maroon 5"                    ,        1979 , "Pop"     , # Adam Levine
  "George Michael"              ,        1963 , "Pop"     ,
  "Wham!"                       ,        1963 , "Pop"     ,
  "Gotye"                       ,        1980 , "Pop"     ,
  "Meghan Trainor"              ,        1993 , "Pop"     ,
  "Carly Rae Jepsen"            ,        1985 , "Pop"     ,
  "Duran Duran"                 ,        1958 , "Pop"     , # Simon Le Bon
  "Culture Club"                ,        1961 , "Pop"     , # Boy George
  # ── R&B / Soul ────────────────────────────────────────────────────────────
  "Stevie Wonder"               ,        1950 , "R&B"     ,
  "Marvin Gaye"                 ,        1939 , "R&B"     ,
  "The Jackson 5"               ,        1958 , "R&B"     , # Michael Jackson
  "The Temptations"             ,        1939 , "R&B"     , # Eddie Kendricks
  "The Supremes"                ,        1944 , "R&B"     , # Diana Ross
  "Four Tops"                   ,        1936 , "R&B"     , # Levi Stubbs
  "Sly & the Family Stone"      ,        1943 , "R&B"     ,
  "Earth, Wind & Fire"          ,        1941 , "R&B"     , # Philip Bailey
  "Kool & the Gang"             ,        1951 , "R&B"     ,
  "Boyz II Men"                 ,        1973 , "R&B"     , # Wanya Morris
  "TLC"                         ,        1970 , "R&B"     , # T-Boz
  "Usher"                       ,        1978 , "R&B"     ,
  "Alicia Keys"                 ,        1981 , "R&B"     ,
  "Rihanna"                     ,        1988 , "R&B"     ,
  "Chris Brown"                 ,        1989 , "R&B"     ,
  "The Weeknd"                  ,        1990 , "R&B"     ,
  "Ashanti"                     ,        1980 , "R&B"     ,
  "Ne-Yo"                       ,        1979 , "R&B"     ,
  # ── Hip-Hop ───────────────────────────────────────────────────────────────
  "MC Hammer"                   ,        1962 , "Hip-Hop" ,
  "Vanilla Ice"                 ,        1967 , "Hip-Hop" ,
  "Sir Mix-a-Lot"               ,        1963 , "Hip-Hop" ,
  "Will Smith"                  ,        1968 , "Hip-Hop" ,
  "Puff Daddy"                  ,        1969 , "Hip-Hop" ,
  "Jay-Z"                       ,        1969 , "Hip-Hop" ,
  "Eminem"                      ,        1972 , "Hip-Hop" ,
  "Nelly"                       ,        1974 , "Hip-Hop" ,
  "50 Cent"                     ,        1975 , "Hip-Hop" ,
  "Kanye West"                  ,        1977 , "Hip-Hop" ,
  "T.I."                        ,        1980 , "Hip-Hop" ,
  "Lil Wayne"                   ,        1982 , "Hip-Hop" ,
  "Nicki Minaj"                 ,        1982 , "Hip-Hop" ,
  "Drake"                       ,        1986 , "Hip-Hop" ,
  "Kendrick Lamar"              ,        1987 , "Hip-Hop" ,
  "Macklemore"                  ,        1983 , "Hip-Hop" ,
  "Post Malone"                 ,        1995 , "Hip-Hop" ,
  "Cardi B"                     ,        1992 , "Hip-Hop" ,
  "Travis Scott"                ,        1991 , "Hip-Hop" ,
  "Lil Nas X"                   ,        1999 , "Hip-Hop" ,
  "Future"                      ,        1983 , "Hip-Hop" ,
  "Missy Elliott"               ,        1971 , "Hip-Hop" ,
  "Ludacris"                    ,        1977 , "Hip-Hop" ,
  "OutKast"                     ,        1975 , "Hip-Hop" , # André 3000
  "DaBaby"                      ,        1991 , "Hip-Hop" ,
  "Roddy Ricch"                 ,        1998 , "Hip-Hop" ,
  "Juice WRLD"                  ,        1998 , "Hip-Hop" ,
  # ── Country ───────────────────────────────────────────────────────────────
  "Kenny Rogers"                ,        1938 , "Country" ,
  "Dolly Parton"                ,        1946 , "Country" ,
  "Garth Brooks"                ,        1962 , "Country" ,
  "Shania Twain"                ,        1965 , "Country" ,
  "Tim McGraw"                  ,        1967 , "Country" ,
  "Faith Hill"                  ,        1967 , "Country" ,
  "Carrie Underwood"            ,        1983 , "Country" ,
  "Luke Bryan"                  ,        1976 , "Country" ,
  "Blake Shelton"               ,        1976 , "Country" ,
  "Morgan Wallen"               ,        1993 , "Country"
)

# ── Merge & compute age ────────────────────────────────────────────────────────
data <- first_no1 |>
  left_join(artist_meta, by = "main_artist") |>
  filter(!is.na(birth_year)) |>
  mutate(age_at_first_no1 = first_hit_year - birth_year) |>
  filter(age_at_first_no1 >= 10, age_at_first_no1 <= 75)

# Order genres by median age (oldest → youngest, top → bottom)
genre_order <- data |>
  group_by(genre) |>
  summarise(med = median(age_at_first_no1), .groups = "drop") |>
  arrange(med) |>
  pull(genre)

data <- data |>
  mutate(genre = factor(genre, levels = genre_order))

# ── Labels: youngest + oldest per genre ───────────────────────────────────────
label_pts <- data |>
  group_by(genre) |>
  slice(c(which.min(age_at_first_no1), which.max(age_at_first_no1))) |>
  ungroup() |>
  distinct(main_artist, .keep_all = TRUE)


# ── Plot ───────────────────────────────────────────────────────────────────────
p <- data |>
  ggplot(aes(x = age_at_first_no1, y = genre)) +

  # Individual artist dots — nudged below the slab
  geom_point(
    aes(color = genre),
    position = position_jitter(height = 0.06, seed = 42),
    size = 2.4,
    alpha = 0.65,
    show.legend = FALSE
  ) +

  # Halfeye distribution slab — nudged above
  stat_halfeye(
    aes(fill = genre, color = genre),
    .width = c(0.5, 0.9),
    slab_alpha = 0.55,
    point_size = 3.5,
    interval_size_range = c(0.8, 1.8),
    position = position_nudge(y = 0.22),
    show.legend = FALSE
  ) +

  # Labels: youngest & oldest per genre
  geom_text(
    data = label_pts,
    aes(label = main_artist, color = genre),
    family = "ah",
    size = 3.8,
    fontface = "bold",
    nudge_y = -0.15,
    show.legend = FALSE
  ) +

  scale_fill_viridis_d(option = "H", end = .85) +
  scale_color_viridis_d(option = "H", end = .85) +

  scale_x_continuous(
    breaks = seq(10, 70, by = 10),
    minor_breaks = seq(10, 70, by = 5),
    limits = c(8, 56),
    expand = c(0, 0),
    position = "top"
  ) +

  labs(
    title = "When do stars flare up?",
    subtitle = "Age at first Billboard Hot 100 #1 hit, by genre (1958-2021)",
    x = NULL,
    y = NULL,
    caption = "Data: Billboard Hot 100 via TidyTuesday 2021 · Birth years: Wikipedia\nFor groups, lead/founding member's birth year used\n#30DayChartChallenge 2026 · Day 10 · Pop Culture · Ilya Kashnitsky @ikashnitsky.phd\n"
  ) +

  theme(
    plot.title = ggtext::element_markdown(size = 34),
    plot.subtitle = ggtext::element_markdown(size = 14),
    plot.caption.position = "plot",
    panel.grid.major.y = element_blank(),
    axis.text.y = element_text(size = 16, face = "bold")
  )

ggsave(
  "out/10-pop-culture.pdf",
  plot = p,
  width = 8,
  height = 8,
  dpi = 300,
  bg = "#eeffff"
)
