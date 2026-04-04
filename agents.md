# agents.md — AI Assistant Instructions

> Loaded automatically by Positron Assistant and GitHub Copilot coding agent.
> Distilled from project-owner system prompts. Last updated: 2026-03-31.

---

## Project context


**What does this project do?**
This is a project for generating dataviz for the #30DayChartChallenge 2026 Edition

**Who is the primary audience for outputs?**
R users and dataviz practitioner via social media: LinkedIn, Bluesky, Mastodon.

**What data sources does it connect to?**
Usially just fetches small datasets from the web.

**What are the main output artefacts?**
Plots.

**Are there naming conventions or domain-specific abbreviations the assistant should know?**
Just the ususal R ones. 

**Any constraints or policies specific to this project?**
be reasonable =)

---

## Who is the developer

**Ilya Kashnitsky** — quantitative demographer and R developer.
Personal site: <https://ikashnitsky.phd> · ORCID: 0000-0003-1835-8687

Primary work context: academic research, data wrangling, statistical modelling,
demographic visualization, and R package development.

---

## Core philosophy

- **FOSS by default.** Open, community-driven tools always. Never suggest proprietary alternatives when a FOSS option is adequate.
- **Code over explanation.** Deliver working code first. If a solution has performance or correctness trade-offs, state them concisely after the code.
- **Rank solutions by:** correctness → readability → performance → dependency weight.
- Flag deprecated functions and proactively suggest their modern replacements.

---

## R ecosystem preferences

### Tidyverse first

Use **dplyr, tidyr, purrr, ggplot2, readr, stringr, lubridate** as the default grammar for all data manipulation and wrangling tasks. Follow the [tidyverse style guide](https://style.tidyverse.org/). Prefer tidy evaluation and `rlang` over base R metaprogramming where readability matters.

### Fastverse for performance

When data size, memory pressure, or speed demands it, switch to **data.table, collapse, kit, magrittr**. Know when to bridge idioms (`tidytable`, `dtplyr`) and when to go native `data.table` for maximum throughput. Always call out the reason for the switch.

### Tidyverse style rules (apply to all R code)

- `snake_case` for all variable and function names.
- Explicit namespacing (`dplyr::filter()`) in package code; optional in scripts.
- Pipe with `|>` (base R native pipe); use `%>%` only when passing the LHS to a non-first argument via the `_` placeholder requires `magrittr`.
- **Inline anonymous functions: always use purrr formula syntax `~ .x`**, not the base R lambda `\(x)`. Examples:

  ```r
  # CORRECT
  map(dfs, ~ .x |> janitor::clean_names())
  map2(x, y, ~ .x / sum(.y))
  keep(cols, ~ is.numeric(.x))

  # AVOID
  map(dfs, \(x) janitor::clean_names(x))
  map2(x, y, \(x, y) x / sum(y))
  keep(cols, \(x) is.numeric(x))
  ```

  Use `~ .x` for single-argument lambdas and `~ .x ... .y` for two-argument forms. For three or more arguments, define a named function instead.

- `ungroup()` after every grouped operation unless intentionally left grouped — annotate with a comment if left grouped on purpose.
- Prefer `across()` over `_if()` / `_at()` / `_all()` scoped variants (deprecated since dplyr 1.0).

### Package development standards

Scaffold with **devtools + usethis + roxygen2 + testthat + pkgdown**.

- Enforce `R CMD CHECK` cleanliness (0 errors, 0 warnings, 0 notes).
- Write `NEWS.md` entries for every user-facing change.
- Document all exported functions with working `@examples`.
- Never export internal helpers; use `@noRd` for them.

### Reproducible environments

Use **renv** for all project environments. When suggesting package installs, always note `renv::snapshot()` after. On Windows, remind about Rtools PATH if building from source.

---

## Database — Windows + Oracle reality

- Use **DBI + ROracle** as the preferred driver (OML4R-compatible). Fall back to **DBI + odbc** only when ROracle is unavailable.
- **Never hardcode credentials.** Always use `keyring` or `Sys.getenv()`.
- Proactively handle Windows Oracle friction:
  - `OCI_LIB64` env var must point to Oracle Instant Client.
  - Oracle Instant Client directory must be on the system `PATH`.
  - Check `Sys.getenv("PATH")` before diagnosing OCI errors.
  - Watch for CRLF line endings in `.sql` files called from R scripts.
- When writing large-result queries, use `dbFetch()` with `n` chunks or `DBI::dbSendQuery()` + cursor iteration — never pull millions of rows at once.
- For analytical workloads that don't require Oracle, prefer **DuckDB** (`duckdb` package) with Parquet files.

---

## Data visualization — ggplot2 rules (non-negotiable)

These rules apply to every ggplot2 code block produced.

**Rule 0 — Show the data.** Never collapse to summary statistics without also showing the underlying distribution or variation. Layer raw data under summaries.

**Rule 1 — Text is horizontal.** No rotated or angled labels, ever. Flip to horizontal bar charts when category names are long. Label lines with `geomtextpath`, annotate points with `ggrepel`, color title words with `ggtext::element_markdown()` instead of adding legends.

**Rule 2 — Maximize font size.** For slides use `base_size = 14` minimum. Legibility over density.

**Rule 3 — Colorblind-friendly.** Default: `scale_*_viridis_d/c()`. Use `{paletteer}` for palette exploration. Never rainbow or red-green contrasts. Three-part compositions: `{tricolore}`; bivariate maps: `{biscale}`.

**Rule 4 — Highlight the story.** Gray out background data, saturate focal elements, annotate key points. Use `ggforce::geom_mark_ellipse()` for callouts.

**Rule 5 — Simplicity wins.** Dotplots beat bar charts. Faceted lines beat stacked areas. Every mark earns its place.

### Chart type defaults

| Task | Package / geom |
|---|---|
| Ranking | `geom_point()` horizontal, or `ggalt::geom_dumbbell()` |
| Rank over time | `ggbump::geom_bump()` |
| Distribution (many groups) | `ggridges::geom_density_ridges()` |
| Distribution (few groups) | `ggdist::stat_halfeye()` or `see::geom_half_violin()` |
| Uncertainty | `ggdist::stat_pointinterval()` — never bare error bars |
| Categorical scatter | `ggbeeswarm::geom_quasirandom()` — never `geom_jitter()` |
| Labeled line chart | `geomtextpath::geom_textline()` |
| Geographic facets | `geofacet::facet_geo()` always when data has a regional unit |
| Multivariate | `GGally::ggpairs()` or `GGally::ggparcoord()` |
| Marginals on scatter | `ggside::geom_xsidehistogram()` |
| Model diagnostics | `ggfortify::autoplot()` |

### Theme defaults

`hrbrthemes::theme_ipsum()` as baseline. `cowplot::theme_cowplot()` for publication. `ggdark::dark_theme_minimal()` for dark outputs.

### Multi-panel layout

`{patchwork}` as primary; `{cowplot}` for image overlays and precise insets.

---

## Platform and tech stack

- **OS:** Windows 10 as the primary machine (Surface Pro); macOS available (older Intel MacBook). Treat Windows as the default target unless told otherwise.
- **Python environments:** Pixi only (not conda/venv/poetry). When writing Python setup instructions, always use `pixi` syntax.
- **Analytical data formats:** DuckDB, Parquet, SAS files (via `haven`), R native.
- **AI/LLM tooling in R:** `ellmer`, `chattr`, GitHub Copilot, Positron AI. Recommend these when they reduce friction; flag limitations honestly.
- **R–Python interop:** `reticulate`. Suggest Python libraries only when R has no clean equivalent. Never recommend Python where R is the better tool.
- **C++/Rust extensions:** `Rcpp` for existing C++ work; prefer `rextendr` (extendr) for new CPU-bound extensions where safety and speed matter.

---

## Reproducible examples

When the user provides no data, always build a minimal reproducible example using built-in datasets (`mtcars`, `penguins`, `diamonds`, `nycflights13`) or inline `tibble()` construction. **Never write code against an assumed data structure that has not been explicitly defined.**

---

## Response style

- **Lead with working code**, not preamble.
- When multiple approaches exist, present the recommended one first, then list alternatives briefly.
- For every non-trivial code solution, append a **⚠️ Watch Out** block with 1–2 *specific*, code-relevant pitfalls tied directly to the code just written. Make these concrete — never generic. Examples of good Watch Outs:
  - "Forgetting `ungroup()` after the grouped `mutate()` above will silently carry groups into the next pipeline step."
  - "ROracle will silently truncate character columns wider than 4000 bytes when writing to Oracle VARCHAR2 — use CLOB for long text."
  - "The `renv` cache on a Windows network drive can cause lock file conflicts; set `RENV_PATHS_CACHE` to a local drive."
- Prefer specific behavioral language over vague filler.
- Skip explanations of fundamentals unless explicitly asked.

---

## What to avoid

- Suggesting proprietary tooling when FOSS exists.
- `attach()`, `setwd()`, or bare `library()` calls inside package code.
- Hard-coded file paths with backslashes; use `here::here()` or `file.path()`.
- `T` / `F` as aliases for `TRUE` / `FALSE`.
- `1:nrow(df)` — use `seq_len(nrow(df))` or `seq_along()`.
- Base R `\(x)` anonymous functions — use purrr `~ .x` formula syntax instead.
- `apply` family where `purrr::map*` or `vapply` is clearer.
- `subset()` inside functions (non-standard evaluation hazard).
- Printing large objects without `head()` or `glimpse()`.
