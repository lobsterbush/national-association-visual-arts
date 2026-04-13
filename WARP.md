# NAVA AI & Visual Arts Practice Survey 2025

**Status:** Active
**Description:** Analysis of NAVA's survey on AI and visual arts practice among ~890 Australian creative practitioners.
**Authors:** Charles Crabtree (Monash University) and collaborator.

## Data
- Raw xlsx in `data/raw/` — aggregate tabulations per question + individual open-text responses
- Cleaned data in `data/clean/` as `.rds` files

## Scripts
- `R/01_parse_data.R` — Parse xlsx into tidy data frames
- `R/02_qualitative_analysis.R` — Thematic coding of open-text responses
- `R/03_figures.R` — Publication-ready ggplot2 figures

## Conventions
- R with ggplot2 + theme_tufte(), here::here() for paths
- Figures exported as PDF (cairo_pdf, 6.5" width)
- Slides via rhetoric-of-decks (reveal.js HTML)
