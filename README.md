# NAVA AI & Visual Arts Practice Survey 2025

## Authors
Charles Crabtree, Senior Lecturer, School of Social Sciences, Monash University and K-Club Professor, University College, Korea University.

## Overview
Analysis of the National Association for the Visual Arts (NAVA) survey on AI and visual arts practice, fielded to ~890 Australian creative practitioners in 2025. Includes descriptive analysis, thematic coding of open-text responses, and a presentation slide deck.

## Requirements
- R (≥ 4.0) with packages: `readxl`, `tidyverse`, `ggthemes`, `here`, `stringr`, `janitor`
- Optional: `showtext` for Google Fonts in figures

## Replication
Run scripts in numbered order from the project root:
```r
source("R/01_parse_data.R")
source("R/02_qualitative_analysis.R")
source("R/03_figures.R")
```

## Data
Raw survey data provided by NAVA. Not for redistribution without NAVA's permission.
