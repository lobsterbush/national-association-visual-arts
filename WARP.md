# NAVA AI & Visual Arts Practice Survey 2025

**Status:** Active
**Description:** Analysis and presentation of NAVA's survey on generative AI among 890 Australian visual artists.
**Authors:** Ben Eltham (Monash University), Charles Crabtree (Monash University)
**Last Updated:** 2026-04-13

## Tech Stack

- **Analysis:** R (tidyverse, ggthemes, readxl, janitor, scales, here)
- **Presentation:** reveal.js 4.6.1 via cdnjs CDN, self-contained HTML
- **Fonts:** Cormorant Garamond, Plus Jakarta Sans, JetBrains Mono (Google Fonts CDN)
- **Deployment:** GitHub Pages from `docs/` on master, password-gated (`index.html` client-side gate)

## Key Architecture

- **Data pipeline:** Raw xlsx → `01_parse_data.R` → `data/clean/*.rds` → `02_qualitative_analysis.R` (thematic coding, platform counts, quotes) → `03_figures.R` (publication PDFs) + `04_slide_figures.R` (title-free PNGs for slides)
- **Slide figures:** `output/figures/slides/*.png` — title-free, transparent bg, base_size=16, 300 DPI. Titles live in the HTML `<h2>`, not the image.
- **Publication figures:** `output/figures/*.pdf` — full titles/subtitles, cairo_pdf, 6.5" wide.
- **Color palette (slides):** indigo `#3730a3` (primary), lighter indigo `#6366f1` (secondary), slate `#94a3b8` (neutral), slate-600 `#475569` (annotations). No red or orange.
- **Presentation:** `presentation.html` at root (working copy), `docs/presentation.html` (deployed copy). Must be kept in sync manually.
- **Dark slides:** Use `data-background-color="#0f0f17"` + class `title-slide`/`divider`/`closing`. White slides use `data-background-color="#f9f9fb"`. `backgroundTransition: 'none'` to prevent flash.
- **GitHub Pages URL:** https://lobsterbush.github.io/national-association-visual-arts/

## Setup

```bash
# Generate clean data
Rscript R/01_parse_data.R
Rscript R/02_qualitative_analysis.R

# Generate figures
Rscript R/03_figures.R          # publication PDFs
Rscript R/04_slide_figures.R    # slide PNGs

# Deploy: copy presentation.html to docs/, commit, push
cp presentation.html docs/presentation.html
```

## Next Steps

- Sync `docs/presentation.html` automatically (currently manual cp)
- Add README.md
