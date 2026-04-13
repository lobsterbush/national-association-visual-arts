# ---------------------------------------------------------------------
# 02_qualitative_analysis.R
# Purpose: Thematic coding of open-text responses, platform frequency
#          coding from Q9, quasi-coding of "Other" responses, and
#          extraction of representative quotes for the slide deck.
# Input:   data/clean/open_text.rds, data/clean/closed_ended.rds
# Output:  data/clean/platform_counts.rds
#          data/clean/q1_other_coded.rds
#          data/clean/thematic_codes.rds
#          output/quotes/representative_quotes.txt
# ---------------------------------------------------------------------

library(tidyverse)
library(here)
library(stringr)

open_text    <- readRDS(here("data", "clean", "open_text.rds"))
closed_ended <- readRDS(here("data", "clean", "closed_ended.rds"))

# =====================================================================
# PART 1: Platform/tool coding from Q9 (n = 534)
# =====================================================================

q9 <- open_text |> filter(qnum == "Q9")

# Lowercase for matching
q9 <- q9 |> mutate(text_lower = str_to_lower(text))

# Define platform patterns (case-insensitive via text_lower)
platform_patterns <- tribble(
  ~platform,          ~pattern,
  "ChatGPT",          "chat\\s*g[bp]t|chatgpt|openai|chat got|chart gpt|cha[rt]\\s*g",
  "Midjourney",       "midjourney|mid\\s*journey|midjorney",
  "Adobe AI",         "adobe|photoshop|firefly|lightroom|premiere|illustrator ai",
  "Claude",           "claude|anthropic|claud[ei]",
  "Gemini",           "gemini|google ai",
  "Copilot",          "copilot|co-pilot|co pilot|microsoft pilot",
  "Grammarly",        "grammarly|grammerl",
  "Deepseek",         "deepseek|deep\\s*seek",
  "DALL-E",           "dall-?e|dali|daly",
  "Stable Diffusion", "stable\\s*diffusion|comfyui|automatic1111",
  "Canva AI",         "canva",
  "Runway ML",        "runway",
  "Leonardo AI",      "leonardo",
  "Perplexity",       "perplexity",
  "Grok",             "grok",
  "Suno/Udio",        "suno|udio",
  "ElevenLabs",       "elevenlabs|eleven labs",
  "Kling",            "kling",
  "HeyGen",           "heygen",
  "Sora",             "sora"
)

# Count mentions per platform
platform_counts <- platform_patterns |>
  rowwise() |>
  mutate(
    n_mentions = sum(str_detect(q9$text_lower, pattern)),
    pct = n_mentions / nrow(q9)
  ) |>
  ungroup() |>
  arrange(desc(n_mentions))

cat("\n=== AI Platform Usage (Q9, n =", nrow(q9), ") ===\n")
print(platform_counts |> select(platform, n_mentions, pct), n = 25)

saveRDS(platform_counts, here("data", "clean", "platform_counts.rds"))

# =====================================================================
# PART 2: Quasi-coding "Other" responses for Q1 (practitioner type)
# =====================================================================

q1_other <- open_text |> filter(qnum == "Q1")
q1_other <- q1_other |> mutate(text_lower = str_to_lower(text))

# Code "Other" into existing or new categories
q1_other <- q1_other |>
  mutate(recoded = case_when(
    str_detect(text_lower, "illustrat")             ~ "Illustrator",
    str_detect(text_lower, "writer|poet|author")    ~ "Writer/Poet",
    str_detect(text_lower, "teach|educator|student") ~ "Educator/Student",
    str_detect(text_lower, "music|sound")           ~ "Musician/Sound Artist",
    str_detect(text_lower, "film|video|moving image") ~ "Filmmaker/Video Artist",
    str_detect(text_lower, "game")                  ~ "Game Developer/Designer",
    str_detect(text_lower, "research|academ|phd")   ~ "Researcher/Academic",
    str_detect(text_lower, "perform|theat")         ~ "Performance Artist",
    str_detect(text_lower, "activi")                ~ "Activist",
    str_detect(text_lower, "therap")                ~ "Art Therapist",
    str_detect(text_lower, "public")                ~ "Public Artist",
    TRUE                                            ~ "Other (miscellaneous)"
  ))

cat("\n=== Q1 'Other' Recoding ===\n")
q1_other |> count(recoded, sort = TRUE) |> print(n = 20)

saveRDS(q1_other, here("data", "clean", "q1_other_coded.rds"))

# =====================================================================
# PART 3: Thematic coding of narrative responses
# =====================================================================
# Code major themes across Q19 (AI impact stories), Q26 (general
# comments), Q14 (unauthorized use), Q12 (successful use)

narrative_qs <- open_text |>
  filter(qnum %in% c("Q19", "Q26", "Q14", "Q12", "Q4")) |>
  mutate(text_lower = str_to_lower(text))

# Filter out very short or non-substantive responses
narrative_qs <- narrative_qs |>
  filter(nchar(text) > 20,
         !str_detect(text_lower, "^(n/?a|none|no|yes|nil|nothing|na|see above|as above)$"))

# Define thematic codes with regex patterns
theme_patterns <- tribble(
  ~theme,                          ~pattern,
  # --- Economic impacts ---
  "Lost work/commissions",         "lost.*(job|work|commission|contract|opportunit|gig)|no longer.*(hired|commission)|replaced.*(by|with) ai|lost income|work.*dried up",
  "Fee pressure/devaluation",      "lower.*(fee|rate|price|pay)|cheap|undercut|devalue|race to the bottom|expect.*(less|cheaper|free)|worth less",
  "Market flooding",               "flood|saturate|glut|oversuppl|dilut|too much.*content|swamp",
  # --- Copyright/IP ---
  "Work scraped without consent",  "scrap|stolen|taken|without.*(consent|permission|knowledge)|used my|train.*on.*my|harvested|appropriat",
  "IP/copyright concerns",         "copyright|intellectual property|ip right|ownership|who owns|legal protect|infring",
  "Deepfakes/impersonation",       "deepfake|impersonat|fake|clone|mimic|replicate.*style|copy.*style|imitat.*my",
  # --- Trust/authenticity ---
  "Eroded trust in art",           "trust|authentic|genuine|originality|real.*art|human.*made|soul|heart|meaning|degrad.*value|cheapened",
  "Uncertainty who made what",     "can'?t tell|don'?t know.*if|indistinguish|blur.*line|hard to tell|which is (real|human)|who made",
  # --- Emotional/existential ---
  "Anxiety/fear/despair",          "anxi|fear|despair|terrif|dread|existential|overwhelm|hopeless|depress|worried|concern|scary|frightening|devastat|angry|rage|furious",
  "Grief/loss of meaning",         "grief|mourn|loss.*meaning|pointless|why bother|purpose|what'?s the point|sad.*about|heartbreak",
  # --- Positive/pragmatic ---
  "Useful for admin/grants",       "admin|grant|application|proposal|bureau|paperwork|form|email|correspondence|clerical|time.*sav|efficien",
  "Accessibility/disability",      "access|disab|dyslexia|adhd|mobility|assist|impairment|second language|esl|non-?native",
  "Creative exploration",          "experiment|explor|new idea|inspir|brainstorm|creativ.*tool|creative possibility|play.*with|exciting|potential",
  # --- Regulation/policy ---
  "Need for regulation",           "regulat|legislat|law|ban|govern|polic|enforce|protect.*artist|mandatory|compulsory|rule",
  "Compensation/licensing",        "compensat|licens|royalt|pay.*artist|remunerat|collect.*manag",
  "Labelling/transparency",        "label|disclos|transparen|watermark|identify.*ai|mark.*ai|flag.*ai",
  # --- First Nations/cultural ---
  "Indigenous cultural concerns",  "first nation|indigenous|aboriginal|torres strait|cultur.*knowledge|cultur.*content|icip|stolen.*stories|mob|country.*stories|dreaming"
)

# Apply thematic codes
coded_responses <- narrative_qs |>
  select(qnum, respondent_id, text, text_lower)

for (i in seq_len(nrow(theme_patterns))) {
  theme_name <- theme_patterns$theme[i]
  pattern    <- theme_patterns$pattern[i]
  coded_responses <- coded_responses |>
    mutate(!!theme_name := str_detect(text_lower, pattern))
}

# Pivot to long format for analysis
theme_cols <- theme_patterns$theme
coded_long <- coded_responses |>
  pivot_longer(cols = all_of(theme_cols),
               names_to = "theme",
               values_to = "present") |>
  filter(present)

# Count theme frequency
theme_summary <- coded_long |>
  count(theme, sort = TRUE) |>
  mutate(pct_of_responses = n / nrow(narrative_qs))

cat("\n=== Thematic Codes (across Q4, Q12, Q14, Q19, Q26) ===\n")
cat("Total coded responses:", nrow(narrative_qs), "\n\n")
print(theme_summary, n = 20)

# Also by question
theme_by_q <- coded_long |>
  count(qnum, theme, sort = TRUE)

cat("\n=== Top Themes by Question ===\n")
theme_by_q |>
  group_by(qnum) |>
  slice_max(n, n = 5) |>
  print(n = 30)

saveRDS(coded_responses, here("data", "clean", "thematic_codes.rds"))
saveRDS(theme_summary,   here("data", "clean", "theme_summary.rds"))
saveRDS(theme_by_q,      here("data", "clean", "theme_by_question.rds"))

# =====================================================================
# PART 4: Extract representative quotes per theme
# =====================================================================

# For each theme, pull 2-3 high-quality quotes (medium length, vivid,
# varied across questions)

extract_quotes <- function(theme_name, coded_df, n_quotes = 3) {
  candidates <- coded_df |>
    filter(!!sym(theme_name)) |>
    mutate(text_len = nchar(text)) |>
    # Prefer medium-length quotes (50-400 chars)
    filter(text_len >= 50, text_len <= 500) |>
    arrange(desc(text_len))

  if (nrow(candidates) == 0) {
    candidates <- coded_df |>
      filter(!!sym(theme_name)) |>
      mutate(text_len = nchar(text)) |>
      filter(text_len >= 30) |>
      arrange(desc(text_len))
  }

  # Try to pick from different questions
  if (nrow(candidates) > n_quotes) {
    selected <- candidates |>
      group_by(qnum) |>
      slice_head(n = 1) |>
      ungroup() |>
      slice_head(n = n_quotes)
    # Fill remainder from any question
    if (nrow(selected) < n_quotes) {
      remaining <- candidates |>
        filter(!respondent_id %in% selected$respondent_id) |>
        slice_head(n = n_quotes - nrow(selected))
      selected <- bind_rows(selected, remaining)
    }
  } else {
    selected <- candidates |> slice_head(n = n_quotes)
  }

  selected |>
    select(qnum, respondent_id, text) |>
    mutate(theme = theme_name)
}

all_quotes <- map_dfr(theme_cols, extract_quotes, coded_df = coded_responses)

# Write human-readable quotes file
sink(here("output", "quotes", "representative_quotes.txt"))
cat("NAVA AI & Visual Arts Survey 2025 — Representative Quotes\n")
cat("=" |> strrep(60), "\n\n")

for (th in unique(all_quotes$theme)) {
  cat("\n## ", th, "\n")
  cat("-" |> strrep(40), "\n")
  quotes_for_theme <- all_quotes |> filter(theme == th)
  for (j in seq_len(nrow(quotes_for_theme))) {
    row <- quotes_for_theme[j, ]
    cat(sprintf('[%s] "%s"\n\n', row$qnum, row$text))
  }
}
sink()

saveRDS(all_quotes, here("data", "clean", "representative_quotes.rds"))

cat("\nExtracted", nrow(all_quotes), "representative quotes across",
    length(unique(all_quotes$theme)), "themes.\n")
cat("Written to output/quotes/representative_quotes.txt\n")

# =====================================================================
# PART 5: Summary statistics for reporting
# =====================================================================

cat("\n\n=== SURVEY OVERVIEW ===\n")
cat("Total open-text responses analyzed:", nrow(narrative_qs), "\n")
cat("Distinct themes coded:", nrow(theme_summary), "\n")
cat("AI platforms identified in Q9:", nrow(platform_counts |> filter(n_mentions > 0)), "\n")
cat("Q1 'Other' responses recoded:", nrow(q1_other), "\n\n")

cat("Top 5 themes by frequency:\n")
theme_summary |> slice_head(n = 5) |>
  mutate(pct_label = scales::percent(pct_of_responses, accuracy = 0.1)) |>
  select(theme, n, pct_label) |>
  print()

cat("\nTop 5 AI platforms by mention count:\n")
platform_counts |> slice_head(n = 5) |>
  select(platform, n_mentions) |>
  print()
