# ---------------------------------------------------------------------
# 03_figures.R
# Purpose: Publication-ready ggplot2 figures for the NAVA AI & Visual
#          Arts Survey 2025.
# Input:   data/clean/*.rds
# Output:  output/figures/*.pdf
# ---------------------------------------------------------------------

library(tidyverse)
library(ggthemes)
library(here)
library(scales)

closed_ended    <- readRDS(here("data", "clean", "closed_ended.rds"))
platform_counts <- readRDS(here("data", "clean", "platform_counts.rds"))
theme_summary   <- readRDS(here("data", "clean", "theme_summary.rds"))
q1_other_coded  <- readRDS(here("data", "clean", "q1_other_coded.rds"))

# ---- Shared styling ----
accent   <- "#3730a3"
accent2  <- "#4338ca"
muted    <- "#6b6b7e"
warning_col <- "#d97706"
bg_col   <- "#f9f9fb"

theme_nava <- function() {
  theme_tufte(base_size = 13) +
    theme(
      plot.title    = element_text(size = 14, face = "bold", margin = margin(b = 8)),
      plot.subtitle = element_text(size = 11, color = muted, margin = margin(b = 12)),
      plot.caption  = element_text(size = 9, color = muted, hjust = 0),
      axis.text     = element_text(size = 11),
      axis.title    = element_text(size = 12),
      plot.background = element_rect(fill = bg_col, color = NA),
      panel.background = element_rect(fill = bg_col, color = NA),
      plot.margin = margin(15, 15, 15, 15)
    )
}

save_fig <- function(plot, filename, width = 6.5, height = 4.5) {
  ggsave(
    here("output", "figures", filename),
    plot   = plot,
    device = cairo_pdf,
    width  = width,
    height = height
  )
  cat("Saved:", filename, "\n")
}

# =====================================================================
# FIG 1: Practitioner types (Q1)
# =====================================================================
q1 <- closed_ended |>
  filter(qnum == "Q1", choice != "Other (please specify)") |>
  mutate(choice = str_wrap(choice, width = 30)) |>
  arrange(n)

fig1 <- ggplot(q1, aes(x = reorder(choice, n), y = n)) +
  geom_segment(aes(xend = choice, y = 0, yend = n), color = accent, linewidth = 0.8) +
  geom_point(size = 3.5, color = accent) +
  geom_text(aes(label = n), hjust = -0.5, size = 3.8, color = muted) +
  coord_flip() +
  labs(
    title    = "85% of respondents are visual artists or craftspeople",
    subtitle = "Q1: What kind of creative practitioner are you? (n = 888, select all)",
    x = NULL, y = "Respondents"
  ) +
  theme_nava() +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15)))

save_fig(fig1, "fig01_practitioner_type.pdf", height = 3.5)

# =====================================================================
# FIG 2: Identity categories (Q3)
# =====================================================================
q3 <- closed_ended |>
  filter(qnum == "Q3", !choice %in% c("None of the above", "Prefer not to say")) |>
  arrange(n)

fig2 <- ggplot(q3, aes(x = reorder(choice, n), y = pct)) +
  geom_segment(aes(xend = choice, y = 0, yend = pct), color = accent, linewidth = 0.8) +
  geom_point(size = 3.5, color = accent) +
  geom_text(aes(label = percent(pct, accuracy = 1)), hjust = -0.3, size = 3.8, color = muted) +
  coord_flip() +
  scale_y_continuous(labels = percent, expand = expansion(mult = c(0, 0.2))) +
  labs(
    title    = "28% of respondents live regionally or remotely",
    subtitle = "Q3: Do you identify as: (n = 890, select all; excl. 'none' and 'prefer not to say')",
    x = NULL, y = NULL
  ) +
  theme_nava()

save_fig(fig2, "fig02_identity.pdf", height = 3.5)

# =====================================================================
# FIG 3: AI adoption (Q6)
# =====================================================================
q6 <- closed_ended |>
  filter(qnum == "Q6") |>
  mutate(
    choice = factor(choice, levels = c("Yes", "Not currently, but I may in the future", "No")),
    fill_col = case_when(
      choice == "Yes" ~ accent,
      choice == "No"  ~ warning_col,
      TRUE            ~ muted
    )
  )

fig3 <- ggplot(q6, aes(x = choice, y = pct, fill = fill_col)) +
  geom_col(width = 0.6) +
  geom_text(aes(label = paste0(percent(pct, accuracy = 1), "\n(n=", n, ")")),
            vjust = -0.3, size = 3.8, color = "grey30") +
  scale_fill_identity() +
  scale_y_continuous(labels = percent, expand = expansion(mult = c(0, 0.2))) +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 20)) +
  labs(
    title    = "46% of creative practitioners use generative AI",
    subtitle = "Q6: Do you use generative AI in your creative practice? (n = 768)",
    x = NULL, y = NULL
  ) +
  theme_nava() +
  theme(axis.text.x = element_text(size = 11))

save_fig(fig3, "fig03_ai_adoption.pdf", height = 4)

# =====================================================================
# FIG 4: What they use AI for (Q7)
# =====================================================================
q7 <- closed_ended |>
  filter(qnum == "Q7", choice != "Other (please specify)") |>
  mutate(choice = str_wrap(choice, width = 35)) |>
  arrange(pct)

fig4 <- ggplot(q7, aes(x = reorder(choice, pct), y = pct)) +
  geom_segment(aes(xend = choice, y = 0, yend = pct), color = accent, linewidth = 0.8) +
  geom_point(size = 3.5, color = accent) +
  geom_text(aes(label = percent(pct, accuracy = 1)), hjust = -0.3, size = 3.5, color = muted) +
  coord_flip() +
  scale_y_continuous(labels = percent, expand = expansion(mult = c(0, 0.15))) +
  labs(
    title    = "Editing and drafting dominate — only 6% use AI for final artwork",
    subtitle = "Q7: What do you use generative AI for? (n = 567, select all)",
    x = NULL, y = NULL
  ) +
  theme_nava()

save_fig(fig4, "fig04_ai_uses.pdf", height = 5)

# =====================================================================
# FIG 5: Main reason for using AI (Q8)
# =====================================================================
q8 <- closed_ended |>
  filter(qnum == "Q8", choice != "Other (please specify)") |>
  mutate(
    choice = str_wrap(choice, width = 25),
    fill_col = ifelse(pct == max(pct), accent, muted)
  ) |>
  arrange(pct)

fig5 <- ggplot(q8, aes(x = reorder(choice, pct), y = pct, fill = fill_col)) +
  geom_col(width = 0.6) +
  geom_text(aes(label = percent(pct, accuracy = 1)), hjust = -0.2, size = 3.8) +
  coord_flip() +
  scale_fill_identity() +
  scale_y_continuous(labels = percent, expand = expansion(mult = c(0, 0.15))) +
  labs(
    title    = "Saving time is the primary motivation (36%)",
    subtitle = "Q8: What is your main reason for using generative AI? (n = 566)",
    x = NULL, y = NULL
  ) +
  theme_nava()

save_fig(fig5, "fig05_main_reason.pdf", height = 4)

# =====================================================================
# FIG 6: % of final work influenced by AI (Q10)
# =====================================================================
q10 <- closed_ended |>
  filter(qnum == "Q10") |>
  mutate(choice = factor(choice, levels = c("0%", "Less than 10%", "Around 25%",
                                             "Around 50%", "More than 75%", "100%", "Not sure")))

fig6 <- ggplot(q10, aes(x = choice, y = pct)) +
  geom_col(fill = accent, width = 0.6) +
  geom_text(aes(label = percent(pct, accuracy = 1)), vjust = -0.5, size = 3.8, color = muted) +
  scale_y_continuous(labels = percent, expand = expansion(mult = c(0, 0.15))) +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 12)) +
  labs(
    title    = "81% of artists say AI influences less than 10% of their final work",
    subtitle = "Q10: What percentage of your final work is generated or influenced by AI? (n = 723)",
    x = NULL, y = NULL
  ) +
  theme_nava()

save_fig(fig6, "fig06_ai_influence_pct.pdf", height = 4)

# =====================================================================
# FIG 7: Positive impacts (Q11)
# =====================================================================
q11 <- closed_ended |>
  filter(qnum == "Q11", choice != "Other (please specify)") |>
  mutate(
    choice = str_wrap(choice, width = 30),
    fill_col = ifelse(choice == str_wrap("No positive impact yet", 30), warning_col, accent)
  ) |>
  arrange(pct)

fig7 <- ggplot(q11, aes(x = reorder(choice, pct), y = pct, fill = fill_col)) +
  geom_col(width = 0.6) +
  geom_text(aes(label = percent(pct, accuracy = 1)), hjust = -0.2, size = 3.5) +
  coord_flip() +
  scale_fill_identity() +
  scale_y_continuous(labels = percent, expand = expansion(mult = c(0, 0.15))) +
  labs(
    title    = "Time savings lead, but 32% report no positive impact",
    subtitle = "Q11: How has AI positively impacted your creative practice? (n = 655, select all)",
    x = NULL, y = NULL
  ) +
  theme_nava()

save_fig(fig7, "fig07_positive_impacts.pdf", height = 4.5)

# =====================================================================
# FIG 8: Work used without permission (Q13)
# =====================================================================
q13 <- closed_ended |>
  filter(qnum == "Q13") |>
  mutate(
    choice = factor(choice, levels = c("Yes", "No", "I don't know")),
    fill_col = case_when(
      choice == "I don't know" ~ warning_col,
      choice == "Yes"          ~ "#dc2626",
      TRUE                     ~ muted
    )
  )

fig8 <- ggplot(q13, aes(x = choice, y = pct, fill = fill_col)) +
  geom_col(width = 0.6) +
  geom_text(aes(label = paste0(percent(pct, accuracy = 1), "\n(n=", n, ")")),
            vjust = -0.3, size = 4, color = "grey30") +
  scale_fill_identity() +
  scale_y_continuous(labels = percent, expand = expansion(mult = c(0, 0.2))) +
  labs(
    title    = "71% of artists don't know if their work was used to train AI",
    subtitle = "Q13: Has your work been used in connection with AI without your permission? (n = 767)",
    x = NULL, y = NULL
  ) +
  theme_nava()

save_fig(fig8, "fig08_unauthorized_use.pdf", height = 4)

# =====================================================================
# FIG 9: What they did about it (Q16)
# =====================================================================
q16 <- closed_ended |>
  filter(qnum == "Q16", choice != "Other (please specify)") |>
  mutate(
    choice = str_wrap(choice, width = 25),
    fill_col = ifelse(choice == str_wrap("Did nothing", 25), warning_col, accent)
  ) |>
  arrange(pct)

fig9 <- ggplot(q16, aes(x = reorder(choice, pct), y = pct, fill = fill_col)) +
  geom_col(width = 0.6) +
  geom_text(aes(label = percent(pct, accuracy = 1)), hjust = -0.2, size = 3.8) +
  coord_flip() +
  scale_fill_identity() +
  scale_y_continuous(labels = percent, expand = expansion(mult = c(0, 0.15))) +
  labs(
    title    = "36% of artists who discovered unauthorized use did nothing",
    subtitle = "Q16: What did you do after finding out? (n = 157, select all)",
    x = NULL, y = NULL
  ) +
  theme_nava()

save_fig(fig9, "fig09_response_to_misuse.pdf", height = 3.5)

# =====================================================================
# FIG 10: Income/practice impacts (Q18)
# =====================================================================
q18 <- closed_ended |>
  filter(qnum == "Q18", choice != "Other (please specify)") |>
  mutate(
    choice = str_wrap(choice, width = 30),
    fill_col = ifelse(choice == str_wrap("No impact", 30), muted, accent)
  ) |>
  arrange(pct)

fig10 <- ggplot(q18, aes(x = reorder(choice, pct), y = pct, fill = fill_col)) +
  geom_col(width = 0.6) +
  geom_text(aes(label = percent(pct, accuracy = 1)), hjust = -0.2, size = 3.5) +
  coord_flip() +
  scale_fill_identity() +
  scale_y_continuous(labels = percent, expand = expansion(mult = c(0, 0.15))) +
  labs(
    title    = "38% report reduced trust in originality as AI's biggest impact",
    subtitle = "Q18: How has generative AI affected your practice or income? (n = 563, select all)",
    x = NULL, y = NULL
  ) +
  theme_nava()

save_fig(fig10, "fig10_income_impacts.pdf", height = 4.5)

# =====================================================================
# FIG 11: Transparency mechanisms (Q20)
# =====================================================================
q20 <- closed_ended |>
  filter(qnum == "Q20", choice != "Other (please specify)") |>
  mutate(choice = str_wrap(choice, width = 35)) |>
  arrange(pct)

fig11 <- ggplot(q20, aes(x = reorder(choice, pct), y = pct)) +
  geom_segment(aes(xend = choice, y = 0, yend = pct), color = accent, linewidth = 0.8) +
  geom_point(size = 3.5, color = accent) +
  geom_text(aes(label = percent(pct, accuracy = 1)), hjust = -0.3, size = 3.8, color = muted) +
  coord_flip() +
  scale_y_continuous(labels = percent, expand = expansion(mult = c(0, 0.15))) +
  labs(
    title    = "85% want visible 'generated by AI' labelling",
    subtitle = "Q20: Most effective transparency mechanisms (n = 627, select all)",
    x = NULL, y = NULL
  ) +
  theme_nava()

save_fig(fig11, "fig11_transparency.pdf", height = 3.5)

# =====================================================================
# FIG 12: Disclosure contexts (Q21)
# =====================================================================
q21 <- closed_ended |>
  filter(qnum == "Q21", choice != "Other (please specify)") |>
  mutate(
    choice = str_wrap(choice, width = 30),
    fill_col = ifelse(str_detect(choice, "All contexts"), accent, muted)
  ) |>
  arrange(pct)

fig12 <- ggplot(q21, aes(x = reorder(choice, pct), y = pct, fill = fill_col)) +
  geom_col(width = 0.6) +
  geom_text(aes(label = percent(pct, accuracy = 1)), hjust = -0.2, size = 3.5) +
  coord_flip() +
  scale_fill_identity() +
  scale_y_continuous(labels = percent, expand = expansion(mult = c(0, 0.15))) +
  labs(
    title    = "71% say AI-generated content should always be disclosed in all contexts",
    subtitle = "Q21: In what contexts should AI content be disclosed? (n = 647, select all)",
    x = NULL, y = NULL
  ) +
  theme_nava()

save_fig(fig12, "fig12_disclosure_contexts.pdf", height = 5)

# =====================================================================
# FIG 13: Barriers to enforcement (Q22)
# =====================================================================
q22 <- closed_ended |>
  filter(qnum == "Q22", choice != "Other (please specify)") |>
  mutate(choice = str_wrap(choice, width = 25)) |>
  arrange(pct)

fig13 <- ggplot(q22, aes(x = reorder(choice, pct), y = pct)) +
  geom_segment(aes(xend = choice, y = 0, yend = pct), color = warning_col, linewidth = 0.8) +
  geom_point(size = 3.5, color = warning_col) +
  geom_text(aes(label = percent(pct, accuracy = 1)), hjust = -0.3, size = 3.8, color = muted) +
  coord_flip() +
  scale_y_continuous(labels = percent, expand = expansion(mult = c(0, 0.15))) +
  labs(
    title    = "Platform opacity is the #1 barrier (76%)",
    subtitle = "Q22: Barriers to identifying if your work was used for AI (n = 606, select all)",
    x = NULL, y = NULL
  ) +
  theme_nava()

save_fig(fig13, "fig13_barriers.pdf", height = 3.5)

# =====================================================================
# FIG 14: Regulatory preferences (Q23)
# =====================================================================
q23 <- closed_ended |>
  filter(qnum == "Q23", choice != "Not sure") |>
  mutate(
    choice = factor(choice, levels = c("Voluntary guidelines", "Code of Practice",
                                        "Enforceable regulation", "Legislation")),
    fill_col = ifelse(pct == max(pct), accent, accent2)
  )

fig14 <- ggplot(q23, aes(x = choice, y = pct, fill = fill_col)) +
  geom_col(width = 0.6) +
  geom_text(aes(label = percent(pct, accuracy = 1)), vjust = -0.5, size = 4) +
  scale_fill_identity() +
  scale_y_continuous(labels = percent, expand = expansion(mult = c(0, 0.15))) +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 15)) +
  labs(
    title    = "Artists want binding rules: 69% support a Code of Practice",
    subtitle = "Q23: What kind of regulatory response should be introduced? (n = 648, select all)",
    x = NULL, y = NULL
  ) +
  theme_nava()

save_fig(fig14, "fig14_regulation.pdf", height = 4)

# =====================================================================
# FIG 15: Compensation scheme (Q24)
# =====================================================================
q24 <- closed_ended |>
  filter(qnum == "Q24") |>
  mutate(
    choice = factor(choice, levels = c("Yes", "Not sure", "No")),
    fill_col = case_when(
      choice == "Yes"      ~ accent,
      choice == "Not sure" ~ muted,
      TRUE                 ~ warning_col
    )
  )

fig15 <- ggplot(q24, aes(x = choice, y = pct, fill = fill_col)) +
  geom_col(width = 0.5) +
  geom_text(aes(label = paste0(percent(pct, accuracy = 1), "\n(n=", n, ")")),
            vjust = -0.3, size = 4, color = "grey30") +
  scale_fill_identity() +
  scale_y_continuous(labels = percent, expand = expansion(mult = c(0, 0.2))) +
  labs(
    title    = "73% support compensation for creators when their work trains AI",
    subtitle = "Q24: Do you support a compensation scheme? (n = 647)",
    x = NULL, y = NULL
  ) +
  theme_nava()

save_fig(fig15, "fig15_compensation.pdf", height = 4)

# =====================================================================
# FIG 16: Collective licensing (Q25)
# =====================================================================
q25 <- closed_ended |>
  filter(qnum == "Q25") |>
  mutate(
    choice = factor(choice, levels = c("Yes", "Not sure", "No")),
    fill_col = case_when(
      choice == "Yes"      ~ accent,
      choice == "Not sure" ~ muted,
      TRUE                 ~ warning_col
    )
  )

fig16 <- ggplot(q25, aes(x = choice, y = pct, fill = fill_col)) +
  geom_col(width = 0.5) +
  geom_text(aes(label = paste0(percent(pct, accuracy = 1), "\n(n=", n, ")")),
            vjust = -0.3, size = 4, color = "grey30") +
  scale_fill_identity() +
  scale_y_continuous(labels = percent, expand = expansion(mult = c(0, 0.2))) +
  labs(
    title    = "50% are unsure about collective licensing — an education opportunity",
    subtitle = "Q25: Would you participate in a collective licensing system? (n = 647)",
    x = NULL, y = NULL
  ) +
  theme_nava()

save_fig(fig16, "fig16_collective_licensing.pdf", height = 4)

# =====================================================================
# FIG 17: AI Platform usage (from Q9 text coding)
# =====================================================================
platforms_top <- platform_counts |>
  filter(n_mentions >= 5) |>
  arrange(n_mentions)

fig17 <- ggplot(platforms_top, aes(x = reorder(platform, n_mentions), y = n_mentions)) +
  geom_segment(aes(xend = platform, y = 0, yend = n_mentions), color = accent, linewidth = 0.8) +
  geom_point(size = 3.5, color = accent) +
  geom_text(aes(label = n_mentions), hjust = -0.5, size = 3.8, color = muted) +
  coord_flip() +
  labs(
    title    = "ChatGPT dominates: mentioned by 60% of respondents",
    subtitle = "AI platforms named in Q9 open-text responses (n = 530, ≥5 mentions shown)",
    x = NULL, y = "Mentions"
  ) +
  theme_nava() +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15)))

save_fig(fig17, "fig17_platform_usage.pdf", height = 5)

# =====================================================================
# FIG 18: Thematic analysis summary
# =====================================================================
theme_plot <- theme_summary |>
  mutate(
    theme = str_wrap(theme, width = 30),
    fill_group = case_when(
      str_detect(theme, "admin|Accessibility|Creative") ~ "Positive/Pragmatic",
      str_detect(theme, "regulation|Compensation|Labelling") ~ "Policy",
      TRUE ~ "Concern"
    )
  ) |>
  arrange(n)

fig18 <- ggplot(theme_plot, aes(x = reorder(theme, n), y = n, fill = fill_group)) +
  geom_col(width = 0.6) +
  geom_text(aes(label = n), hjust = -0.2, size = 3.2) +
  coord_flip() +
  scale_fill_manual(
    values = c("Concern" = warning_col, "Positive/Pragmatic" = accent, "Policy" = accent2),
    name = NULL
  ) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
  labs(
    title    = "Admin utility and IP concerns dominate the open-text responses",
    subtitle = "Thematic codes across Q4, Q12, Q14, Q19, Q26 (n = 1,051 responses coded)",
    x = NULL, y = "Frequency"
  ) +
  theme_nava() +
  theme(legend.position = "bottom")

save_fig(fig18, "fig18_thematic_analysis.pdf", height = 6)

cat("\nAll figures saved to output/figures/\n")
