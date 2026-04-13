# ---------------------------------------------------------------------
# 04_slide_figures.R
# Purpose: Generate title-free, high-resolution PNG figures for the
#          reveal.js slide deck. Titles live in the HTML, not the image.
# Input:   data/clean/*.rds
# Output:  output/figures/slides/*.png
# ---------------------------------------------------------------------

library(tidyverse)
library(ggthemes)
library(here)
library(scales)

closed_ended    <- readRDS(here("data", "clean", "closed_ended.rds"))
platform_counts <- readRDS(here("data", "clean", "platform_counts.rds"))
theme_summary   <- readRDS(here("data", "clean", "theme_summary.rds"))
q1_other_coded  <- readRDS(here("data", "clean", "q1_other_coded.rds"))

# ---- Shared styling (larger for slides) ----
accent      <- "#3730a3"
accent2     <- "#4338ca"
muted       <- "#6b6b7e"
warning_col <- "#d97706"

theme_slide <- function() {
  theme_tufte(base_size = 16) +
    theme(
      plot.title       = element_blank(),
      plot.subtitle    = element_blank(),
      plot.caption     = element_blank(),
      axis.text        = element_text(size = 14),
      axis.title       = element_text(size = 15),
      plot.background  = element_rect(fill = "transparent", color = NA),
      panel.background = element_rect(fill = "transparent", color = NA),
      plot.margin      = margin(10, 15, 10, 15),
      legend.text      = element_text(size = 13),
      legend.title     = element_blank()
    )
}

save_slide <- function(plot, filename, width = 10, height = 6) {
  dir.create(here("output", "figures", "slides"), showWarnings = FALSE, recursive = TRUE)
  ggsave(
    here("output", "figures", "slides", filename),
    plot   = plot,
    device = "png",
    dpi    = 300,
    width  = width,
    height = height,
    bg     = "transparent"
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
  geom_point(size = 4, color = accent) +
  geom_text(aes(label = n), hjust = -0.5, size = 5, color = muted) +
  coord_flip() +
  labs(x = NULL, y = "Respondents") +
  theme_slide() +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15)))

save_slide(fig1, "fig01_practitioner_type.png", height = 5)

# =====================================================================
# FIG 2: Identity categories (Q3)
# =====================================================================
q3 <- closed_ended |>
  filter(qnum == "Q3", !choice %in% c("None of the above", "Prefer not to say")) |>
  arrange(n)

fig2 <- ggplot(q3, aes(x = reorder(choice, n), y = pct)) +
  geom_segment(aes(xend = choice, y = 0, yend = pct), color = accent, linewidth = 0.8) +
  geom_point(size = 4, color = accent) +
  geom_text(aes(label = percent(pct, accuracy = 1)), hjust = -0.3, size = 5, color = muted) +
  coord_flip() +
  scale_y_continuous(labels = percent, expand = expansion(mult = c(0, 0.2))) +
  labs(x = NULL, y = NULL) +
  theme_slide()

save_slide(fig2, "fig02_identity.png", height = 5)

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
            vjust = -0.3, size = 5, color = "grey30") +
  scale_fill_identity() +
  scale_y_continuous(labels = percent, expand = expansion(mult = c(0, 0.2))) +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 20)) +
  labs(x = NULL, y = NULL) +
  theme_slide() +
  theme(axis.text.x = element_text(size = 13))

save_slide(fig3, "fig03_ai_adoption.png", height = 5.5)

# =====================================================================
# FIG 4: What they use AI for (Q7)
# =====================================================================
q7 <- closed_ended |>
  filter(qnum == "Q7", choice != "Other (please specify)") |>
  mutate(choice = str_wrap(choice, width = 35)) |>
  arrange(pct)

fig4 <- ggplot(q7, aes(x = reorder(choice, pct), y = pct)) +
  geom_segment(aes(xend = choice, y = 0, yend = pct), color = accent, linewidth = 0.8) +
  geom_point(size = 4, color = accent) +
  geom_text(aes(label = percent(pct, accuracy = 1)), hjust = -0.3, size = 4.5, color = muted) +
  coord_flip() +
  scale_y_continuous(labels = percent, expand = expansion(mult = c(0, 0.15))) +
  labs(x = NULL, y = NULL) +
  theme_slide()

save_slide(fig4, "fig04_ai_uses.png", height = 6.5)

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
  geom_text(aes(label = percent(pct, accuracy = 1)), hjust = -0.2, size = 5) +
  coord_flip() +
  scale_fill_identity() +
  scale_y_continuous(labels = percent, expand = expansion(mult = c(0, 0.15))) +
  labs(x = NULL, y = NULL) +
  theme_slide()

save_slide(fig5, "fig05_main_reason.png", height = 5.5)

# =====================================================================
# FIG 6: % of final work influenced by AI (Q10)
# =====================================================================
q10 <- closed_ended |>
  filter(qnum == "Q10") |>
  mutate(choice = factor(choice, levels = c("0%", "Less than 10%", "Around 25%",
                                             "Around 50%", "More than 75%", "100%", "Not sure")))

fig6 <- ggplot(q10, aes(x = choice, y = pct)) +
  geom_col(fill = accent, width = 0.6) +
  geom_text(aes(label = percent(pct, accuracy = 1)), vjust = -0.5, size = 5, color = muted) +
  scale_y_continuous(labels = percent, expand = expansion(mult = c(0, 0.15))) +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 12)) +
  labs(x = NULL, y = NULL) +
  theme_slide()

save_slide(fig6, "fig06_ai_influence_pct.png", height = 5.5)

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
  geom_text(aes(label = percent(pct, accuracy = 1)), hjust = -0.2, size = 4.5) +
  coord_flip() +
  scale_fill_identity() +
  scale_y_continuous(labels = percent, expand = expansion(mult = c(0, 0.15))) +
  labs(x = NULL, y = NULL) +
  theme_slide()

save_slide(fig7, "fig07_positive_impacts.png", height = 6)

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
            vjust = -0.3, size = 5, color = "grey30") +
  scale_fill_identity() +
  scale_y_continuous(labels = percent, expand = expansion(mult = c(0, 0.2))) +
  labs(x = NULL, y = NULL) +
  theme_slide()

save_slide(fig8, "fig08_unauthorized_use.png", height = 5.5)

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
  geom_text(aes(label = percent(pct, accuracy = 1)), hjust = -0.2, size = 5) +
  coord_flip() +
  scale_fill_identity() +
  scale_y_continuous(labels = percent, expand = expansion(mult = c(0, 0.15))) +
  labs(x = NULL, y = NULL) +
  theme_slide()

save_slide(fig9, "fig09_response_to_misuse.png", height = 5)

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
  geom_text(aes(label = percent(pct, accuracy = 1)), hjust = -0.2, size = 4.5) +
  coord_flip() +
  scale_fill_identity() +
  scale_y_continuous(labels = percent, expand = expansion(mult = c(0, 0.15))) +
  labs(x = NULL, y = NULL) +
  theme_slide()

save_slide(fig10, "fig10_income_impacts.png", height = 6)

# =====================================================================
# FIG 11: Transparency mechanisms (Q20)
# =====================================================================
q20 <- closed_ended |>
  filter(qnum == "Q20", choice != "Other (please specify)") |>
  mutate(choice = str_wrap(choice, width = 35)) |>
  arrange(pct)

fig11 <- ggplot(q20, aes(x = reorder(choice, pct), y = pct)) +
  geom_segment(aes(xend = choice, y = 0, yend = pct), color = accent, linewidth = 0.8) +
  geom_point(size = 4, color = accent) +
  geom_text(aes(label = percent(pct, accuracy = 1)), hjust = -0.3, size = 5, color = muted) +
  coord_flip() +
  scale_y_continuous(labels = percent, expand = expansion(mult = c(0, 0.15))) +
  labs(x = NULL, y = NULL) +
  theme_slide()

save_slide(fig11, "fig11_transparency.png", height = 5)

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
  geom_text(aes(label = percent(pct, accuracy = 1)), hjust = -0.2, size = 4.5) +
  coord_flip() +
  scale_fill_identity() +
  scale_y_continuous(labels = percent, expand = expansion(mult = c(0, 0.15))) +
  labs(x = NULL, y = NULL) +
  theme_slide()

save_slide(fig12, "fig12_disclosure_contexts.png", height = 6.5)

# =====================================================================
# FIG 13: Barriers to enforcement (Q22)
# =====================================================================
q22 <- closed_ended |>
  filter(qnum == "Q22", choice != "Other (please specify)") |>
  mutate(choice = str_wrap(choice, width = 25)) |>
  arrange(pct)

fig13 <- ggplot(q22, aes(x = reorder(choice, pct), y = pct)) +
  geom_segment(aes(xend = choice, y = 0, yend = pct), color = warning_col, linewidth = 0.8) +
  geom_point(size = 4, color = warning_col) +
  geom_text(aes(label = percent(pct, accuracy = 1)), hjust = -0.3, size = 5, color = muted) +
  coord_flip() +
  scale_y_continuous(labels = percent, expand = expansion(mult = c(0, 0.15))) +
  labs(x = NULL, y = NULL) +
  theme_slide()

save_slide(fig13, "fig13_barriers.png", height = 5)

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
  geom_text(aes(label = percent(pct, accuracy = 1)), vjust = -0.5, size = 5) +
  scale_fill_identity() +
  scale_y_continuous(labels = percent, expand = expansion(mult = c(0, 0.15))) +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 15)) +
  labs(x = NULL, y = NULL) +
  theme_slide()

save_slide(fig14, "fig14_regulation.png", height = 5.5)

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
            vjust = -0.3, size = 5, color = "grey30") +
  scale_fill_identity() +
  scale_y_continuous(labels = percent, expand = expansion(mult = c(0, 0.2))) +
  labs(x = NULL, y = NULL) +
  theme_slide()

save_slide(fig15, "fig15_compensation.png", height = 5.5)

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
            vjust = -0.3, size = 5, color = "grey30") +
  scale_fill_identity() +
  scale_y_continuous(labels = percent, expand = expansion(mult = c(0, 0.2))) +
  labs(x = NULL, y = NULL) +
  theme_slide()

save_slide(fig16, "fig16_collective_licensing.png", height = 5.5)

# =====================================================================
# FIG 17: AI Platform usage (from Q9 text coding)
# =====================================================================
platforms_top <- platform_counts |>
  filter(n_mentions >= 5) |>
  arrange(n_mentions)

fig17 <- ggplot(platforms_top, aes(x = reorder(platform, n_mentions), y = n_mentions)) +
  geom_segment(aes(xend = platform, y = 0, yend = n_mentions), color = accent, linewidth = 0.8) +
  geom_point(size = 4, color = accent) +
  geom_text(aes(label = n_mentions), hjust = -0.5, size = 5, color = muted) +
  coord_flip() +
  labs(x = NULL, y = "Mentions") +
  theme_slide() +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15)))

save_slide(fig17, "fig17_platform_usage.png", height = 6.5)

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
  geom_text(aes(label = n), hjust = -0.2, size = 4) +
  coord_flip() +
  scale_fill_manual(
    values = c("Concern" = warning_col, "Positive/Pragmatic" = accent, "Policy" = accent2),
    name = NULL
  ) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
  labs(x = NULL, y = "Frequency") +
  theme_slide() +
  theme(legend.position = "bottom")

save_slide(fig18, "fig18_thematic_analysis.png", height = 7.5)

cat("\nAll slide figures saved to output/figures/slides/\n")
