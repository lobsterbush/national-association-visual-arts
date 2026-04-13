
library(tidyverse)
library(ggthemes)
library(here)
library(scales)

closed_ended    <- readRDS(here("data", "clean", "closed_ended.rds"))
platform_counts <- readRDS(here("data", "clean", "platform_counts.rds"))
theme_summary   <- readRDS(here("data", "clean", "theme_summary.rds"))

accent   <- "#4f46e5"
accent2  <- "#6366f1"
muted    <- "#64748b"
warn     <- "#d97706"
bg       <- "#ffffff"

theme_deck <- function() {
  theme_tufte(base_size = 18) +
    theme(
      plot.title       = element_blank(),
      plot.subtitle    = element_blank(),
      axis.text        = element_text(size = 15, color = "#334155"),
      axis.title       = element_text(size = 16, color = muted),
      plot.background  = element_rect(fill = bg, color = NA),
      panel.background = element_rect(fill = bg, color = NA),
      plot.margin      = margin(10, 25, 10, 10),
      legend.text      = element_text(size = 14),
      legend.position  = "bottom"
    )
}

sv <- function(p, fn, w = 10, h = 5.5) {
  ggsave(here("docs", "figures", fn), plot = p, device = png,
         width = w, height = h, dpi = 300, bg = "white")
  cat("OK:", fn, "\n")
}

# F01
q1 <- closed_ended |> filter(qnum=="Q1", choice!="Other (please specify)") |>
  mutate(choice=str_wrap(choice,28)) |> arrange(n)
sv(ggplot(q1, aes(reorder(choice,n), n)) +
  geom_segment(aes(xend=choice,y=0,yend=n), color=accent, linewidth=.9) +
  geom_point(size=4, color=accent) +
  geom_text(aes(label=n), hjust=-.5, size=5, color=muted) +
  coord_flip() + labs(x=NULL,y="Respondents") + theme_deck() +
  scale_y_continuous(expand=expansion(mult=c(0,.15))),
  "fig01_practitioner_type.png", h=4.5)

# F02
q3 <- closed_ended |> filter(qnum=="Q3", !choice %in% c("None of the above","Prefer not to say")) |> arrange(n)
sv(ggplot(q3, aes(reorder(choice,n), pct)) +
  geom_segment(aes(xend=choice,y=0,yend=pct), color=accent, linewidth=.9) +
  geom_point(size=4, color=accent) +
  geom_text(aes(label=percent(pct,1)), hjust=-.3, size=5, color=muted) +
  coord_flip() + scale_y_continuous(labels=percent, expand=expansion(mult=c(0,.2))) +
  labs(x=NULL,y=NULL) + theme_deck(),
  "fig02_identity.png", h=4.5)

# F03
q6 <- closed_ended |> filter(qnum=="Q6") |>
  mutate(choice=factor(choice, levels=c("Yes","Not currently, but I may in the future","No")),
         fc=case_when(choice=="Yes"~accent, choice=="No"~warn, TRUE~muted))
sv(ggplot(q6, aes(choice, pct, fill=fc)) +
  geom_col(width=.55) +
  geom_text(aes(label=paste0(percent(pct,1),"\n(n=",n,")")), vjust=-.3, size=5, color="#334155") +
  scale_fill_identity() +
  scale_y_continuous(labels=percent, expand=expansion(mult=c(0,.22))) +
  scale_x_discrete(labels=function(x) str_wrap(x,18)) +
  labs(x=NULL,y=NULL) + theme_deck(),
  "fig03_ai_adoption.png", h=5)

# F04
q7 <- closed_ended |> filter(qnum=="Q7", choice!="Other (please specify)") |>
  mutate(choice=str_wrap(choice,32)) |> arrange(pct)
sv(ggplot(q7, aes(reorder(choice,pct), pct)) +
  geom_segment(aes(xend=choice,y=0,yend=pct), color=accent, linewidth=.9) +
  geom_point(size=4, color=accent) +
  geom_text(aes(label=percent(pct,1)), hjust=-.3, size=4.5, color=muted) +
  coord_flip() + scale_y_continuous(labels=percent, expand=expansion(mult=c(0,.15))) +
  labs(x=NULL,y=NULL) + theme_deck(),
  "fig04_ai_uses.png", h=6.5)

# F05
q8 <- closed_ended |> filter(qnum=="Q8", choice!="Other (please specify)") |>
  mutate(choice=str_wrap(choice,22), fc=ifelse(pct==max(pct), accent, "#94a3b8")) |> arrange(pct)
sv(ggplot(q8, aes(reorder(choice,pct), pct, fill=fc)) +
  geom_col(width=.55) +
  geom_text(aes(label=percent(pct,1)), hjust=-.2, size=5) +
  coord_flip() + scale_fill_identity() +
  scale_y_continuous(labels=percent, expand=expansion(mult=c(0,.15))) +
  labs(x=NULL,y=NULL) + theme_deck(),
  "fig05_main_reason.png", h=5)

# F06
q10 <- closed_ended |> filter(qnum=="Q10") |>
  mutate(choice=factor(choice, levels=c("0%","Less than 10%","Around 25%","Around 50%","More than 75%","100%","Not sure")))
sv(ggplot(q10, aes(choice, pct)) +
  geom_col(fill=accent, width=.55) +
  geom_text(aes(label=percent(pct,1)), vjust=-.5, size=5, color=muted) +
  scale_y_continuous(labels=percent, expand=expansion(mult=c(0,.15))) +
  scale_x_discrete(labels=function(x) str_wrap(x,10)) +
  labs(x=NULL,y=NULL) + theme_deck(),
  "fig06_ai_influence_pct.png", h=5)

# F07
q11 <- closed_ended |> filter(qnum=="Q11", choice!="Other (please specify)") |>
  mutate(choice=str_wrap(choice,28),
         fc=ifelse(choice==str_wrap("No positive impact yet",28), warn, accent)) |> arrange(pct)
sv(ggplot(q11, aes(reorder(choice,pct), pct, fill=fc)) +
  geom_col(width=.55) +
  geom_text(aes(label=percent(pct,1)), hjust=-.2, size=4.5) +
  coord_flip() + scale_fill_identity() +
  scale_y_continuous(labels=percent, expand=expansion(mult=c(0,.15))) +
  labs(x=NULL,y=NULL) + theme_deck(),
  "fig07_positive_impacts.png", h=5.5)

# F08
q13 <- closed_ended |> filter(qnum=="Q13") |>
  mutate(choice=factor(choice, levels=c("Yes","No","I don't know")),
         fc=case_when(choice=="I don't know"~warn, choice=="Yes"~"#dc2626", TRUE~"#94a3b8"))
sv(ggplot(q13, aes(choice, pct, fill=fc)) +
  geom_col(width=.55) +
  geom_text(aes(label=paste0(percent(pct,1),"\n(n=",n,")")), vjust=-.3, size=5, color="#334155") +
  scale_fill_identity() +
  scale_y_continuous(labels=percent, expand=expansion(mult=c(0,.22))) +
  labs(x=NULL,y=NULL) + theme_deck(),
  "fig08_unauthorized_use.png", h=5)

# F09
q16 <- closed_ended |> filter(qnum=="Q16", choice!="Other (please specify)") |>
  mutate(choice=str_wrap(choice,22), fc=ifelse(choice==str_wrap("Did nothing",22), warn, accent)) |> arrange(pct)
sv(ggplot(q16, aes(reorder(choice,pct), pct, fill=fc)) +
  geom_col(width=.55) +
  geom_text(aes(label=percent(pct,1)), hjust=-.2, size=5) +
  coord_flip() + scale_fill_identity() +
  scale_y_continuous(labels=percent, expand=expansion(mult=c(0,.15))) +
  labs(x=NULL,y=NULL) + theme_deck(),
  "fig09_response_to_misuse.png", h=4.5)

# F10
q18 <- closed_ended |> filter(qnum=="Q18", choice!="Other (please specify)") |>
  mutate(choice=str_wrap(choice,28), fc=ifelse(choice==str_wrap("No impact",28), "#94a3b8", accent)) |> arrange(pct)
sv(ggplot(q18, aes(reorder(choice,pct), pct, fill=fc)) +
  geom_col(width=.55) +
  geom_text(aes(label=percent(pct,1)), hjust=-.2, size=4.5) +
  coord_flip() + scale_fill_identity() +
  scale_y_continuous(labels=percent, expand=expansion(mult=c(0,.15))) +
  labs(x=NULL,y=NULL) + theme_deck(),
  "fig10_income_impacts.png", h=5.5)

# F11
q20 <- closed_ended |> filter(qnum=="Q20", choice!="Other (please specify)") |>
  mutate(choice=str_wrap(choice,32)) |> arrange(pct)
sv(ggplot(q20, aes(reorder(choice,pct), pct)) +
  geom_segment(aes(xend=choice,y=0,yend=pct), color=accent, linewidth=.9) +
  geom_point(size=4, color=accent) +
  geom_text(aes(label=percent(pct,1)), hjust=-.3, size=5, color=muted) +
  coord_flip() + scale_y_continuous(labels=percent, expand=expansion(mult=c(0,.15))) +
  labs(x=NULL,y=NULL) + theme_deck(),
  "fig11_transparency.png", h=4.5)

# F12
q21 <- closed_ended |> filter(qnum=="Q21", choice!="Other (please specify)") |>
  mutate(choice=str_wrap(choice,28), fc=ifelse(str_detect(choice,"All contexts"), accent, "#94a3b8")) |> arrange(pct)
sv(ggplot(q21, aes(reorder(choice,pct), pct, fill=fc)) +
  geom_col(width=.55) +
  geom_text(aes(label=percent(pct,1)), hjust=-.2, size=4.5) +
  coord_flip() + scale_fill_identity() +
  scale_y_continuous(labels=percent, expand=expansion(mult=c(0,.15))) +
  labs(x=NULL,y=NULL) + theme_deck(),
  "fig12_disclosure_contexts.png", h=6.5)

# F13
q22 <- closed_ended |> filter(qnum=="Q22", choice!="Other (please specify)") |>
  mutate(choice=str_wrap(choice,22)) |> arrange(pct)
sv(ggplot(q22, aes(reorder(choice,pct), pct)) +
  geom_segment(aes(xend=choice,y=0,yend=pct), color=warn, linewidth=.9) +
  geom_point(size=4, color=warn) +
  geom_text(aes(label=percent(pct,1)), hjust=-.3, size=5, color=muted) +
  coord_flip() + scale_y_continuous(labels=percent, expand=expansion(mult=c(0,.15))) +
  labs(x=NULL,y=NULL) + theme_deck(),
  "fig13_barriers.png", h=4.5)

# F14
q23 <- closed_ended |> filter(qnum=="Q23", choice!="Not sure") |>
  mutate(choice=factor(choice, levels=c("Voluntary guidelines","Code of Practice","Enforceable regulation","Legislation")),
         fc=ifelse(pct==max(pct), accent, accent2))
sv(ggplot(q23, aes(choice, pct, fill=fc)) +
  geom_col(width=.55) +
  geom_text(aes(label=percent(pct,1)), vjust=-.5, size=5) +
  scale_fill_identity() +
  scale_y_continuous(labels=percent, expand=expansion(mult=c(0,.15))) +
  scale_x_discrete(labels=function(x) str_wrap(x,14)) +
  labs(x=NULL,y=NULL) + theme_deck(),
  "fig14_regulation.png", h=5)

# F15
q24 <- closed_ended |> filter(qnum=="Q24") |>
  mutate(choice=factor(choice, levels=c("Yes","Not sure","No")),
         fc=case_when(choice=="Yes"~accent, choice=="Not sure"~"#94a3b8", TRUE~warn))
sv(ggplot(q24, aes(choice, pct, fill=fc)) +
  geom_col(width=.45) +
  geom_text(aes(label=paste0(percent(pct,1),"\n(n=",n,")")), vjust=-.3, size=5, color="#334155") +
  scale_fill_identity() +
  scale_y_continuous(labels=percent, expand=expansion(mult=c(0,.22))) +
  labs(x=NULL,y=NULL) + theme_deck(),
  "fig15_compensation.png", h=5)

# F16
q25 <- closed_ended |> filter(qnum=="Q25") |>
  mutate(choice=factor(choice, levels=c("Yes","Not sure","No")),
         fc=case_when(choice=="Yes"~accent, choice=="Not sure"~"#94a3b8", TRUE~warn))
sv(ggplot(q25, aes(choice, pct, fill=fc)) +
  geom_col(width=.45) +
  geom_text(aes(label=paste0(percent(pct,1),"\n(n=",n,")")), vjust=-.3, size=5, color="#334155") +
  scale_fill_identity() +
  scale_y_continuous(labels=percent, expand=expansion(mult=c(0,.22))) +
  labs(x=NULL,y=NULL) + theme_deck(),
  "fig16_collective_licensing.png", h=5)

# F17
pt <- platform_counts |> filter(n_mentions>=5) |> arrange(n_mentions)
sv(ggplot(pt, aes(reorder(platform,n_mentions), n_mentions)) +
  geom_segment(aes(xend=platform,y=0,yend=n_mentions), color=accent, linewidth=.9) +
  geom_point(size=4, color=accent) +
  geom_text(aes(label=n_mentions), hjust=-.5, size=5, color=muted) +
  coord_flip() + labs(x=NULL,y="Mentions") + theme_deck() +
  scale_y_continuous(expand=expansion(mult=c(0,.15))),
  "fig17_platform_usage.png", h=6.5)

# F18
tp <- theme_summary |>
  mutate(theme=str_wrap(theme,28),
         fg=case_when(str_detect(theme,"admin|Accessibility|Creative")~"Positive",
                      str_detect(theme,"regulation|Compensation|Labelling")~"Policy",
                      TRUE~"Concern")) |> arrange(n)
sv(ggplot(tp, aes(reorder(theme,n), n, fill=fg)) +
  geom_col(width=.55) +
  geom_text(aes(label=n), hjust=-.2, size=4.2) +
  coord_flip() +
  scale_fill_manual(values=c("Concern"=warn, "Positive"=accent, "Policy"=accent2), name=NULL) +
  scale_y_continuous(expand=expansion(mult=c(0,.15))) +
  labs(x=NULL,y="Frequency") + theme_deck(),
  "fig18_thematic_analysis.png", h=7.5)

cat("\nAll 18 figures rendered clean for slides\n")
