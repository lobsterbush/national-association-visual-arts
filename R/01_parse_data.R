# ---------------------------------------------------------------------
# 01_parse_data.R
# Purpose: Parse the NAVA AI & Visual Arts Survey 2025 xlsx into tidy
#          data frames (structured answers + open-text responses).
# Input:   data/raw/13 Aug Artificial Intelligence AI and Visual Arts
#          Practice Survey 2025.xlsx
# Output:  data/clean/closed_ended.rds, data/clean/open_text.rds
# ---------------------------------------------------------------------

library(readxl)
library(tidyverse)
library(here)
library(janitor)

raw_path <- here("data", "raw",
                 "13 Aug Artificial Intelligence AI and Visual Arts Practice Survey 2025.xlsx")

sheets <- excel_sheets(raw_path)

# ---- Question metadata ----
q_meta <- tribble(
  ~sheet,         ~qnum, ~question_text,                                                                                                ~type,
  "Question 1",   "Q1",  "What kind of creative practitioner are you?",                                                                 "closed_other",
  "Question 3",   "Q3",  "Do you identify as:",                                                                                         "closed",
  "Question 4",   "Q4",  "First Nations concerns about AI and Indigenous cultural content",                                              "open",
  "Question 5",   "Q5",  "Protections for Indigenous Cultural and Intellectual Property (ICIP)",                                         "open",
  "Question 6",   "Q6",  "Do you use generative AI in your creative practice?",                                                         "closed",
  "Question 7",   "Q7",  "What do you use generative AI for?",                                                                          "closed_other",
  "Question 8",   "Q8",  "What is your main reason for using generative AI?",                                                           "closed_other",
  "Question 9",   "Q9",  "Which AI platforms or tools do you use, and how?",                                                             "open",
  "Question 10",  "Q10", "What percentage of your final work is generated or influenced by AI tools?",                                   "closed",
  "Question 11",  "Q11", "How has AI positively impacted your creative practice?",                                                       "closed_other",
  "Question 12",  "Q12", "Example of how you've used AI successfully in your work",                                                     "open",
  "Question 13",  "Q13", "Has your creative work ever been used in connection with an AI system without your knowledge or permission?",   "closed",
  "Question 14",  "Q14", "What happened when work was used without permission",                                                         "open",
  "Question 15",  "Q15", "Where was your work used or encountered in relation to AI platforms?",                                         "open",
  "Question 16",  "Q16", "What did you do after finding out your work was used without permission?",                                     "closed_other",
  "Question 17",  "Q17", "What was the outcome?",                                                                                       "open",
  "Question 18",  "Q18", "How has generative AI affected your practice or income?",                                                      "closed_other",
  "Question 19",  "Q19", "Story about how AI has affected your work, income, or reputation",                                            "open",
  "Question 20",  "Q20", "Which transparency mechanisms are most effective?",                                                            "closed_other",
  "Question 21",  "Q21", "In what contexts should AI content always be disclosed?",                                                      "closed_other",
  "Question 22",  "Q22", "What barriers would you face in verifying if your work was used for AI?",                                      "closed_other",
  "Question 23",  "Q23", "What kind of regulatory response should be introduced?",                                                       "closed",
  "Question 24",  "Q24", "Do you support a compensation scheme for AI training use?",                                                    "closed",
  "Question 25",  "Q25", "Would you participate in a collective licensing system?",                                                       "closed",
  "Question 26",  "Q26", "Other comments about AI, copyright, or creative practice",                                                    "open",
  "Question 27",  "Q27", "What else should we be asking you?",                                                                           "open",
  "Question 28",  "Q28", "Do you consent to NAVA using your responses anonymously?",                                                     "closed"
)

# ---- Parse closed-ended questions ----
parse_closed <- function(sheet_name, qnum) {
  raw <- read_excel(raw_path, sheet = sheet_name, col_names = FALSE)

  # Find the "Answer Choices" header row
  header_row <- which(pull(raw, 1) == "Answer Choices")
  if (length(header_row) == 0) return(NULL)

 # Find the "Answered" summary row to know where choices end
  answered_row <- which(pull(raw, 2) == "Answered" | pull(raw, 1) == "Answered")
  if (length(answered_row) == 0) return(NULL)
  end_row <- min(answered_row) - 1

  if (end_row <= header_row) return(NULL)

  choices <- raw[(header_row + 1):end_row, ]

  # Columns: answer_choice, pct, n (and sometimes col4)
  df <- tibble(
    qnum        = qnum,
    choice      = as.character(pull(choices, 1)),
    pct         = as.numeric(pull(choices, 2)),
    n           = as.numeric(pull(choices, 3))
  ) |>
    filter(!is.na(choice))

  # Get total answered/skipped
  answered_val <- raw |>
    filter(pull(raw, 2) == "Answered" | pull(raw, 1) == "Answered") |>
    pull(3) |>
    first() |>
    as.numeric()

  skipped_val <- raw |>
    filter(pull(raw, 2) == "Skipped" | pull(raw, 1) == "Skipped") |>
    pull(3) |>
    first() |>
    as.numeric()

  # Handle case where n is in col 2 instead of col 3
  if (all(is.na(df$n)) && !all(is.na(df$pct))) {
    df$n <- df$pct
    df$pct <- NA
  }

  df$answered <- answered_val
  df$skipped  <- skipped_val

  # Recalculate pct if missing
 if (any(is.na(df$pct)) && !is.na(answered_val)) {
    df$pct <- df$n / answered_val
  }

  df
}

# ---- Parse open-text responses ----
parse_open <- function(sheet_name, qnum) {
  raw <- read_excel(raw_path, sheet = sheet_name, col_names = FALSE)

  # Find the "Respondent ID" header row
  id_row <- which(pull(raw, 1) == "Respondent ID")
  if (length(id_row) == 0) return(NULL)

  responses <- raw[(id_row + 1):nrow(raw), ]

  # Determine which column has the text
  # Pattern: col1 = respondent_id, col2 = date, col3 = response text
  df <- tibble(
    qnum          = qnum,
    respondent_id = as.character(pull(responses, 1)),
    date          = as.character(pull(responses, 2)),
    text          = as.character(pull(responses, 3))
  ) |>
    filter(!is.na(text), text != "NA")

  df
}

# ---- Run parsing ----
closed_list <- list()
open_list   <- list()

for (i in seq_len(nrow(q_meta))) {
  row <- q_meta[i, ]

  if (row$type %in% c("closed", "closed_other")) {
    result <- parse_closed(row$sheet, row$qnum)
    if (!is.null(result)) closed_list[[row$qnum]] <- result
  }

  if (row$type %in% c("open", "closed_other")) {
    result <- parse_open(row$sheet, row$qnum)
    if (!is.null(result)) open_list[[row$qnum]] <- result
  }
}

closed_ended <- bind_rows(closed_list)
open_text    <- bind_rows(open_list)

# Attach question text
closed_ended <- closed_ended |>
  left_join(q_meta |> select(qnum, question_text), by = "qnum")

open_text <- open_text |>
  left_join(q_meta |> select(qnum, question_text), by = "qnum")

# ---- Save ----
saveRDS(closed_ended, here("data", "clean", "closed_ended.rds"))
saveRDS(open_text,    here("data", "clean", "open_text.rds"))
saveRDS(q_meta,       here("data", "clean", "q_meta.rds"))

cat("Parsed", nrow(closed_ended), "closed-ended rows and",
    nrow(open_text), "open-text responses.\n")
cat("Saved to data/clean/\n")
