###############################################################################
# preprocess_analysis_data.R
# Author: Berkan Akin · 2025-07-15
# Purpose:
#   - Load all model outputs + metadata + model‐size info
#   - Check response format, mark valid/invalid
#   - Merge everything into one long table
#   - Compute accuracy, revised_accuracy, chance rates
#   - Summarize valid vs. total per model & extract invalid responses
#   - Write final analysis_data_long.csv
###############################################################################

library(tidyverse)
library(stringr)
library(stringi)
library(tools)

# ─────────────────────────────────────────────────────────────────────────────
# A. File paths (relative to repo root)
# ─────────────────────────────────────────────────────────────────────────────
model_data_dir <- "data collection/analysis_data/model_data_raw/"
metadata_path  <- "data collection/analysis_data/llm_itemset_metadata.csv"
size_tbl_path  <- "data collection/analysis_data/model_parameters_table.csv"
output_path    <- "data collection/analysis_data/analysis_data_long.csv"

# ─────────────────────────────────────────────────────────────────────────────
# B. 1. Load per‐model CSVs into nested list: model_data_list[[model]][[model_cond]]
# ─────────────────────────────────────────────────────────────────────────────
files <- list.files(model_data_dir,
                    pattern = "_(experimental|control)\\.csv$",
                    full.names = TRUE, recursive = TRUE)
if (!length(files)) stop("No model CSVs found in ", model_data_dir)

# Helper: get model ID from filename
clean_var_name <- function(fp) {
  base <- file_path_sans_ext(basename(fp))
  gsub("-", "_", sub("_(experimental|control)$", "", base))
}

model_data_list <- list()
for (f in files) {
  stem      <- file_path_sans_ext(basename(f))      # e.g. "gpt-4o_control"
  condition <- if (str_ends(stem, "_control")) "control" else "experimental"
  model_id  <- clean_var_name(f)
  model_data_list[[model_id]][[stem]] <- read.csv(f, stringsAsFactors = FALSE)
}

# ─────────────────────────────────────────────────────────────────────────────
# C. 2. Mark valid responses (single‐word, non‐NA)
# ─────────────────────────────────────────────────────────────────────────────
add_valid <- function(df) {
  df %>% mutate(valid = !is.na(predicted_word) &
                  str_detect(str_trim(predicted_word), "^[^\\s]+$"))
}
model_data_list <- map(model_data_list, ~ map(.x, add_valid))

# ─────────────────────────────────────────────────────────────────────────────
# D. 3. Flatten list → one big data frame, add model & condition columns
# ─────────────────────────────────────────────────────────────────────────────
flat_dfs <- model_data_list %>%
  imap(function(model_entry, model_id) {
    map2(model_entry, names(model_entry), function(df, cond) {
      df %>% mutate(model = model_id,
                    condition_type = ifelse(str_detect(cond, "control"),
                                            "control", "experimental"))
    })
  }) %>% flatten()
models_long <- bind_rows(flat_dfs)

# ─────────────────────────────────────────────────────────────────────────────
# E. 4. Load metadata, tidy, and join
# ─────────────────────────────────────────────────────────────────────────────
metadata <- read.csv(metadata_path, stringsAsFactors = FALSE)
meta_tidy <- metadata %>%
  transmute(
    trial,
    prompt,
    expected_output,
    task_level = case_when(
      lvl == 0 & lvl0_first ~ 0L,
      lvl == 1             ~ 1L,
      lvl == 2             ~ 2L,
      TRUE                 ~ NA_integer_
    ),
    task_level_all = lvl,
    item_id
  )

data_long <- models_long %>%
  left_join(meta_tidy, by = "trial") %>%
  mutate(
    task_level     = ifelse(condition_type=="control", 0L, task_level),
    task_level_all = ifelse(condition_type=="control", 0L, task_level_all),
    item_id        = ifelse(condition_type=="control", NA, item_id)
  )

# ─────────────────────────────────────────────────────────────────────────────
# F. 5. Load model size table, join, and log‐transform
# ─────────────────────────────────────────────────────────────────────────────
size_tbl <- read.csv(size_tbl_path, stringsAsFactors = FALSE) %>%
  mutate(
    model_size = coalesce(active_parameters, total_parameters),
    log_size   = log10(model_size),
    model_rank = dense_rank(active_parameters)
  )
data_long <- data_long %>%
  left_join(size_tbl, by = "model") %>%
  mutate(
    task_level = factor(task_level, ordered = TRUE),
    model      = factor(model)
  )

# ─────────────────────────────────────────────────────────────────────────────
# G. 6. Compute accuracy & revised_accuracy
# ─────────────────────────────────────────────────────────────────────────────
data_long <- data_long %>%
  mutate(
    accuracy = as.integer(correct),
    revised_accuracy = case_when(
      condition_type=="experimental" ~ as.integer(expected_output == predicted_word),
      condition_type=="control"      ~ as.integer(
        str_to_lower(stri_trans_general(expected_output,"Latin-ASCII")) ==
          str_to_lower(stri_trans_general(predicted_word,"Latin-ASCII"))
      ),
      TRUE ~ NA_integer_
    )
  )

# ─────────────────────────────────────────────────────────────────────────────
# H. 7. Compute chance probabilities
# ─────────────────────────────────────────────────────────────────────────────
data_long <- data_long %>%
  mutate(
    chance_prob = case_when(
      task_level==0 ~ 1/14,
      task_level %in% c(1,2) &
        str_detect(prompt, fixed(expected_output)) ~ 1/14,
      task_level %in% c(1,2) ~ 1e-10,
      TRUE ~ NA_real_
    ),
    chance_prob_all = case_when(
      task_level_all==0 ~ 1/14,
      task_level_all%in%c(1,2)&
        str_detect(prompt, fixed(expected_output)) ~ 1/14,
      task_level_all%in%c(1,2) ~ 1e-10,
      TRUE ~ NA_real_
    )
  ) %>%
  mutate(
    chance_prob     = ifelse(condition_type=="control", 1e-10, chance_prob),
    chance_prob_all = ifelse(condition_type=="control", 1e-10, chance_prob_all)
  )

# ─────────────────────────────────────────────────────────────────────────────
# I. 8. Summarize valid vs total per model & condition
# ─────────────────────────────────────────────────────────────────────────────
summary_tbl <- data_long %>%
  group_by(model, condition_type) %>%
  summarise(
    valid = sum(valid, na.rm = TRUE),
    total = n(),
    ratio = sprintf("%d/%d", valid, total),
    .groups = "drop"
  )
print(summary_tbl)

# ─────────────────────────────────────────────────────────────────────────────
# J. 9. Extract all invalid responses for inspection
# ─────────────────────────────────────────────────────────────────────────────
invalid_responses <- data_long %>%
  filter(!valid & !is.na(predicted_word)) %>%
  transmute(
    model, condition_type, trial,
    prompt, predicted_word, expected_output, correct
  )
print(invalid_responses)

# ─────────────────────────────────────────────────────────────────────────────
# K. 10. Sanity checks
# ─────────────────────────────────────────────────────────────────────────────
cat("-- Valid counts by condition/level --\n")
print(data_long %>% count(condition_type, task_level, valid))

cat("-- Any accuracy mismatches? --\n")
print(data_long %>% filter(accuracy != revised_accuracy))

# ─────────────────────────────────────────────────────────────────────────────
# L. 11. Write final CSV
# ─────────────────────────────────────────────────────────────────────────────
write_csv(data_long, output_path)
cat("Wrote analysis data to:", output_path, "\n")
