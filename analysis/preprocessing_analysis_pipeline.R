###############################################################################
#  Tidy pipeline + analysis skeleton for “meta-rule” accuracy project
#  ------------------------------------------------------------------
#  • Builds ONE master long table from:
#       – list of model-specific response data frames (`model_data_list`)
#       – trial-level metadata  (metadata.csv already read as `metadata`)
#       – model-size lookup     (model_parameters_table.csv on disk)
#  • Adds:
#       – accuracy  (0/1 integer from `correct`)
#       – valid     (logical copy of `valid_format`)
#       – task_level, baseline, condition, log_size
#  • Produces two analysis views (h1_data, h2_data) + example GLMM calls
###############################################################################

setwd("/Users/berkanakin/Library/CloudStorage/OneDrive-Personal/Studium/Master/UvA Psychology (research) Master/Courses/Thesis/data analysis/preliminary/analysis_data_long")


# ─────────────────────────────────────────────────────────────────────────────
# 0. Load data ------------------------------------------------------------
# ─────────────────────────────────────────────────────────────────────────────

model_data_list <- readRDS("/Users/berkanakin/Library/CloudStorage/OneDrive-Personal/Studium/Master/UvA Psychology (research) Master/Courses/Thesis/data analysis/preliminary/preprocessing/model_data_list.rds")
metadata <-read.csv("/Users/berkanakin/Library/CloudStorage/OneDrive-Personal/Studium/Master/UvA Psychology (research) Master/Courses/Thesis/data analysis/preliminary/metadata/llm_itemset_metadata.csv")
size_tbl <- read.csv("/Users/berkanakin/Library/CloudStorage/OneDrive-Personal/Studium/Master/UvA Psychology (research) Master/Courses/Thesis/data analysis/preliminary/model_info_data/model_parameters_table.csv")


# ─────────────────────────────────────────────────────────────────────────────

# ─────────────────────────────────────────────────────────────────────────────
# 0. Load packages ------------------------------------------------------------
# ─────────────────────────────────────────────────────────────────────────────
library(tidyverse)   # dplyr, purrr, readr, etc.
library(lme4)        # (g)lmer
library(emmeans)     # pairwise marginal means
library(segmented)   # breakpoint estimation (H2c)
library(dplyr)
library(stringi)
library(stringr)


# ─────────────────────────────────────────────────────────────────────────────
# 1. Stack ALL model-response data frames into one “models_long” --------------
#    • model_data_list is assumed to exist: a named list of data frames,
#      each with columns: trial, correct (TRUE/FALSE), valid_format (TRUE/FALSE),
#      plus any other trial-wise variables you kept.
#    • We DROP prompt / expected_output because they are identical across models
#      and would otherwise duplicate thousands of characters.
# ─────────────────────────────────────────────────────────────────────────────

# ─────────────────────────────────────────────────────────────────────────────
# 1. Flatten nested model list: [model][condition] → [flat list of data frames]
#    • Standardizes 'correct' to logical
#    • Ensures all rows have a consistent 'valid' column
# ─────────────────────────────────────────────────────────────────────────────
flat_model_dfs <- model_data_list %>%
  imap(~ {
    model_id <- .y
    map2(.x, names(.x), function(df, cond_name) {
      df %>%
        mutate(
          model          = model_id,
          condition_type = ifelse(str_detect(cond_name, "control"), "control", "experimental"),
          correct        = as.logical(correct),                          # Ensure logical
          valid          = !is.na(predicted_word) & str_count(predicted_word, "\\s+") == 0
        )
    })
  }) %>%
  flatten()

# ─────────────────────────────────────────────────────────────────────────────
# Stack all cleaned model × condition data
#    • Drop long/unneeded text columns
#    • Keep one 'valid' column only
# ─────────────────────────────────────────────────────────────────────────────
models_long <- bind_rows(flat_model_dfs) %>%
  dplyr::select(-dplyr::any_of(c("system_prompt","model_output", "valid_format", "output_tokens", "total_tokens", "context_used"
  ))) %>%                    # Drop original 'valid_format'
  mutate(
    accuracy = as.integer(correct)  # Create accuracy outcome column
  )



# ─────────────────────────────────────────────────────────────────────────────
# 2. Prepare trial-level metadata --------------------------------------------
#    • Only a handful of columns are required for analyses
#      (task_level, baseline). Keep others if you plan exploratory checks.
# ─────────────────────────────────────────────────────────────────────────────

meta_tidy <- metadata %>%
  transmute(
    trial,
    task_level = case_when(
      lvl == 0 & lvl0_first ~ 0L,
      lvl == 1              ~ 1L,
      lvl == 2              ~ 2L,
      TRUE                  ~ NA_integer_     # ← CORRECT (no parentheses)
    ),
    task_level_all = lvl,
    item_id
  )


# ─────────────────────────────────────────────────────────────────────────────
# 3. Merge responses ↔︎ metadata, derive experimental condition ---------------
#    • “control” rows have no meta info ⇒ NA after join
# ─────────────────────────────────────────────────────────────────────────────
data_long <- models_long %>%
  left_join(meta_tidy, by = "trial") 

# Task level variable for control

data_long$task_level[data_long$condition_type=="control"] <- 0
data_long$task_level_all[data_long$condition_type=="control"] <- 0
data_long$item_id[data_long$condition_type=="control"] <- NA
# ─────────────────────────────────────────────────────────────────────────────
# 4. Add model-size information ----------------------------------------------
#    • We read the lookup table once. Active parameters preferred; if missing
#      fall back to total_parameters. Log-transform size for better scaling.
# ─────────────────────────────────────────────────────────────────────────────
size_tbl <- size_tbl %>%
  mutate(
    model_size = coalesce(active_parameters, total_parameters),
    log_size   = log10(model_size),
    model_size_ranking = dense_rank(active_parameters)
  )

# Join size columns and final type conversions
data_long <- data_long %>%
  left_join(size_tbl, by = "model") %>%
  mutate(
    # Ordered factor ensures polynomial trend coding works out of the box
    task_level = factor(task_level,
                        levels  = sort(unique(task_level), na.last = TRUE),
                        ordered = TRUE),
    model      = factor(model)                   # random-effects grouping factor
  )

# ─────────────────────────────────────────────────────────────────────────────
#  accuracy_revised----------------------------------
# ─────────────────────────────────────────────────────────────────────────────

# Add a revised_accuracy column based on condition type
data_long <- data_long %>%
  mutate(
    revised_accuracy = case_when(
      condition_type == "experimental" ~ as.integer(expected_output == predicted_word),
      condition_type == "control" ~ as.integer(
        str_to_lower(stri_trans_general(expected_output, "Latin-ASCII")) ==
          str_to_lower(stri_trans_general(predicted_word, "Latin-ASCII"))
      ),
      TRUE ~ NA_integer_  # for safety
    )
  )

# Subset rows where accuracy and revised_accuracy don't match
mismatches <- data_long %>%
  filter(accuracy != revised_accuracy)

# Print mismatched rows
print(mismatches)
# ─────────────────────────────────────────────────────────────────────────────
# 6. Chance responding ----------------------------------
# ─────────────────────────────────────────────────────────────────────────────

data_long <- data_long %>%
  mutate(chance_prob = case_when(
    task_level == 0 ~ 1/14,
    task_level %in% c(1, 2) & str_detect(prompt, fixed(expected_output)) ~ 1/14,
    task_level %in% c(1, 2) ~ 1e-10,   # avoid qlogis(0) = -Inf
    TRUE ~ NA_real_
  ))


data_long <- data_long %>%
  mutate(chance_prob_all = case_when(
    task_level_all == 0 ~ 1/14,
    task_level_all %in% c(1, 2) & str_detect(prompt, fixed(expected_output)) ~ 1/14,
    task_level_all %in% c(1, 2) ~ 1e-10,   # avoid qlogis(0) = -Inf
    TRUE ~ NA_real_
  ))

data_long$chance_prob[data_long$condition_type=="control"] <- 1e-10
data_long$chance_prob_all[data_long$condition_type=="control"] <- 1e-10

# ─────────────────────────────────────────────────────────────────────────────
# 7. Quick sanity checks ----------------------------------
# ─────────────────────────────────────────────────────────────────────────────
# How many trials survive the format check?
data_long %>% count(valid) %>% print()

# Control that every model × level combo has ≥ 1 valid row
data_long %>%
  filter(valid) %>%
  count(model, task_level) %>%
  pivot_wider(names_from = task_level, values_from = n, values_fill = 0) %>%
  print(n = Inf)


# ─────────────────────────────────────────────────────────────────────────────
write_csv(data_long,  "analysis_data_long.csv")


# ─────────────────────────────────────────────────────────────────────────────
# 8. Add missing input_tokens ----------------------------------
# ─────────────────────────────────────────────────────────────────────────────

library(ggplot2)

ggplot(data_long[data_long$condition_type=="experimental",], aes(x = trial, y = input_tokens, color = model)) +
  geom_line(alpha = 0.7) +
  labs(
    title = "Input Tokens per Trial by Model",
    x = "Trial Number",
    y = "Input Tokens"
  ) +
  theme_minimal()

## This shows that they are all the same

#Adding missing input tokens

data_long$input_tokens[data_long$condition_type=="control"& data_long$model =="claude_4_sonnet"] <- data_long$input_tokens[data_long$condition_type=="control"& data_long$model =="grok_3"]
any(is.na(data_long$input_tokens[data_long$condition_type=="control"]))


any(is.na(data_long$input_tokens[data_long$condition_type=="experimental"]))

data_long$input_tokens[data_long$condition_type=="experimental"& data_long$model =="claude_4_sonnet"] <- data_long$input_tokens[data_long$condition_type=="experimental"& data_long$model =="grok_3"]
data_long$input_tokens[data_long$condition_type=="experimental"& data_long$model =="gpt_3_5_turbo"] <- data_long$input_tokens[data_long$condition_type=="experimental"& data_long$model =="grok_3"]


