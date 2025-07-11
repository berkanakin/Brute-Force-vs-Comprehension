# Exploratory Analysis –  Script
# ======================================
# Author: <Berkan Akin>
# Date:   2025‑07‑11
#
# This  R script reproduces the exploratory analyses
# reported in the manuscript (Section “Exploratory Analysis”).
# The code is structured by research question (Q1 – Q5) 
#---------------------------------------------------------------------------
# Prerequisites ------------------------------------------------------------
#---------------------------------------------------------------------------
# Objects expected in the global environment
#   * df            – analysis_data_long_all_with_output.csv
#   * metadata      – llm_itemset_metadata.csv
#   * h2_data.      – h2_data.csv
#   * pretty_names  – named vector that maps internal model IDs → pretty labels
#
# df        <- read.csv("https://github.com/berkanakin/Brute-Force-vs-Comprehension/blob/main/data%20collection/analysis_data/analysis_data_long_all_with_output.csv")
# metadata  <- read.csv("https://github.com/berkanakin/Brute-Force-vs-Comprehension/blob/main/data%20collection/meta_data/llm_itemset_metadata.csv")
# pretty_names <-  c(claude_4_sonnet = "Claude Sonnet 4",
#                     deepseek_v3     = "Deepseek V3",
#                     deepseek_r1     = "Deepseek R1",
#                     gemini_2_0_flash= "Gemini 2.0 Flash",
#                     gpt_3_5_turbo   = "GPT-3.5 T",
#                     gpt_4_1         = "GPT-4.1",
#                     gpt_4o          = "GPT-4o",
#                     grok_3          = "Grok 3",
#                     llama_3_3       = "Llama 3.3",
#                     llama_4         = "Llama 4",
#                     gpt_o3          = "o3")
#---------------------------------------------------------------------------

## 0.1  Packages -------------------------------------------------------------

dep_pkgs <- c(
  "dplyr", "tidyr", "stringr", "purrr", "lme4", "lmerTest", "emmeans",
  "performance", "broom.mixed", "car", "reticulate", "lavaan"
)

inst <- dep_pkgs[!dep_pkgs %in% installed.packages()[, "Package"]]
if (length(inst)) install.packages(inst, repos = "https://cloud.r-project.org")

invisible(lapply(dep_pkgs, library, character.only = TRUE, quietly = TRUE))

## 0.2  Python‑based tokenizer (tiktoken) -----------------------------------
# Used in Q5 to count output tokens (CoT length).

tiktoken <- reticulate::import("tiktoken")           # pip install tiktoken
enc      <- tiktoken$encoding_for_model("gpt-4")      # GPT‑4 tokenizer

# Helper: number of tokens in a string --------------------------------------

token_len <- function(txt) length(enc$encode(txt))

## 0.3  Helper: build regex that requires ALL words to be present ------------

build_all_words_regex <- function(x) {
  words <- str_extract_all(x, boundary("word"))[[1]]
  words <- words[words != ""]
  str_c(map(words, ~ str_c("(?=.*\\b", .x, "\\b)")), collapse = "")
}


# Data wrangling -----------------------------------------------------------

df <- df |>
  inner_join(metadata, by = "trial") |>
  filter(valid, condition_type == "experimental")

#---------------------------------------------------------------------------
# Question 1 – What explains the Level‑0 → Level‑1 accuracy decline? (H2a)
#---------------------------------------------------------------------------

## 1A) Key‑prompt coverage -------------------------------------------------

df_prompt_hits <- df |>
  mutate(
    prompt_regex         = map_chr(prompt_key, build_all_words_regex),
    has_all_prompt_words = map2_lgl(model_output, prompt_regex,
                                    ~ str_detect(.x, regex(.y, ignore_case = TRUE))
    )
  ) |>
  group_by(model, task_level) |>
  summarise(
    n_hits  = sum(has_all_prompt_words, na.rm = TRUE),
    n_total = n(),
    prop    = n_hits / n_total,
    .groups = "drop"
  ) |>
  mutate(
    model      = recode(model, !!!pretty_names),
    task_level = factor(task_level,
                        levels = c(0, 1, 2),
                        labels = c("Level 0", "Level 1", "Level 2"))
  ) |>
  filter(task_level != "Level 2") |>
  mutate(
    se      = sqrt(prop * (1 - prop) / n_total),
    ci_low  = prop - 1.96 * se,
    ci_high = prop + 1.96 * se
  )

## 1B) Mediation analysis (GLMM & SEM) ------------------------------------

df_med <- df |>
  filter(task_level %in% c(0, 1)) |>
  transmute(
    model,
    item_id         = item_id.y,
    task_level      = as.numeric(task_level == 1),        # 0 = L0, 1 = L1
    prompt_hit      = as.numeric(str_detect(
      model_output,
      regex(map_chr(prompt_key, build_all_words_regex),
            ignore_case = TRUE)
    )),
    revised_accuracy = as.numeric(revised_accuracy)
  )

### GLMMs ------------------------------------------------------------------

m_H2_unmediated <- glmer(
  revised_accuracy ~ task_level +
    (1 + task_level | model) + (1 | item_id),
  family  = binomial,
  data    = df_med,
  control = glmerControl(optimizer = "bobyqa")
)

m_H2_mediated <- update(
  m_H2_unmediated,
  . ~ task_level + prompt_hit + (1 + task_level | model) + (1 | item_id)
)

### Mediation via SEM ------------------------------------------------------

sem_model <- '
  prompt_hit        ~ a * task_level
  revised_accuracy  ~ b * prompt_hit + c_prime * task_level

  indirect := a * b
  total    := c_prime + (a * b)
'

fit_sem <- sem(
  sem_model,
  data      = df_med,
  ordered   = c("prompt_hit", "revised_accuracy"),
  estimator = "WLSMV"
)

#---------------------------------------------------------------------------
# Question 2 – Do LLMs recover at Level 2? (Continuation of H2a)
#---------------------------------------------------------------------------

df_lvl2 <- filter(df, task_level == 2)

# Print reasoning chains for all *correct* Level‑2 trials for manual review
# Redirect to a log file if you prefer: sink("lvl2_correct.txt"); …; sink()
df_lvl2 |>
  filter(revised_accuracy == 1) |>
  select(model, item_id.y, model_output) |>
  arrange(model) |>
  print(n = Inf)

#---------------------------------------------------------------------------
# Question 3 – Have the “edge‑case” models collapsed at Level 1? (H2b)
#---------------------------------------------------------------------------

edge_cases <- read_csv("successful_outputs_lvl1_scored.csv",
                       show_col_types = FALSE)

edge_cases |>
  select(model, item_id, rule_inference, rule_application, model_output) |>
  arrange(model) |>
  print(n = Inf)

#---------------------------------------------------------------------------
# Question 4 – Has o3 collapsed at Level 2?
#---------------------------------------------------------------------------

df_o3_lvl2 <- df |>
  filter(model == "gpt_o3", task_level == 2)

# Inspect o3 reasoning for all Level‑2 trials
print(df_o3_lvl2 |>
        select(item_id.y, revised_accuracy, model_output), n = Inf)

# ---------------------------------------------------------------------------
# 5.  Does CoT length moderate the task‑level decline?  (H2c) ---------------
# ---------------------------------------------------------------------------

## 5.1  Basis data -----------------------------------------------------------
# `h2_data` is the confirmatory‑analysis frame used in the preregistered GLMM.
# It must already be in the workspace (e.g., loaded in Section 0 or earlier).

stopifnot(exists("h2_data"))

# Exclude model “gpt_o3” because it truncates outputs and therefore
# under‑counts tokens. Consistency is crucial for the CoT‑length analysis.
h2_data <- h2_data %>%
  filter(model != "gpt_o3")

## 5.2  Per‑trial output‑token counts ---------------------------------------
# Re‑use the long `df` that holds *all* trials and the `model_output` column.

cot_df <- df %>%
  filter(model != "gpt_o3") %>%                      # keep data sets aligned
  mutate(output_tokens = vapply(model_output, token_len, integer(1))) %>%
  select(trial, model, output_tokens)

## 5.3  Merge with `h2_data` -------------------------------------------------
h2_data <- h2_data %>%
  left_join(cot_df, by = c("trial", "model"))

## 5.4  Group‑mean‑centred CoT length ---------------------------------------
h2_data <- h2_data %>%
  group_by(model) %>%
  mutate(cot_length_c = output_tokens - mean(output_tokens, na.rm = TRUE)) %>%
  ungroup()

## 5.5  GLM with interaction -----------------------------------------------

#   m_H2c <- glmer(
#   revised_accuracy ~ task_level_num * cot_length_c + input_tokens_z
#   (1 + task_level | model) +
#   (1 | item_id),
#   family  = binomial,
#   data    = h2_data,
#   control = glmerControl(optimizer = "bobyqa")
#   )


# The GLMM failed to converge, so we fit a simpler informative—binomial GLM.  
# Here, we use raw output_tokens, because simplified GLM contains no model-level random effects

m_H2c <- glm(
  revised_accuracy ~ task_level_num * scale(output_tokens) + input_tokens_z,
  data   = h2_data,
  family = binomial
)

print(summary(m_H2c))

# ---------------------------------------------------------------------------
# Session info (for reproducibility) ----------------------------------------
# ---------------------------------------------------------------------------

sessionInfo()
