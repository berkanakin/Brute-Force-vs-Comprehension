# ---------------------------------------------------------------------------
#  Brute-Force-vs-Comprehension | Descriptive  Analyses
# ---------------------------------------------------------------------------
#  This R script recreates the descriptive statistics reported in the
#  manuscript (control vs. self-referential accuracy at task levels 0‑2)
#  and summarises model output length (number of tokens generated) per
#  model × condition.
#  ------------------------------------------------------------------------
#  INPUT  : analysis_data_long.csv                     (accuracy section)
#           analysis_data_long_all_with_output.csv     (output‑length section)
#           – both stored in the repo under
#             data collection/analysis_data/
#  OUTPUT : *summary_tbl*  – accuracy + Wilson CIs
#           *summary_tokens* – mean ± SD output tokens
#  ------------------------------------------------------------------------
#  Author : Berkan Akın | berkan.akin@student.uva.nl
#  Updated: 2025‑07‑09
# ---------------------------------------------------------------------------

# 0  Load required packages --------------------------------------------------
required_pkgs <- c("dplyr", "tidyr", "readr", "binom", "here", "reticulate", "PropCIs")
new_pkgs      <- setdiff(required_pkgs, rownames(installed.packages()))
if (length(new_pkgs)) install.packages(new_pkgs, repos = "https://cloud.r-project.org")

lapply(required_pkgs, library, character.only = TRUE)

# 1  Helper: build raw.githubusercontent URL ----------------------------------
gh_raw <- function(file_path,
                   repo   = "berkanakin/Brute-Force-vs-Comprehension",
                   branch = "main") {
  sprintf(
    "https://raw.githubusercontent.com/%s/%s/%s",
    repo, branch, URLencode(file_path, reserved = TRUE)
  )
}

# ---------------------------------------------------------------------------
#  PART A · Accuracy.                 ---------------------------------------
# ---------------------------------------------------------------------------

accuracy_url <- gh_raw("data collection/analysis_data/analysis_data_long.csv")

data_long <- read_csv(accuracy_url, show_col_types = FALSE, progress = FALSE)

wilson_ci <- function(k, n, conf.level = 0.95) {
  binom::binom.wilson(k, n, conf.level) %>%
    transmute(mean_acc = mean, ci_low = lower, ci_high = upper)
}

# 1. Accuracy summaries ------------------------------------------------------

# Experimental trials --------------------------------------------------------
exp_summary <- data_long %>%
  filter(valid, condition_type == "experimental", !is.na(task_level)) %>%
  group_by(model, task_level) %>%
  summarise(n = n(), k = sum(revised_accuracy), .groups = "drop") %>%
  bind_cols(wilson_ci(k = .$k, n = .$n)) %>%
  mutate(condition = "experimental")

exp_summary[exp_summary$task_level==1,]

# Control trials -------------------------------------------------------------
control_summary <- data_long %>%
  filter(valid, condition_type == "control") %>%
  group_by(model) %>%
  summarise(n = n(), k = sum(revised_accuracy), .groups = "drop") %>%
  bind_cols(wilson_ci(k = .$k, n = .$n)) %>%
  mutate(task_level = 0, condition = "control")

# Merge & preview ------------------------------------------------------------
summary_tbl <- bind_rows(exp_summary, control_summary) %>%
  arrange(model, condition, task_level)

setwd("/Users/berkanakin/Library/CloudStorage/OneDrive-Personal/Studium/Master/UvA Psychology (research) Master/Courses/Thesis/data analysis/preliminary")
write.csv(summary_tbl, "raw_accuracy.csv")

print("--- Accuracy summary (Wilson 95% CI) ---")
print(summary_tbl, n = Inf)

#  2.1. Per‑model absolute drop: Control → SR‑L0 -------------------------------
per_model_drop <- data_long %>%
  filter(valid, (condition_type == "control") |
           (condition_type == "experimental" & task_level == 0)) %>%
  mutate(cond_key = ifelse(condition_type == "control", "control_acc", "sr_l0_acc")) %>%
  group_by(model, cond_key) %>%
  summarise(acc = mean(revised_accuracy), .groups = "drop") %>%
  tidyr::pivot_wider(names_from = cond_key, values_from = acc) %>%
  mutate(abs_drop = control_acc - sr_l0_acc) %>%
  arrange(model)

cat("\n--- Per‑model drop (Control ‑ SR‑L0) ---\n")
print(per_model_drop, n = Inf)

#

## -----------------------------------------------------------
##  H1  •  Control  vs  Self-referential Level-0  (per model)
## -----------------------------------------------------------
library(dplyr)
library(tidyr)
library(PropCIs)

per_model_drop <- data_long %>%
  ## keep only the two conditions we need
  filter(
    valid,
    condition_type == "control" |
      (condition_type == "experimental" & task_level == 0)
  ) %>%
  ## tag rows so we can pivot later
  mutate(cond_key = ifelse(condition_type == "control", "control", "sr_l0")) %>%
  
  ## ── counts per model × condition ─────────────────────────
  group_by(model, cond_key) %>%
  summarise(
    n       = n(),
    correct = sum(revised_accuracy),
    .groups = "drop"
  ) %>%
  
  ## ── wide format: n_control, correct_sr_l0, … ────────────
  pivot_wider(
    names_from  = cond_key,
    names_glue  = "{.value}_{cond_key}",
    values_from = c(n, correct)
  ) %>%
  
  ## ── proportions & absolute drop ──────────────────────────
  mutate(
    prop_control = correct_control / n_control,
    prop_sr_l0   = correct_sr_l0   / n_sr_l0,
    abs_drop     = prop_control - prop_sr_l0          # ≥ 0 means SR-L0 is worse
  ) %>%
  
  ## ── 95 % CI for the difference in proportions ───────────
  rowwise() %>%
  mutate(
    ci = list(
      PropCIs::diffscoreci(
        correct_control, n_control,
        correct_sr_l0,  n_sr_l0,
        conf.level = 0.95
      )
    ),
    lower_delta = ci$conf.int[1],
    upper_delta = ci$conf.int[2]
  ) %>%
  ungroup() %>%
  select(-ci) %>%                       # drop the list column
  arrange(model)

cat("\n--- Per-model drop (Control – SR-L0) with 95 % CIs ---\n")
print(per_model_drop, n = Inf)



# 2.1. Per‑model absolute drop:  SR‑L0 → SR‑L1-------------------------------

acc_drop <- h2_data %>%
  filter(valid, task_level %in% c("0", "1")) %>%          # Levels 0 & 1 only
  group_by(model, task_level) %>%
  summarise(
    n       = n(),
    correct = sum(revised_accuracy),
    .groups = "drop"
  ) %>%
  pivot_wider(
    names_from  = task_level,                             # n_L0, correct_L1 …
    names_glue  = "{.value}_L{task_level}",
    values_from = c(n, correct)
  ) %>%
  mutate(
    prop_L0 = correct_L0 / n_L0,
    prop_L1 = correct_L1 / n_L1,
    delta   = prop_L1 - prop_L0
  ) %>%
  rowwise() %>%
  mutate(
    ci = list(PropCIs::diffscoreci(correct_L1, n_L1,
                                   correct_L0, n_L0,
                                   conf.level = 0.95)),
    lower_delta = ci$conf.int[1],    # grab CI bounds
    upper_delta = ci$conf.int[2]
  ) %>%
  ungroup() %>%
  select(-ci)

print(acc_drop)


## 3 Bonferroni-Holm one-sample t-tests vs chance ---------------------------
chance <- 1/14  # ≈ 0.0714

ttest_table <- data_long %>%
  filter(valid, condition_type == "experimental", task_level == 0) %>%
  group_by(model) %>%
  summarise(
    n      = n(),
    mean   = mean(revised_accuracy),
    t_stat = {
      tst <- t.test(revised_accuracy, mu = chance, alternative = "greater")
      tst$statistic
    },
    p_raw  = {
      tst <- t.test(revised_accuracy, mu = chance, alternative = "greater")
      tst$p.value
    },
    .groups = "drop") %>%
  mutate(p_adj = p.adjust(p_raw, method = "holm"))

cat("\n--- One-sample t-tests vs chance (Holm-adjusted) ---\n")
print(ttest_table, n = Inf)


# ---------------------------------------------------------------------------
#  PART B · Output‑length summary  -------------------------------------------
# ---------------------------------------------------------------------------

# 2  Load full dataset incl. model_output column -----------------------------
length_url <- gh_raw(
  "data collection/analysis_data/analysis_data_long_all_with_output.csv")

df_len <- read_csv(length_url, show_col_types = FALSE, progress = FALSE)

# 3  GPT‑4 tokenizer via Python tiktoken -------------------------------------
#    → ensure `tiktoken` is installed once in the active Python env
try(import("tiktoken"), silent = TRUE) |>
  {
    if (inherits(., "try-error")) {
      message("Installing Python package 'tiktoken' (once‑off)…")
      reticulate::py_install("tiktoken", method = "auto", pip = TRUE)
    }
  }

tiktoken <- import("tiktoken")
enc      <- tiktoken$encoding_for_model("gpt-4")

token_len <- function(txt) length(enc$encode(txt))


# 4  Add token counts & tidy condition factor -------------------------------
summary_tokens <- df_len %>%
  mutate(output_tokens = vapply(model_output, token_len, integer(1)),
         condition = case_when(
           condition_type == "control"                        ~ "Control",
           condition_type == "experimental" & task_level == 0 ~ "SR‑L0",
           condition_type == "experimental" & task_level == 1 ~ "SR‑L1",
           condition_type == "experimental" & task_level == 2 ~ "SR‑L2",
           TRUE                                               ~ NA_character_
         )) %>%
  filter(valid, !is.na(condition)) %>%
  mutate(condition = factor(condition,
                            levels = c("Control", "SR‑L0", "SR‑L1", "SR‑L2"))) %>%
  group_by(model, condition) %>%
  summarise(n       = n(),
            mean_tk = mean(output_tokens),
            sd_tk   = sd(output_tokens),
            .groups = "drop") %>%
  arrange(model, condition)

print("\n--- Output‑length summary (tokens) ---")
print(summary_tokens, n = Inf)

# 5  (OPTIONAL) Export artefacts --------------------------------------------
# here("data analysis") |> dir.create(showWarnings = FALSE, recursive = TRUE)
# write_csv(summary_tbl,    here("data analysis", "summary_accuracy_wilson_ci.csv"))
# write_csv(summary_tokens, here("data analysis", "summary_output_length.csv"))
