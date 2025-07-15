# =====================================================================
#   check_invalid_trials.R  ·  (Author: Berkan Akin, 2025‑07‑15)
# ---------------------------------------------------------------------
#   • Screens excluded trials for exclusion reasons
#   • Summarizes invalids by task level and condition
#   • Fits GLMMs to test for differential invalid rates
#   • Outputs a tidy summary for manuscript reporting
# =====================================================================

library(tidyverse)
library(lme4)

# ---- 1. Load data ----
# data <- read.csv("https://github.com/berkanakin/Brute-Force-vs-Comprehension/blob/main/data%20collection/analysis_data/analysis_data_long_all_with_output.csv")

# ---- 2. Extract invalids ----
invalids <- data %>%
  filter(valid == FALSE & !is.na(task_level))

str(invalids)

# ---- 3. (Optional) Quick reason check: define helper functions ----

# Reason 1: Output cut off mid-sentence (likely token limit)
is_truncated <- function(response, min_len = 30) {
  nchar(response) > min_len && !grepl("[.?!\"]\\s*$", response)
}

# Reason 2: Missing "Answer:" prefix
is_prefix_missing <- function(response) {
  !grepl("(?i)answer\\s*:", response)
}

# Reason 3: More than one word after "Answer:"
is_overlong_answer <- function(response) {
  # Extract after "Answer:"; allow punctuation
  ans <- tolower(sub(".*answer\\s*:\\s*", "", response, ignore.case = TRUE))
  n_words <- str_count(ans, "\\w+")
  n_words > 1
}

# Check reasons per invalid trial
invalids$truncated      <- sapply(invalids$predicted_word, is_truncated)
invalids$prefix_missing <- sapply(invalids$predicted_word, is_prefix_missing)
invalids$overlong_ans   <- sapply(invalids$predicted_word, is_overlong_answer)

# ---- 4. Summary of reasons ----
reason_summary <- invalids %>%
  summarise(
    N = n(),
    truncated         = sum(truncated, na.rm = TRUE),
    prefix_missing    = sum(prefix_missing, na.rm = TRUE),
    overlong_ans      = sum(overlong_ans, na.rm = TRUE)
  )
print(reason_summary)

# ---- 5. Summarize by task level (experimental condition only) ----
experimental_invalids <- invalids %>%
  filter(condition_type == "experimental")

invalid_counts <- table(experimental_invalids$task_level)
total_trials <- c("0" = 268, "1" = 65, "2" = 5)
percent_invalids <- (invalid_counts / total_trials[names(invalid_counts)]) * 100

percent_df <- data.frame(
  task_level    = as.integer(names(percent_invalids)),
  invalid_count = as.vector(invalid_counts),
  total_trials  = as.numeric(total_trials[names(invalid_counts)]),
  percent_invalid = round(percent_invalids, 2)
)
print(percent_df)

# ---- 6. Chi-squared test: Are invalid rates even across levels? ----
chisq_test <- chisq.test(
  x = as.integer(invalid_counts),
  p = as.numeric(total_trials[names(invalid_counts)]) / sum(total_trials)
)
print(chisq_test)

# ---- 7. Binomial GLMM: valid ~ condition/task_level ----
# (a) By condition at task level 0
check_l0 <- data %>% filter(task_level == 0 & !is.na(valid))
model2_ri <- glmer(valid ~ condition_type + (1 | model), data = check_l0, family = binomial)
model2_rs <- glmer(valid ~ condition_type + (1 + condition_type | model), data = check_l0, family = binomial)
summary(model2_ri)
anova(model2_ri, model2_rs)

# (b) By task level within experimental/self-ref
check_exp <- data %>% filter(condition_type == "experimental" & !is.na(task_level))
model1 <- glmer(valid ~ task_level + (1 + task_level | model), data = check_exp, family = binomial)
summary(model1)

# ---- 8. Output: Key findings ----

cat("\n=== Exclusion Reason Summary ===\n")
print(reason_summary)
cat("\n=== Invalids by Task Level (experimental) ===\n")
print(percent_df)
cat("\n=== Chi-squared test ===\n")
print(chisq_test)
cat("\n=== GLMM (task_level) ===\n")
print(summary(model1)$coefficients)
cat("\n=== GLMM (condition_type at L0) ===\n")
print(summary(model2_ri)$coefficients)

# ---- 9. Save for appendix/reporting ----
write.csv(percent_df, "outputs/results/invalids_by_task_level.csv", row.names = FALSE)
write.csv(reason_summary, "outputs/results/invalids_reason_summary.csv", row.names = FALSE)

# =====================================================================
# Interpretation:
# • Most exclusions stem from (1) cut-off outputs (token limit), (2) missing "Answer:" prefix,
#   and (3) multi-word responses.
# • No significant difference in invalid rate by task level or condition (all p > .05).
# • Invalid rate per level and per model documented for transparency (see Table D1.1).
# =====================================================================
