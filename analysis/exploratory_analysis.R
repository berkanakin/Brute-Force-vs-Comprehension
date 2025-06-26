df <- read.csv("/Users/berkanakin/Library/CloudStorage/OneDrive-Personal/Studium/Master/UvA Psychology (research) Master/Courses/Thesis/data analysis/preliminary/analysis_data_long/analysis_data_long_with_expected_output.csv")

# Does the reasoning at lvl 2 indicate that o3 collapsed or not?

df<- df[df$task_level==2 & df$model=="gpt_o3",]
df <- df[rowSums(!is.na(df)) > 0, ]

df$model_output[1]
df$model_output[2]
df$model_output[3]
df$model_output[4]
df$model_output[5]

# critical meta rule trial by model and task level

library(dplyr)
library(stringr)

# 1) Define the phrase/variants to detect
patterns <- c(
  "critical meta-rule trial",      # exact match
  "critical meta rule trial",      # without hyphens
  "critical meta-rule trial:[^ ]+",# with a colon and suffix
  "critical meta-rule trial \\d+",  # numbered trial
  "meta-rule",
  "meta rule",
  "vertical",
  "instead",
  "next trial",
  "track",
  "tracked",
  "instead of responding to the presented prompt",
  "form a meaningful sentence"
)
pattern_regex <- str_c(patterns, collapse = "|")

# 2) Filter to experimental condition, flag occurrences, and summarize
df_summary <- df %>%
  filter(condition_type == "experimental") %>%
  mutate(
    is_critical = str_detect(
      model_output,
      regex(pattern_regex, ignore_case = TRUE)
    )
  ) %>%
  group_by(model, task_level) %>%
  summarise(
    n_hits  = sum(is_critical),
    n_total = n(),
    prop    = n_hits / n_total,
    .groups = "drop"
  )

df_matches <- df %>%
  filter(condition_type == "experimental") %>%
  filter(str_detect(model_output, regex(pattern_regex, ignore_case = TRUE))) %>%
  dplyr::select(model, task_level, model_output)


# Print results
print(df_summary)
print(df_matches)
