df <- read.csv("/Users/berkanakin/Library/CloudStorage/OneDrive-Personal/Studium/Master/UvA Psychology (research) Master/Courses/Thesis/data analysis/preliminary/analysis_data_long/analysis_data_long_with_expected_output.csv")
metadata <-read.csv("/Users/berkanakin/Library/CloudStorage/OneDrive-Personal/Studium/Master/UvA Psychology (research) Master/Courses/Thesis/data analysis/preliminary/metadata/llm_itemset_metadata.csv")

df <- read.csv("https://raw.githubusercontent.com/berkanakin/Brute-Force-vs-Comprehension/refs/heads/main/data/analysis_data/analysis_data_long_all_with_output.csv?token=GHSAT0AAAAAADGK6RDI4DXYNNTBSJKYOH322DKLXIA")

# Does the reasoning at lvl 2 indicate that o3 collapsed or not?

df<- df[df$task_level==2 & df$model=="gpt_o3",]
df <- df[rowSums(!is.na(df)) > 0, ]

df$model_output[1]
df$model_output[2]
df$model_output[3]
df$model_output[4]
df$model_output[5]

df_o3 <- merge(df, metadata, by = "trial")

df_o3$lvl2_id[df_o3$predicted_word ==df_o3$lvl1_solution]
df_o3$model_output[df_o3$predicted_word ==df_o3$lvl1_solution]
df_o3$lvl2_solution[4]

df_o3$solution
df_o3$predicted_word
df_o3$revised_accuracy
df_o3$lvl1_solution

df_o3$lvl2_id[df_o3$lvl1_id ==df_o3$lvl2_id]



############################################################
## Power analysis for a one-sample (exact) binomial test  ##
## H0: p = 1/14  vs.  H1: p = 0.20  (one-sided, α = .05)  ##
############################################################

## install & load helper package (for the asymptotic method)
if (!requireNamespace("pwr", quietly = TRUE)) {
  install.packages("pwr")
}
library(pwr)

## --- Asymptotic (Cohen’s h) solution ---------------------
p_null <- 1/14          # chance level
p_alt  <- 0.20          # observed accuracy
alpha  <- 0.05          # test size
target_power <- 0.80    # desired power

# Cohen’s h for proportions
h <- 2 * asin(sqrt(p_alt)) - 2 * asin(sqrt(p_null))

# Solve for n (one-sample, one-sided)
ss <- pwr.p.test(h = h,
                 sig.level = alpha,
                 power = target_power,
                 alternative = "greater")

ceiling(ss$n)           # → 37




# Question2  critical meta rule trial by model and task level

#Descriptively


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

#  "This is a critical meta-rule trial"



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

df_summary <- df_summary[!is.na(df_summary$task_level),]

str(df_summary)

library(dplyr)
library(ggplot2)

# 0. Add pretty model names
pretty_names <- c(
  claude_4_sonnet  = "Claude Sonnet 4",
  deepseek_r1      = "Deepseek R1",
  deepseek_v3      = "Deepseek V3",
  gemini_2_0_flash = "Gemini 2.0 Flash",
  gpt_3_5_turbo    = "GPT-3.5 T",
  gpt_4_1          = "GPT-4.1",
  gpt_4o           = "GPT-4o",
  gpt_o3           = "OpenAI o3",
  grok_3           = "Grok 3",
  llama_3_3        = "Llama 3.3",
  llama_4          = "Llama 4"
)

df_summary <- df_summary %>%
  mutate(model = recode(model, !!!pretty_names))

# 1. Compute SE and CI
df_summary <- df_summary %>%
  mutate(se     = sqrt(prop * (1 - prop) / n_total),
         ci_low = prop - 1.96 * se,
         ci_high= prop + 1.96 * se)

# 2. Factor levels for task_level
df_summary$task_level <- factor(df_summary$task_level,
                                levels = c(0, 1, 2),
                                labels = c("Level 0", "Level 1", "Level 2"))

# 3. Plot
# Load required packages
library(ggplot2)
library(dplyr)

ggplot(df_summary, aes(x = model, y = prop, fill = task_level)) +
  geom_col(position = position_dodge(width = .8), width = .7) +
  
  geom_errorbar(aes(ymin = ci_low, ymax = ci_high),
                position = position_dodge(width = .8),
                width = .1,
                linewidth = 0.2) +
  
  geom_text(aes(y = -0.04, label = n_total),
            position = position_dodge(width = .8),
            vjust = 1, size = 3,
            family = "Times New Roman") +
  
  scale_fill_manual(values = c(
    "Level 0" = "#91CF60",
    "Level 1" = "#F7D965",
    "Level 2" = "#0E5E9E"
  ),
  name = "Task Level") +
  
  scale_y_continuous(
    limits = c(-0.10, 1.1),
    breaks = seq(0, 1, 0.1),
    expand = c(0, 0)
  ) +
  
  labs(x = "Model",
       y = "Proportion of Outputs With All Keywords") +
  
  # FULL FONT CONTROL — Times New Roman everywhere
  theme_apa() +
  theme(
    text              = element_text(family = "Times New Roman", size = 11),
    axis.text.x       = element_text(family = "Times New Roman", size = 11, angle = 45, hjust = 1, margin = margin(t = 8)),
    axis.text.y       = element_text(family = "Times New Roman", size = 11),
    axis.title.x      = element_text(family = "Times New Roman", size = 11),
    axis.title.y      = element_text(family = "Times New Roman", size = 11),
    legend.text       = element_text(family = "Times New Roman", size = 11),
    legend.title      = element_text(family = "Times New Roman", size = 11),
    legend.position   = "bottom",
    legend.background = element_rect(fill = "white", colour = "black"),
    strip.text        = element_text(family = "Times New Roman", size = 11),
    plot.margin       = margin(t = 20, b = 20)
  ) +
  
  coord_cartesian(clip = "off")



##### alternative: look how many of the ouputs per level include all of the words that are part of the correct to be completed proimpt which is stored in the variable prompt_key


df <- merge(df, metadata, by = "trial")
df<- df[df$valid==T & df$condition_type=="experimental",]

str(df)


library(dplyr)
library(stringr)
library(purrr)

build_all_words_regex <- function(x) {
  words <- str_extract_all(x, boundary("word"))[[1]]
  words <- words[words != ""]
  str_c(map(words, ~ str_c("(?=.*\\b", .x, "\\b)")), collapse = "")
}

df_prompt_hits <- df %>%
  mutate(
    prompt_regex = map_chr(prompt_key, build_all_words_regex),
    has_all_prompt_words = map2_lgl(
      model_output,
      prompt_regex,
      ~ str_detect(.x, regex(.y, ignore_case = TRUE))   # ← no perl =
    )
  ) %>%
  group_by(model, task_level) %>%
  summarise(
    n_hits  = sum(has_all_prompt_words, na.rm = TRUE),
    n_total = n(),
    prop    = n_hits / n_total,
    .groups = "drop"
  )

df_prompt_hits <- df_prompt_hits[!is.na(df_prompt_hits$task_level),]
df_summary <- df_prompt_hits

df_summary <- df_summary %>%
  mutate(model = recode(model, !!!pretty_names))

# 1. Compute SE and CI
df_summary <- df_summary %>%
  mutate(se     = sqrt(prop * (1 - prop) / n_total),
         ci_low = prop - 1.96 * se,
         ci_high= prop + 1.96 * se)

# 2. Factor levels for task_level
df_summary$task_level <- factor(df_summary$task_level,
                                levels = c(0, 1, 2),
                                labels = c("Level 0", "Level 1", "Level 2"))

# 2. Factor levels for task_level
df_summary <- df_summary[!df_summary$task_level=="Level 2",]

###### add trial level variable for hits

df <- df %>%
  filter(valid == TRUE, condition_type == "experimental") %>%
  mutate(
    prompt_regex = map_chr(prompt_key, build_all_words_regex),
    prompt_hit_logical = map2_lgl(
      model_output,
      prompt_regex,
      ~ tryCatch(
        str_detect(.x, regex(.y, ignore_case = TRUE)),
        error = function(e) FALSE  # fallback to FALSE if something goes wrong
      )
    ),
    prompt_hit = as.integer(prompt_hit_logical)
  ) %>%
  select(-prompt_regex, -prompt_hit_logical)

## subset

df <- df[!df$task_level==2,]
df <- df[!is.na(df$task_level),]

### Mediation analysis

library(tidyverse)
library(lme4)          # GLMMs
library(lmerTest)      # p-values for lme4
library(emmeans)       # contrasts & trends
library(performance)   # diagnostics
library(broom.mixed)   # tidy model summaries
library(car)           # Anova() type-III
library(dplyr)
library(purrr)


m_H2_unmediated <- glmer(
  revised_accuracy ~ task_level +
    (1 + task_level| model) +
    (1 | item_id.y),
  family = binomial,
  data   = df,
  control = glmerControl(optimizer = "bobyqa")
)

m_H2_mediated <- glmer(
  revised_accuracy ~ task_level + prompt_hit +
    (1 + task_level| model) +
    (1 | item_id.y),
  family = binomial,
  data   = df,
  control = glmerControl(optimizer = "bobyqa")
)




summary(m_H2_unmediated)
summary(m_H2_mediated)


##
library(dplyr)

df_sem <- df %>%
  filter(task_level %in% c(0, 1)) %>%
  mutate(
    task_level = as.numeric(task_level == 1),     # 1 = Level 1, 0 = Level 0
    prompt_hit = as.numeric(prompt_hit == 1),
    revised_accuracy = as.numeric(revised_accuracy == 1)
    
  )

library(lavaan)

model <- '
  # Mediation structure
  prompt_hit        ~ a * task_level
  revised_accuracy  ~ b * prompt_hit + c_prime * task_level

  # Indirect and total effects
  indirect := a * b
  total    := c_prime + (a * b)
'

fit <- sem(model, data = df_sem,
           ordered = c("prompt_hit", "revised_accuracy"),
           estimator = "WLSMV")   # robust estimator for categorical outcomes

summary(fit, standardized = TRUE, fit.measures = TRUE, rsquare = TRUE)





