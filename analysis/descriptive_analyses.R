
# 0 Load packages and data

setwd("/Users/berkanakin/Library/CloudStorage/OneDrive-Personal/Studium/Master/UvA Psychology (research) Master/Courses/Thesis/data analysis/preliminary/descriptive analyses")
data_long <- read.csv("/Users/berkanakin/Library/CloudStorage/OneDrive-Personal/Studium/Master/UvA Psychology (research) Master/Courses/Thesis/data analysis/preliminary/analysis_data_long/analysis_data_long.csv")

library(dplyr)      # data wrangling
library(tidyr)      # pivot_wider
library(ggplot2)    # plotting
library(papaja)     # theme_apa()
library(broom)      # for prop.test tidying (optional)
library(binom)

df <- data_long %>% 
  filter(valid,
         condition_type == "experimental",
         !is.na(task_level))

# 0 ADD the mapping vector
pretty_names <- c(
  claude_4_sonnet = "Claude Sonnet 4",
  deepseek_r1     = "Deepseek R1",
  deepseek_v3     = "Deepseek V3",
  gemini_2_0_flash= "Gemini 2.0 Flash",
  gpt_3_5_turbo   = "GPT-3.5 T",
  gpt_4_1         = "GPT-4.1",
  gpt_4o          = "GPT-4o",
  gpt_o3          = "o3",
  grok_3          = "Grok 3",
  llama_3_3       = "Llama 3.3",
  llama_4         = "Llama 4",
  deepseek_r1     = "Deepseek R1"
)

# 1 AFTER you load df, create a pretty-name column (or overwrite)
df <- df %>% 
  mutate(model = dplyr::recode(model, !!!pretty_names, .default = model))
# overwrites ‘model’ with nice labels

# ---- everything else (summaries, table_wide, sum_plot) stays the same ----


# Summarise accuracy by model and task level (mean and SE)

sum_tbl_sd <- df %>% 
  group_by(model, task_level) %>% 
  summarise(n          = n(),
            mean_acc   = mean(revised_accuracy),
            sd_acc     = sd(revised_accuracy),
            .groups = "drop")

# Mean and CI

sum_tbl_ci <- df %>% 
  group_by(model, task_level) %>% 
  summarise(n        = n(),
            mean_acc = mean(revised_accuracy),
            se       = sqrt(mean_acc * (1 - mean_acc) / n),        # Wald SE
            ci_low   = mean_acc - 1.96 * se,
            ci_high  = mean_acc + 1.96 * se,
            .groups = "drop")

sum_tbl_ci <- df %>% 
  group_by(model, task_level) %>% 
  summarise(
    n        = n(),
    mean_acc = mean(revised_accuracy),
    se       = sqrt(mean_acc * (1 - mean_acc) / n),       # Wald SE
    ci_low   = pmax(mean_acc - 1.96 * se, 0),             # clamp at 0
    ci_high  = pmin(mean_acc + 1.96 * se, 1),             # clamp at 1
    .groups = "drop"
  )




sum_tbl_ci <- df %>%
  group_by(model, task_level) %>%
  summarise(
    n   = n(),
    k   = sum(revised_accuracy),
    .groups = "drop"
  ) %>%
  bind_cols(
    binom::binom.wilson(.$k, .$n)[, c("mean", "lower", "upper")]
  ) %>%
  rename(mean_acc = mean, ci_low = lower, ci_high = upper)
##


## let me also add the control condition in the graph

df_c <- data_long[data_long$condition_type=="control",]
df_c <- df_c %>% 
  mutate(model = dplyr::recode(model, !!!pretty_names, .default = model))

control_summary <- df_c %>%
  filter(condition_type == "control", valid == TRUE) %>%  # ← now only valid trials
  group_by(model) %>%
  summarise(
    n = n(),                                # valid trials
    k = sum(revised_accuracy),             # correct valid trials
    .groups = "drop"
  ) %>%
  bind_cols(
    binom::binom.wilson(.$k, .$n)[, c("mean", "lower", "upper")]
  ) %>%
  rename(mean_acc = mean, ci_low = lower, ci_high = upper) %>%
  mutate(condition = "control")  %>%
  mutate(task_level = 0)

sum_tbl_ci <- sum_tbl_ci %>% mutate(condition = "self-referential")


##merge both
sum_plot <- bind_rows(
  sum_tbl_ci,
  control_summary
)

sum_plot$task_level <- factor(
  sum_plot$task_level,
  levels = c("Control", "0", "1", "2"),
  labels = c("Control", "Level 0", "Level 1", "Level 2")
)






  




# Build APA table

# Choose one of the summaries above:
table_wide <- sum_tbl_sd %>%   # or sum_tbl_ci
  mutate(level_label = paste0("Level ", task_level),
         cell_text   = sprintf("%.2f (%.2f)", mean_acc, sd_acc))  # mean (SD)
# For CI instead:
# cell_text = sprintf("%.2f [%.2f–%.2f]", mean_acc, ci_low, ci_high)

table_wide <- table_wide %>% 
  select(model, level_label, cell_text) %>% 
  pivot_wider(names_from = level_label, values_from = cell_text) %>% 
  arrange(model)

table_wide_ci <- sum_tbl_ci %>% 
  # 1. Build the cell text → “.83 [.71–.93]”
  mutate(
    ci_text = sprintf("%.2f [%.2f–%.2f]", mean_acc, ci_low, ci_high),
    level_label = paste0("Level ", task_level)
  ) %>% 
  # 2. Keep only the columns we need
  select(model, level_label, ci_text) %>% 
  # 3. Pivot to wide: rows = models, cols = Level 0 / 1 / 2
  pivot_wider(
    names_from  = level_label,
    values_from = ci_text
  )


# ── packages ────────────────────────────────────────────────────────────
install.packages(c("officer", "flextable"))      # run once if needed
library(officer)
library(flextable)

# ── 1.  convert the data.frame to a flextable ───────────────────────────
ft <- flextable(table_wide_ci)

# Optional APA-ish styling
ft <- theme_booktabs(ft)                         # clean horizontal rules
ft <- fontsize(ft, size = 12, part = "all")      # Times size 12
ft <- set_table_properties(ft, width = .95, layout = "autofit")
ft <- align(ft, align = "center", part = "all")  # centre numbers
ft <- autofit(ft)                                # shrink to content

# ── 2.  write it into a new Word file ───────────────────────────────────
doc <- read_docx()                               # start a blank document
doc <- body_add_par(doc, "Table 1", style = "heading 1")  # table title
doc <- body_add_flextable(doc, ft)               # the actual table
doc <- body_add_par(doc,
                    "Note. Values are mean proportion correct with 95 % Wilson CIs.",
                    style = "Normal")

print(doc, target = "model_accuracy_table.docx") # <- saved in your WD

#### just for control condition another table


# ── PACKAGES ──────────────────────────────────────────────────────────────
library(dplyr)
library(binom)
library(officer)
library(flextable)

# ── 0. Start from your filtered long data ─────────────────────────────────
df <- data_long %>% 
  filter(valid, !is.na(task_level))

# ── 1. Summary helper: returns Wilson CI per model × level ───────────────
summarise_cond <- function(.data) {
  .data %>%
    group_by(model, task_level) %>%
    summarise(
      n   = n(),
      k   = sum(revised_accuracy),
      .groups = "drop"
    ) %>%
    bind_cols(
      binom::binom.wilson(.$k, .$n)[, c("mean", "lower", "upper")]
    ) %>%
    rename(
      mean_acc = mean,
      ci_low   = lower,
      ci_high  = upper
    )
}

# ── 2. Get control-only summary ───────────────────────────────────────────
ctrl_tbl <- df %>%
  filter(condition_type == "control") %>%
  summarise_cond() %>%
  arrange(model, task_level)

# ── 3. Format for APA-style table (mean and CI in same cell) ──────────────
formatted_tbl <- ctrl_tbl %>%
  mutate(
    `Accuracy (95% CI)` = sprintf("%.2f (%.2f–%.2f)", mean_acc, ci_low, ci_high)
  ) %>%
  select(Model = model, `Task Level` = task_level, `Accuracy (95% CI)`) %>%
  group_by(Model) %>%
  slice_head(n = 10) %>%         # keep only first 10 rows per model
  ungroup()

# ── 4. Export to Word using flextable ─────────────────────────────────────
ft <- flextable(formatted_tbl) %>%
  autofit() %>%
  set_table_properties(layout = "fixed")

doc <- read_docx() %>%
  body_add_par("Appendix B – Control Condition Accuracy", style = "heading 1") %>%
  body_add_flextable(ft)

print(doc, target = "Control_Accuracy_Table.docx")























# APA Themed bar plot

#  First make sure task_level is an ordered factor
sum_plot <- sum_tbl_ci  

sum_plot$task_level <- factor(sum_plot$task_level,
                              levels = c(0, 1, 2),
                              labels = c("Level 0", "Level 1", "Level 2"))

# ── add a label column ───────────────────────────────────────────
sum_plot <- sum_plot |>
  mutate(label_n =n)

# ── the plot ─────────────────────────────────────────────────────
ggplot(sum_plot,
       aes(x = model,
           y = mean_acc,
           fill = task_level)) +
  geom_col(position = position_dodge(width = .8),
           width    = .7) +
  geom_errorbar(aes(ymin = ci_low, ymax = ci_high),
                position = position_dodge(width = .8),
                width = .1,
                size = 0.2) +  # ← thinner error bars
  
  # ↓↓↓  sample-size labels BELOW the bars  ↓↓↓
  geom_text(aes(y = -0.04,
                label = n),
            position = position_dodge(width = .8),
            vjust = 1, size = 3,
            family = "Times New Roman") +
  
  scale_fill_manual(values = c(
    "Control" = "grey50",     # unsaturated grey
    "Level 0" = "#91CF60",    # green
    "Level 1" = "#F7D965",    # yellow
    "Level 2" = "#0E5E9E"     # blue
  ),
  name = "Task Level") +
  
  scale_y_continuous(
    limits = c(-0.10, 1.1),
    breaks = seq(0, 1, 0.1),  # ← finer gridlines
    expand = c(0, 0)
  ) +
  labs(x = "Model",
       y = "Accuracy (Proportion Correct)") +
  theme_apa() +
  theme(
    plot.margin = margin(t = 20, b = 20),
    legend.position   = "bottom",
    text              = element_text(family = "Times New Roman", size = 12),
    axis.text.x       = element_text(angle = 45, hjust = 1, margin = margin(t = 8)),
    legend.background = element_rect(fill = "white", colour = "black")
  ) +
  coord_cartesian(clip = "off")


## Valid trials table

library(dplyr)
library(tidyr)
library(dplyr)
library(tidyr)

# First filter out rows with NA task_level
validity_table <- data_long %>%
  filter(!is.na(task_level)) %>%   # Filter out NA task_levels
  group_by(model, condition_type, task_level) %>%
  summarise(
    Valid_Trials = sum(valid),
    Total_Trials = n(),
    Proportion_Valid = round((Valid_Trials / Total_Trials) * 100, 2),
    .groups = 'drop'
  ) %>%
  arrange(model, task_level, condition_type)

library(dplyr)
library(tidyr)
library(flextable)
library(officer)

# Filter and organize data
summary_table <- data_long %>%
  filter(!is.na(task_level)) %>%
  mutate(condition_label = case_when(
    condition_type == "control" ~ "Control (200)",
    condition_type == "experimental" & task_level == 0 ~ "Self-referential lvl0 (268)",
    condition_type == "experimental" & task_level == 1 ~ "Self-referential lvl1 (65)",
    condition_type == "experimental" & task_level == 2 ~ "Self-referential lvl2 (5)",
    TRUE ~ "Other"
  )) %>%
  filter(condition_label != "Other") %>%
  group_by(model, condition_label) %>%
  summarise(valid_count = sum(valid), .groups = "drop") %>%
  pivot_wider(names_from = condition_label, values_from = valid_count, values_fill = 0) %>%
  arrange(model)

# Compute percentages
final_table <- summary_table %>%
  mutate(
    `Control (200)` = sprintf("%d (%.1f%%)", `Control (200)`, (`Control (200)` / 200) * 100),
    `Self-referential lvl0 (268)` = sprintf("%d (%.1f%%)", `Self-referential lvl0 (268)`, (`Self-referential lvl0 (268)` / 268) * 100),
    `Self-referential lvl1 (65)` = sprintf("%d (%.1f%%)", `Self-referential lvl1 (65)`, (`Self-referential lvl1 (65)` / 65) * 100),
    `Self-referential lvl2 (5)` = sprintf("%d (%.1f%%)", `Self-referential lvl2 (5)`, (`Self-referential lvl2 (5)` / 5) * 100)
  )

# Create Flextable
ft <- flextable(final_table) %>%
  theme_vanilla() %>%
  autofit() %>%
  set_caption("Table X. Valid Trials per Model and Condition (Percentage)") %>%
  align(align = "center", part = "all") %>%
  bold(part = "header")

# Save as Word document
doc <- read_docx() %>%
  body_add_flextable(ft)

print(doc, target = "Valid_Trials_Summary.docx")

##
# Print invalid trials per your filtering criteria
invalid_trials <- data_long %>%
  filter(valid == FALSE, condition_type == "control" | !is.na(task_level))

invalid_trials$predicted_word[34]
invalid_trials$predicted_word[1]

## which ones don't end with a dot

num_without_dot <- sum(!grepl("\\.$", invalid_trials$predicted_word, perl = TRUE))

num_with_answer <- sum(grepl("(?i)\\banswer:", invalid_trials$predicted_word, perl = TRUE))


print(invalid_trials)

data_long[!is.na(data_long$task_level) & is.na(data_long$predicted_word),]
nrow(data_long[!is.na(data_long$task_level),])



### How much is the per model drop in self-ref vs control

# get unique model names
models <- unique(data_long$model)

# for each model, compute control mean minus experimental@lvl0 mean
per_model_drop <- sapply(models, function(m) {
  mean(data_long$revised_accuracy[
    data_long$model == m & data_long$condition_type == "control"
  ], na.rm=T) -
    mean(data_long$revised_accuracy[
      data_long$model == m &
        data_long$condition_type == "experimental" &
        data_long$task_level == 0
    ], na.rm=T)
})

# result is a named numeric vector
per_model_drop

# per model absolute performance

per_model_drop <- sapply(models, function(m) {
  mean(data_long$revised_accuracy[
    data_long$model == m & data_long$condition_type == "control"
  ], na.rm=T) -
    mean(data_long$revised_accuracy[
      data_long$model == m &
        data_long$condition_type == "experimental" &
        data_long$task_level == 0
    ], na.rm=T)
})











##### Output length

df <- read.csv("/Users/berkanakin/Library/CloudStorage/OneDrive-Personal/Studium/Master/UvA Psychology (research) Master/Courses/Thesis/data analysis/preliminary/analysis_data_long/analysis_data_long_all_with_output.csv")


## ── 0.  packages ──────────────────────────────────────────────────────────
library(dplyr)
library(tidyr)
library(reticulate)      # interface to Python
tiktoken <- import("tiktoken")        # needs `pip install tiktoken`

## ── 1.  GPT-4 tokenizer (cl100k_base) ─────────────────────────────────────
enc <- tiktoken$encoding_for_model("gpt-4")   # returns a tokenizer object

token_len <- function(txt) length(enc$encode(txt))

## ── 2.  add token counts to df ────────────────────────────────────────────
df <- df %>%
  mutate(output_tokens = vapply(model_output, token_len, integer(1)))

## ── 3.  build a 4-level “condition” factor ────────────────────────────────
df <- df %>%
  mutate(condition = case_when(
    condition_type == "control" ~ "Control",
    condition_type == "experimental" & task_level == 0 ~ "SR-L0",
    condition_type == "experimental" & task_level == 1 ~ "SR-L1",
    condition_type == "experimental" & task_level == 2 ~ "SR-L2",
    TRUE ~ NA_character_
  )) %>%
  filter(!is.na(condition), valid)

df$condition <- factor(df$condition,
                       levels = c("Control", "SR-L0", "SR-L1", "SR-L2"))

## ── 4.  summary: mean ± SD token length per model × condition ────────────
summary_tbl <- df %>%
  group_by(model, condition) %>%
  dplyr::summarise(
    n       = dplyr::n(),
    mean_tk = mean(output_tokens),
    sd_tk   = sd(output_tokens),
    .groups = "drop"
  ) %>%
  arrange(model, condition)

## ── 5.  (optional) inspect / save ─────────────────────────────────────────
print(summary_tbl, n = Inf)

# If you’ll plot a bar-liner later, leave it long; otherwise:
# wide_tbl <- pivot_wider(summary_tbl,
#                         names_from  = condition,
#                         values_from = c(mean_tk, sd_tk))

# write.csv(summary_tbl, "model_output_length_summary.csv", row.names = FALSE)


## ── assumes `summary_tbl` from previous step ─────────────────────────────
## columns: model • condition • n • mean_tk • sd_tk

library(ggplot2)
library(papaja)      # theme_apa()
library(dplyr)

## 0 ── pretty model names (same vector you used before) ───────────────────
pretty_names <- c(
  claude_4_sonnet = "Claude Sonnet 4",
  deepseek_v3     = "Deepseek V3",
  deepseek_r1     = "Deepseek R1",
  gemini_2_0_flash= "Gemini 2.0 Flash",
  gpt_3_5_turbo   = "GPT-3.5 T",
  gpt_4_1         = "GPT-4.1",
  gpt_4o          = "GPT-4o",
  gpt_o3          = "o3",
  grok_3          = "Grok 3",
  llama_3_3       = "Llama 3.3",
  llama_4         = "Llama 4"
)

plot_df <- summary_tbl %>%
  mutate(
    model     = plyr::revalue(model, pretty_names),   # human labels
    se        = sd_tk / sqrt(n),                     # SEM
    ci_low    = pmax(mean_tk - 1.96 * se, 0),        # 95 % CI
    ci_high   = mean_tk + 1.96 * se,
    label_n   = n                                    # for printing below bars
  )

## 1 ── colour palette: grey for control, same G-Y-B for SR levels ─────────
pal <- c(
  "Control" = "grey60",
  "SR-L0"   = "#7EC67B",   # soft green
  "SR-L1"   = "#F7D965",   # soft yellow
  "SR-L2"   = "#0E5E9E"    # steel-blue
)

## 2 ── APA-style bar plot ─────────────────────────────────────────────────
ggplot(plot_df,
       aes(x = model, y = mean_tk, fill = condition)) +
  
  geom_col(position = position_dodge(width = .8), width = .7) +
  
  geom_errorbar(aes(ymin = ci_low, ymax = ci_high),
                position = position_dodge(width = .8),
                width = .15, size = .3) +
  
  # sample-size labels BELOW bars
  geom_text(aes(y = -5, label = label_n),              # adjust −5 if needed
            position = position_dodge(width = .8),
            vjust = 1, size = 3,
            family = "Times New Roman") +
  
  scale_fill_manual(name = "Condition", values = pal) +
  
  scale_y_continuous(expand = c(0, 0),
                     limits = c(-10,
                                max(plot_df$ci_high) * 1.1)) +
  
  labs(x = "Model",
       y = "Mean Output Length (Tokens)") +
  
  theme_apa() +
  theme(
    legend.position   = "bottom",
    text              = element_text(family = "Times New Roman", size = 12),
    axis.text.x       = element_text(angle = 45, hjust = 1,
                                     margin = margin(t = 8)),
    panel.grid.major.y = element_line(colour = "grey88", linewidth = .3),
    panel.grid.minor   = element_blank(),
    plot.margin        = margin(t = 20, b = 20)
  ) +
  
  coord_cartesian(clip = "off")

as.data_frame(pretty_names)

p <- as_data_frame(pretty_names)
p[,1]





















