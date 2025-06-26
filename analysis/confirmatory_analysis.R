# ──────────────────────────────────────────────────────────────────────────────
#   Load data
# ──────────────────────────────────────────────────────────────────────────────

setwd("/Users/berkanakin/Library/CloudStorage/OneDrive-Personal/Studium/Master/UvA Psychology (research) Master/Courses/Thesis/data analysis/preliminary/confirmatory_analysis")
data_long <- read.csv("/Users/berkanakin/Library/CloudStorage/OneDrive-Personal/Studium/Master/UvA Psychology (research) Master/Courses/Thesis/data analysis/preliminary/analysis_data_long/analysis_data_long.csv")


# ──────────────────────────────────────────────────────────────────────────────
#   Libraries & helpers
# ──────────────────────────────────────────────────────────────────────────────
library(tidyverse)
library(lme4)          # GLMMs
library(lmerTest)      # p-values for lme4
library(emmeans)       # contrasts & trends
library(performance)   # diagnostics
library(broom.mixed)   # tidy model summaries
library(car)           # Anova() type-III
library(dplyr)
library(purrr)
# OPTIONAL:
# library(simr)        # power via simulation
# library(segmented)   # break-point models

# Turn 'task_level' into an ordered factor
# (to request linear / polynomial contrasts without re-coding)
data_long <- data_long %>% 
  mutate(task_level = factor(task_level, levels = 0:2, ordered = TRUE))

## turn covariates into proper scale

data_long <- data_long %>%                   
  mutate(
    # trial-wise baseline guessing probability (0-1) → logit scale
    logit_p = qlogis(chance_prob),
    logit_p_z = scale(qlogis(chance_prob)),
    # numeric task level (0,1,2) for trend tests
    task_level_num = as.numeric(as.character(task_level)),
    # unordered factor for dummy coding in the main GLMM
    task_level     = relevel(factor(task_level_num), ref = "0"),
    input_tokens_z = scale(input_tokens)
  )

## Names

pretty_names <- c(
  claude_4_sonnet = "Claude Sonnet 4",
  deepseek_v3     = "Deepseek V3",
  deepseek_r1     = "Deepseek R1",
  gemini_2_0_flash= "Gemini 2.0 Flash",
  gpt_3_5_turbo   = "GPT-3.5 T",
  gpt_4_1         = "GPT-4.1",
  gpt_4o          = "GPT-4o",
  grok_3          = "Grok 3",
  llama_3_3       = "Llama 3.3",
  llama_4         = "Llama 4",
  gpt_o3          = "o3"
)

# ──────────────────────────────────────────────────────────────────────────────
#   Hypothesis 1  – Level-0 only
# ──────────────────────────────────────────────────────────────────────────────
h1_data <- data_long %>% 
  filter(task_level == 0, valid, condition_type %in% c("control","experimental"))

# Use this formula as default
h1_data$model <- factor(h1_data$model)

h1_formula <- accuracy ~ condition_type + (1 | model)
h1_formula <- revised_accuracy ~ condition_type + scale(input_tokens) + (1 | model)

h1_formula <- revised_accuracy ~ condition_type +
  scale(input_tokens) +
  (1 + condition_type | model)  # random intercept *and* slope

h1_formula <- revised_accuracy ~ condition_type +
  (1 + condition_type | model) 


m_h1 <- glmer(h1_formula, family = binomial, data = h1_data,
            control = glmerControl(optimizer = "bobyqa"))

summary(m_h1)
anova(m1, test = "Chisq")           # likelihood-ratio test
emmeans(m1, pairwise ~ condition_type, adjust = "bonferroni")



## alternative way of looking at things-----------------------------------------

h1_formula <- revised_accuracy ~ condition_type * model
#or even
h1_formula <- revised_accuracy ~ condition_type * model + offset(logit_p)

m_h1_fixed <- glm(h1_formula, family = binomial, data = h1_data)
summary(m_h1_fixed)

## Predicted accuracy (probability scale) for every model × condition
emm <- emmeans(m_h1_fixed,
               ~ condition_type | model,     # “by model” table
               type = "response")            # = probabilities
emm

## Pairwise self-referential cost within each model
pairs(emm, by = "model", adjust = "bonferroni")

## Grand-mean self-referential cost, averaging over models
emm_overall <- emmeans(m_h1_fixed, ~ condition_type, type = "response")
contrast(emm_overall, method = "revpairwise")

emm_df <- as.data.frame(emm)

# identify whichever column names contain “LCL” and “UCL”
lcl_name <- grep("LCL", names(emm_df), value = TRUE)
ucl_name <- grep("UCL", names(emm_df), value = TRUE)

emm_df <- emm_df |>
  rename(
    lower = all_of(lcl_name),
    upper = all_of(ucl_name)
  )

emm_df <- emm_df |>
  mutate(
    model = factor(model,
                   levels = names(pretty_names),
                   labels = unname(pretty_names)),
    condition_type = factor(condition_type,
                            levels = c("control", "experimental"),
                            labels = c("Control", "Self-ref."))
  )

##insert emm_df down there for plot_df

# ─────────────────────────────────────────────────────────────
# Individual models
# ─────────────────────────────────────────────────────────────
# 1. Simulate posterior draws for random effects
# ─────────────────────────────────────────────────────────────
set.seed(123)
sims <- sim(m_h1, n.sims = 10000)

model_names <- rownames(coef(m_h1)$model)

# Extract posterior draws: fixed + model-specific slope
slope_draws <- sapply(model_names, function(mod) {
  fixef(sims)[, "condition_typeexperimental"] + ranef(sims)$model[, mod, "condition_typeexperimental"]
})

# ─────────────────────────────────────────────────────────────
# 2. Summarize: mean, SD, 95% CI, pseudo p-value
# ─────────────────────────────────────────────────────────────
slopes_summary <- t(apply(slope_draws, 2, function(x) {
  c(Beta = mean(x),
    SE = sd(x),
    CI_lower = quantile(x, 0.025),
    CI_upper = quantile(x, 0.975),
    p_pseudo = 2 * min(mean(x < 0), mean(x > 0)))
}))

slopes_df_h1 <- as.data.frame(slopes_summary) %>%
  rownames_to_column(var = "Model") %>%
  mutate(across(where(is.numeric), ~ round(.x, 3)))
print(slopes_df_h1)

## Visualise results

library(merTools)
library(dplyr)


newdat <- expand.grid(
  model          = levels(h1_data$model),        # every model
  condition_type = c("control", "experimental")
)

set.seed(123)
pi <- predictInterval(
  m_h1,
  newdata      = newdat,
  n.sims       = 1000,
  level        = 0.95,
  type         = "probability",   # returns on the 0–1 scale
  include.resid.var = FALSE       # marginal over residual variance
)

plot_df <- cbind(newdat, pi) |>          # fit = mean, lwr, upr = CI
  rename(prob = fit, lower = lwr, upper = upr)

##
## 1.  Replace the machine IDs with publication-ready names
##

# mapping: old factor level  ->  new printed name
pretty_names <- c(
  claude_4_sonnet = "Claude Sonnet 4",
  deepseek_v3     = "Deepseek V3",
  gemini_2_0_flash= "Gemini 2.0 Flash",
  gpt_3_5_turbo   = "GPT 3.5 T",
  gpt_4_1         = "GPT 4.1",
  gpt_4o          = "GPT 4o",
  gpt_o3          = "o3",
  grok_3          = "Grok 3",
  llama_3_3       = "Llama 3.3",
  llama_4         = "Llama 4"
)

plot_df <- plot_df |>
  mutate(model = factor(model,
                        levels = names(pretty_names),      # keep *this* order
                        labels = unname(pretty_names)))

##
## 2.  Make an APA-style plot
##

library(ggplot2)
library(jtools) 
install.packages("jtools") # for theme_apa()

apa_plot <- ggplot(
  emm_df,
  aes(x     = model,
      y     = prob,                     # point prediction
      shape = condition_type,
      linetype = condition_type,
      group = condition_type)
) +
  geom_point(size = 2.5, position = position_dodge(width = .5)) +
  geom_errorbar(
    aes(ymin = lower, ymax = upper),
    width     = .15,
    position  = position_dodge(width = .5)
  ) +
  scale_y_continuous(
    limits = c(0, 1),
    breaks = seq(0, 1, .2),
    labels = scales::percent_format(accuracy = 1)
  ) +
  scale_shape_manual(values = c(16, 17)) +   # filled circle / triangle
  scale_linetype_manual(values = c("solid", "solid")) +
  scale_colour_grey(start = .2, end = .6, guide = "none") +  # greys only
  labs(
    x = NULL,
    y = "Predicted Accuracy (± 95 % CI)",
    shape    = "Condition",
    linetype = "Condition"
  ) +
  theme_apa(legend.pos = "bottom") +
  theme(
    text = element_text(family = "Times New Roman", size = 12),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.background = element_rect(fill = "white", colour = "black")
  )

# display
print(apa_plot)

# save at APA-friendly size & resolution
ggsave("apa_marginal_means_per_model.png",
       plot = apa_plot, width = 6.5, height = 4, dpi = 300)



# ──────────────────────────────────────────────────────────────────────────────
#  H2a  •  Do meta-rules reduce accuracy after controlling for
#        trial-specific chance accuracy?
#  Data frame: h2_data   (one row per model × trial)
# ──────────────────────────────────────────────────────────────────────────────

h2_data <- data_long %>% 
  filter(valid,
         condition_type == "experimental",
         !is.na(task_level))


## 1.1  Prepare predictors -----------------------------------

h2_data <- h2_data %>%                   
  mutate(
    # trial-wise baseline guessing probability (0-1) → logit scale
    logit_p = qlogis(chance_prob),
    logit_p_z = scale(qlogis(chance_prob)),
    # numeric task level (0,1,2) for trend tests
    task_level_num = as.numeric(as.character(task_level)),
    # unordered factor for dummy coding in the main GLMM
    task_level     = relevel(factor(task_level_num), ref = "0"),
    input_tokens_z = scale(h2_data$input_tokens)
  )

# 1.2 Model Comparisons

# Model Comparisons

# Model 1: Random intercepts only
m_intercept <- glmer(
  revised_accuracy ~ task_level_num + logit_p + (1 | model) + (1 | trial),
  family = binomial,
  data = h2_data,
  control = glmerControl(optimizer = "bobyqa")
)

# Model 2: Random slopes by model
m_slope <- glmer(
  revised_accuracy ~ task_level_num + logit_p + (1 + task_level_num | model) + (1 | trial),
  family = binomial,
  data = h2_data,
  control = glmerControl(optimizer = "bobyqa")
)

summary(m_slope)

# Likelihood-ratio test (nested model comparison)
anova(m_intercept, m_slope)


## 1.3  Main GLMM (random intercepts  for model & trial, and random slopes for model by task level) ------

m_overall <- glmer(
  revised_accuracy ~ task_level + logit_p + (1 + task_level_num | model) + (1 | trial),
  family = binomial,
  data = h2_data,
  control = glmerControl(optimizer = "bobyqa")
)

summary(m_overall)

##1.3.1 Main GLMM (controlling for input_tokens and trial)

h2_data$input_tokens_z <- scale(h2_data$input_tokens)
h2_data$logit_p_z <- scale(h2_data$logit_p)

m_overall2 <- glmer(
  revised_accuracy ~ task_level_num + logit_p_z + input_tokens_z +
    (1 + task_level_num | model) +
    (1 | trial),
  family = binomial,
  data   = h2_data,
  control = glmerControl(optimizer = "bobyqa")
)

summary(m_overall2)

##1.3.1 Main GLMM (controlling for input_tokens and item_id instead of trial)

h2_data$input_tokens_z <- scale(h2_data$input_tokens)
h2_data$logit_p_z <- scale(h2_data$logit_p)

m_overall2 <- glmer(
  revised_accuracy ~ task_level_num + logit_p_z + # + input_tokens_z
    (1 + task_level_num | model) +
    (1 | item_id),
  family = binomial,
  data   = h2_data,
  control = glmerControl(optimizer = "bobyqa")
)

summary(m_overall2)

## Per LLM estimates

# Unpooled model with interaction: accuracy ~ task_level * model
m_H2a_unpooled <- glmer(
  revised_accuracy ~ task_level * model + logit_p_z +
    (1 | item_id),
  family = binomial,
  data   = h2_data,
  control = glmerControl(optimizer = "bobyqa")
)

# Unpooled model with interaction: accuracy ~ task_level * model
m_H2a_unpooled <- glm(
  revised_accuracy ~ task_level * model + logit_p_z,
  family = binomial,
  data   = h2_data
)


library(emmeans)

# Marginal means with 95% CIs
emm_results <- emmeans(m_H2a_unpooled, ~ model * task_level, type = "response")
emm_df <- as.data.frame(emm_results)




library(ggplot2)
library(papaja)
library(dplyr)
library(plyr)

emm_df$model <- revalue(emm_df$model, pretty_names)


# Format task level
emm_df$task_level <- factor(emm_df$task_level,
                            levels = c(0, 1, 2),
                            labels = c("Level 0", "Level 1", "Level 2"))


emm_df$asymp.UCL[emm_df$asymp.UCL>0.999] <- 0


#####


## ── 1.  Get cell Ns with the right filters ────────────────────────────────
n_tbl <- h2_data %>%
  filter(condition_type == "experimental", valid) %>%
  mutate(task_level = factor(task_level,
                             levels = c(0, 1, 2),
                             labels = c("Level 0", "Level 1", "Level 2"))) %>%
  group_by(model, task_level) %>%
  dplyr::summarise(label_n = n(), .groups = "drop")


library(plyr)  # must be loaded

n_tbl$model <- revalue(n_tbl$model, pretty_names)

summary(m_overall2)
## ── 2.  Join those Ns onto the emmeans table ──────────────────────────────
emm_df <- emm_df %>% 
  left_join(n_tbl, by = c("model", "task_level"))



delta_df <- emm_df %>%
  select(model, task_level, prob) %>%
  pivot_wider(names_from = task_level, values_from = prob) %>%
  mutate(delta_L1_L0 = `Level 1` - `Level 0`) %>%
  select(model, delta_L1_L0)





## ── 2. set plotting aesthetics & palette ─────────────────────────────────────
palette_lvl <- c(
  "Level 0" = "#7EC67B",  # soft green
  "Level 1" = "#F7D965",  # soft yellow
  "Level 2" = "#0E5E9E"   # steel-blue
)

ggplot(emm_df,
       aes(x = model, y = prob, fill = task_level)) +
  
  ## bars
  geom_col(position = position_dodge(width = .8), width = .7) +
  
  ## error bars (thin)
  geom_errorbar(aes(ymin = asymp.LCL, ymax = asymp.UCL),
                position = position_dodge(width = .8),
                width = .15, size = .3) +
  
  ## sample-size labels *below* the baseline
  geom_text(aes(y = -0.04, label = label_n),
            position = position_dodge(width = .8),
            vjust = 1, size = 3,
            family = "Times New Roman") +
  
  ## colour scale
  scale_fill_manual(name = "Task Level", values = palette_lvl) +
  
  ## chance line (red & thin)
  geom_hline(yintercept = 1/14,
             linetype   = "dashed",
             colour     = "red",
             size       = .4) +
  
  ## y-axis every 10 %
  scale_y_continuous(breaks = seq(0, 1, .1),
                     limits = c(-.10, 1.1),
                     expand = c(0, 0)) +
  
  labs(x = "Model", y = "Predicted Accuracy") +
  
  theme_apa() +
  theme(
    legend.position = "bottom",
    text            = element_text(family = "Times New Roman", size = 12),
    axis.text.x     = element_text(angle = 45, hjust = 1,
                                   margin = margin(t = 8)),
    plot.margin     = margin(t = 20, b = 20)
  ) +
  
  coord_cartesian(clip = "off")

###Pairwise comparisons

## 2 ── estimated marginal means per model × level (probability scale) ─────
emm_by_model <- emmeans(
  m_H2a_unpooled,
  ~ task_level | model,          # “task_level within each model”
  type = "response"              # back-transform ⇒ probabilities
)

## 3 ── pair-wise comparisons between levels, *within* each model ──────────
pairwise_lvls <- contrast(
  emm_by_model,
  method = "pairwise",           # L0–L1, L0–L2, L1–L2
  adjust = "none"                # or "bonferroni", "holm", …
)

## 4 ── tidy data.frame of comparisons with CIs on the probability scale ───
pairwise_df <- summary(pairwise_lvls, infer = c(TRUE, TRUE)) |>
  as.data.frame()

# ── peek ──────────────────────────────────────────────────────────────────
head(pairwise_df)














### if input tokens differ systematically by task level we need to control for that

mean(data_long$input_tokens[data_long$task_level_all==0])
mean(data_long$input_tokens[data_long$task_level_all==1])
mean(data_long$input_tokens[data_long$task_level_all==2])

mean_input_tokens_lvl0 <- mean(data_long$input_tokens[data_long$task_level==0], na.rm=T)
mean_input_tokens_lvl1 <-mean(data_long$input_tokens[data_long$task_level==1], na.rm=T)
mean_input_tokens_lvl2 <-mean(data_long$input_tokens[data_long$task_level==2], na.rm=T)
sd_input_tokens_lvl0 <- sd(data_long$input_tokens[data_long$task_level==0], na.rm=T)
sd_input_tokens_lvl1 <- sd(data_long$input_tokens[data_long$task_level==1], na.rm=T)
sd_input_tokens_lvl2 <- sd(data_long$input_tokens[data_long$task_level==2], na.rm=T)

anova_result <- aov(input_tokens ~ factor(task_level), data = data_long)
summary(anova_result)
TukeyHSD(anova_result)


## 1.3  Pairwise (Holm-adjusted) level contrasts ------------

emm_overall  <- emmeans(m_overall2, "task_level")
pair_overall <- pairs(emm_overall, adjust = "holm")

## 1.4  Trend-shape checks (quadratic, log, exp) ------------

m_quad <- update(m_overall,
                 formula = . ~ poly(task_level_num, 2, raw = TRUE) +
                   + logit_p + (1 + task_level_num | model) + (1 | trial))

h2_data <- h2_data %>%
  mutate(level_log = log1p(task_level_num),
         level_exp = exp(task_level_num))

m_log <- update(m_overall, formula = . ~ level_log + logit_p + (1 + task_level_num | model) + (1 | trial))
m_exp <- update(m_overall, formula = . ~ level_exp + logit_p + (1 + task_level_num | model) + (1 | trial))
                 

AIC(m_overall, m_quad, m_log, m_exp)
# Linear model (m_overall) has the smallest AIC → keep it


###-------------------------

### Make table of random slopes

# ─────────────────────────────────────────────────────────────
# Required packages
# ─────────────────────────────────────────────────────────────
library(lme4)
library(arm)
library(dplyr)
library(tidyr)
library(tibble)
library(flextable)
library(officer)

m_slope <- m_overall2
# ─────────────────────────────────────────────────────────────
# 1. Simulate posterior draws for random effects
# ─────────────────────────────────────────────────────────────
set.seed(123)
sims <- sim(m_slope, n.sims = 2000)

model_names <- rownames(coef(m_slope)$model)

# Extract posterior draws: fixed + model-specific slope
slope_draws <- sapply(model_names, function(mod) {
  fixef(sims)[, "task_level_num"] + ranef(sims)$model[, mod, "task_level_num"]
})

# ─────────────────────────────────────────────────────────────
# 2. Summarize: mean, SD, 95% CI, pseudo p-value
# ─────────────────────────────────────────────────────────────
slopes_summary <- t(apply(slope_draws, 2, function(x) {
  c(Beta = mean(x),
    SE = sd(x),
    CI_lower = quantile(x, 0.025),
    CI_upper = quantile(x, 0.975),
    p_pseudo = 2 * min(mean(x < 0), mean(x > 0)))
}))

slopes_df <- as.data.frame(slopes_summary) %>%
  rownames_to_column(var = "Model") %>%
  mutate(across(where(is.numeric), ~ round(.x, 3)))

# ─────────────────────────────────────────────────────────────
# 3. Format APA-style Word table
# ─────────────────────────────────────────────────────────────
ft <- flextable(slopes_df) %>%
  theme_vanilla() %>%
  autofit() %>%
  set_caption("Table X\nRandom Slopes of Task Level by Language Model (Posterior Estimates)") %>%
  set_header_labels(
    Model = "Model",
    Beta = "B",
    SE = "SD",
    CI_lower = "95% CI (LL)",
    CI_upper = "95% CI (UL)",
    p_pseudo = "p (approx.)"
  ) %>%
  align(align = "center", part = "all") %>%
  italic(j = "Model", part = "body")

# ─────────────────────────────────────────────────────────────
# 4. Export to Word
# ─────────────────────────────────────────────────────────────
doc <- read_docx()
doc <- body_add_par(doc, "Random Slopes Table", style = "heading 2")
doc <- body_add_flextable(doc, ft)
print(doc, target = "Random_Slopes_Task_Level_Table.docx")




# ─────────────────────────────────────────────────────────────
#  For each model (LLM): accuracy ~ task_level + logit_p
#  No random effects → simple glm() per model
# ─────────────────────────────────────────────────────────────

fit_single_model <- function(df) {
  glmer(revised_accuracy ~ task_level_num + logit_p + (1 | trial), 
        family = binomial, data = df)
}

# if power too low
fit_single_model <- function(df) {            
  glm(accuracy ~ task_level_num + logit_p, family = binomial, data = df)
}

fit_single_model <- function(df) {            
  glm(revised_accuracy ~ task_level_num, family = binomial, data = df)
}


per_llm_models <- h2_data %>%
  group_by(model) %>%
  group_map(~ fit_single_model(.x), .keep = TRUE) %>%
  setNames(unique(h2_data$model))

# Extract coefficient for task_level (overall slope) ----------
task_coef_tbl <- map_dfr(per_llm_models, ~ tidy(.x), .id = "model") %>%
  filter(term == "task_level_num") %>%              # slope of numeric level
  mutate(p_adj = p.adjust(p.value, method = "holm"))  # adjust across LLMs

task_coef_tbl[10,]

# Pairwise contrasts within each model ------------------------
pw_fun <- function(fit) {
  pairs(emmeans(fit, "task_level_num"), adjust = "holm") %>%
    as.data.frame()
}

pw_contrasts_tbl <- map_dfr(per_llm_models, pw_fun, .id = "model")  # 3 contrasts × models

# Optional: adjust the 3 × n_models tests in one step
pw_contrasts_tbl$p_adj_overall <- p.adjust(pw_contrasts_tbl$p.value, method = "holm")


# ─────────────────────────────────────────────────────────────
# H2 Visualization % Table
# ─────────────────────────────────────────────────────────────


library(broom.mixed)
library(dplyr)
library(flextable)
library(officer)

# Get confidence intervals for overall model
overall_ci <- confint(m_overall, method = "Wald") %>%
  as.data.frame() %>%
  rownames_to_column("term") %>%
  filter(term == "task_level") %>%
  rename(CI_lower = `2.5 %`, CI_upper = `97.5 %`) %>%
  mutate(model = "Overall")

# Add confidence intervals to each LLM model
task_coef_tbl_ci <- map_dfr(per_llm_models, ~ tidy(.x, conf.int = TRUE), .id = "model") %>%
  filter(term == "task_level_num") %>%
  mutate(p_adj = p.adjust(p.value, method = "holm")) %>%
  rename(CI_lower = conf.low, CI_upper = conf.high)

# Get fixed effect row from overall model
overall_row <- tidy(m_overall, conf.int = TRUE) %>%
  filter(term == "task_level_num") %>%
  rename(CI_lower = conf.low, CI_upper = conf.high) %>%
  mutate(model = "Overall", p_adj = NA)

# Combine all results into one table
table_task <- bind_rows(overall_row, task_coef_tbl_ci) 

library(dplyr)

table_task <- bind_rows(overall_row, task_coef_tbl_ci) %>%
  dplyr::rename(
    Model = model,
    Beta = estimate,
    SE = std.error,
    z = statistic,
    p = p.value
  ) %>%
  dplyr::select(Model, Beta, SE, z, CI_lower, CI_upper, p, p_adj) %>%
  mutate(across(Beta:p_adj, ~ round(.x, 3)))


ft <- flextable(table_task)

ft <- ft %>%
  autofit() %>%
  theme_vanilla() %>%
  set_caption(caption = "Table X\nFixed Effect of Task Level on Accuracy for Each Language Model") %>%
  set_header_labels(
    Beta     = "B",
    SE       = "SE",
    z        = "z",
    CI_lower = "95% CI (LL)",
    CI_upper = "95% CI (UL)",
    p        = "p",
    p_adj    = "p (adj)"
  ) %>%
  align(align = "center", part = "all") %>%
  italic(j = c("Model"), part = "body")

# Save to Word
doc <- read_docx()
doc <- body_add_flextable(doc, ft)
print(doc, target = "H2_TaskLevel_APA_Table.docx")



##### H2b

m_H2b <- glmer(
  revised_accuracy ~ task_level_num*log_size + logit_p_z + input_tokens_z +
    (1 + task_level_num | model) +
    (1 | item_id),
  family = binomial,
  data   = h2_data,
  control = glmerControl(optimizer = "bobyqa")
)

summary(m_H2b)



library(dplyr)

# ── 1.  Total-cost lookup keyed to your internal model IDs ──────────────────
library(tibble)

llm_cost_total <- tribble(
  ~model,          ~token_cost_total,     # USD per 1 000 total tokens
  "claude_4_sonnet", 0.00180,             # Claude Sonnet 4
  "deepseek_v3",     0.00018,             # DeepSeek V3
  "gemini_2_0_flash",0.00004,             # Gemini 2.0 Flash
  "gpt_3_5_turbo",   0.00035,             # GPT-3.5 Turbo
  "gpt_4_1",         0.00400,             # GPT-4.1
  "gpt_4o",          0.00200,             # GPT-4o
  "gpt_o3",          0.00500,             # o3 (approx.)
  "grok_3",          0.00180,             # Grok 3
  "llama_3_3",       0.00008,             # Llama 3.3 70 B
  "llama_4",         0.000102             # Llama 4
)


# ── 2.  Merge onto your long-format data set ────────────────────────────────
h2_data <- h2_data %>%
  left_join(llm_cost_total, by = "model")


m_H2b <- glmer(
  revised_accuracy ~ task_level_num*log_size + logit_p_z + # input_tokens_z +
    (1 + task_level_num | model) +
    (1 | item_id),
  family = binomial,
  data   = h2_data,
  control = glmerControl(optimizer = "bobyqa")
)


summary(m_H2b)






























# Outdated

# ─────────────────────────────────────────────────────────────────────────────
# Hypothesis 2  → Level-0, Level-1, and Level-2 trials, valid only, self-ref condition
# ─────────────────────────────────────────────────────────────────────────────


h2_data <- data_long %>% 
  filter(valid,
         condition_type == "experimental",
         !is.na(task_level))

# Use these formulas as default

h2_formula <- accuracy ~ task_level + (1 | model)
h2_formula_poly <- accuracy ~ poly(as.numeric(task_level), 2, raw = TRUE) + (1 | model)

# again deal with model_size; run only if needed
if (mean(is.na(h2_data$model_size)) > .50) {
  h2_formula <- accuracy ~ task_level + (1 | model)
  h2_formula_poly <- accuracy ~ poly(as.numeric(task_level), 2, raw = TRUE) + (1 | model)
} else {
  h2_data <- h2_data %>% mutate(log_size = log(model_size))
  h2_formula <- accuracy ~ task_level * log_size + (1 | model)
  h2_formula_poly <- accuracy ~ poly(as.numeric(task_level), 2, raw = TRUE) * log_size + (1 | model)
}

m2 <- glmer(h2_formula, family = binomial, data = h2_data,
            control = glmerControl(optimizer = "bobyqa"))

summary(m2)
car::Anova(m2, type = 3)

## Planned pairwise comparisons 0 ↔ 1 ↔ 2 (controls FDR vs. whole model)
pairs(emmeans(m2, "task_level"), adjust = "holm")

## Polynomial trend test (linear ± quadratic)
m2_poly <- glmer(h2_formula_poly, family = binomial, data = h2_data)
anova(m2, m2_poly)   # does adding quadratic help?



## Testing H2 while controlling for chance-level responding across levels

# Approach 1: offset

h2_data <- h2_data %>% mutate(offset_ll = qlogis(p_chance))

m2 <- glmer(accuracy ~ task_level + (1 | model) + offset(offset_ll),
            family = binomial, data = h2_data,
            control = glmerControl(optimizer = "bobyqa"))

summary(m2)
car::Anova(m2, type = 3)
emmeans(m2, "task_level", adjust = "holm")

#approach 2: Fixed covariate. ### this is the current approach that i want to keep and apply for the individual models

h2_data <- h2_data %>%
  mutate(logit_p = qlogis(chance_prob))  # transforms 0‒1 safely


h2_data$task_level <- as.numeric(as.character(h2_data$task_level))


m2 <- glmer(
  accuracy ~ task_level + logit_p + (1 | model) + (1 | trial),
  family = binomial,
  data = h2_data
)
summary(m2)

h2_data$task_level <- relevel(factor(h2_data$task_level, ordered = FALSE), ref="0") # factor

m2 <- glmer(
  accuracy ~ task_level + logit_p + (1 | model) + (1 | trial),
  family = binomial,
  data = h2_data
)

pairs(emmeans(m2, "task_level"), adjust = "holm")


## Polynomial trend test (linear ± quadratic)
h2_formula_poly <- accuracy ~ poly(as.numeric(task_level), 2, raw = TRUE) + logit_p + (1 | model) + (1 | trial)
m2_poly <- glmer(h2_formula_poly, family = binomial, data = h2_data)
anova(m2, m2_poly)   # does adding quadratic help?


### Other trend analysis

h2_data$level_log   <- log1p(h2_data$task_level_num)        # log(1 + level)
h2_data$level_exp   <- exp(h2_data$task_level_num)          # exponential growth
h2_data$level_inv   <- 1 / (h2_data$task_level_num + 1)     # inverse


# Logarithmic
m_log <- glmer(
  accuracy ~ level_log + logit_p + (1 | model) + (1 | trial),
  family = binomial,
  data = h2_data
)

# Exponential
m_exp <- glmer(
  accuracy ~ level_exp + logit_p + (1 | model) + (1 | trial),
  family = binomial,
  data = h2_data
)

# Compare AICs
AIC(m2, m2_poly, m_log, m_exp)



# ────────────────────────────────────────────────
#   Optional break-point analysis (H2c idea)
# ────────────────────────────────────────────────
# A gentler alternative to segmented(): use emmeans to find 
# the task_level at which the linear predictor crosses chance (0 on logit scale)


# Step 2: Convert to logit (qlogis = log(p / (1-p)))
h2_data <- h2_data %>%
  mutate(offset_term = qlogis(chance_prob)) %>%
  mutate(task_level_num = as.numeric(as.character(task_level)))

# Step 3

m2_offset <- glmer(
  accuracy ~ task_level_num + (1 | model) + offset(offset_term),
  family = binomial,
  data = h2_data
)

m2_offset <- glmer(
  accuracy ~ task_level+ (1 | model) + offset(offset_term),
  family = binomial,
  data = h2_data
)

library(dplyr)
library(ggplot2)

# Quick sanity table
data_long %>%
  count(task_level, chance_prob) %>%
  arrange(task_level, desc(chance_prob))

# Look at level-0 rows that ended up with the tiny chance
data_long %>% 
  filter(task_level == 0, chance_prob < .001) %>% 
  select(trial, expected_output, prompt) %>% 
  glimpse(20)





emtrends(m2_offset, ~ 1, var = "task_level_num")

# ────────────────────────────────────────────────
#   Descriptives & plotting
# ────────────────────────────────────────────────
h2_data %>% 
  group_by(task_level, model) %>% 
  summarise(acc = mean(accuracy), n = n(), .groups = "drop") %>% 
  ggplot(aes(task_level, acc, colour = model, group = model)) +
  geom_point() + geom_line() +
  geom_smooth(method = "loess", se = FALSE, colour = "black") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(x = "Task level", y = "Accuracy", 
       title = "Accuracy by meta-rule level and model")


# ────────────────────────────────────────────────
#   Diagnostics (convergence, overdispersion, ICC)
# ────────────────────────────────────────────────
check_model(m1)
check_model(m2)
performance::check_overdispersion(m2)
performance::icc(m2)










