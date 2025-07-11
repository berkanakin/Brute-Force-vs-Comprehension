# =====================================================================
#  confirmatory_analysis.R · (Author: Berkan Akin, 2025‑07‑10)
#  -------------------------------------------------------------------
#  Reproduces the key inferential results reported in the manuscript.
#
#  Covered hypotheses & models
#  ---------------------------
#  • **H1 – Condition effect (Level‑0)**
#      • Model 1  – Pooled GLMM  (random intercept + slope for condition_type)
#      • Model 2  – Unpooled GLM (condition_type × model)
#  • **H2a/b – Task‑level decline (Levels 0 → 1)**
#      • Model 3  – Pooled GLMM  (random intercept + slope for task_level)
#      • Model 4  – Unpooled GLM (task_level × model)
#  • **H2c – Moderation by model size (log_size)**
#      • Model 5  – Pooled GLMM  (task_level × log_size interaction)
#
#  For each model we extract marginal probabilities, odds ratios, and
#  contrasts via **emmeans**, saving tidy .csv files to */outputs/results/*.
# =====================================================================

# ── 0  Packages & directories ─────────────────────────────────────────
if (!requireNamespace("pacman", quietly = TRUE)) install.packages("pacman")

pacman::p_load(
  tidyverse, lme4, emmeans, broom, broom.mixed, here, fs
)

fs::dir_create(here("outputs", "results"))

# ── 1  Load analysis data ─────────────────────────────────────────────
h1_data <- read_csv(here("data/h1_data.csv"))
h2_data <- read_csv(here("data/h2_data.csv"))

# =====================================================================
#  H1 – Condition effect (Level‑0)
# =====================================================================

## ---------------- Model 1 : Pooled GLMM ------------------------------
form_H1_pool <- revised_accuracy ~ condition_type + (1 + condition_type | model)

m_H1_pool <- glmer(form_H1_pool, data = h1_data, family = binomial,
                   control = glmerControl(optimizer = "bobyqa"))

# Fixed effects
fixef_H1_pool <- broom.mixed::tidy(m_H1_pool, effects = "fixed", conf.int = TRUE)
write_csv(fixef_H1_pool, here("outputs/results", "H1_model1_fixed.csv"))

# Marginal probabilities and contrast
emm_H1_cond <- emmeans(m_H1_pool, ~ condition_type, type = "response")
write_csv(as.data.frame(emm_H1_cond),
          here("outputs/results", "H1_model1_marginal_probs.csv"))
write_csv(as.data.frame(contrast(emm_H1_cond, method="pairwise", adjust="bonferroni")),
          here("outputs/results", "H1_model1_pairwise.csv"))

## ---------------- Model 2 : Unpooled GLM -----------------------------
m_H1_unpooled <- glm(revised_accuracy ~ condition_type * model + input_tokens_z,
                     data = h1_data, family = binomial)

emm_H1_by_model <- emmeans(m_H1_unpooled, ~ condition_type | model, type="response")
emm_H1_contrasts_by_model <- pairs(emm_H1_by_model, by = "model", adjust = "bonferroni")

write_csv(as.data.frame(emm_H1_by_model),
          here("outputs/results", "H1_model2_probs_by_model.csv"))
write_csv(as.data.frame(emm_H1_contrasts_by_model),
          here("outputs/results", "H1_model2_contrasts_by_model.csv"))

# =====================================================================
#  H2a – Task‑level decline (Levels 0 → 1)
# =====================================================================

## ---------------- Model 3 : Pooled GLMM ------------------------------
form_H2_pool <- revised_accuracy ~ task_level_num + logit_p + input_tokens_z + # without offset term: +logit_p
  (1 + task_level_num | model) + (1 | item_id) 
form_H2_pool <- revised_accuracy ~ task_level + logit_p + input_tokens_z + # without offset term: +logit_p
  (1 + task_level | model) + (1 | item_id) 

m_H2_pool <- glmer(form_H2_pool, data = h2_data, family = binomial,
                   control = glmerControl(optimizer = "bobyqa"))

# Fixed effects
fixef_H2_pool <- broom.mixed::tidy(m_H2_pool, effects = "fixed", conf.int = TRUE)
write_csv(fixef_H2_pool, here("outputs/results", "H2_model3_fixed.csv"))

# Marginal probabilities by task level
emm_H2_level <- emmeans(m_H2_pool, ~ task_level, type = "response")
contrast_H2_pool <- contrast(emm_H2_level, method="pairwise", adjust="bonferroni")
contrast_H2_pool_link <- summary(
  contrast_H2_pool,
  type   = "link",          # gives log-odds difference
  infer  = c(TRUE, TRUE)    # SE & z also returned
)


write_csv(as.data.frame(emm_H2_level),
          here("outputs/results", "H2_model3_marginal_probs.csv"))
write_csv(as.data.frame(contrast(emm_H2_level, method="pairwise", adjust="bonferroni")),
          here("outputs/results", "H2_model3_pairwise.csv"))


## ---------------- Model 4 : Unpooled GLM -----------------------------
# Note: original GLMM with item intercept failed; GLM reported instead.

m_H2_unpooled <- glm(revised_accuracy ~ task_level_num * model + logit_p + input_tokens_z,
                     data = h2_data, family = binomial)

m_H2_unpooled <- glm(revised_accuracy ~ task_level* model + logit_p + input_tokens_z,
                     data = h2_data, family = binomial)


# Save full summary
write_csv(broom::tidy(m_H2_unpooled, conf.int = TRUE),
          here("outputs/results", "H2_model4_full_summary.csv"))

# Estimated marginal means & contrasts
emm_H2_by_model <- emmeans(m_H2_unpooled, ~ task_level | model, type = "response")
write_csv(as.data.frame(emm_H2_by_model),
          here("outputs/results", "H2_model4_probs_by_model.csv"))
contrast_H2_by_model <- contrast(emm_H2_by_model, method = "pairwise", adjust = "none")
write_csv(as.data.frame(contrast_H2_by_model),
          here("outputs/results", "H2_model4_contrasts_by_model.csv"))

contrast_H2_unpool_link <- summary(
  contrast_H2_by_model,
  type   = "link",          # gives log-odds difference
  infer  = c(TRUE, TRUE)    # SE & z also returned
)


# =====================================================================
#  H2b – Collapse to Chance (Levels 0 → 1)
# =====================================================================


library(emmeans)
library(dplyr)

chance <- 0.061           # chance rate = 1/16

## 1 ─ refit Model 4 without offset ------------------------------------
m_H2_unpooled <- glm(
  revised_accuracy ~ task_level * model + input_tokens_z,
  data   = h2_data,
  family = binomial
)

## 2 ─ EMMs for Level 1 only (probability scale) -----------------------
emm_lvl1 <- emmeans(
  m_H2_unpooled,
  ~ model | task_level,
  type = "response"
)

emm_lvl1 <- subset(emm_lvl1, task_level == 1)  # keep Level-1 rows

## 3 ─ one-sided 95 % upper CI -----------------------------------------
emm_lvl1_ci <- confint(
  emm_lvl1,
  level  = 0.95,
  side   = "upper",        # one-sided: gives upper.CL
  adjust = "none"
)

## 4 ─ classify collapse ----------------------------------------------
collapse_tbl <- as_tibble(emm_lvl1_ci) %>%
  transmute(
    model,
    prob_L1   = prob,
    upper_L1  = asymp.UCL,
    collapsed = upper_L1 <= chance        # TRUE = “no better than chance”
  )

chance <- 0.061

chance <- 0.061
logit_chance <- qlogis(chance)      # ≈ -2.72

test_lvl1 <- test(
  emm_lvl1,                         # still an emmGrid
  null  = logit_chance,             # same scale as internal logits
  side  = ">",                      # H₀: prop > chance
  by    = "model",
  adjust = "none"
) %>%
  as_tibble() %>%
  mutate(collapsed = p.value > 0.05)



collapse_tbl <- h2_data %>%
  filter(valid, task_level == "1") %>%
  group_by(model) %>%
  summarise(n = n(), k = sum(revised_accuracy), .groups = "drop") %>%
  mutate(
    p_gt_chance = purrr::map2_dbl(k, n,
                                  ~ binom.test(.x, .y, p = chance,
                                               alternative = "greater")$p.value),
    collapsed = p_gt_chance > 0.05
  )


library(dplyr)
library(purrr)

chance <- 0.061          # collapse region: 0 … 0.061

bf_tbl <- h2_data %>%
  filter(valid, task_level == "1") %>%
  group_by(model) %>%
  summarise(k = sum(revised_accuracy),
            n = n(),
            .groups = "drop") %>%
  mutate(
    post_prob_collapse = pbeta(chance, k + 1, n - k + 1),          # P(θ ≤ .061 | data)
    BF_collapse = (post_prob_collapse / (1 - post_prob_collapse))  # odds posterior
    * ((1 - chance) / chance),                      # ÷ prior odds
    BF_above    = 1 / BF_collapse,
    collapsed   = BF_collapse > 3                             # substantial evidence
  )

print(bf_tbl)



# =====================================================================
#  H2c – Moderation by model size (log_size)
# =====================================================================

## ---------------- Model 5 : Pooled GLMM ------------------------------
form_H2c <- revised_accuracy ~ task_level_num * scale(log_size) + logit_p +
  (1 + task_level_num | model) + (1 | item_id)

m_H2c <- glmer(form_H2c, data = h2_data, family = binomial,
               control = glmerControl(optimizer = "bobyqa"))

# Fixed effects
fixef_H2c <- broom.mixed::tidy(m_H2c, effects = "fixed", conf.int = TRUE)
write_csv(fixef_H2c, here("outputs/results", "H2_model5_fixed.csv"))

# Simple‑effects: task‑level effect at ±1 SD log_size ------------------
emm_H2c_levels <- emmeans(m_H2c,
                          ~ task_level_num | log_size_sd,
                          at = list(`scale(log_size)` = c(-1, 0, 1)),
                          type = "response")
write_csv(as.data.frame(emm_H2c_levels),
          here("outputs/results", "H2_model5_marginal_probs.csv"))

# Contrast L0–L1 at each log_size value
contrast_H2c <- contrast(emm_H2c_levels, method = "pairwise", by = "log_size_sd",
                         adjust = "bonferroni")
write_csv(as.data.frame(contrast_H2c),
          here("outputs/results", "H2_model5_pairwise.csv"))

# =====================================================================
#  Console report (quick glance)
# =====================================================================

cat("\nModel 1 (H1 pooled) fixed effects:\n"); print(fixef_H1_pool)
cat("\nModel 2 contrasts (H1) by model:\n"); print(head(read_csv(here("outputs/results", "H1_model2_contrasts_by_model.csv"))))
cat("\nModel 3 (H2 pooled) fixed effects:\n"); print(fixef_H2_pool)
cat("\nModel 4 contrasts (H2) by model (no adjust):\n"); print(head(as.data.frame(contrast_H2_by_model)))
cat("\nModel 5 (H2c) fixed effects:\n"); print(fixef_H2c)

cat("\n✓ H1, H2a/b, and H2c confirmatory analyses complete. Results saved to /outputs/results.\n")

  )