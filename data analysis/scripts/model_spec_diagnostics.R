# =====================================================================
#  model_spec_diagnostics.R  ·  (Author: Berkan Akin, 2025‑07‑10)
#  -------------------------------------------------------------------
#  • Fits all models for H1–H2c
#  • Compares random‑intercept vs. random‑slope structures (LRT + AIC/BIC)
#  • Runs diagnostics (singularity, collinearity, dispersion, DHARMa)
#  • Saves tidy summaries, LRT tables, and residual plots to /outputs
#
#  Modelling notes
#  --------------
#  • Models 1 & 2 (H1) omit the logit‑chance offset term because it is
#    perfectly collinear with the experimental condition, causing
#    convergence failure. Model 1 additionally drops the z‑scored input
#    length covariate (input_tokens_z) as it was non‑significant and led
#    to a singular random‑effects fit.
#  • Model 4 (H2a unpooled) was originally specified with an item‑ID
#    random intercept, but that version did not converge (boundary fit;
#    variance ≈ 0). We therefore report the equivalent logistic GLM
#    without that term.
#
#  • The *original* (non‑converging) fits are kept **commented‑out**
#    below so convergence issues can be reproduced if desired.
# =====================================================================

# ── 0  Load packages ──────────────────────────────────────────────────
if (!requireNamespace("pacman", quietly = TRUE)) install.packages("pacman")

pacman::p_load(
  tidyverse, lme4, glmmTMB, broom.mixed, performance, DHARMa, here, fs
)

fs::dir_create(here("outputs", "diagnostics"))

# ── helper: diagnostics & messaging -----------------------------------
run_diagnostics <- function(model, name) {
  cat("\n──────────────── Diagnostic summary ·", name, "────────────────\n")
  
  ## 1  Singularity (merMod only) --------------------------------------
  if (inherits(model, "merMod") && lme4::isSingular(model))
    cat("• Singular fit (variance ≈ 0)\n")
  
  ## 2  Collinearity ---------------------------------------------------
  vif_tbl <- suppressWarnings(performance::check_collinearity(model, quiet = TRUE))
  max_vif <- max(vif_tbl$VIF, na.rm = TRUE)
  cat("• Max VIF:", round(max_vif, 2), "\n")
  
  ## 3  Dispersion -----------------------------------------------------
  disp <- performance::check_overdispersion(model)
  cat("• Dispersion ratio:", round(disp$dispersion_ratio, 2), "\n")
  
  ## 4  DHARMa residual tests -----------------------------------------
  sim <- DHARMa::simulateResiduals(model, plot = FALSE)
  cat("• DHARMa uniformity p:", DHARMa::testUniformity(sim)$p.value, "\n")
  cat("• DHARMa dispersion  p:", DHARMa::testDispersion(sim)$p.value,  "\n")
  
  ## 5  Save residual plot & tidy table -------------------------------
  png(here("outputs/diagnostics", paste0(name, "_residuals.png")), 800, 600)
  plot(sim)
  dev.off()
  
  broom.mixed::tidy(model, effects = "fixed", conf.int = TRUE) %>%
    write_csv(here("outputs", paste0(name, "_tidy.csv")))
}

compare_models <- function(m0, m1, label0, label1) {
  tbl <- anova(m0, m1, test = "Chisq")
  out <- tibble(model0 = label0, model1 = label1,
                df0 = tbl$Df[1], df1 = tbl$Df[2],
                AIC0 = AIC(m0),  AIC1 = AIC(m1),
                BIC0 = BIC(m0),  BIC1 = BIC(m1),
                LRT  = tbl$Chisq[2], p = tbl$`Pr(>Chisq)`[2])
  print(out)
  write_csv(out, here("outputs", paste0(label0, "_vs_", label1, "_LRT.csv")))
  invisible(out)
}

# ── 1  Load data ------------------------------------------------------
h1_data <- read_csv(here("data/h1_data.csv"))
h2_data <- read_csv(here("data/h2_data.csv"))

# ============================  H1  ====================================
cat("\n============ H1 (condition effect) ============\n")

# -- ORIGINAL preregistered fit (failed: offset + input length) --------
# form_H1_full <- revised_accuracy ~ condition_type + offset(logit_p) + input_tokens_z +
#   (1 + condition_type | model)
# try(glmer(form_H1_full, data = h1_data, family = binomial,
#           control = glmerControl(optimizer="bobyqa")))

# -- Model 1a/1b : pooled ----------------------------------------------
form_H1_int   <- revised_accuracy ~ condition_type + (1 | model)          # random‑intercept
form_H1_slope <- revised_accuracy ~ condition_type + (1 + condition_type | model)  # random‑slope

m_H1_int   <- glmer(form_H1_int, data = h1_data, family = binomial,
                    control = glmerControl(optimizer = "bobyqa"))
m_H1_slope <- glmer(form_H1_slope, data = h1_data, family = binomial,
                    control = glmerControl(optimizer = "bobyqa"))

lrt_H1 <- compare_models(m_H1_int, m_H1_slope, "H1_int", "H1_slope")

m_H1_pool <- if (lrt_H1$p < .05) m_H1_slope else m_H1_int
run_diagnostics(m_H1_pool, "H1_pooled")

# -- ORIGINAL unpooled fit with offset (failed) ------------------------
# m_H1_unpooled_full <- glm(revised_accuracy ~ condition_type * model + offset(logit_p) + input_tokens_z,
#                           data = h1_data, family = binomial)
# ## produced perfect‑collinearity warning & inflated VIFs

# -- Model 2 : unpooled (condition × model) ----------------------------
m_H1_unpooled <- glm(revised_accuracy ~ condition_type * model + input_tokens_z,
                     data = h1_data, family = binomial)
run_diagnostics(m_H1_unpooled, "H1_unpooled")

# ============================  H2a/b  ================================
cat("\n============ H2a–b (task‑level decline) ============\n")

# -- ORIGINAL pooled mixed‑effects (with item random slope & offset) ---
# form_H2a_full <- revised_accuracy ~ task_level_num + offset(logit_p) + input_tokens_z +
#   (1 + task_level_num | model) + (1 | item_id)
# try(glmer(form_H2a_full, data = h2_data, family = binomial,
#           control=glmerControl(optimizer="bobyqa")))

# -- Model 3a/3b : pooled final specs ----------------------------------
form_H2a_int   <- revised_accuracy ~ task_level_num + offset(logit_p) + input_tokens_z +
  (1 | model) + (1 | item_id)
form_H2a_slope <- revised_accuracy ~ task_level_num + offset(logit_p) + input_tokens_z +
  (1 + task_level_num | model) + (1 | item_id)

m_H2a_int   <- glmer(form_H2a_int, data = h2_data, family = binomial,
                     control = glmerControl(optimizer = "bobyqa"))
m_H2a_slope <- glmer(form_H2a_slope, data = h2_data, family = binomial,
                     control = glmerControl(optimizer = "bobyqa"))

lrt_H2a <- compare_models(m_H2a_int, m_H2a_slope, "H2a_int", "H2a_slope")

m_H2a_pool <- if (lrt_H2a$p < .05) m_H2a_slope else m_H2a_int
run_diagnostics(m_H2a_pool, "H2a_pooled")

# -- ORIGINAL unpooled GLMM with item ID (failed) ----------------------
# m_H2a_unpooled_glmm <- glmer(revised_accuracy ~ task_level_num * model + offset(logit_p) + input_tokens_z +
#                                (1 | item_id), data = h2_data, family = binomial,
#                                control = glmerControl(optimizer="bobyqa"))
# ## boundary fit; variance(item_id)=0

# -- Model 4 : unpooled GLM (task_level × model) -----------------------
m_H2a_unpooled <- glm(revised_accuracy ~ task_level_num * model + offset(logit_p) + input_tokens_z,
                      data = h2_data, family = binomial)
run_diagnostics(m_H2a_unpooled, "H2a_unpooled")

# ============================  H2c  ===================================
cat("\n============ H2c (task-level × model size) ============\n")

# random-intercept only
form_H2c_int   <- revised_accuracy ~ task_level_num * scale(log_size) + logit_p +
  (1 | model) + (1 | item_id)

# random slope for task_level by model
form_H2c_slope <- revised_accuracy ~ task_level_num * scale(log_size) + logit_p +
  (1 + task_level_num | model) + (1 | item_id)

m_H2c_int   <- glmer(form_H2c_int,   data = h2_data, family = binomial,
                     control = glmerControl(optimizer = "bobyqa"))
m_H2c_slope <- glmer(form_H2c_slope, data = h2_data, family = binomial,
                     control = glmerControl(optimizer = "bobyqa"))

# model-selection table
lrt_H2c <- compare_models(m_H2c_int, m_H2c_slope, "H2c_int", "H2c_slope")

# choose better model
m_H2c_pool <- if (lrt_H2c$p < .05) m_H2c_slope else m_H2c_int
run_diagnostics(m_H2c_pool, "H2c_pooled")

