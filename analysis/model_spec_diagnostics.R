

# ================================================================
#  model_spec_diagnostics.R  ·  (Author: Berkan Akin, 2025)
#  --------------------------------------------------------------
#  • Fits all preregistered GLMMs (H1–H2c)
#  • Performs model–selection comparisons (LRT + AIC/BIC)
#  • Runs standard diagnostics (singularity, dispersion, DHARMa residuals)
#  • Writes tidy summaries, LRT tables, and diagnostic plots to /outputs
# ================================================================

# ── 0  Load packages ───────────────────────────────────────────────────────
if (!requireNamespace("pacman", quietly = TRUE)) install.packages("pacman")

pacman::p_load(
  tidyverse, lme4, broom.mixed, performance, DHARMa, here
)

fs::dir_create(here("outputs", "diagnostics"))

# ── helper: diagnostics & messaging ----------------------------------------
run_diagnostics <- function(model, name) {
  cat("
──────────────── Diagnostic summary ·", name, "────────────────
")
  
  # MerMod-specific check ---------------------------------------------------
  if (inherits(model, "merMod")) {
    if (isSingular(model)) cat("• Singular fit (variance ≈ 0)
")
  }
  
  # Over‑dispersion (works for glm & merMod) --------------------------------
  disp <- performance::check_overdispersion(model)
  cat("• Dispersion ratio:", round(disp$dispersion_ratio, 2), "
")
  
  # DHARMa residual simulation ---------------------------------------------
  sim <- DHARMa::simulateResiduals(model, plot = FALSE)
  cat("• DHARMa uniformity p:", DHARMa::testUniformity(sim)$p.value, "
")
  cat("• DHARMa dispersion  p:", DHARMa::testDispersion(sim)$p.value,  "
")
  
  png(here("outputs/diagnostics", paste0(name, "_residuals.png")), 800, 600)
  plot(sim)
  dev.off()
  
  broom.mixed::tidy(model, effects = "fixed", conf.int = TRUE, exponentiate = FALSE) %>%
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

# ── 1  Load data -----------------------------------------------------------
h1_data <- read_csv(here("data/h1_data.csv"))
h2_data <- read_csv(here("data/h2_data.csv"))

# ── 2  H1 pooled vs slope --------------------------------------------------
cat("\n============ H1  (condition effect) ============\n")
form_int   <- revised_accuracy ~ condition_type + (1 | model)
form_slope <- revised_accuracy ~ condition_type + (1 + condition_type | model)

m_H1_int   <- glmer(form_int, data = h1_data, family = binomial,
                    control = glmerControl(optimizer = "bobyqa"))
try_slope  <- try(glmer(form_slope, data = h1_data, family = binomial,
                        control = glmerControl(optimizer = "bobyqa")))

if (inherits(try_slope, "try-error")) {
  cat("Random‑slope model failed; proceeding with intercept‑only.\n")
  m_H1_pool <- m_H1_int
} else {
  m_H1_slope <- try_slope
  lrt_H1 <- compare_models(m_H1_int, m_H1_slope, "H1_int", "H1_slope")
  m_H1_pool <- if (lrt_H1$p < .05) m_H1_slope else m_H1_int
}
run_diagnostics(m_H1_pool, "H1_pooled")

# ── 3  H1 unpooled ---------------------------------------------------------
cat("\n------------ H1  unpooled (condition × model) ------------\n")
m_H1_unpooled <- glm(revised_accuracy ~ condition_type * model,
                     family = binomial, data = h1_data)
summary(m_H1_unpooled)
run_diagnostics(m_H1_unpooled, "H1_unpooled")

# ── 4  H2a pooled ----------------------------------------------------------
cat("\n============ H2a  (task‑level main effect) ============\n")
form_H2a_int   <- revised_accuracy ~ task_level_num + logit_p_z +
  (1 | model) + (1 | item_id)
form_H2a_slope <- revised_accuracy ~ task_level_num + logit_p_z +
  (1 + task_level_num | model) + (1 | item_id)

m_H2a_int   <- glmer(form_H2a_int, data = h2_data, family = binomial,
                     control = glmerControl(optimizer = "bobyqa"))
m_H2a_slope <- glmer(form_H2a_slope, data = h2_data, family = binomial,
                     control = glmerControl(optimizer = "bobyqa"))

lrt_H2a <- compare_models(m_H2a_int, m_H2a_slope, "H2a_int", "H2a_slope")

m_H2a_pool <- if (lrt_H2a$p < .05) m_H2a_slope else m_H2a_int
run_diagnostics(m_H2a_pool, "H2a_pooled")

# ── 5  H2a unpooled --------------------------------------------------------
cat("\n------------ H2a  unpooled (task × model) ------------\n")
m_H2a_unpooled <- glm(revised_accuracy ~ task_level_num * model + logit_p_z,
                      family = binomial, data = h2_data)
summary(m_H2a_unpooled)
run_diagnostics(m_H2a_unpooled, "H2a_unpooled")

# ── 6  H2b  accuracy‑collapse (offset) ------------------------------------
cat("\n============ H2b  (accuracy‑collapse, offset) ============\n")
form_H2b <- revised_accuracy ~ task_level_num * model + offset(logit_p_z)
m_H2b <- glm(form_H2b, binomial, h2_data)
summary(m_H2b)
run_diagnostics(m_H2b, "H2b_unpooled")

# ── 7  H2c  moderation by model size --------------------------------------
cat("\n============ H2c  (moderation by model size) ============\n")
form_H2c <- revised_accuracy ~ task_level_num * log_size + logit_p_z +
  (1 + task_level_num | model) + (1 | item_id)

m_H2c <- glmer(form_H2c, data = h2_data, family = binomial,
               control = glmerControl(optimizer = "bobyqa"))
summary(m_H2c)
run_diagnostics(m_H2c, "H2c_pooled")

# ── 8  Save model objects ---------------------------------------------------
model_obj <- list(H1_pooled   = m_H1_pool,
                  H1_unpooled = m_H1_unpooled,
                  H2a_pooled  = m_H2a_pool,
                  H2a_unpooled= m_H2a_unpooled,
                  H2b_unpooled= m_H2b,
                  H2c_pooled  = m_H2c)

saveRDS(model_obj, here("outputs", "model_objects.rds"))

cat("\n✓ All models, comparisons, and diagnostics complete.\nResults saved in the /outputs folder.\n")



# ── 7  H2c  moderation by model size --------------------------------------
cat("\n============ H2c  (moderation by model size) ============\n")

## random-intercept-only
form_H2c_int <- revised_accuracy ~ task_level_num * log_size + logit_p_z +
  (1 | model) + (1 | item_id)

## random slope for task_level_num by model
form_H2c_slope <- revised_accuracy ~ task_level_num * log_size + logit_p_z +
  (1 + task_level_num | model) + (1 | item_id)

m_H2c_int   <- glmer(form_H2c_int,   data = h2_data, family = binomial,
                     control = glmerControl(optimizer = "bobyqa"))

m_H2c_slope <- glmer(form_H2c_slope, data = h2_data, family = binomial,
                     control = glmerControl(optimizer = "bobyqa"))

## likelihood-ratio test and information criteria
lrt_H2c <- compare_models(m_H2c_int, m_H2c_slope, "H2c_int", "H2c_slope")

## choose better model
m_H2c_best <- if (lrt_H2c$p < .05) m_H2c_slope else m_H2c_int

## diagnostics on the chosen model
run_diagnostics(m_H2c_best, "H2c_pooled")







