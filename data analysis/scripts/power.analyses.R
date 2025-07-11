# ---------------------------------------------------------------------------
#  power_analyses.R
# ---------------------------------------------------------------------------
#  Prospective & post‑hoc power analyses for the thesis
#  “Brute Force vs. Comprehension: Can Large Language Models Flexibly Adapt
#   to Novel Task Demands?”.
#
#  * Part 1 – Prospective power for H2a (Level‑0 vs Level‑1 accuracy drop):
#      • Generates synthetic data that mirror the planned mixed design
#        (11 models × 268 L0 trials × 65 L1 trials).
#      • Fits the confirmatory GLMM (accuracy ~ TaskLevel + (1|model) +
#        (1|item)).
#      • Uses {mixedpower} to estimate power for the fixed TaskLevel effect
#        at (a) the current design, (b) a grid of baseline accuracies &
#        planned drops (default = 10 pp).
#  * Part 2 – One‑sample binomial power for H2b (Level‑2 accuracy vs chance):
#      • Computes the minimum n to detect p = .20-.40 against p = 1⁄14 with
#        ≥80 % power (one‑sided, α = .05).
#
#  Inputs   : none (all data simulated on‑the‑fly)
#  Outputs  : printed power tables in the console; objects `pow_current`,
#             `grid_power` available in the workspace for further use.
#  Requires :
#      • R ≥ 4.2
#      • lme4  ≥ 1.1‑35   (CRAN)
#      • mixedpower       (GitHub: DejanDraschkow/mixedpower)
#      • pwr              (CRAN) – for the exact binomial solution
#  Usage    : source("power_analyses.R")  # ≈ 20–30 s on a modern laptop
# ---------------------------------------------------------------------------
#  Author   : Berkan Akın  <berkan.akin@student.uva.nl>
#  Created  : 2025‑07‑04
#  Updated  : 2025‑07‑09
#  License  : MIT + file LICENSE
# ---------------------------------------------------------------------------

##############################################################################
#  Load / install packages ---------------------------------------------------
##############################################################################
if (!requireNamespace("lme4", quietly = TRUE)) install.packages("lme4")
if (!requireNamespace("remotes", quietly = TRUE)) install.packages("remotes")
if (!requireNamespace("mixedpower", quietly = TRUE)) {
  remotes::install_github("DejanDraschkow/mixedpower",
                          build_vignettes = FALSE, upgrade = "never")
}
if (!requireNamespace("pwr", quietly = TRUE))     install.packages("pwr")

library(lme4)
library(mixedpower)
library(dplyr)
library(pwr)

##############################################################################
#  1 · Data‑generator for prospective power (H2a) ----------------------------
##############################################################################
make_H2a <- function(n_models = 11,
                     n_lvl0   = 268,
                     n_lvl1   = 65,
                     acc_lvl0 = 0.60,   # baseline accuracy
                     acc_lvl1 = 0.50,   # accuracy at level‑1
                     sd_model = 0.40,
                     sd_item  = 0.50,
                     seed      = 42) {
  set.seed(seed)
  total_items <- n_lvl0 + n_lvl1
  model <- factor(rep(seq_len(n_models), each = total_items))
  lvl   <- factor(rep(c(rep(0, n_lvl0), rep(1, n_lvl1)), times = n_models))
  # random intercepts
  u_model <- rnorm(n_models, 0, sd_model)[model]
  u_item  <- rnorm(total_items, 0, sd_item)[rep(seq_len(total_items), n_models)]
  # fixed‑effect log‑odds
  logit0 <- qlogis(acc_lvl0)
  delta  <- qlogis(acc_lvl1) - logit0  # L1 vs L0 effect in log‑odds
  eta <- logit0 + ifelse(lvl == 1, delta, 0) + u_model + u_item
  p   <- pmin(pmax(plogis(eta), 1e‑4), 1 - 1e‑4)  # clamp to avoid 0/1
  data.frame(model    = model,
             item     = rep(seq_len(total_items), n_models),
             TaskLevel = lvl,
             accuracy  = rbinom(length(p), 1, p))
}

##############################################################################
#  2 · Fit GLMM to a pilot simulation ---------------------------------------
##############################################################################
pilot <- make_H2a()                 # default: 60 % → 50 %
pilot$model_num <- as.numeric(pilot$model)

fit <- glmer(accuracy ~ TaskLevel + (1 | model) + (1 | item),
             data = pilot, family = binomial,
             control = glmerControl(optimizer = "bobyqa",
                                    optCtrl = list(maxfun = 2e5)))
stopifnot(!isSingular(fit))

##############################################################################
#  3 · Power for current design (11 models) ---------------------------------
##############################################################################
pow_current <- mixedpower(model          = fit,
                          data           = pilot,
                          fixed_effects  = "TaskLevel",
                          simvar         = "model_num",
                          steps          = 11,         # current number of LLMs
                          critical_value = 1.96,
                          n_sim          = 1000)
print(pow_current)

##############################################################################
#  4 · Power grid over baselines & drops ------------------------------------
##############################################################################
baselines <- seq(0.20, 0.80, by = 0.05)   # baseline accuracies
drops_pp  <- 10                            # drop in percentage points

grid_power <- expand.grid(b0 = baselines, drop = drops_pp) |>
  rowwise() |>
  mutate(power = {
    pilot_g <- make_H2a(acc_lvl0 = b0, acc_lvl1 = b0 - drop / 100)
    pilot_g$model_num <- as.numeric(pilot_g$model)
    fit_g <- glmer(accuracy ~ TaskLevel + (1 | model) + (1 | item),
                   data = pilot_g, family = binomial)
    mixedpower(model          = fit_g, data = pilot_g,
               fixed_effects  = "TaskLevel",
               simvar         = "model_num",
               steps          = 11,
               critical_value = 1.96,
               n_sim          = 500)[1, 1]
  }) |>
  ungroup()

print(grid_power)

##############################################################################
#  5 · Power grid with 95 % CIs ---------------------------------------------
##############################################################################
grid_power_ci <- expand.grid(b0 = baselines, drop = drops_pp) |>
  rowwise() |>
  mutate(res = list({
    pilot_g <- make_H2a(acc_lvl0 = b0, acc_lvl1 = b0 - drop / 100)
    pilot_g$model_num <- as.numeric(pilot_g$model)
    fit_g <- glmer(accuracy ~ TaskLevel + (1 | model) + (1 | item),
                   data = pilot_g, family = binomial,
                   control = glmerControl(optimizer = "bobyqa",
                                          optCtrl = list(maxfun = 2e5)))
    mixedpower(model          = fit_g, data = pilot_g,
               fixed_effects  = "TaskLevel",
               simvar         = "model_num",
               steps          = 11,
               critical_value = 1.96,
               n_sim          = 1000)
  }),
  power = res$`11`[1],
  lower = res$lower[1],
  upper = res$upper[1]) |>
  ungroup() |>
  select(-res)

print(grid_power_ci)

##############################################################################
#  6 · One‑sample binomial power (H2b) --------------------------------------
##############################################################################
#  H0: p = 1/14  vs  H1: p = 0.20, one‑sided α = .05
p_null        <- 1/14
p_alt         <- 0.20 # or .40
alpha         <- 0.05
target_power  <- 0.80

h <- 2 * asin(sqrt(p_alt)) - 2 * asin(sqrt(p_null))  # Cohen’s h

ss <- pwr.p.test(h = h, sig.level = alpha,
                 power = target_power, alternative = "greater")

cat("\nMinimum n for 80 % power (one‑sample exact binomial):",
    ceiling(ss$n), "trials\n")
