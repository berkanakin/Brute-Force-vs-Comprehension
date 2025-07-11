##############################################################################
#  power_analyses.R
#  Author: Berkan Akin · 2025-07-04
#  Requires: lme4 ≥ 1.1-35, mixedpower (GitHub)
##############################################################################

# Prospective power for Level-0 vs Level-1 (H2a)

## ── 0  packages ────────────────────────────────────────────────────────────
if (!requireNamespace("lme4",     quietly = TRUE)) install.packages("lme4")
if (!requireNamespace("remotes",  quietly = TRUE)) install.packages("remotes")
if (!requireNamespace("mixedpower", quietly = TRUE)) {
  remotes::install_github("DejanDraschkow/mixedpower",
                          build_vignettes = FALSE, upgrade = "never")
}
library(lme4)
library(mixedpower)
library(dplyr)

## ── 1  data generator  ───────────────────────────────────────────────────────
make_H2a <- function(n_models = 11,
                     n_lvl0   = 268,
                     n_lvl1   = 65,
                     acc_lvl0 = 0.60,   # baseline accuracy
                     acc_lvl1 = 0.50,   # accuracy at level-1
                     sd_model = 0.40,
                     sd_item  = 0.50,
                     seed = 42) {
  
  set.seed(seed)
  total_items <- n_lvl0 + n_lvl1
  
  model  <- factor(rep(seq_len(n_models), each = total_items))
  lvl    <- factor(rep(c(rep(0, n_lvl0), rep(1, n_lvl1)), times = n_models))
  
  # random intercepts
  u_model <- rnorm(n_models, 0, sd_model)[model]
  u_item  <- rnorm(total_items, 0, sd_item)[rep(seq_len(total_items), n_models)]
  
  # fixed effects
  logit0 <- qlogis(acc_lvl0)
  delta  <- qlogis(acc_lvl1) - logit0            # effect in log-odds
  
  eta <- logit0 + ifelse(lvl == 1, delta, 0) + u_model + u_item
  p   <- pmin(pmax(plogis(eta), 1e-4), 1 - 1e-4)  # clamp
  
  data.frame(model = model,
             item  = rep(seq_len(total_items), n_models),
             TaskLevel = lvl,
             accuracy  = rbinom(length(p), 1, p))
}

## ── 2  fit GLMM for the planned design ─────────────────────────────────────
pilot <- make_H2a()                 # 60 % → 50 % example
pilot$model_num <- as.numeric(pilot$model)

fit  <- glmer(accuracy ~ TaskLevel +
                (1 | model) + (1 | item),
              data = pilot, family = binomial,
              control = glmerControl(optimizer = "bobyqa",
                                     optCtrl = list(maxfun = 2e5)))
stopifnot(!isSingular(fit))

## ── 3  power for current design (11 models) ────────────────────────────────
pow_current <- mixedpower(
  model          = fit,
  data           = pilot,
  fixed_effects  = "TaskLevel",
  simvar         = "model_num",
  steps          = 11,        # current number of LLMs
  critical_value = 1.96,
  n_sim          = 1000
)
print(pow_current)

## ── 4  power grid: baselines 10–90 %, drops 5/7/10 pp ─────────────────────
baselines <- seq(0.20, 0.80, by = 0.05)
drops_pp  <- 10 # alternatively: c(5, 7, 10)

grid_power <- expand.grid(b0 = baselines, drop = drops_pp) |>
  rowwise() |>
  mutate(power = {
    pilot_g <- make_H2a(acc_lvl0 = b0,
                        acc_lvl1 = b0 - drop / 100)
    pilot_g$model_num <- as.numeric(pilot_g$model)
    fit_g   <- glmer(accuracy ~ TaskLevel + (1|model) + (1|item),
                     data = pilot_g, family = binomial)
    mixedpower(model = fit_g, data = pilot_g,
               fixed_effects = "TaskLevel",
               simvar = "model_num",
               steps = 11, critical_value = 1.96,
               n_sim = 500)[1,1]
  }) |>
  ungroup()

print(grid_power)


#### With confidence intervals

grid_power <- expand.grid(b0 = baselines, drop = drops_pp) |>
  rowwise() |>
  mutate(
    # -- run one simulation per row ------------------------------------------
    res = list({
      pilot_g <- make_H2a(acc_lvl0 = b0,
                          acc_lvl1 = b0 - drop / 100)
      pilot_g$model_num <- as.numeric(pilot_g$model)
      
      fit_g <- glmer(accuracy ~ TaskLevel + (1 | model) + (1 | item),
                     data = pilot_g, family = binomial,
                     control = glmerControl(optimizer = "bobyqa",
                                            optCtrl = list(maxfun = 2e5)))
      
      mixedpower(model = fit_g, data = pilot_g,
                 fixed_effects  = "TaskLevel",
                 simvar         = "model_num",
                 steps          = 11,
                 critical_value = 1.96,
                 n_sim          = 1000)   # or 500 if you need it faster
    }),
    # -- extract point estimate and 95 % CI ----------------------------------
    power = res$`11`[1],
    lower = res$lower[1],
    upper = res$upper[1]
  ) |>
  ungroup() |>
  select(-res)            # drop helper column

print(grid_power)

## Poweranalyses as part of the exploratory analyses

# comparison against chance power analysis (H2b-Level 2)

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
p_alt  <- 0.20          # observed accuracy: =0.20-0.40 
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





