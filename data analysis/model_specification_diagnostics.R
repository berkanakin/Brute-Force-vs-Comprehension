library(tidyverse)
library(lme4)          # GLMMs
library(lmerTest)      # p-values for lme4
library(emmeans)       # contrasts & trends
library(performance)   # diagnostics
library(broom.mixed)   # tidy model summaries
library(car)           # Anova() type-III
library(dplyr)


### Model Specification and Diagnostics

#H1 pooled
h1_formula <- revised_accuracy ~ condition_type + scale(input_tokens) + (1 | model) #random intercept

h1_formula <- revised_accuracy ~ condition_type +
  scale(input_tokens) +
  (1 + condition_type | model)  # random intercept *and* slope #boundary (singular) fit: see help('isSingular') for second h1_formuala....

m_h1 <- glmer(h1_formula, family = binomial, data = h1_data,
              control = glmerControl(optimizer = "bobyqa"))

# anova to test which one is better but the random slope one doesnt converge anyways...

#H1 unpooled

h1_formula <- revised_accuracy ~ condition_type * model
#or even
h1_formula <- revised_accuracy ~ condition_type * model + offset(logit_p) # Warning message: glm.fit: fitted probabilities numerically 0 or 1 occurred 

m_h1_fixed <- glm(h1_formula, family = binomial, data = h1_data)
summary(m_h1_fixed)

#H2 

#H2a

#Pooled

# Model 1: Random intercepts only
m_intercept <- glmer(
  revised_accuracy ~ task_level_num + logit_p_z + (1 | model) + (1 | item_id),
  family = binomial,
  data = h2_data,
  control = glmerControl(optimizer = "bobyqa")
)

# Model 2: Random slopes by model
m_slope <- glmer(
  revised_accuracy ~ task_level_num + logit_p_z + (1 + task_level_num | model) + (1 | item_id),
  family = binomial,
  data = h2_data,
  control = glmerControl(optimizer = "bobyqa")
)


# Likelihood-ratio test (nested model comparison)
anova(m_intercept, m_slope)

# lower AIC for m_slope

## final model

m_overall2 <- glmer(
  revised_accuracy ~ task_level_num + logit_p_z + # + input_tokens_z (with input tokens i think it doesnt converge)
    (1 + task_level_num | model) +
    (1 | item_id),
  family = binomial,
  data   = h2_data,
  control = glmerControl(optimizer = "bobyqa")
)

#H2a Unpooled

# Unpooled model with interaction: accuracy ~ task_level * model
m_H2a_unpooled <- glmer(
  revised_accuracy ~ task_level * model + logit_p_z +
    (1 | item_id),
  family = binomial,
  data   = h2_data,
  control = glmerControl(optimizer = "bobyqa") # model failed to converge
)

# Unpooled model with interaction: accuracy ~ task_level * model
m_H2a_unpooled <- glm( 
  revised_accuracy ~ task_level * model + logit_p_z,
  family = binomial,
  data   = h2_data
) # this one worked

###

m_quad <- update(m_overall,
                 formula = . ~ poly(task_level_num, 2, raw = TRUE) +
                   + logit_p + (1 + task_level_num | model) + (1 | trial))

h2_data <- h2_data %>%
  mutate(level_log = log1p(task_level_num),
         level_exp = exp(task_level_num))

m_log <- update(m_overall, formula = . ~ level_log + logit_p + (1 + task_level_num | model) + (1 | trial))
m_exp <- update(m_overall, formula = . ~ level_exp + logit_p + (1 + task_level_num | model) + (1 | trial))

## these could be done to see how the decline is but i dont know if its necessary

AIC(m_overall, m_quad, m_log, m_exp)
# Linear model (m_overall) has the smallest AIC → keep it

##H2b

m_H2b_unpooled <- glm(
  revised_accuracy ~ task_level * model + offset(logit_p_z),
  family = binomial,
  data   = h2_data
)


##H2c)

m_H2c <- glmer(
  revised_accuracy ~ task_level_num*log_size + logit_p_z + input_tokens_z +
    (1 + task_level_num | model) +
    (1 | item_id),
  family = binomial,
  data   = h2_data,
  control = glmerControl(optimizer = "bobyqa")
) # this one also worked out

summary(m_H2c)





