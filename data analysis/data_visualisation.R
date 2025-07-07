# ─────────────────────────────────────────────────────────────
# 1.  Packages
# ─────────────────────────────────────────────────────────────
library(lme4)
library(emmeans)      # for model-based marginal means
library(dplyr)
library(ggplot2)
library(ggthemes)     # clean theme base
library(cowplot)      # nice APA title spacing

m_slope <- m_overall2
# ─────────────────────────────────────────────────────────────
# 2.  Model-based marginal means (+ SE) from m_slope
# ─────────────────────────────────────────────────────────────

library(dplyr)

emm_tbl <- emmeans(
  m_slope,
  ~ task_level,
  type = "response"
) |>
  as.data.frame() |>
  dplyr::rename(
    mean_acc   = prob,
    se         = SE,
    task_level = task_level  # redundant, unless renaming
  )


# ─────────────────────────────────────────────────────────────
# 3.  Chance accuracy baseline (mean of chance_prob by level)
# ─────────────────────────────────────────────────────────────

chance_tbl <- split(h2_data, h2_data$task_level) |>
  lapply(function(df) mean(df$chance_prob, na.rm = TRUE)) |>
  stack() |>
  setNames(c("mean_chance", "task_level")) |>
  arrange(task_level)



# ─────────────────────────────────────────────────────────────
# 4.  Merge for plotting
# ─────────────────────────────────────────────────────────────
plot_df <- left_join(emm_tbl, chance_tbl, by = "task_level")

# ─────────────────────────────────────────────────────────────
# 5.  APA-7 style plot
# ─────────────────────────────────────────────────────────────
p <- ggplot(plot_df,
            aes(x = factor(task_level),
                y = mean_acc,
                group = 1)) +
  geom_line(size = .8, colour = "black") +
  geom_point(size = 3, colour = "black") +
  geom_errorbar(aes(ymin = mean_acc - se,
                    ymax = mean_acc + se),
                width = .15, linewidth = .6) +
  geom_point(aes(y = mean_chance),
             shape = 17, size = 3, colour = "grey30") +            # triangles
  geom_line(aes(y = mean_chance, group = 2),
            linetype = "dashed", colour = "grey30") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),
                     limits = c(0, max(plot_df$mean_acc + plot_df$se) * 1.1)) +
  labs(x = "Task Level",
       y = "Predicted Accuracy Across LLMs",
       title = "Figure X\nAccuracy Across Meta-Rule Levels",
       subtitle = "Circles = model-adjusted means (±1 SE); triangles = mean chance accuracy") +
  theme_clean(base_size = 11) +                                    # ggthemes::theme_clean()
  theme(
    plot.title      = element_text(face = "bold", size = 12,
                                   margin = margin(b = 6)),
    plot.subtitle   = element_text(size = 10, margin = margin(b = 8)),
    axis.title      = element_text(size = 11),
    axis.text       = element_text(size = 10),
    panel.grid.major.y = element_line(colour = "grey88", linewidth = .3),
    panel.grid.minor = element_blank(),
    legend.position = "none"
  )

ggplot(plot_df, aes(x = factor(task_level))) +
  
  ## ── predicted accuracy (solid line + circle) ─────────────────────────────
  geom_line(aes(y = mean_acc,
                group = 1,
                linetype = "Predicted accuracy"),
            size = .8, colour = "black") +
  geom_point(aes(y = mean_acc,
                 shape = "Predicted accuracy"),
             size = 3,  colour = "black") +
  geom_errorbar(aes(ymin = mean_acc - se,
                    ymax = mean_acc + se),
                width = .15, linewidth = .6,
                show.legend = FALSE) +
  
  ## ── chance level (dashed line + triangle) ───────────────────────────────
  geom_line(aes(y = mean_chance,
                group = 1,
                linetype = "Chance level"),
            size = .8, colour = "grey30") +
  geom_point(aes(y = mean_chance,
                 shape = "Chance level"),
             size = 3,  colour = "grey30") +
  
  ## ── legend definitions ──────────────────────────────────────────────────
  scale_shape_manual(name = NULL,
                     values = c("Predicted accuracy" = 16,
                                "Chance level"       = 17)) +
  scale_linetype_manual(name = NULL,
                        values = c("Predicted accuracy" = "solid",
                                   "Chance level"       = "dashed")) +
  
  ## ── y-axis as percentage ────────────────────────────────────────────────
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),
                     limits = c(0, max(plot_df$mean_acc + plot_df$se) * 1.1)) +
  
  ## ── axis labels ─────────────────────────────────────────────────────────
  labs(x = "Task Level",
       y = "Predicted Accuracy Across LLMs") +
  
  ## ── APA-like theme ──────────────────────────────────────────────────────
  theme_clean(base_size = 11) +
  theme(
    plot.title       = element_text(face = "bold", size = 12,
                                    margin = margin(b = 6),
                                    family = "Times New Roman"),
    plot.subtitle    = element_blank(),
    axis.title       = element_text(size = 11, family = "Times New Roman"),
    axis.text        = element_text(size = 10, family = "Times New Roman"),
    panel.grid.major.y = element_line(colour = "grey88", linewidth = .3),
    panel.grid.minor = element_blank(),
    legend.position  = "bottom",
    legend.text      = element_text(family = "Times New Roman", size = 10)
  )

# ─────────────────────────────────────────────────────────────
# 6.  Save high-resolution versions
# ─────────────────────────────────────────────────────────────
ggsave("FigureX_AccuracyLevels.png", p,
       width = 5, height = 4.5, units = "in", dpi = 600)
ggsave("FigureX_AccuracyLevels.pdf", p,
       width = 5, height = 4.5, units = "in")
