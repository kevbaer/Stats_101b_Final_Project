library(tinytable)

Anova_Interaction <- aov(
  SCORE ~ TREATMENT * SEX + Error(NAME),
  data = dat_long
) |>
  broom::tidy() |>
  tt() |>
  format_tt(replace = "", digits = 3) |>
  style_tt(3, "p.value", background = "#8ee7af") |>
  style_tt(2, 1:7, line = "b", line_width = .1, line_color = "grey40")

reg_model <- lm(SCORE ~ TREATMENT + SEX + NAME, data = dat_long)

#TukeyHSD------------------

library(emmeans)

tukey_model <- aov(
  SCORE ~ CLEAN_TREATMENT + Error(NAME),
  data = dat_long
)

Tukey_Comparison_Table <- emmeans(
  tukey_model,
  specs = revpairwise ~ CLEAN_TREATMENT,
  adjust = "tukey"
) |>
  SimDesign::quiet() |>
  pluck("contrasts") |>
  broom::tidy() |>
  filter(str_detect(contrast, "Control")) |>
  select(-term) |>
  tt() |>
  format_tt(replace = "", digits = 3) |>
  style_tt(2, "adj.p.value", background = "#8ee7af")


Tukey_Comparison_Plot <- emmeans(
  tukey_model,
  ~CLEAN_TREATMENT,
  adjust = "tukey"
) |>
  contrast(method = "trt.vs.ctrl") |>
  SimDesign::quiet() |>
  plot() +
  theme_bw(base_size = 16, base_family = "Barlow") +
  scale_y_discrete(
    labels = ~ str_remove(.x, " - Control")
  ) +
  labs(
    y = NULL,
    title = "Tukey Estimates for Difference from Control",
    subtitle = "95% Confidence Interval",
    x = "Difference from Control"
  ) +
  ggview::canvas(width = 8, height = 6) +
  theme(plot.title.position = "plot") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "#b3114b")
