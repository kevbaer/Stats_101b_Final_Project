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
