library(ggfortify)
library(patchwork)
library(ggview)

plot_resid_1 <- reg_model |>
  autoplot(which = 1, label.n = 0, alpha = .5) |>
  pluck("plots") |>
  pluck(1) +
  canvas(width = 8, height = 6)


plot_resid_2 <- reg_model |>
  autoplot(
    which = 2,
    label.n = 0,
    alpha = .5,
    ad.colour = "#b3114b",
    ad.size = 1
  ) |>
  pluck("plots") |>
  pluck(1) +
  canvas(width = 8, height = 6)

plot_resid_3 <- reg_model |>
  autoplot(which = 3, label.n = 0, alpha = .5) |>
  pluck("plots") |>
  pluck(1) +
  canvas(width = 8, height = 6)

plot_resid_4 <- reg_model |>
  autoplot(which = 5, label.n = 0, alpha = .8) |>
  pluck("plots") |>
  pluck(1) +
  canvas(width = 8, height = 6) +
  theme(
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    panel.grid.major.x = element_blank()
  ) +
  scale_x_discrete(expand = expansion(mult = c(0.03, 0.03)))

plot_final_resid <- (plot_resid_1 + plot_resid_2) /
  (plot_resid_3 + plot_resid_4) +
  canvas(width = 10, height = 7)
