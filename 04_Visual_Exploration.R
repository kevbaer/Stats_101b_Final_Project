library(tidyverse)
library(googlesheets4)
set_theme(theme_bw(base_size = 16, base_family = "Barlow"))


add_clean_treatment <- function(X) {
  X |>
    mutate(
      CLEAN_TREATMENT = recode_values(
        TREATMENT,
        from = c("METH", "CLEAN", "TABLET", "DOUBLE"),
        to = c(
          "Methamphetamine",
          "Control",
          "Single Caffeine",
          "Double Caffeine"
        )
      )
    ) |>
    mutate(
      CLEAN_TREATMENT = factor(
        CLEAN_TREATMENT,
        levels = c(
          "Control",
          "Single Caffeine",
          "Double Caffeine",
          "Methamphetamine"
        )
      )
    )
}

raw_file <- read_sheet("1ZgRN1rhLDxdqz7HC7zoD6mphQ61XnV881gx_QjguWys")

dat <- raw_file |>
  filter(COLLECTOR == "Kevin") |>
  select(NAME, SEX, starts_with("TOTAL_"))

dat_long <- dat |>
  pivot_longer(
    starts_with("total"),
    names_prefix = "TOTAL_SCORE_",
    names_to = "TREATMENT",
    values_to = "SCORE"
  ) |>
  add_clean_treatment()

dat_diff_long <- dat |>
  mutate(
    DIFF_TABLET = TOTAL_SCORE_TABLET - TOTAL_SCORE_CLEAN,
    DIFF_METH = TOTAL_SCORE_METH - TOTAL_SCORE_CLEAN,
    DIFF_DOUBLE = TOTAL_SCORE_DOUBLE - TOTAL_SCORE_CLEAN
  ) |>
  select(NAME, SEX, starts_with("DIFF_")) |>
  pivot_longer(
    starts_with("DIFF_"),
    names_prefix = "DIFF_",
    names_to = "TREATMENT",
    values_to = "DIFFERENCE"
  ) |>
  add_clean_treatment()


# Plotting ----------------------------------------------------------------

Distribution_of_Scores <- dat_long |>
  ggplot() +
  aes(x = SCORE) +
  geom_density(fill = "#ffea88") +
  geom_dotplot(binwidth = .3, dotsize = .3) +
  facet_wrap(~CLEAN_TREATMENT, ncol = 1) +
  scale_x_continuous(
    limits = c(0, 6),
    n.breaks = 7,
    expand = expansion(mult = c(0.03, 0.03))
  ) +
  scale_y_continuous(labels = NULL) +
  theme(
    axis.ticks.y = element_blank(),
    strip.background = element_rect(fill = "#b2e3ff")
  ) +
  labs(y = NULL, x = "Score", title = "Distribution of Scores") +
  ggview::canvas(width = 6, height = 10)

Individual_Scores_Across_Treatments <- dat_long |>
  ggplot() +
  aes(x = CLEAN_TREATMENT, y = SCORE, fill = NAME, color = NAME, group = NAME) +
  geom_point() +
  geom_line() +
  facet_wrap(~NAME) +
  theme(legend.position = "none")

Differences_from_Control <- dat_diff_long |>
  ggplot() +
  aes(x = DIFFERENCE) +
  geom_density(fill = "#ffea88") +
  geom_dotplot(binwidth = .3, dotsize = .3) +
  facet_wrap(~CLEAN_TREATMENT, ncol = 1) +
  scale_y_continuous(labels = NULL) +
  theme(
    axis.ticks.y = element_blank(),
    strip.background = element_rect(fill = "#b2e3ff")
  ) +
  labs(
    y = NULL,
    x = "Differences from Control",
    title = "Distribution of Differences"
  ) +
  ggview::canvas(width = 6, height = 6)


Interaction_Plot <- dat_long |>
  summarize(m_SCORE = mean(SCORE), .by = c(SEX, CLEAN_TREATMENT)) |>
  mutate(SEX_LABEL = ifelse(CLEAN_TREATMENT == "Methamphetamine", SEX, "")) |>
  ggplot() +
  aes(x = CLEAN_TREATMENT, y = m_SCORE, group = SEX, color = SEX) +
  geom_point() +
  geom_line() +
  ggview::canvas(width = 8, height = 6) +
  scale_color_manual(values = c("#2db25f", "#5c4cbf")) +
  ggrepel::geom_text_repel(
    aes(label = SEX_LABEL),
    size = 8,
    nudge_x = .2,
    min.segment.length = Inf
  ) +
  guides(color = "none") +
  labs(x = NULL, y = "Average Score", title = "Interaction Plot")


# For Later ---------------------------------------------------------------

aov(SCORE ~ DRUG + SEX + Error(NAME), data = dat_long) |>
  summary()
