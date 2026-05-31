library(pwr)

pwr.t.test(
  n = NULL,
  d = .5,
  sig.level = 0.05,
  power = .8,
  type = "paired",
  alternative = "two.sided"
)

# n = 33.36713, round up to 34.


pwr.anova.test(
  k = 2,
  n = NULL,
  f = .5,
  sig.level = .05,
  power = .8
)


library(pwr2)

pwr.2way(
  a = 4, # Caffeine Level
  b = 2, # Age
  alpha = 0.05,
  size.A = 34,
  size.B = 34,
  f.A = 0.25,
  f.B = 0.25
)

