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
