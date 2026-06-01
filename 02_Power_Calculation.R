library(pwr2)

pwr.2way(
  a = 4, # Caffeine Level
  b = 2, # Age
  alpha = 0.05,
  size.A = 24,
  size.B = 24,
  f.A = 0.25,
  f.B = 0.25
)

# power = .83

pwr.2way(
  a = 4, # Caffeine Level
  b = 2, # Age
  alpha = 0.05,
  size.A = 34,
  size.B = 34,
  f.A = 0.25,
  f.B = 0.25
)

# power = .94
