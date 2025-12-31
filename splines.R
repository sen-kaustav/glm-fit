library(tidyverse)
library(tidymodels)
library(splines)

x <- seq_len(100)
y <- sin(20 * x * pi / 180) +
  0.01 * x +
  0.001 * (x - 50)^2 +
  0.00002 * (x - 50)^3

df <- tibble(x, y)

mod1 <- linear_reg() |>
  fit(y ~ ns(x, knots = c(40, 60), Boundary.knots = c(20, 80)), data = df)

degree <- 3
mod2 <- linear_reg() |>
  fit(
    y ~ bs(x, df = 3 + degree, degree = degree),
    data = df
  )

augment(mod2, df) |>
  ggplot(aes(x, .pred)) +
  geom_point(aes(y = y), alpha = 0.4, size = 2) +
  geom_line(color = "royalblue", size = 1) +
  geom_vline(
    xintercept = quantile(df$x, c(0.25, 0.5, 0.75)),
    linetype = "dashed",
    color = "grey70"
  ) +
  scale_y_continuous(limits = c(-1, 7)) +
  theme_minimal(base_family = "Inter") +
  theme(panel.grid.minor = element_blank())
