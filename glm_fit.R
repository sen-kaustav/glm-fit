library(tidyverse)
library(arrow)
library(broom)
library(showtext)

font_add_google("Inter", "Inter")
showtext_auto()

data <- read_parquet(fs::path("data", "motor_data.parquet"))

fct_vars <- data |>
  select(where(is.factor)) |>
  names()

pred_vars <- fct_vars

formula <- as.formula(
  paste(
    "Claim_Count ~ ",
    paste(pred_vars, collapse = "+"),
    "+ offset(log(Exposure))"
  )
)

mod <- glm(formula, data = data, family = poisson("log"))

df_coefs <- mod |>
  tidy()

make_relativity_plot(data, df_coefs, "Area")

data |>
  count(License_Age_bin, wt = Exposure) |>
  mutate(prop = n / sum(n))

coef <- "License_Age_bin"

make_relativity_plot <- function(df_data, df_coef, coef) {
  df_plot <- df_data |>
    summarise(Exposure = sum(Exposure), .by = .data[[coef]]) |>
    mutate(
      Exposure_Prop = Exposure / sum(Exposure),
      var_name = paste0(.env$coef, .data[[coef]])
    ) |>
    left_join(
      select(df_coef, term, relativity = estimate),
      by = join_by(var_name == term)
    ) |>
    mutate(relativity = replace_na(relativity, 1))

  max_relativity <- max(df_plot$relativity)
  max_exposure_prop <- max(df_plot$Exposure_Prop)

  ggplot(df_plot, aes(x = .data[[coef]])) +
    geom_col(aes(.data[[coef]], Exposure_Prop), fill = "grey70", alpha = 0.6) +
    geom_col(
      aes(.data[[coef]], Exposure_Prop),
      data = filter(df_plot, relativity == 1),
      fill = "grey50"
    ) +
    geom_line(
      aes(
        y = relativity / max_relativity * max_exposure_prop,
        group = 1
      ),
      color = "#422CB2",
      linewidth = 1.1,
      alpha = 0.7
    ) +
    geom_point(
      aes(y = relativity / max_relativity * max_exposure_prop),
      color = "#260F99",
      size = 3
    ) +
    geom_hline(
      yintercept = 1 / max_relativity * max_exposure_prop,
      color = "#422CB2",
      linetype = "dashed",
      alpha = 0.5,
      linewidth = 1.05,
    ) +
    annotate(
      "text",
      x = max(as.numeric(df_plot[[coef]])),
      y = (1 / max_relativity * max_exposure_prop) + 0.01 * max_exposure_prop,
      label = "Baseline",
      color = "#422CB2",
      size = 3,
      hjust = 1,
      vjust = 0,
      fontface = "bold",
      family = "Inter"
    ) +
    scale_y_continuous(
      name = "Exposure (%)",
      labels = scales::percent_format(accuracy = 1),
      sec.axis = sec_axis(
        \(y) y * max_relativity / max_exposure_prop,
        name = "Relativity",
        labels = scales::percent_format(accuracy = 1)
      ),
    ) +
    labs(title = paste0("Relativity plot for ", coef)) +
    theme_minimal(base_family = "Inter", base_size = 10) +
    theme(
      plot.title = element_text(face = "bold", size = rel(1.6)),
      axis.title.y.left = element_text(
        face = "bold",
        color = "grey40",
        margin = margin(r = 10)
      ),
      axis.title.y.right = element_text(
        face = "bold",
        color = "#260F99",
        margin = margin(l = 10)
      ),
      axis.text.y.right = element_text(
        color = "#260F99",
        margin = margin(l = 10)
      ),
      axis.title.x = element_text(
        face = "bold",
        color = "grey70",
        size = rel(0.9),
        hjust = 1,
        margin = margin(t = 10)
      )
    )
}
