library(tidyverse)
library(tidymodels)
library(poissonreg)
library(arrow)

data <- read_parquet(r"(C:\glm-clean\clean_data\motor_data.parquet)")


rec_poly <- recipe(data, Claim_Count ~ Value_vehicle) |>
  step_log(Value_vehicle) |>
  step_normalize(Value_vehicle) |>
  step_poly(
    all_of("Value_vehicle"),
    degree = 3,
    options = list(raw = FALSE),
    keep_original_cols = FALSE
  )

df <- rec_poly |>
  prep() |>
  bake(new_data = NULL)

rec_spec <- recipe(
  data,
  Claim_Count ~ Value_vehicle + Seniority_bin + Type_risk + Area + Exposure
) |>
  step_log(Value_vehicle) |>
  step_center(Value_vehicle) |>
  step_bs(Value_vehicle, deg_free = 6)

# rec_spec |>
#   prep() |>
#   bake(new_data = NULL) |>
#   names()

mod_spec <- poisson_reg() |>
  set_engine("glm", family = poisson("log"))

wflow <- workflow() |>
  add_recipe(rec_spec) |>
  add_model(
    mod_spec,
    formula = Claim_Count ~ . -
      Exposure +
      offset(log(Exposure))
  ) |>
  fit(data = data)

wflow |> tidy(exponentiate = TRUE) |> print(n = 30)

data |>
  select(Value_vehicle) |>
  summarise(
    min_vehicle_value = min(Value_vehicle),
    max_vehicle_value = max(Value_vehicle),
    mean_vehicle_value = mean(Value_vehicle),
    median_vehicle_value = median(Value_vehicle),
  )

df_coef <- wflow |>
  tidy(exponentiate = TRUE) |>
  filter(grepl("Value_vehicle", term, fixed = TRUE)) |>
  select(term, estimate)

df_baseline <- data |>
  mutate(
    across(where(is.factor), list(baseline = \(x) levels(x)[[1]])),
    across(where(is.numeric), list(mean = \(x) mean(x)))
  ) |>
  select(ends_with("_baseline"), ends_with("_mean")) |>
  distinct() |>
  rename_with(\(x) str_remove(x, "_baseline|_mean$")) |>
  mutate(Exposure = 1, across(where(is.character), as.factor)) |>
  select(-c(ID, Claim_Count, Claim_Amount))

min_Value_vehicle <- min(data$Value_vehicle)
max_Value_vehicle <- max(data$Value_vehicle)
mean_Value_vehicle <- mean(data$Value_vehicle)

Value_vehicle_vec <- seq(
  from = min_Value_vehicle,
  # from  = 5000,
  to = max_Value_vehicle,
  length.out = 500
)

new_data <-
  expand_grid(
    df_baseline |> select(-Value_vehicle),
    Value_vehicle = c(mean_Value_vehicle, Value_vehicle_vec)
  )

pred <- wflow |>
  predict(new_data = new_data, type = "raw")

new_data |>
  mutate(
    relativity = pred / pred[1]
  ) |>
  arrange(Value_vehicle) |>
  select(Value_vehicle, relativity) |>
  ggplot(aes(Value_vehicle, relativity)) +
  geom_line() +
  geom_vline(
    xintercept = mean_Value_vehicle,
    color = "grey60",
    linetype = "dashed"
  )
