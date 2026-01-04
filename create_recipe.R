create_recipe <- function(vars_cat, vars_cont_df, formula, train_df) {
  rec <- recipe(formula, train_df)

  vars_to_log <- filter(vars_cont_df, scale == "Log") |> pull(var)
  if (length(vars_to_log) != 0) {
    rec <- rec |>
      step_log(all_of(vars_to_log))
  }

  vars_to_sqrt <- filter(vars_cont_df, scale == "Sqrt") |> pull(var)
  if (length(vars_to_sqrt) != 0) {
    rec <- rec |>
      step_sqrt(all_of(vars_to_sqrt))
  }

  vars_to_centre <- filter(vars_cont_df, transform == "Centre") |> pull(var)
  if (length(vars_to_centre) != 0) {
    rec <- rec |>
      step_center(all_of(vars_to_centre))
  }

  vars_to_normalise <- filter(vars_cont_df, transform == "Normalise") |>
    pull(var)
  if (length(vars_to_normalise) != 0) {
    rec <- rec |>
      step_normalize(all_of(vars_to_normalise))
  }

  vars_to_poly_1 <- filter(vars_cont_df, poly == 1) |> pull(var)
  if (length(vars_to_poly_1) != 0) {
    rec <- rec |>
      step_poly(all_of(vars_to_poly_1), degree = 1)
  }

  vars_to_poly_2 <- filter(vars_cont_df, poly == 2) |> pull(var)
  if (length(vars_to_poly_2) != 0) {
    rec <- rec |>
      step_poly(all_of(vars_to_poly_2), degree = 2)
  }

  vars_to_poly_3 <- filter(vars_cont_df, poly == 3) |> pull(var)
  if (length(vars_to_poly_3) != 0) {
    rec <- rec |>
      step_poly(all_of(vars_to_poly_3), degree = 3)
  }

  rec
}
