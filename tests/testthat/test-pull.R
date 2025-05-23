library(parsnip)
library(rsample)

lr_spec <- linear_reg() |> set_engine("lm")

set.seed(1)
car_set_1 <-
  workflow_set(
    list(reg = mpg ~ ., nonlin = mpg ~ wt + 1 / sqrt(disp)),
    list(lm = lr_spec)
  ) |>
  workflow_map(
    "fit_resamples",
    resamples = vfold_cv(mtcars, v = 3),
    control = tune::control_resamples(save_pred = TRUE)
  )

# ------------------------------------------------------------------------------

test_that("pulling objects is deprecated", {
  expect_snapshot(
    error = TRUE,
    car_set_1 |> pull_workflow_set_result("reg_lm")
  )
  expect_snapshot(
    error = TRUE,
    car_set_1 |> pull_workflow("reg_lm")
  )
})
