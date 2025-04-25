skip_on_cran()

# ------------------------------------------------------------------------------

library(parsnip)
suppressPackageStartupMessages(library(rsample))
suppressPackageStartupMessages(library(tune))

# ------------------------------------------------------------------------------

lr_spec <- linear_reg() |> set_engine("lm")
knn_spec <-
  nearest_neighbor(neighbors = tune()) |>
  set_engine("kknn") |>
  set_mode("regression")

set.seed(1)
car_set_1 <-
  workflow_set(
    list(reg = mpg ~ ., nonlin = mpg ~ wt + 1 / sqrt(disp)),
    list(lm = lr_spec)
  )

car_set_2 <-
  car_set_1 |>
  workflow_map(
    "fit_resamples",
    resamples = vfold_cv(mtcars, v = 3),
    control = tune::control_resamples(save_pred = TRUE)
  )

test_that("predict() errors informatively with workflow sets", {
  expect_snapshot(predict(car_set_1), error = TRUE)

  expect_snapshot(predict(car_set_2), error = TRUE)
})
