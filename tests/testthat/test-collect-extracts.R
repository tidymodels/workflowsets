skip_on_cran()

test_that("collect_extracts works", {
  set.seed(1)
  folds <- rsample::vfold_cv(mtcars, v = 3)

  wflow_set <-
    workflow_set(
      list(reg = mpg ~ ., nonlin = mpg ~ wt + 1 / sqrt(disp)),
      list(lm = parsnip::linear_reg())
    )

  wflow_set_trained <-
    wflow_set |>
    workflow_map(
      "fit_resamples",
      resamples = folds,
      control = tune::control_resamples(extract = function(x) {
        x
      })
    )

  extracts <- collect_extracts(wflow_set_trained)

  expect_equal(nrow(extracts), 6)
  expect_contains(
    class(extracts$.extracts[[1]]),
    "workflow"
  )
  expect_named(extracts, c("wflow_id", "id", ".extracts", ".config"))
})


test_that("collect_extracts fails gracefully without .extracts column", {
  set.seed(1)
  folds <- rsample::vfold_cv(mtcars, v = 3)

  wflow_set <-
    workflow_set(
      list(reg = mpg ~ ., nonlin = mpg ~ wt + 1 / sqrt(disp)),
      list(lm = parsnip::linear_reg())
    )

  wflow_set_trained <-
    wflow_set |>
    workflow_map("fit_resamples", resamples = folds)

  expect_snapshot(
    error = TRUE,
    collect_extracts(wflow_set_trained)
  )
})
