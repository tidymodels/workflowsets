skip_on_cran()

test_that("collect_notes works", {
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
        warn("hey!")
      })
    )

  expect_snapshot(error = TRUE, collect_notes(wflow_set))
  notes <- collect_notes(wflow_set_trained)

  expect_equal(nrow(notes), 6)
  expect_contains(notes$note, "hey!")
  expect_named(notes, c("wflow_id", "id", "location", "type", "note"))
})
