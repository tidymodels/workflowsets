skip_on_cran()
skip_if_not_installed("kknn")
skip_if_not_installed("modeldata")

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
  ) |>
  workflow_map(
    "fit_resamples",
    resamples = vfold_cv(mtcars, v = 3),
    control = tune::control_resamples(save_pred = TRUE)
  )

set.seed(1)
resamples <- vfold_cv(mtcars, v = 3, repeats = 2)

set.seed(1)
car_set_2 <-
  workflow_set(
    list(reg = mpg ~ ., nonlin = mpg ~ wt + 1 / sqrt(disp)),
    list(lm = lr_spec)
  ) |>
  workflow_map(
    "fit_resamples",
    resamples = resamples,
    control = tune::control_resamples(save_pred = TRUE)
  )

set.seed(1)
car_set_3 <-
  workflow_set(
    list(reg = mpg ~ ., nonlin = mpg ~ wt + 1 / sqrt(disp)),
    list(knn = knn_spec)
  ) |>
  workflow_map(
    "tune_bayes",
    resamples = resamples,
    control = tune::control_bayes(save_pred = TRUE),
    seed = 1,
    iter = 2,
    initial = 3
  )

car_set_23 <- dplyr::bind_rows(car_set_2, car_set_3)

# ------------------------------------------------------------------------------

check_prediction_results <- function(ind, x, summarize = FALSE, ...) {
  id_val <- x$wflow_id[ind]

  cols <- c(".row", "mpg", ".config", ".pred")

  orig <-
    collect_predictions(x$result[[ind]], summarize = summarize, ...) |>
    dplyr::select(dplyr::all_of(cols))

  if (any(names(list(...)) == "summarize")) {
    cols <- c(grep("^id", names(orig), value = TRUE), cols)
  }

  everythng <-
    collect_predictions(x, summarize = summarize, ...) |>
    dplyr::filter(wflow_id == id_val) |>
    dplyr::select(dplyr::all_of(cols))
  all.equal(orig, everythng)
}

# ------------------------------------------------------------------------------

test_that("collect predictions", {
  expect_no_error(
    res_car_set_1 <- collect_predictions(car_set_1)
  )
  expect_true(nrow(mtcars) * nrow(car_set_1) == nrow(res_car_set_1))

  expect_no_error(
    res_car_set_2 <- collect_predictions(car_set_2)
  )
  expect_true(nrow(mtcars) * nrow(car_set_2) == nrow(res_car_set_2))

  expect_no_error(
    res_car_set_2_reps <- collect_predictions(car_set_2, summarize = FALSE)
  )
  expect_true(nrow(mtcars) * nrow(car_set_2) * 2 == nrow(res_car_set_2_reps))

  expect_no_error(
    res_car_set_3 <- collect_predictions(car_set_3)
  )
  expect_true(nrow(mtcars) * nrow(car_set_2) * 5 == nrow(res_car_set_3))

  expect_no_error(
    res_car_set_3_reps <- collect_predictions(car_set_3, summarize = FALSE)
  )
  expect_true(
    nrow(mtcars) * nrow(car_set_2) * 5 * 2 == nrow(res_car_set_3_reps)
  )

  # ---------------------------------------------------------------------------
  # These don't seem to get captured by covr
  for (i in 1:nrow(car_set_1)) {
    expect_true(check_prediction_results(i, car_set_1))
  }
  for (i in 1:nrow(car_set_2)) {
    expect_true(check_prediction_results(i, car_set_2))
  }

  for (i in 1:nrow(car_set_1)) {
    expect_true(check_prediction_results(i, car_set_1, summarize = FALSE))
  }
  for (i in 1:nrow(car_set_2)) {
    expect_true(check_prediction_results(i, car_set_2, summarize = FALSE))
  }
})

skip_if(packageVersion("tune") <= "1.1.0")

test_that("dropping tuning parameter columns", {
  expect_named(
    collect_predictions(car_set_1),
    c("wflow_id", ".config", "preproc", "model", ".row", "mpg", ".pred"),
    ignore.order = TRUE
  )
  expect_named(
    collect_predictions(car_set_2),
    c("wflow_id", ".config", "preproc", "model", ".row", "mpg", ".pred"),
    ignore.order = TRUE
  )

  expect_named(
    collect_predictions(car_set_1, summarize = FALSE),
    c("wflow_id", ".config", "preproc", "model", "id", ".pred", ".row", "mpg"),
    ignore.order = TRUE
  )
  expect_named(
    collect_predictions(car_set_2, summarize = FALSE),
    c(
      "wflow_id",
      ".config",
      "preproc",
      "model",
      "id",
      "id2",
      ".pred",
      ".row",
      "mpg"
    ),
    ignore.order = TRUE
  )

  expect_no_error(
    best_iter <- collect_predictions(
      car_set_3,
      select_best = TRUE,
      metric = "rmse"
    )
  )
  expect_true(
    nrow(dplyr::distinct(best_iter[, c(".config", "wflow_id")])) == 2
  )
  expect_no_error(
    no_param <-
      select_bare_predictions(car_set_3$result[[1]], metric = "rmse", TRUE)
  )
  expect_named(
    no_param,
    c(".row", "mpg", ".config", ".iter", ".pred"),
    ignore.order = TRUE
  )
})


test_that("mixed object types", {
  expect_true(".iter" %in% names(collect_predictions(car_set_23)))
})
