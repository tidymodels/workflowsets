skip_if_not_installed("kknn")
skip_if_not_installed("modeldata")

library(parsnip)
suppressPackageStartupMessages(library(rsample))
suppressPackageStartupMessages(library(tune))
library(kknn)

# ------------------------------------------------------------------------------

lr_spec <- linear_reg() |> set_engine("lm")
knn_spec <-
  nearest_neighbor(neighbors = tune()) |>
  set_engine("kknn") |>
  set_mode("regression")
glmn_spec <-
  linear_reg(penalty = tune()) |>
  set_engine("glmnet")

set.seed(1)
folds <- vfold_cv(mtcars, v = 3)

car_set_1 <-
  workflow_set(
    list(reg = mpg ~ ., nonlin = mpg ~ wt + 1 / sqrt(disp)),
    list(lm = lr_spec, knn = knn_spec)
  ) |>
  dplyr::slice(-4)

# ------------------------------------------------------------------------------

test_that("basic mapping", {
  expect_no_error({
    res_1 <-
      car_set_1 |>
      workflow_map(resamples = folds, seed = 2, grid = 2)
  })

  # check reproducibility
  expect_no_error({
    res_2 <-
      car_set_1 |>
      workflow_map(resamples = folds, seed = 2, grid = 2)
  })
  expect_equal(collect_metrics(res_1), collect_metrics(res_2))

  # ---------------------------------------------------------------------------

  expect_snapshot(
    error = TRUE,
    two_class_set |>
      workflow_map("foo", seed = 1, resamples = folds, grid = 2)
  )

  expect_snapshot(
    error = TRUE,
    two_class_set |>
      workflow_map(fn = 1L, seed = 1, resamples = folds, grid = 2)
  )

  expect_snapshot(
    error = TRUE,
    two_class_set |>
      workflow_map(fn = tune::tune_grid, seed = 1, resamples = folds, grid = 2)
  )
})


test_that("map logging", {
  # since the logging prints execution times, we capture output then make a
  # snapshot without those lines
  expect_no_error({
    logging_res <-
      capture.output(
        res <-
          car_set_1 |>
          workflow_map(resamples = folds, seed = 2, verbose = TRUE),
        type = "message"
      )
  })
  logging_res <- logging_res[!grepl("s\\)$", logging_res)]
  expect_snapshot(
    cat(logging_res, sep = "\n")
  )
})

test_that("missing packages", {
  skip_if(rlang::is_installed("glmnet"))
  car_set_2 <-
    workflow_set(
      list(reg = mpg ~ .),
      list(glmn = glmn_spec)
    )

  expect_snapshot(
    {
      res <-
        car_set_2 |>
        workflow_map(resamples = folds, seed = 2, verbose = FALSE)
    },
    transform = function(lines) {
      gsub("\\([0-9]+ms\\)", "(ms)", lines)
    }
  )
  expect_true(inherits(res, "workflow_set"))
  expect_equal(res$result[[1]], list())
})


test_that("failers", {
  skip_on_cran()
  car_set_3 <-
    workflow_set(
      list(reg = mpg ~ .),
      list(knn = knn_spec, lm = lr_spec)
    )

  expect_no_error({
    res_quiet <-
      car_set_3 |>
      workflow_map(resamples = folds, seed = 2, verbose = FALSE, grid = "a")
  })
  expect_true(inherits(res_quiet, "workflow_set"))
  expect_true(inherits(res_quiet$result[[1]], "try-error"))

  expect_snapshot(
    {
      res_loud <-
        car_set_3 |>
        workflow_map(resamples = folds, seed = 2, verbose = TRUE, grid = "a")
    },
    transform = function(lines) {
      gsub("\\([0-9]+ms\\)", "(ms)", lines)
    }
  )
  expect_true(inherits(res_loud, "workflow_set"))
  expect_true(inherits(res_loud$result[[1]], "try-error"))
})

test_that("workflow_map can handle cluster specifications", {
  skip_on_cran()
  skip_if_not_installed("tidyclust")
  library(tidyclust)
  library(recipes)

  set.seed(1)
  mtcars_tbl <- mtcars |> dplyr::select(where(is.numeric))
  folds <- vfold_cv(mtcars_tbl, v = 3)

  wf_set_spec <-
    workflow_set(
      list(all = recipe(mtcars_tbl, ~.), some = ~ mpg + hp),
      list(km = k_means(num_clusters = tune()))
    )

  wf_set_fit <-
    workflow_map(wf_set_spec, fn = "tune_cluster", resamples = folds)

  wf_set_fit
})

test_that("fail informatively on mismatched spec/tuning function", {
  skip_on_cran()
  skip_if_not_installed("tidyclust")
  library(tidyclust)

  set.seed(1)
  mtcars_tbl <- mtcars |> dplyr::select(where(is.numeric))
  folds <- vfold_cv(mtcars_tbl, v = 3)

  wf_set_1 <-
    workflow_set(
      list(reg = mpg ~ .),
      list(
        dt = decision_tree("regression", min_n = tune()),
        km = k_means(num_clusters = tune())
      )
    )

  wf_set_2 <-
    workflow_set(
      list(reg = mpg ~ .),
      list(
        dt = decision_tree("regression", min_n = tune()),
        km = k_means(num_clusters = tune()),
        hc = hier_clust()
      )
    )

  wf_set_3 <-
    workflow_set(
      list(reg = mpg ~ .),
      list(
        dt = decision_tree("regression", min_n = tune()),
        nn = nearest_neighbor("regression", neighbors = tune()),
        km = k_means(num_clusters = tune())
      )
    )

  # pass a cluster spec to `tune_grid()`
  expect_snapshot(
    error = TRUE,
    workflow_map(wf_set_1, resamples = folds)
  )

  expect_snapshot(
    error = TRUE,
    workflow_map(wf_set_2, resamples = folds)
  )

  # pass a model spec to `tune_cluster()`
  expect_snapshot(
    error = TRUE,
    workflow_map(wf_set_1, resamples = folds, fn = "tune_cluster")
  )

  expect_snapshot(
    error = TRUE,
    workflow_map(wf_set_3, resamples = folds, fn = "tune_cluster")
  )
})
