skip_if_not_installed("kknn")
skip_if_not_installed("modeldata")

test_that("fit_best fits with correct hyperparameters", {
  skip_on_cran()

  library(tune)
  library(modeldata)
  library(rsample)
  library(yardstick)

  data(Chicago)
  Chicago <- Chicago[1:1195, ]

  time_val_split <-
    sliding_period(
      Chicago,
      date,
      "month",
      lookback = 38,
      assess_stop = 1
    )

  chi_features_map <-
    chi_features_set |>
    option_add(
      control = control_grid(save_workflow = TRUE),
      # choose metrics resulting in different rankings
      metrics = metric_set(rmse, iic)
    ) |>
    workflow_map(resamples = time_val_split, grid = 21, seed = 1)

  chi_features_map

  # metric: rmse
  fit_best_wf <- fit_best(chi_features_map)
  expect_s3_class(fit_best_wf, "workflow")

  rankings <- rank_results(chi_features_map, "rmse")
  tune_res <- extract_workflow_set_result(
    chi_features_map,
    rankings$wflow_id[1]
  )
  tune_params <- select_best(tune_res, metric = "rmse")
  manual_wf <- fit_best(tune_res, parameters = tune_params)

  manual_wf$pre$mold$blueprint$recipe$fit_times <-
    fit_best_wf$pre$mold$blueprint$recipe$fit_times
  manual_wf$fit$fit$elapsed$elapsed <-
    fit_best_wf$fit$fit$elapsed$elapsed
  expect_equal(manual_wf, fit_best_wf)

  # metric: iic
  fit_best_wf_2 <- fit_best(chi_features_map, "iic")
  expect_s3_class(fit_best_wf_2, "workflow")

  rankings_2 <- rank_results(chi_features_map, "iic")
  tune_res_2 <- extract_workflow_set_result(
    chi_features_map,
    rankings_2$wflow_id[1]
  )
  tune_params_2 <- select_best(tune_res_2, metric = "iic")
  manual_wf_2 <- fit_best(tune_res_2, parameters = tune_params_2)

  manual_wf_2$pre$mold$blueprint$recipe$fit_times <-
    fit_best_wf_2$pre$mold$blueprint$recipe$fit_times
  manual_wf_2$fit$fit$elapsed$elapsed <-
    fit_best_wf_2$fit$fit$elapsed$elapsed
  expect_equal(manual_wf_2, fit_best_wf_2)
})

test_that("fit_best errors informatively with bad inputs", {
  skip_on_cran()

  library(tune)
  library(modeldata)
  library(rsample)
  library(yardstick)

  data(Chicago)
  Chicago <- Chicago[1:1195, ]

  time_val_split <-
    sliding_period(
      Chicago,
      date,
      "month",
      lookback = 38,
      assess_stop = 1
    )

  chi_features_map <-
    chi_features_set |>
    option_add(
      # set needed `save_workflow` option
      control = control_grid(save_workflow = TRUE)
    ) |>
    workflow_map(resamples = time_val_split, grid = 21, seed = 1)

  expect_snapshot(
    fit_best(chi_features_res),
    error = TRUE
  )

  expect_snapshot(
    fit_best(chi_features_map, metric = "boop"),
    error = TRUE
  )

  expect_snapshot(
    fit_best(chi_features_map, boop = "bop"),
    error = TRUE
  )
})
