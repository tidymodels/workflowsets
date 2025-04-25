check_metric_results <- function(ind, x, ...) {
  id_val <- x$wflow_id[ind]

  if (any(names(list(...)) == "summarize")) {
    cols <- c(".metric", ".estimator", ".estimate", ".config", "id")
  } else {
    cols <- c(".metric", ".estimator", "mean", "n", "std_err", ".config")
  }

  orig <-
    collect_metrics(x$result[[ind]], ...) |>
    dplyr::select(dplyr::all_of(cols))

  everythng <-
    collect_metrics(x, ...) |>
    dplyr::filter(wflow_id == id_val) |>
    dplyr::select(dplyr::all_of(cols))
  all.equal(orig, everythng)
}

# ------------------------------------------------------------------------------

test_that("collect summarized metrics", {
  for (i in 1:nrow(two_class_res)) {
    expect_true(check_metric_results(i, two_class_res))
  }
  for (i in 1:nrow(chi_features_res)) {
    expect_true(check_metric_results(i, chi_features_res))
  }

  for (i in 1:nrow(two_class_res)) {
    expect_true(check_metric_results(i, two_class_res, summarize = FALSE))
  }
  for (i in 1:nrow(chi_features_res)) {
    expect_true(check_metric_results(i, chi_features_res, summarize = FALSE))
  }
})

test_that("ranking models", {
  # expected number of rows per metric per model
  param_lines <-
    c(
      none_cart = 10,
      none_glm = 1,
      none_mars = 2,
      yj_trans_cart = 10,
      yj_trans_glm = 1,
      yj_trans_mars = 2
    )

  expect_no_error(ranking_1 <- rank_results(two_class_res))
  expect_equal(nrow(ranking_1), sum(param_lines * 2))

  expect_no_error(ranking_2 <- rank_results(two_class_res, select_best = TRUE))
  expect_equal(nrow(ranking_2), nrow(two_class_res) * 2)
})
