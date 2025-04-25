#' Rank the results by a metric
#'
#' This function sorts the results by a specific performance metric.
#'
#' @inheritParams collect_metrics.workflow_set
#' @param rank_metric A character string for a metric.
#' @inheritParams tune::fit_best.tune_results
#' @param select_best A logical giving whether the results should only contain
#' the numerically best submodel per workflow.
#' @details
#' If some models have the exact same performance,
#' `rank(value, ties.method = "random")` is used (with a reproducible seed) so
#' that all ranks are integers.
#'
#' No columns are returned for the tuning parameters since they are likely to
#' be different (or not exist) for some models. The `wflow_id` and `.config`
#' columns can be used to determine the corresponding parameter values.
#' @return A tibble with columns: `wflow_id`, `.config`, `.metric`, `mean`,
#' `std_err`, `n`, `preprocessor`, `model`, and `rank`.
#'
#' @includeRmd man-roxygen/example_data.Rmd note
#'
#' @examples
#' chi_features_res
#'
#' rank_results(chi_features_res)
#' rank_results(chi_features_res, select_best = TRUE)
#' rank_results(chi_features_res, rank_metric = "rsq")
#' @export
rank_results <- function(
  x,
  rank_metric = NULL,
  eval_time = NULL,
  select_best = FALSE
) {
  check_wf_set(x)
  check_string(rank_metric, allow_null = TRUE)
  check_bool(select_best)
  result_1 <- extract_workflow_set_result(x, id = x$wflow_id[[1]])
  met_set <- tune::.get_tune_metrics(result_1)
  if (!is.null(rank_metric)) {
    tune::check_metric_in_tune_results(tibble::as_tibble(met_set), rank_metric)
  }

  metric_info <- pick_metric(x, rank_metric)
  metric <- metric_info$metric
  direction <- metric_info$direction
  wflow_info <- dplyr::bind_cols(
    purrr::map_dfr(x$info, I),
    dplyr::select(x, wflow_id)
  )

  eval_time <- tune::choose_eval_time(result_1, metric, eval_time = eval_time)

  results <- collect_metrics(x) |>
    dplyr::select(
      wflow_id,
      .config,
      .metric,
      mean,
      std_err,
      n,
      dplyr::any_of(".eval_time")
    ) |>
    dplyr::full_join(wflow_info, by = "wflow_id") |>
    dplyr::select(-comment, -workflow)

  if (!is.null(eval_time) && ".eval_time" %in% names(results)) {
    results <- results[results$.eval_time == eval_time, ]
  }

  types <- x |>
    dplyr::full_join(wflow_info, by = "wflow_id") |>
    dplyr::mutate(
      is_race = purrr::map_lgl(result, \(.x) inherits(.x, "tune_race")),
      num_rs = purrr::map_int(result, get_num_resamples)
    ) |>
    dplyr::select(wflow_id, is_race, num_rs)

  ranked <-
    dplyr::full_join(results, types, by = "wflow_id") |>
    dplyr::filter(.metric == metric)

  if (any(ranked$is_race)) {
    # remove any racing results with less resamples than the total number
    rm_rows <-
      ranked |>
      dplyr::filter(is_race & (num_rs > n)) |>
      dplyr::select(wflow_id, .config) |>
      dplyr::distinct()
    if (nrow(rm_rows) > 0) {
      ranked <- dplyr::anti_join(ranked, rm_rows, by = c("wflow_id", ".config"))
      results <- dplyr::anti_join(
        results,
        rm_rows,
        by = c("wflow_id", ".config")
      )
    }
  }

  if (direction == "maximize") {
    ranked$mean <- -ranked$mean
  }

  if (select_best) {
    best_by_wflow <-
      dplyr::group_by(ranked, wflow_id) |>
      dplyr::slice_min(mean, with_ties = FALSE) |>
      dplyr::ungroup() |>
      dplyr::select(wflow_id, .config)
    ranked <- dplyr::inner_join(
      ranked,
      best_by_wflow,
      by = c("wflow_id", ".config")
    )
  }

  # ensure reproducible rankings when there are ties
  withr::with_seed(
    1,
    {
      ranked <-
        ranked |>
        dplyr::mutate(rank = rank(mean, ties.method = "random")) |>
        dplyr::select(wflow_id, .config, rank)
    }
  )

  dplyr::inner_join(results, ranked, by = c("wflow_id", ".config")) |>
    dplyr::arrange(rank) |>
    dplyr::rename(preprocessor = preproc)
}

get_num_resamples <- function(x) {
  purrr::map_dfr(x$splits, \(.x) .x$id) |>
    dplyr::distinct() |>
    nrow()
}
