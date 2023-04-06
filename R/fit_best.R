#' @importFrom tune fit_best
#' @rdname reexports
tune::fit_best

#' Fit a model to the numerically optimal configuration
#'
#' `fit_best()` takes results from tuning many models and fits the workflow
#' configuration associated with the best performance to the training set.
#'
#' @param x A [`workflow_set`][workflow_set()] object that has been evaluated
#' with [workflow_map()]. Note that the workflow set must have been fitted with
#' the [control option][option_add] `save_workflow = TRUE`.
#' @param metric A character string giving the metric to rank results by.
#' @param ... Additional options to pass to
#' [tune::fit_best][tune::fit_best.tune_results].
#'
#' @details
#' This function is a shortcut for the steps needed to fit the
#' numerically optimal configuration in a fitted workflow set.
#' The function ranks results, extracts the tuning result pertaining
#' to the best result, and then again calls `fit_best()` (itself a
#' wrapper) on the tuning result containing the best result.
#'
#' In pseudocode:
#'
#' ```
#' rankings <- rank_results(wf_set, metric, select_best = TRUE)
#' tune_res <- extract_workflow_set_result(wf_set, rankings$wflow_id[1])
#' fit_best(tune_res, metric)
#' ```
#'
#' @includeRmd man-roxygen/example_data.Rmd note
#'
#' @examplesIf rlang::is_installed(c("kknn", "modeldata", "recipes", "yardstick", "dials")) && identical(Sys.getenv("NOT_CRAN"), "true")
#' library(tune)
#' library(modeldata)
#' library(rsample)
#'
#' data(Chicago)
#' Chicago <- Chicago[1:1195,]
#'
#' time_val_split <-
#'    sliding_period(
#'       Chicago,
#'       date,
#'       "month",
#'       lookback = 38,
#'       assess_stop = 1
#'    )
#'
#' chi_features_set
#'
#' chi_features_res_new <-
#'    chi_features_set %>%
#'    # note: must set `save_workflow = TRUE` to use `fit_best()`
#'    option_add(control = control_grid(save_workflow = TRUE)) %>%
#'    # evaluate with resamples
#'    workflow_map(resamples = time_val_split, grid = 21, seed = 1, verbose = TRUE)
#'
#' chi_features_res_new
#'
#' # sort models by performance metrics
#' rank_results(chi_features_res_new)
#'
#' # fit the numerically optimal configuration to the training set
#' chi_features_wf <- fit_best(chi_features_res_new)
#'
#' chi_features_wf
#'
#' # to select optimal value based on a specific metric:
#' fit_best(chi_features_res_new, metric = "rmse")
#' @name fit_best.workflow_set
#' @export
fit_best.workflow_set <- function(x, metric = NULL,  ...) {
   if (is.null(metric)) {
      result_1 <- extract_workflow_set_result(x, id = x$wflow_id[[1]])
      metric <- .get_tune_metric_names(result_1)[1]
   }

   rankings <- rank_results(x, rank_metric = metric, select_best = TRUE)

   tune_res <- extract_workflow_set_result(x, id = rankings$wflow_id[1])

   best_params <- select_best(tune_res, metric = metric)

   fit_best(tune_res, metric = metric, parameters = best_params, ...)
}
