#' Obtain and format results produced by tuning functions for workflow sets
#'
#' @param x A `workflow_set` object where all workflows have been evaluated.
#' @param summarize A logical for whether the performance estimates should be
#'  summarized via the mean (over resamples) or the raw performance values (per
#'  resample) should be returned along with the resampling identifiers.
#' @param ... Not currently used.
#' @return A tibble.
#' @export
collect_metrics.workflow_set <- function(x, summarize = TRUE, ...) {
   check_incompete(x, fail = TRUE)
   # TODO add a rank?
   # TODO option for only best
   res <- purrr::map(x$results, tune::collect_metrics, summarize = summarize)
   check_consistent_metrics(res)
   res <- purrr::map2(res, x$wflow_id, add_object_name)
   res <- purrr::map2(res, x$preprocs, add_preproc_name)
   res <- purrr::map2(res, x$models,   add_model_name)
   param <- purrr::map(x$objects, ~ tune::tune_args(.x)$name)
   res <- purrr::map2(res, param, nest_cols)
   all_names <- purrr::map(res, ~ names(.x))
   all_names <- unique(unlist(all_names))
   if (any(all_names == ".iter")) {
      res <- purrr::map(res, maybe_add_iter)
   }
   res <- dplyr::bind_rows(res)
   not_ids <- names(res)[!(names(res) %in% c("wflow_id", "info", ".config"))]
   res <- dplyr::select(res, wflow_id, info, .config, !!!not_ids)
   res
}

nest_cols <- function(x, nms) {
   nest_cols <- c("preprocs", "models")
   if (length(nms) > 0) {
      nest_cols <- c(nms, nest_cols)
   }
   res <- tidyr::nest(x, info = nest_cols)
   res
}

add_object_name <- function(x, nms) {
   dplyr::mutate(x, wflow_id = nms)
}
add_preproc_name <- function(x, nms) {
   dplyr::mutate(x, preprocs = nms)
}
add_model_name <- function(x, nms) {
   dplyr::mutate(x, models = nms)
}
maybe_add_iter <- function(x) {
   if (!any(names(x) == ".iter")) {
      x <- dplyr::mutate(x, .iter = 0)
   }
   x
}

check_consistent_metrics <- function(x) {
   metrics <-
      x %>%
      dplyr::distinct(wflow_id, .metric, .estimator) %>%
      dplyr::group_by(wflow_id, .estimator) %>%
      dplyr::count() %>%
      dplyr::ungroup()
   if (length(table(metrics$n)) > 0) {
      rlang::warn("There were inconsistent metrics across the workflow results.")
   }
   invisible(NULL)
}

check_incompete <- function(x, fail = TRUE) {
   empty_res <- purrr::map_lgl(x$results, ~ identical(.x, list()))
   n_empty <- sum(empty_res)
   if (n_empty > 0) {
      msg <- paste("There were", empty_res, "workflows that had no results.")
      if (fail) {
         halt(msg)
      } else {
         rlang::warn(msg)
      }
   }
   invisible(NULL)
}

