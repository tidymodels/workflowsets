#' Obtain and format results produced by tuning functions for workflow sets
#'
#' Return a tibble of performance metrics for all models or submodels.
#'
#' @param x A [`workflow_set`][workflow_set()] object that has been evaluated
#' with [workflow_map()].
#' @param ... Not currently used.
#' @param summarize A logical for whether the performance estimates should be
#'  summarized via the mean (over resamples) or the raw performance values (per
#'  resample) should be returned along with the resampling identifiers. When
#'  collecting predictions, these are averaged if multiple assessment sets
#'  contain the same row.
#' @param parameters An optional tibble of tuning parameter values that can be
#'  used to filter the predicted values before processing. This tibble should
#'  only have columns for each tuning parameter identifier (e.g. `"my_param"`
#'  if `tune("my_param")` was used).
#' @param select_best A single logical for whether the numerically best results
#' are retained. If `TRUE`, the `parameters` argument is ignored.
#' @param metric A character string for the metric that is used for
#' `select_best`.
#' @return A tibble.
#' @details
#'
#' When applied to a workflow set, the metrics and predictions that are returned
#' do not contain the actual tuning parameter columns and values (unlike when
#' these collect functions are run on other objects). The reason is that workflow
#' sets can contain different types of models or models with different tuning
#' parameters.
#'
#' If the columns are needed, there are two options. First, the `.config` column
#' can be used to merge the tuning parameter columns into an appropriate object.
#' Alternatively, the `map()` function can be used to get the metrics from the
#' original objects (see the example below).
#'
#' @seealso [tune::collect_metrics()], [rank_results()]
#'
#' @includeRmd man-roxygen/example_data.Rmd note
#'
#' @examples
#' library(dplyr)
#' library(purrr)
#' library(tidyr)
#'
#' two_class_res
#'
#' # ------------------------------------------------------------------------------
#' \donttest{
#' collect_metrics(two_class_res)
#'
#' # Alternatively, if the tuning parameter values are needed:
#' two_class_res |>
#'   dplyr::filter(grepl("cart", wflow_id)) |>
#'   mutate(metrics = map(result, collect_metrics)) |>
#'   dplyr::select(wflow_id, metrics) |>
#'   tidyr::unnest(cols = metrics)
#' }
#'
#' collect_metrics(two_class_res, summarize = FALSE)
#' @export
collect_metrics.workflow_set <- function(x, ..., summarize = TRUE) {
  rlang::check_dots_empty()
  check_incompete(x, fail = TRUE)
  check_bool(summarize)
  x <-
    dplyr::mutate(
      x,
      metrics = purrr::map(
        result,
        collect_metrics,
        summarize = summarize
      ),
      metrics = purrr::map2(metrics, result, remove_parameters)
    )
  info <- dplyr::bind_rows(x$info) |> dplyr::select(-workflow, -comment)
  x <-
    dplyr::select(x, wflow_id, metrics) |>
    dplyr::bind_cols(info) |>
    tidyr::unnest(cols = c(metrics)) |>
    reorder_cols()
  check_consistent_metrics(x, fail = FALSE)
  x
}

remove_parameters <- function(x, object) {
  prm <- tune::.get_tune_parameter_names(object)
  x <- dplyr::select(x, -dplyr::one_of(prm))
  x
}

reorder_cols <- function(x) {
  if (any(names(x) == ".iter")) {
    cols <- c("wflow_id", ".config", ".iter", "preproc", "model")
  } else {
    cols <- c("wflow_id", ".config", "preproc", "model")
  }
  dplyr::relocate(x, !!!cols)
}

#' @export
#' @rdname collect_metrics.workflow_set
collect_predictions.workflow_set <-
  function(
    x,
    ...,
    summarize = TRUE,
    parameters = NULL,
    select_best = FALSE,
    metric = NULL
  ) {
    rlang::check_dots_empty()
    check_incompete(x, fail = TRUE)
    check_bool(summarize)
    check_bool(select_best)
    check_string(metric, allow_null = TRUE)
    if (select_best) {
      x <-
        dplyr::mutate(
          x,
          predictions = purrr::map(
            result,
            \(.x)
              select_bare_predictions(
                .x,
                summarize = summarize,
                metric = metric
              )
          )
        )
    } else {
      x <-
        dplyr::mutate(
          x,
          predictions = purrr::map(
            result,
            get_bare_predictions,
            summarize = summarize,
            parameters = parameters
          )
        )
    }
    info <- dplyr::bind_rows(x$info) |> dplyr::select(-workflow, -comment)
    x <-
      dplyr::select(x, wflow_id, predictions) |>
      dplyr::bind_cols(info) |>
      tidyr::unnest(cols = c(predictions)) |>
      reorder_cols()
    x
  }

select_bare_predictions <- function(x, metric, summarize) {
  res <-
    tune::collect_predictions(
      x,
      summarize = summarize,
      parameters = tune::select_best(x, metric = metric)
    )
  remove_parameters(res, x)
}

get_bare_predictions <- function(x, ...) {
  res <- tune::collect_predictions(x, ...)
  remove_parameters(res, x)
}

#' @export
#' @rdname collect_metrics.workflow_set
collect_notes.workflow_set <- function(x, ...) {
  check_incompete(x)

  res <- dplyr::rowwise(x)
  res <- dplyr::mutate(res, notes = list(collect_notes(result)))
  res <- dplyr::ungroup(res)
  res <- dplyr::select(res, wflow_id, notes)
  res <- tidyr::unnest(res, cols = notes)

  res
}

#'
#' @export
#' @rdname collect_metrics.workflow_set
collect_extracts.workflow_set <- function(x, ...) {
  check_incompete(x)

  res <- dplyr::rowwise(x)
  res <- dplyr::mutate(res, extracts = list(collect_extracts(result)))
  res <- dplyr::ungroup(res)
  res <- dplyr::select(res, wflow_id, extracts)
  res <- tidyr::unnest(res, cols = extracts)

  res
}
