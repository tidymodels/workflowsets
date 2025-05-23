#' Extract elements from a workflow set
#'
#' `r lifecycle::badge("deprecated")`
#'
#' `pull_workflow_set_result()` retrieves the results of [workflow_map()] for a
#' particular workflow while `pull_workflow()` extracts the unfitted workflow
#' from the `info` column.
#'
#'
#' @inheritParams comment_add
#' @param id A single character string for a workflow ID.
#' @details
#' The [extract_workflow_set_result()] and [extract_workflow()] functions should
#' be used instead of these functions.
#' @return `pull_workflow_set_result()` produces a `tune_result` or
#' `resample_results` object. `pull_workflow()` returns an unfit workflow
#' object.
#' @export
pull_workflow_set_result <- function(x, id) {
  lifecycle::deprecate_stop(
    "0.1.0",
    "pull_workflow_set_result()",
    "extract_workflow_set_result()"
  )
}

#' @export
#' @rdname pull_workflow_set_result
pull_workflow <- function(x, id) {
  lifecycle::deprecate_stop("0.1.0", "pull_workflow()", "extract_workflow()")
}
