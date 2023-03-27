#' Extract elements from a workflow set
#'
#' `r lifecycle::badge("soft-deprecated")`
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
#' @examples
#' library(tune)
#'
#' two_class_res
#'
#' pull_workflow_set_result(two_class_res, "none_cart")
#'
#' pull_workflow(two_class_res, "none_cart")
#' @export
pull_workflow_set_result <- function(x, id) {
  lifecycle::deprecate_warn(
    "0.1.0",
    "pull_workflow_set_result()",
    "extract_workflow_set_result()"
  )
  if (length(id) != 1) {
    rlang::abort("'id' should have a single value.")
  }
  y <- x %>% dplyr::filter(wflow_id == id[1])
  if (nrow(y) != 1) {
    halt("No workflow ID found for '", id[1], "'")
  }
  y$result[[1]]
}

#' @export
#' @rdname pull_workflow_set_result
pull_workflow <- function(x, id) {
  lifecycle::deprecate_warn("0.1.0", "pull_workflow()", "extract_workflow()")
  if (length(id) != 1) {
    rlang::abort("'id' should have a single value.")
  }
  y <- x %>% dplyr::filter(wflow_id == id[1])
  if (nrow(y) != 1) {
    halt("No workflow ID found for '", id[1], "'")
  }
  y$info[[1]]$workflow[[1]]
}
