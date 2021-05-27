#' Extract elements from a workflow set
#'
#' `extract_workflow_set_result()` retrieves the results of [workflow_map()] for a
#' particular workflow while `extract_workflow()` extracts the unfitted workflow
#' from the `info` column. These functions supersede `pull_workflow_set_result()`
#' and `pull_workflow()` (since their names are more consistent with other
#' tidymodels functions).
#' @param x A workflow set.
#' @param id A single character string for a workflow ID.
#' @param ... Other options (not currently used).
#' @return `extract_workflow_set_result()` produces a `tune_result` or
#' `resample_results` object. `extract_workflow()` returns an unfit workflow
#' object.
#' @examples
#' library(tune)
#'
#' extract_workflow_set_result(two_class_res, "none_cart")
#'
#' extract_workflow(two_class_res, "none_cart")
#' @export
extract_workflow_set_result <- function(x, id, ...) {
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
#' @rdname extract_workflow_set_result
extract_workflow.workflow_set <- function(x, id, ...) {
   if (length(id) != 1) {
      rlang::abort("'id' should have a single value.")
   }
   y <- x %>% dplyr::filter(wflow_id == id[1])
   if (nrow(y) != 1) {
      halt("No workflow ID found for '", id[1], "'")
   }
   y$info[[1]]$workflow[[1]]
}
