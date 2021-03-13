#' Extract elements from a workflow set
#'
#' `pull_workflow_set_result()` retrieves the results of [workflow_map()] for a
#' particular workflow while `pull_workflow()` extracts the unfitted workflow
#' from the `info` column.
#' @param x A workflow set.
#' @param id A single character string for a workflow ID.
#' @return `pull_workflow_set_result()` produces a `tune_result` or
#' `resample_results` object. `pull_workflow()` returns an unfit workflow
#' object.
#' @examples
#' library(tune)
#'
#' pull_workflow_set_result(two_class_res, "none_cart")
#'
#' pull_workflow(two_class_res, "none_cart")
#' @export
pull_workflow_set_result <- function(x, id) {
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
   if (length(id) != 1) {
      rlang::abort("'id' should have a single value.")
   }
   y <- x %>% dplyr::filter(wflow_id == id[1])
   if (nrow(y) != 1) {
      halt("No workflow ID found for '", id[1], "'")
   }
   y$info[[1]]$workflow[[1]]
}
