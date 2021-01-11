#' Extract a workflow from a workflow set
#'
#' @param x A workflow set.
#' @param id A single character string for a workflow ID
#' @return The value in the `result` or `object` field of the workflow set,
#' depending on the function.
#' @export
pull_workflow_result <- function(x, id) {
   y <- x %>% dplyr::filter(wflow_id == id[1])
   if (nrow(y) != 1) {
      halt("No workflow ID found for '", id[1], "'")
   }
   y$result[[1]]
}

#' @export
#' @rdname pull_workflow_result
pull_workflow <- function(x, id) {
   y <- x %>% dplyr::filter(wflow_id == id[1])
   if (nrow(y) != 1) {
      halt("No workflow ID found for '", id[1], "'")
   }
   y$workflow[[1]]
}
