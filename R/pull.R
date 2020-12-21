#' Extract a workflow from a workflow set
#'
#' @param x A workflow set.
#' @param id A single character string for a workflow ID
#' @return The value in the `result` field of the workflow set.
#' @export
pull_workflow_result <- function(x, id) {
   y <- x %>% dplyr::filter(wflow_id == id[1])
   if (nrow(y) != 1) {
      halt("No workflow ID found for '", id[1], "'")
   }
   y$result[[1]]
}
