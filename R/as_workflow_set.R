#' Save results from tuning or resampling functions as a workflow set
#'
#' If results have been generated directly from functions like
#' [tune::tune_grid()], they can be combined into a workflow set using this
#' function.
#' @param ... One or more named objects. Names should be unique and the
#' objects should have at least one of the following classes:
#' `iteration_results`, `tune_results`, `resample_results`, or `tune_race`. Each
#' element should also contain the original workflow (accomplished using the
#' `save_workflow` option in the control function).
#' @return A workflow set. Note that the `option` column will not reflect the
#' options that were used to create each object.
#' @export
as_workflow_set <- function(...) {
   object <- rlang::list2(...)
   wflows <- purrr::map(object, tune::.get_tune_workflow)
   check_names(wflows)
   check_for_workflow(wflows)

   res <- tibble::tibble(wflow_id = names(wflows))
   res <-
      res %>%
      dplyr::mutate(
         workflow  = unname(wflows),
         info = purrr::map(workflow, get_info),
         option    = purrr::map(1:nrow(res), ~ new_workflow_set_options())
      )
   res$result <- unname(object)

   res %>%
      dplyr::select(wflow_id, info, option, result) %>%
      new_workflow_set()
}
