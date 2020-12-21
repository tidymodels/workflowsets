#' Save tuning or resampling results as a workflow set
#'
#' @param object A named list of results. Names should be unique and the
#' elements in `object` should have at least one of the following classes:
#' `iteration_results`, `tune_results`, `resample_results`, or `tune_race`. Each
#' element should also contain the original workflow (accomplished using the
#' `save_workflow` option in the control function).
#' @return A workflow set. Note that the `option` column will not reflect the
#' options that were used to create each object.
#' @export
as_workflow_set <- function(object) {
   wflows <- purrr::map(object, tune::.get_tune_workflow)
   check_names(wflows)
   check_for_workflow(wflows)
   check_result_types(object)

   res <- tibble::tibble(wflow_id = names(wflows))
   res <-
      res %>%
      dplyr::mutate(
         object  = unname(wflows),
         preproc = purrr::map_chr(wflows, preproc_type),
         model   = purrr::map_chr(wflows, model_type),
         option  = purrr::map(1:nrow(res), ~ list())
      )
   res$result <- unname(object)

   res %>%
      dplyr::select(wflow_id, preproc, model, object, option, result) %>%
      new_workflow_set()
}
