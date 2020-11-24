filter_metric_names <- function(x, nms) {
   dplyr::select(x, -dplyr::all_of(nms))
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

#' @export
summary.workflow_set <- function(object, ...) {
   # TODO check for empty results
   # TODO option for unsummarized results
   # TODO nest parameters?
   # TODO check for consistent metrics
   res <- purrr::map(object$results, tune::collect_metrics)
   param <- purrr::map(object$objects, ~ tune_args(.x)$name)
   res <- purrr::map2(res, param, filter_metric_names)
   res <- purrr::map2(res, object$wflow_id, add_object_name)
   res <- purrr::map2(res, object$preprocs, add_preproc_name)
   res <- purrr::map2(res, object$models,   add_model_name)
   all_names <- purrr::map(res, ~ names(.x))
   all_names <- unique(unlist(all_names))
   if (any(all_names == ".iter")) {
      res <- purrr::map(res, maybe_add_iter)
   }
   res <- dplyr::bind_rows(res)
   not_ids <- names(res)[!(names(res) %in% c("wflow_id", "preprocs", "models", ".config"))]
   res <- dplyr::select(res, wflow_id, preprocs, models, .config, !!!not_ids)
   # TODO add a rank?
   res
}



