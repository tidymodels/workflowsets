#' Rank the results by a metric
#'
#' @param x A workflow set that has all results.
#' @param rank_metric A character string for a metric.
#' @examples
#' rank_results(cell_models)
#' rank_results(cell_models, rank_metric = "accuracy")
#' @export
rank_results <- function(x, rank_metric = NULL) {
   metric_info <- pick_metric(x, rank_metric)
   metric <- metric_info$metric
   direction <- metric_info$direction

   results <- collect_metrics(x) %>%
      dplyr::select(wflow_id, .config, .metric, mean, std_err)
   types <- x %>%
      dplyr::mutate(model = get_model_type(x), preprocessor = get_preproc_type(x)) %>%
      dplyr::select(wflow_id, model, preprocessor)
   ranked <-
      dplyr::full_join(results, types, by = "wflow_id") %>%
      dplyr::filter(.metric == metric)

   if (direction == "maximize") {
      ranked$mean <- -ranked$mean
   }

   ranked <-
      ranked %>%
      dplyr::mutate(
         rank = paste0(wflow_id, .config),
         rank = factor(rank),
         rank = stats::reorder(rank, mean),
         rank = as.numeric(rank)
      ) %>%
      dplyr::select(wflow_id, .config, model, preprocessor, rank)
   dplyr::full_join(results, ranked, by = c("wflow_id", ".config")) %>%
      dplyr::arrange(rank)

}


get_preproc_type <- function(x) {
   purrr::map_chr(x$objects, preproc_type)
}

get_model_type <- function(x) {
   purrr::map_chr(x$objects, model_type)
}

