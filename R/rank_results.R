#' Rank the results by a metric
#'
#' @param x A workflow set that has all results.
#' @param rank_metric A character string for a metric.
#' @param select_best A logical; should the results only contain the numerically
#' best submodel per workflow.
#' @examples
#' rank_results(cell_models)
#' rank_results(cell_models, select_best = TRUE)
#' rank_results(cell_models, rank_metric = "accuracy")
#' @export
rank_results <- function(x, rank_metric = NULL, select_best = FALSE) {
   metric_info <- pick_metric(x, rank_metric)
   metric <- metric_info$metric
   direction <- metric_info$direction

   results <- collect_metrics(x) %>%
      dplyr::select(wflow_id, .config, .metric, mean, std_err, n)
   types <- x %>%
      dplyr::mutate(
         model = get_model_type(x),
         preprocessor = get_preproc_type(x),
         is_race = purrr::map_lgl(result, ~ inherits(.x, "tune_race")),
         num_rs = purrr::map_int(result, get_num_resamples)
         ) %>%
      dplyr::select(wflow_id, model, preprocessor, is_race, num_rs)

   ranked <-
      dplyr::full_join(results, types, by = "wflow_id") %>%
      dplyr::filter(.metric == metric)

   if (any(ranked$is_race)) {
      # remove any racing results with less resamples than the total number
      rm_rows <-
         ranked %>%
         dplyr::filter(is_race & (num_rs > n)) %>%
         dplyr::select(wflow_id, .config) %>%
         dplyr::distinct()
      if (nrow(rm_rows) > 0) {
         ranked <- dplyr::anti_join(ranked, rm_rows, by = c("wflow_id", ".config"))
         results <- dplyr::anti_join(results, rm_rows, by = c("wflow_id", ".config"))
      }
   }

   if (direction == "maximize") {
      ranked$mean <- -ranked$mean
   }

   if (select_best) {
      best_by_wflow <-
         dplyr::group_by(ranked, wflow_id) %>%
         dplyr::slice_min(mean, with_ties = FALSE) %>%
         dplyr::ungroup() %>%
         dplyr::select(wflow_id, .config)
      ranked <- dplyr::inner_join(ranked, best_by_wflow, by = c("wflow_id", ".config"))
   }

   # ensure reproducible rankings when there are ties
   withr::with_seed(
      1,
      {
         ranked <-
            ranked %>%
            dplyr::mutate(rank = rank(mean, ties.method = "random")) %>%
            dplyr::select(wflow_id, .config, model, preprocessor, rank)
      }
   )

   dplyr::inner_join(results, ranked, by = c("wflow_id", ".config")) %>%
      dplyr::arrange(rank)

}


get_preproc_type <- function(x) {
   purrr::map_chr(x$object, preproc_type)
}

get_model_type <- function(x) {
   purrr::map_chr(x$object, model_type)
}

get_num_resamples <- function(x) {
   purrr::map_dfr(x$splits, ~ .x$id) %>%
      dplyr::distinct() %>%
      nrow()
}
