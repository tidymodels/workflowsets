
check_consistent_metrics <- function(x, fail = TRUE) {
   check_incompete(x, fail = fail)
   metrics <-
      x %>%
      purrr::map_dfr(~ dplyr::select(.x, .metric, .estimator)) %>%
      dplyr::group_by(.estimator, .metric) %>%
      dplyr::count() %>%
      dplyr::ungroup()
   n_combos <- unique(metrics$n)
   metrics$pct <- round(metrics$n/sum(metrics$n)*100, 1)
   msg <- paste0(metrics$.metric, " (",  metrics$pct, "%)", collapse = ", ")
   msg <- paste("There were inconsistent metrics across the workflow results:", msg)
   if (length(n_combos) > 1) {
      if (fail) {
         halt(msg)
      } else {
         rlang::warn(msg)
      }
   }
   invisible(NULL)
}

check_incompete <- function(x, fail = TRUE) {
   empty_res <- purrr::map_lgl(x$results, ~ identical(.x, list()))
   failed_res <- purrr::map_lgl(x$results, ~ inherits(.x, "try-error"))

   n_empty <- sum(empty_res | failed_res)
   if (n_empty > 0) {
      msg <- paste("There were", n_empty, "workflows that had no results.")
      if (fail) {
         halt(msg)
      } else {
         rlang::warn(msg)
      }
   }
   invisible(NULL)
}

# TODO check for consistent resamples
