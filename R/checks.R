
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
   empty_res <- purrr::map_lgl(x$result, ~ identical(.x, list()))
   failed_res <- purrr::map_lgl(x$result, ~ inherits(.x, "try-error"))

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


check_options <- function(which, opts, global) {
   if (all(is.null(opts))) {
      return(opts)
   }
   opt_nms <- names(opts)
   if (length(opt_nms) == 0 | any(opt_nms == "") | any(is.na(opt_nms))) {
      halt("'options' requires names for all elements.")
   }
   gbl_nms <- names(global)
   match_names <- opt_nms %in% which
   if (any(!match_names)) {
      bad_names <- opt_nms[!match_names]
      bad_names <- paste0("'", bad_names, "'", collapse = ", ")
      halt("Most option names do not match the workflow IDs: ", bad_names)
   }

   num_tasks <- length(which)
   all_nms <- purrr::map(opts, names)
   all_nms <- unlist(all_nms)
   all_nms <- unique(all_nms)

   dup_opt_nms <- all_nms %in% gbl_nms

   if (any(dup_opt_nms)) {
      dup_opt_nms <- all_nms[dup_opt_nms]
      dup_opt_nms <- paste0("'", dup_opt_nms, "'", collapse = ", ")
      halt("Some options are common to `...` and 'options': ", dup_opt_nms)
   }
   opts
}

