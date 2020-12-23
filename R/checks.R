
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


# if global in local, overwrite or fail?

common_options <- function(model, global) {
   old_names <- names(model)
   new_names <- names(global)
   common_names <- intersect(old_names, new_names)
   if (length(common_names) > 0) {
      res <- paste0("'", common_names, "'", collapse = ", ")
   } else {
      res <- ""
   }
   res
}

check_options <- function(model, id, global, action = "fail") {
   res <- purrr::map_chr(model, common_options, global)
   flag <- nchar(res) > 0
   if (any(flag)) {
      msg <- "There are existing options that are being modified\n"
      msg <- paste0(msg, paste0("\t", id[flag], ": ", res[flag], collapse = "\n"))
      if (action == "fail") {
         halt(msg)
      }
      if (action == "warn") {
         rlang::warn(msg)
      }
   }
   invisible(NULL)
}


check_fn <- function(fn, x) {
   has_tune <- nrow(tune::tune_args(x)) > 0
   if (!has_tune & fn != "fit_resamples") {
      fn <- "fit_resamples"
      cols <- tune::get_tune_colors()
      msg <- "No tuning parameters. `fit_resamples()` will be attempted"
      message(cols$symbol$info(cli::symbol$info), " ", cols$message$info(msg))
   }
   fn
}



check_names <- function(x) {
   nms <- names(x)
   if (any(nms == "")) {
      bad <- which(nms == "")
      msg <- "Objects in these positions are not named:"
      msg <- paste(msg, paste0(bad, collapse = ", "))
      halt(msg)
   }
   xtab <- table(nms)
   if (any(xtab > 1)) {
      msg <- "The object names should be unique:"
      msg <- paste(msg, paste0("'", names(xtab)[xtab > 1], "'", collapse = ", "))
      halt(msg)
   }
   invisible(NULL)
}

check_for_workflow <- function(x) {
   no_wflow <- purrr::map_lgl(x, ~ !inherits(.x, "workflow"))
   if (any(no_wflow)) {
      bad <- names(no_wflow)[no_wflow]
      msg <- "Some objects do not have workflows:"
      msg <- paste(msg, paste0("'", bad, "'", collapse = ", "))
      msg <- paste0(msg, ". Use the control option `save_workflow` and re-run.")
      halt(msg)
   }
   invisible(NULL)
}

allowed_obj_types <- c("iteration_results", "tune_results", "resample_results",
                       "tune_race")

check_result_types <- function(x) {
   right_type <- purrr::map_lgl(x, ~ inherits(.x, allowed_obj_types))
   if (any(!right_type)) {
      bad <- names(right_type)[!right_type]
      msg <- "Some objects are not tuning or resampling results:"
      msg <- paste(msg, paste0("'", bad, "'", collapse = ", "))
      halt(msg)
   }
   invisible(NULL)
}
