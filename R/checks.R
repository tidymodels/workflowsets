
check_consistent_metrics <- function(x, fail = TRUE) {
   metric_info <-
      dplyr::distinct(x, .metric, wflow_id) %>%
      dplyr::mutate(has = TRUE) %>%
      tidyr::pivot_wider(names_from = ".metric", values_from = "has", values_fill = FALSE) %>%
      dplyr::select(-wflow_id) %>%
      purrr::map_dbl(~ sum(!.x))

   if (any(metric_info > 0)) {
      incp_metrics <- names(metric_info)[metric_info > 0]
      msg <- paste("Some metrics were not used in all workflows:",
                   paste(incp_metrics, collapse = ", "))

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


# in case there are no tuning parameters, we can avoid warnings

recheck_options <- function(opts, .fn) {
   if (.fn == "fit_resamples") {
      allowed <- c("object", "resamples", "metrics", "control")
      nms <- names(opts)
      disallowed <- !(nms %in% allowed)
      if (any(disallowed)) {
         opts <- opts[!disallowed]
      }
   }
   opts
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
      msg <- "The workflow names should be unique:"
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

check_for_tune_results <- function(x) {
   res <- purrr::map_lgl(x, bad_result_col)
   if (any(res)) {
      rlang::abort("Some elements of 'result` do not have class `tune_results`.")
   }
   invisible(NULL)
}
bad_result_col <- function(x) {
   if(length(x) > 0) {
      res <- !inherits(x, "tune_results")
   } else {
      res <- FALSE
   }
   res
}

check_consistent_resamples <- function(x) {
   empty_res <- purrr::map_lgl(x$result, ~ length(.x) == 0)
   tmp <- x$result[!empty_res]
   tmp_id <- x$wflow_id[!empty_res]
   rs_hash <- purrr::map_chr(tmp, rsample::fingerprint)
   if (length(unique(rs_hash)) > 1) {
      fail_res <-
         tibble::tibble(hash = rs_hash, id = tmp_id) %>%
         dplyr::group_by(hash) %>%
         dplyr::summarize(obj = paste0(id, collapse = ", "), .groups = "drop") %>%
         dplyr::ungroup()
      msg <- paste("Different resamples were used in the workflow results. These",
                   "objects used different resamples:",
                   paste0("{", fail_res$obj, "}", collapse = ", "))
      rlang::abort(msg)
   }
   invisible(NULL)
}



