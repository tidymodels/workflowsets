#' Process a series of workflows
#'
#' `workflow_map()` will execute the same function across the workflows in the
#' set. The various `tune_*()` functions can be used as well as
#' [tune::fit_resamples()].
#' @param object A workflow set.
#' @param fn The function to run. Acceptable values are: [tune::tune_grid()],
#' [tune::tune_bayes()], [tune::fit_resamples()], `finetune::tune_race_anova()`,
#' `finetune::tune_race_win_loss()`, or `finetune::tune_sim_anneal()`.
#' @param verbose A logical for logging progress.
#' @param seed A single integer that is set prior to each function execution.
#' @param ... Options to pass to the modeling function. See details below.
#' @return An updated workflow set. The `option` column will be updated with
#' any options for the `tune` package functions given to `workflow_map()`. Also,
#' the results will be added to the `result` column. If the computations for a
#' workflow fail, an `try-catch` object will be saved in place of the results
#' (without stopping execution).
#' @seealso [workflow_set()], [as_workflow_set()], [extract_workflow_set_result()]
#' @details
#'
#' When passing options, anything passed in the `...` will be combined with any
#' values in the `option` column. The values in `...` will override that
#' column's values and the new options are added to the `options` column.
#'
#' Any failures in execution result in the corresponding row of `results` to
#' contain a `try-error` object.
#'
#' In cases where a model has no tuning parameters is mapped to one of the
#' tuning functions, [tune::fit_resamples()] will be used instead and a
#' warning is issued if `verbose = TRUE`.
#'
#' If a workflow required packages that are not installed, a message is printed
#' and `workflow_map()` continues with the next workflow (if any).
#'
#' @examples
#' # An example of processed results
#' chi_features_res
#'
#' # Code examples at
#' if (interactive()) {
#'   system.file("example-data", package = "workflowsets")
#' }
#' @export
workflow_map <- function(object, fn = "tune_grid", verbose = FALSE,
                         seed = sample.int(10^4, 1), ...) {

   fn_info <- dplyr::filter(allowed_fn, func == fn)
   if (nrow(fn_info) == 0) {
      msg <- paste0("Function '", fn, "' can't be used. Allowable values ",
                    "are: ", allowed_fn_list)
      halt(msg)
   }

   on.exit({
      cols <- tune::get_tune_colors()
      message(cols$symbol$danger("Execution stopped; returning current results"))
      return(new_workflow_set(object))
      })

   dots <- rlang::list2(...)
   # check and add options to options column
   if (length(dots) > 0) {
      object <- rlang::exec("option_add", object, !!!dots)
   }

   iter_seq <- seq_along(object$wflow_id)
   iter_chr <- format(iter_seq)
   n <- length(iter_seq)

   # Check for tuning when there is none?
   # Also we should check that the resamples objects are the same using the
   # new fingerprinting option.

   for (iter in iter_seq) {
      wflow <- extract_workflow(object, object$wflow_id[[iter]])
      .fn <- check_fn(fn, wflow, verbose)
      .fn_info <- dplyr::filter(allowed_fn, func == .fn)

      log_progress(verbose, object$wflow_id[[iter]], NULL, iter_chr[iter],
                   n, .fn, NULL)

      if (has_all_pkgs(wflow)) {

         opt <- recheck_options(object$option[[iter]], .fn)
         run_time <- system.time({
            cl <- rlang::call2(.fn, .ns = .fn_info$pkg, object = wflow, !!!opt)
            withr::with_seed(
               seed[1],
               object$result[[iter]] <- try(rlang::eval_tidy(cl), silent = TRUE)
            )
         })
         object <- new_workflow_set(object)
         log_progress(verbose, object$wflow_id[[iter]], object$result[[iter]],
                      iter_chr[iter], n, .fn, run_time)
      }
   }
   on.exit(return(new_workflow_set(object)))
}

# nocov
allowed_fn <-
   tibble::tibble(
      func = c("tune_grid", "tune_bayes", "fit_resamples", "tune_race_anova",
               "tune_race_win_loss", "tune_sim_anneal"),
      pkg = c(rep("tune", 3), rep("finetune", 3))
   )
allowed_fn_list <- paste0("'", allowed_fn$func, "'", collapse = ", ")
# nocov end

# ------------------------------------------------------------------------------

log_progress <- function(verbose, id, res, iter, n, .fn, elapsed) {
   if (!verbose) {
      return(invisible(NULL))
   }
   cols <- tune::get_tune_colors()
   event <- ifelse(grepl("tune", .fn), "tuning:    ", "resampling:")
   msg <- paste0(iter, " of ", n, " ", event, " ", id)

   if (inherits(res, "try-error")) {
      # When a bad arg is passed (usually)
      errors_msg <- gsub("\n", "", as.character(res))
      errors_msg <- gsub("Error : ", "", errors_msg, fixed = TRUE)
      message(
         cols$symbol$danger(cli::symbol$cross), " ",
         cols$message$info(msg),
         cols$message$info(" failed with: "),
         cols$message$danger(errors_msg)
      )
      return(invisible(NULL))
   }

   if (is.null(res)) {
      message(
         cols$symbol$info("i"), " ",
         cols$message$info(msg)
      )
   } else {
      all_null <- isTRUE(all(is.null(unlist(res$.metrics))))
      if (inherits(res, "try-error") || all_null) {
         if (all_null) {
            res <- collect_notes(res)
         }
         errors_msg <- gsub("\n", "", as.character(res))
         errors_msg <- gsub("Error : ", "", errors_msg, fixed = TRUE)
         message(
            cols$symbol$danger(cli::symbol$cross), " ",
            cols$message$info(msg),
            cols$message$info(" failed with "),
            cols$message$danger(errors_msg)
         )
      } else {
         time_msg <- paste0(" (", prettyunits::pretty_sec(elapsed[3]), ")")
         message(
            cols$symbol$success(cli::symbol$tick), " ",
            cols$message$info(msg),
            cols$message$info(time_msg)
         )
      }
   }

   invisible(NULL)
}

