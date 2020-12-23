#'Process a series of workflows
#'
#' `workflow_map()` will execute the same function across the workflows in the
#' set. The various `tune_*()` functions can be used as well as
#' `fit_resamples()`.
#' @param object A workflow set.
#' @param fn The function to run. Acceptable values are: [tune::tune_grid()],
#' [tune::tune_bayes()], [tune::fit_resamples()], [finetune::tune_race_anova()],
#' [finetune::tune_race_win_loss()], or [finetune::tune_sim_anneal()].
#' @param verbose A logical for logging progress.
#' @param seed A single integer that is set prior to each function execution.
#' @param ... Options to pass to the modeling function. See details below.
#' @return An updated workflow set.
#' @details
#'
#' When passing options, anything passed in the `...` will be combined with any
#' values in the `option` column. The values in `...` will override that
#' column's values and the new options are added to the `options` column.
#'
#' Any failures in execution result in the corresponding row of `results` to
#' contain a `try-error` object.
#' @examples
#' example("workflow_set", run.donttest = FALSE)
#'
#' cell_fits <-
#'    cell_models_by_group %>%
#'    workflow_map("fit_resamples",
#'                 resamples = val_set,
#'                 metrics = metric_set(roc_auc))
#'
#' rank_results(cell_fits)
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
      message(cols$symbol$danger("Execution stepped; returning current results"))
      return(new_workflow_set(object))
      })

   dots <- rlang::list2(...)
   # check and add options to options column
   if (length(dots) > 0) {
      object <- rlang::exec("add_options", object, !!!dots)
   }

   iter_seq <- seq_along(object$wflow_id)
   iter_chr <- format(iter_seq)
   n <- length(iter_seq)

   # Check for tuning when there is none?
   # Also we should check that the resamples objects are the same using the
   # new fingerprinting option.

   for (iter in iter_seq) {
      log_progress(verbose, object$wflow_id[[iter]], NULL, iter_chr[iter],
                   n, fn, NULL)

      .fn <- check_fn(fn, object$object[[iter]])
      .fn_info <- dplyr::filter(allowed_fn, func == .fn)

      opt <- object$option[[iter]]
      run_time <- system.time({
         cl <-
            rlang::call2(.fn, .ns = .fn_info$pkg, object = object$object[[iter]], !!!opt)
         withr::with_seed(
            seed[1],
            object$result[[iter]] <- try(rlang::eval_tidy(cl), silent = TRUE)
         )
      })
      log_progress(verbose, object$wflow_id[[iter]], object$result[[iter]],
                   iter_chr[iter], n, fn, run_time)
   }
   new_workflow_set(object)
}


allowed_fn <-
   tibble::tibble(
      func = c("tune_grid", "tune_bayes", "fit_resamples", "tune_race_anova",
               "tune_race_win_loss", "tune_sim_anneal"),
      pkg = c(rep("tune", 3), rep("finetune", 3))
   )
allowed_fn_list <- paste0("'", allowed_fn$func, "'", collapse = ", ")
