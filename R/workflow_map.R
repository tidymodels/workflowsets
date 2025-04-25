#' Process a series of workflows
#'
#' `workflow_map()` will execute the same function across the workflows in the
#' set. The various `tune_*()` functions can be used as well as
#' [tune::fit_resamples()].
#' @param object A workflow set.
#' @param fn The name of the function to run, as a character. Acceptable values are:
#' ["tune_grid"][tune::tune_grid()],
#' ["tune_bayes"][tune::tune_bayes()],
#' ["fit_resamples"][tune::fit_resamples()],
#' ["tune_race_anova"][finetune::tune_race_anova()],
#' ["tune_race_win_loss"][finetune::tune_race_win_loss()], or
#' ["tune_sim_anneal"][finetune::tune_sim_anneal()]. Note that users need not
#' provide the namespace or parentheses in this argument,
#' e.g. provide `"tune_grid"` rather than `"tune::tune_grid"` or `"tune_grid()"`.
#' @param verbose A logical for logging progress.
#' @param seed A single integer that is set prior to each function execution.
#' @param ... Options to pass to the modeling function. See details below.
#' @return An updated workflow set. The `option` column will be updated with
#' any options for the `tune` package functions given to `workflow_map()`. Also,
#' the results will be added to the `result` column. If the computations for a
#' workflow fail, a `try-catch` object will be saved in place of the results
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
#' If a workflow requires packages that are not installed, a message is printed
#' and `workflow_map()` continues with the next workflow (if any).
#'
#' @includeRmd man-roxygen/example_data.Rmd note
#'
#' @examplesIf rlang::is_installed(c("kknn", "modeldata", "recipes", "yardstick", "dials")) && identical(Sys.getenv("NOT_CRAN"), "true")
#' library(workflowsets)
#' library(workflows)
#' library(modeldata)
#' library(recipes)
#' library(parsnip)
#' library(dplyr)
#' library(rsample)
#' library(tune)
#' library(yardstick)
#' library(dials)
#'
#' # An example of processed results
#' chi_features_res
#'
#' # Recreating them:
#'
#' # ---------------------------------------------------------------------------
#' data(Chicago)
#' Chicago <- Chicago[1:1195, ]
#'
#' time_val_split <-
#'   sliding_period(
#'     Chicago,
#'     date,
#'     "month",
#'     lookback = 38,
#'     assess_stop = 1
#'   )
#'
#' # ---------------------------------------------------------------------------
#'
#' base_recipe <-
#'   recipe(ridership ~ ., data = Chicago) |>
#'   # create date features
#'   step_date(date) |>
#'   step_holiday(date) |>
#'   # remove date from the list of predictors
#'   update_role(date, new_role = "id") |>
#'   # create dummy variables from factor columns
#'   step_dummy(all_nominal()) |>
#'   # remove any columns with a single unique value
#'   step_zv(all_predictors()) |>
#'   step_normalize(all_predictors())
#'
#' date_only <-
#'   recipe(ridership ~ ., data = Chicago) |>
#'   # create date features
#'   step_date(date) |>
#'   update_role(date, new_role = "id") |>
#'   # create dummy variables from factor columns
#'   step_dummy(all_nominal()) |>
#'   # remove any columns with a single unique value
#'   step_zv(all_predictors())
#'
#' date_and_holidays <-
#'   recipe(ridership ~ ., data = Chicago) |>
#'   # create date features
#'   step_date(date) |>
#'   step_holiday(date) |>
#'   # remove date from the list of predictors
#'   update_role(date, new_role = "id") |>
#'   # create dummy variables from factor columns
#'   step_dummy(all_nominal()) |>
#'   # remove any columns with a single unique value
#'   step_zv(all_predictors())
#'
#' date_and_holidays_and_pca <-
#'   recipe(ridership ~ ., data = Chicago) |>
#'   # create date features
#'   step_date(date) |>
#'   step_holiday(date) |>
#'   # remove date from the list of predictors
#'   update_role(date, new_role = "id") |>
#'   # create dummy variables from factor columns
#'   step_dummy(all_nominal()) |>
#'   # remove any columns with a single unique value
#'   step_zv(all_predictors()) |>
#'   step_pca(!!stations, num_comp = tune())
#'
#' # ---------------------------------------------------------------------------
#'
#' lm_spec <- linear_reg() |> set_engine("lm")
#'
#' # ---------------------------------------------------------------------------
#'
#' pca_param <-
#'   parameters(num_comp()) |>
#'   update(num_comp = num_comp(c(0, 20)))
#'
#' # ---------------------------------------------------------------------------
#'
#' chi_features_set <-
#'   workflow_set(
#'     preproc = list(
#'       date = date_only,
#'       plus_holidays = date_and_holidays,
#'       plus_pca = date_and_holidays_and_pca
#'     ),
#'     models = list(lm = lm_spec),
#'     cross = TRUE
#'   )
#'
#' # ---------------------------------------------------------------------------
#'
#' chi_features_res_new <-
#'   chi_features_set |>
#'   option_add(param_info = pca_param, id = "plus_pca_lm") |>
#'   workflow_map(resamples = time_val_split, grid = 21, seed = 1, verbose = TRUE)
#'
#' chi_features_res_new
#' @export
workflow_map <- function(
  object,
  fn = "tune_grid",
  verbose = FALSE,
  seed = sample.int(10^4, 1),
  ...
) {
  check_wf_set(object)

  rlang::arg_match(fn, allowed_fn$func)
  check_object_fn(object, fn)
  check_bool(verbose)
  check_number_decimal(seed)

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

    log_progress(
      verbose,
      object$wflow_id[[iter]],
      NULL,
      iter_chr[iter],
      n,
      .fn,
      NULL
    )

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
      log_progress(
        verbose,
        object$wflow_id[[iter]],
        object$result[[iter]],
        iter_chr[iter],
        n,
        .fn,
        run_time
      )
    }
  }
  on.exit(return(new_workflow_set(object)))
}

# nocov
allowed_fn <-
  tibble::tibble(
    func = c(
      "tune_grid",
      "tune_bayes",
      "fit_resamples",
      "tune_race_anova",
      "tune_race_win_loss",
      "tune_sim_anneal",
      "tune_cluster"
    ),
    pkg = c(rep("tune", 3), rep("finetune", 3), "tidyclust")
  )
allowed_fn_list <- paste0("'", allowed_fn$func, "'", collapse = ", ")
# nocov end

# ---------------------------------------------
check_object_fn <- function(object, fn, call = rlang::caller_env()) {
  wf_specs <- purrr::map(
    object$wflow_id,
    \(.x) extract_spec_parsnip(object, id = .x)
  )
  is_cluster_spec <- purrr::map_lgl(wf_specs, inherits, "cluster_spec")

  if (identical(fn, "tune_cluster")) {
    if (!all(is_cluster_spec)) {
      cli::cli_abort(
        "To tune with {.fn tune_cluster}, each workflow's model \\
            specification must inherit from {.cls cluster_spec}, but \\
            {.var {object$wflow_id[!is_cluster_spec]}} {?does/do} not.",
        call = call
      )
    }
    return(invisible())
  }

  is_model_spec <- purrr::map_lgl(wf_specs, inherits, "model_spec")

  msg <-
    "To tune with {.fn {fn}}, each workflow's model \\
     specification must inherit from {.cls model_spec}, but \\
     {.var {object$wflow_id[!is_model_spec]}} {?does/do} not."

  if (any(is_cluster_spec)) {
    msg <- c(
      msg,
      "i" = "{cli::qty(object$wflow_id[is_cluster_spec])} \\
                The workflow{?/s} {.var {object$wflow_id[is_cluster_spec]}} \\
                {?is a /are} cluster specification{?/s}. Did you intend to \\
                set `fn = 'tune_cluster'`?"
    )
  }
  if (!all(is_model_spec)) {
    cli::cli_abort(msg, call = call)
  }

  return(invisible())
}

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
      cols$symbol$danger(cli::symbol$cross),
      " ",
      cols$message$info(msg),
      cols$message$info(" failed with: "),
      cols$message$danger(errors_msg)
    )
    return(invisible(NULL))
  }

  if (is.null(res)) {
    message(
      cols$symbol$info("i"),
      " ",
      cols$message$info(msg)
    )
  } else {
    all_null <- isTRUE(all(is.null(unlist(res$.metrics))))
    if (inherits(res, "try-error") || all_null) {
      if (all_null) {
        res <- collect_res_notes(res)
      }
      errors_msg <- gsub("\n", "", as.character(res))
      errors_msg <- gsub("Error : ", "", errors_msg, fixed = TRUE)
      message(
        cols$symbol$danger(cli::symbol$cross),
        " ",
        cols$message$info(msg),
        cols$message$info(" failed with "),
        cols$message$danger(errors_msg)
      )
    } else {
      time_msg <- paste0(" (", prettyunits::pretty_sec(elapsed[3]), ")")
      message(
        cols$symbol$success(cli::symbol$tick),
        " ",
        cols$message$info(msg),
        cols$message$info(time_msg)
      )
    }
  }

  invisible(NULL)
}

collect_res_notes <- function(x, show = 1) {
  y <- purrr::map_dfr(x$.notes, I)
  show <- min(show, nrow(y))
  y <- paste0(y$.notes[1:show])
  gsub("[\r\n]", "", y)
}
