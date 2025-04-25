check_wf_set <- function(x, arg = caller_arg(x), call = caller_env()) {
  if (!inherits(x, "workflow_set")) {
    cli::cli_abort(
      "{arg} must be a workflow set, not {obj_type_friendly(x)}.",
      call = call
    )
  }

  invisible(TRUE)
}

check_consistent_metrics <- function(x, fail = TRUE, call = caller_env()) {
  metric_info <-
    dplyr::distinct(x, .metric, wflow_id) |>
    dplyr::mutate(has = TRUE) |>
    tidyr::pivot_wider(
      names_from = ".metric",
      values_from = "has",
      values_fill = FALSE
    ) |>
    dplyr::select(-wflow_id) |>
    purrr::map_dbl(\(.x) sum(!.x))

  if (any(metric_info > 0)) {
    incp_metrics <- names(metric_info)[metric_info > 0]

    if (fail) {
      cli::cli_abort(
        "The metrics {incp_metrics} were not used in all workflows.",
        call = call
      )
    } else {
      cli::cli_warn(
        "The metrics {incp_metrics} were not used in all workflows.",
        call = call
      )
    }
  }
  invisible(NULL)
}

check_incompete <- function(x, fail = TRUE, call = caller_env()) {
  empty_res <- purrr::map_lgl(x$result, \(.x) identical(.x, list()))
  failed_res <- purrr::map_lgl(x$result, \(.x) inherits(.x, "try-error"))

  n_empty <- sum(empty_res | failed_res)
  if (n_empty > 0) {
    if (fail) {
      cli::cli_abort(
        "There {?was/were} {n_empty} workflow{?s} that had no results.",
        call = call
      )
    } else {
      cli::cli_warn(
        "There {?was/were} {n_empty} workflow{?s} that had no results.",
        call = call
      )
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

check_options <- function(
  model,
  id,
  global,
  action = "fail",
  call = caller_env()
) {
  res <- purrr::map_chr(model, common_options, global)
  flag <- nchar(res) > 0
  if (any(flag)) {
    msg <- "There are existing options that are being modified\n"
    msg <- paste0(msg, paste0("\t", id[flag], ": ", res[flag], collapse = "\n"))
    if (action == "fail") {
      cli::cli_abort(msg, call = call)
    }
    if (action == "warn") {
      cli::cli_warn(msg, call = call)
    }
  }
  invisible(NULL)
}

check_tune_args <- function(x, call = caller_env()) {
  arg_names <- c(
    "resamples",
    "param_info",
    "grid",
    "metrics",
    "control",
    "iter",
    "objective",
    "initial",
    "eval_time"
  )
  bad_args <- setdiff(x, arg_names)
  if (length(bad_args) > 0) {
    cli::cli_abort(
      "The option{?s} {.arg {bad_args}} cannot be used as {?an argument/arguments}
      for {.fn fit_resamples} or the {.fn tune_*} functions.",
      call = call
    )
  }
  invisible(NULL)
}

# in case there are no tuning parameters, we can avoid warnings

recheck_options <- function(opts, .fn) {
  if (.fn == "fit_resamples") {
    allowed <- c("object", "resamples", "metrics", "control", "eval_time")
    nms <- names(opts)
    disallowed <- !(nms %in% allowed)
    if (any(disallowed)) {
      opts <- opts[!disallowed]
    }
  }
  opts
}


check_fn <- function(fn, x, verbose) {
  has_tune <- nrow(tune::tune_args(x)) > 0
  if (!has_tune & !fn %in% c("fit_resamples", "tune_cluster")) {
    fn <- "fit_resamples"
    if (verbose) {
      cols <- tune::get_tune_colors()
      msg <- "No tuning parameters. `fit_resamples()` will be attempted"
      message(cols$symbol$info("i"), "\t", cols$message$info(msg))
    }
  }
  fn
}

check_names <- function(x, call = caller_env()) {
  nms <- names(x)
  if (any(nms == "")) {
    bad <- which(nms == "")
    cli::cli_abort("Objects in the positions {bad} are not named.", call = call)
  } else if (all(is.null(nms))) {
    cli::cli_abort("The values must be named.", call = call)
  }
  xtab <- table(nms)
  if (any(xtab > 1)) {
    cli::cli_abort(
      "The workflow names should be unique: {.val {names(xtab)[xtab > 1]}}.",
      call = call
    )
  }
  invisible(NULL)
}

check_for_workflow <- function(x, call = caller_env()) {
  no_wflow <- purrr::map_lgl(x, \(.x) !inherits(.x, "workflow"))
  if (any(no_wflow)) {
    bad <- names(no_wflow)[no_wflow]
    cli::cli_abort(
      c(
        "The objects {.val {bad}} do not have workflows.",
        "i" = "Use the control option {.code save_workflow} and re-run."
      ),
      call = call
    )
  }
  invisible(NULL)
}


has_required_container_type <- function(x) {
  rlang::is_list(x)
}
has_required_container_columns <- function(x) {
  columns <- required_container_columns()
  ok <- all(columns %in% names(x))
  ok
}
required_container_columns <- function() {
  c("wflow_id", "info", "option", "result")
}


has_valid_column_info_structure <- function(x) {
  info <- x$info
  rlang::is_list(info)
}
has_valid_column_info_inner_types <- function(x) {
  info <- x$info
  is_tibble_indicator <- purrr::map_lgl(info, tibble::is_tibble)
  all(is_tibble_indicator)
}
has_valid_column_info_inner_names <- function(x) {
  columns <- required_info_inner_names()
  info <- x$info
  list_of_names <- purrr::map(info, names)
  has_names_indicator <- purrr::map_lgl(list_of_names, identical, y = columns)
  all(has_names_indicator)
}
required_info_inner_names <- function() {
  c("workflow", "preproc", "model", "comment")
}


has_valid_column_result_structure <- function(x) {
  result <- x$result
  rlang::is_list(result)
}
has_valid_column_result_inner_types <- function(x) {
  result <- x$result
  valid_indicator <- purrr::map_lgl(result, is_valid_result_inner_type)
  all(valid_indicator)
}
has_valid_column_result_fingerprints <- function(x) {
  result <- x$result

  # Drop default results
  default_indicator <- purrr::map_lgl(result, is_default_result_element)
  result <- result[!default_indicator]

  # Not sure how to fingerprint racing objects just yet. See
  # https://github.com/tidymodels/rsample/issues/212
  racing_indicator <- purrr::map_lgl(result, inherits, "tune_race")
  result <- result[!racing_indicator]

  tune_indicator <- purrr::map_lgl(result, inherits, "tune_results")
  result <- result[tune_indicator]
  if (length(result) > 0) {
    hashes <- purrr::map_chr(result, rsample::.get_fingerprint)
  } else {
    hashes <- NA_character_
  }

  # Drop NAs for results created before rsample 0.1.0, which won't have a hash
  pre_0.1.0 <- is.na(hashes)
  hashes <- hashes[!pre_0.1.0]
  result <- result[!pre_0.1.0]

  if (rlang::is_empty(hashes)) {
    # No hashes to check
    TRUE
  } else {
    # Should collapse to a single hash value if all resamples are the same
    uniques <- unique(hashes)
    length(uniques) == 1L
  }
}
is_valid_result_inner_type <- function(x) {
  if (is_default_result_element(x)) {
    # Default, before any results are filled
    return(TRUE)
  }
  is.null(x) || inherits(x, "tune_results") || inherits(x, "try-error")
}
is_default_result_element <- function(x) {
  identical(x, list())
}

has_valid_column_option_structure <- function(x) {
  option <- x$option
  rlang::is_list(option)
}
has_valid_column_option_inner_types <- function(x) {
  option <- x$option
  valid_options_indicator <- purrr::map_lgl(
    option,
    inherits,
    "workflow_set_options"
  )
  all(valid_options_indicator)
}


has_valid_column_wflow_id_structure <- function(x) {
  wflow_id <- x$wflow_id
  rlang::is_character(wflow_id)
}
has_valid_column_wflow_id_strings <- function(x) {
  wflow_id <- x$wflow_id
  uniques <- unique(wflow_id)

  if (length(wflow_id) != length(uniques)) {
    return(FALSE)
  }
  if (any(is.na(wflow_id))) {
    return(FALSE)
  }
  if (any(wflow_id == "")) {
    return(FALSE)
  }

  TRUE
}

has_all_pkgs <- function(w) {
  pkgs <- generics::required_pkgs(w, infra = FALSE)
  if (length(pkgs) > 0) {
    is_inst <- purrr::map_lgl(
      pkgs,
      \(.x) rlang::is_true(requireNamespace(.x, quietly = TRUE))
    )
    if (!all(is_inst)) {
      cols <- tune::get_tune_colors()
      msg <- paste0(
        "The workflow requires packages that are not installed: ",
        paste0("'", cols$message$danger(pkgs[!is_inst]), "'", collapse = ", "),
        ". Skipping this workflow."
      )
      message(
        cols$symbol$danger(cli::symbol$cross),
        " ",
        cols$message$warning(msg)
      )
      res <- FALSE
    } else {
      res <- TRUE
    }
  } else {
    res <- TRUE
  }
  res
}
