#' Extract elements of workflow sets
#'
#' @description
#' These functions extract various elements from a workflow set object. If they
#' do not exist yet, an error is thrown.
#'
#' - `extract_preprocessor()` returns the formula, recipe, or variable
#'   expressions used for preprocessing.
#'
#' - `extract_spec_parsnip()` returns the parsnip model specification.
#'
#' - `extract_fit_parsnip()` returns the parsnip model fit object.
#'
#' - `extract_fit_engine()` returns the engine specific fit embedded within
#'   a parsnip model fit. For example, when using [parsnip::linear_reg()]
#'   with the `"lm"` engine, this returns the underlying `lm` object.
#'
#' - `extract_mold()` returns the preprocessed "mold" object returned
#'   from [hardhat::mold()]. It contains information about the preprocessing,
#'   including either the prepped recipe, the formula terms object, or
#'   variable selectors.
#'
#' - `extract_recipe()` returns the recipe. The `estimated` argument specifies
#'    whether the fitted or original recipe is returned.
#'
#' - `extract_workflow_set_result()` returns the results of [workflow_map()]
#'    for a particular workflow.
#'
#' - `extract_workflow()` returns the workflow object. The workflow will not
#'    have been estimated.
#'
#' - `extract_parameter_set_dials()` returns the parameter set
#'   _that will be used to fit_ the supplied row `id` of the workflow set.
#'   Note that workflow sets reference a parameter set associated with the
#'   `workflow` contained in the `info` column by default, but can be
#'   fitted with a modified parameter set via the [option_add()] interface.
#'   This extractor returns the latter, if it exists, and returns the former
#'   if not, mirroring the process that [workflow_map()] follows to provide
#'   tuning functions a parameter set.
#'
#' - `extract_parameter_dials()` returns the `parameters` object
#'    _that will be used to fit_ the supplied tuning `parameter` in the supplied
#'   row `id` of the workflow set. See the above notes in
#'   `extract_parameter_set_dials()` on precedence.
#'
#' @inheritParams comment_add
#' @param id A single character string for a workflow ID.
#' @param parameter A single string for the parameter ID.
#' @param estimated A logical for whether the original (unfit) recipe or the
#' fitted recipe should be returned.
#' @param ... Other options (not currently used).
#' @details
#'
#' These functions supersede the `pull_*()` functions (e.g.,
#' [extract_workflow_set_result()]).
#' @return
#' The extracted value from the object, `x`, as described in the
#' description section.
#'
#' @includeRmd man-roxygen/example_data.Rmd note
#'
#' @examples
#' library(tune)
#'
#' two_class_res
#'
#' extract_workflow_set_result(two_class_res, "none_cart")
#'
#' extract_workflow(two_class_res, "none_cart")
#' @export
extract_workflow_set_result <- function(x, id, ...) {
  check_wf_set(x)
  y <- filter_id(x, id)
  y$result[[1]]
}

#' @export
#' @rdname extract_workflow_set_result
extract_workflow.workflow_set <- function(x, id, ...) {
  y <- filter_id(x, id)
  y$info[[1]]$workflow[[1]]
}

#' @export
#' @rdname extract_workflow_set_result
extract_spec_parsnip.workflow_set <- function(x, id, ...) {
  y <- filter_id(x, id)
  extract_spec_parsnip(y$info[[1]]$workflow[[1]])
}


#' @export
#' @rdname extract_workflow_set_result
extract_recipe.workflow_set <- function(x, id, ..., estimated = TRUE) {
  check_empty_dots(...)
  if (!rlang::is_bool(estimated)) {
    cli::cli_abort(
      "{.arg estimated} must be a single {.code TRUE} or {.code FALSE}."
    )
  }
  y <- filter_id(x, id)
  extract_recipe(y$info[[1]]$workflow[[1]], estimated = estimated)
}
check_empty_dots <- function(..., call = caller_env()) {
  opts <- list(...)
  if (any(names(opts) == "estimated")) {
    cli::cli_abort("{.arg estimated} should be a named argument.", call = call)
  }
  if (length(opts) > 0) {
    cli::cli_abort("{.arg ...} are not used in this function.", call = call)
  }
  invisible(NULL)
}


#' @export
#' @rdname extract_workflow_set_result
extract_fit_parsnip.workflow_set <- function(x, id, ...) {
  y <- filter_id(x, id)
  extract_fit_parsnip(y$info[[1]]$workflow[[1]])
}

#' @export
#' @rdname extract_workflow_set_result
extract_fit_engine.workflow_set <- function(x, id, ...) {
  y <- filter_id(x, id)
  extract_fit_engine(y$info[[1]]$workflow[[1]])
}

#' @export
#' @rdname extract_workflow_set_result
extract_mold.workflow_set <- function(x, id, ...) {
  y <- filter_id(x, id)
  extract_mold(y$info[[1]]$workflow[[1]])
}

#' @export
#' @rdname extract_workflow_set_result
extract_preprocessor.workflow_set <- function(x, id, ...) {
  y <- filter_id(x, id)
  extract_preprocessor(y$info[[1]]$workflow[[1]])
}

#' @export
#' @rdname extract_workflow_set_result
extract_parameter_set_dials.workflow_set <- function(x, id, ...) {
  y <- filter_id(x, id)

  if ("param_info" %in% names(y$option[[1]])) {
    return(y$option[[1]][["param_info"]])
  }

  extract_parameter_set_dials(y$info[[1]]$workflow[[1]])
}

#' @export
#' @rdname extract_workflow_set_result
extract_parameter_dials.workflow_set <- function(x, id, parameter, ...) {
  res <- extract_parameter_set_dials(x, id)
  res <- extract_parameter_dials(res, parameter)

  res
}

# ------------------------------------------------------------------------------

filter_id <- function(x, id, call = caller_env()) {
  check_string(id)
  out <- dplyr::filter(x, wflow_id == id)
  if (nrow(out) != 1L) {
    cli::cli_abort(
      "{.arg id} must correspond to a single row in {.arg x}.",
      call = call
    )
  }
  out
}
