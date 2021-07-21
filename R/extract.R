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
#'  - `extract_workflow_set_result()` returns the results of [workflow_map()]
#'    for a particular workflow.
#'
#' - `extract_workflow()` returns the workflow object. The workflow will not
#'    have been estimated.
#'
#' @param x A workflow set.
#' @param id A single character string for a workflow ID.
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
#' @examples
#' library(tune)
#'
#' extract_workflow_set_result(two_class_res, "none_cart")
#'
#' extract_workflow(two_class_res, "none_cart")
#' @export
extract_workflow_set_result <- function(x, id, ...) {
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
      rlang::abort("`estimated` must be a single `TRUE` or `FALSE`.")
   }
   y <- filter_id(x, id)
   extract_recipe(y$info[[1]]$workflow[[1]], estimated = estimated)
}
check_empty_dots <- function(...) {
   opts <- list(...)
   if (any(names(opts) == "estimated")) {
      rlang::abort("'estimated' should be a named argument.")
   }
   if (length(opts) > 0) {
      rlang::abort("'...' are not used in this function.")
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

# ------------------------------------------------------------------------------

filter_id <- function(x, id) {
   if (!rlang::is_string(id)) {
      halt("`id` must be a single string.")
   }
   out <- dplyr::filter(x, wflow_id == id)
   if (nrow(out) != 1L) {
      halt("`id` must correspond to a single row in `x`.")
   }
   out
}
