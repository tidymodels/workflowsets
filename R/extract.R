#' Extract elements of workflow sets
#'
#' @description
#' These functions extract various elements from a workflow object. If they do
#' not exist yet, an error is thrown.
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
#'   with the `"lm"` engine, this would return the underlying `lm` object.
#'
#' - `extract_mold()` returns the preprocessed "mold" object returned
#'   from [hardhat::mold()]. It contains information about the preprocessing,
#'   including either the prepped recipe, the formula terms object, or
#'   variable selectors.
#'
#' - `extract_recipe()` returns the recipe. The `estimated` argument specifies
#'    whether the fitted or original recipe is returned.
#'
#'  - `extract_workflow_set_result()` retrieves the results of [workflow_map()]
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
#' Extracting the underlying fit objects can be helpful for describing the
#'  model via `print()`, `summarize()`, `plot()`, and so on.
#'
#' However, users should not invoke the `predict()` method on an extracted
#'  model. There may be preprocessing operations that `workflows` has executed on
#'  the data prior to giving it to the model. Bypassing these can lead to errors
#'  or silently generating incorrect predictions.
#'
#' *Good*:
#' ```r
#'    workflow_fit %>% predict(new_data)
#' ```
#'
#' *Bad*:
#' ```r
#'    workflow_fit %>% extract_fit_engine()  %>% predict(new_data)
#'    # or
#'    workflow_fit %>% extract_fit_parsnip() %>% predict(new_data)
#' ```
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
   check_id(x, id)
   y <- x %>% dplyr::filter(wflow_id == id[1])
   y$result[[1]]
}

#' @export
#' @rdname extract_workflow_set_result
extract_workflow.workflow_set <- function(x, id, ...) {
   check_id(x, id)
   y <- x %>% dplyr::filter(wflow_id == id[1])
   y$info[[1]]$workflow[[1]]
}

#' @export
#' @rdname extract_workflow_set_result
extract_spec_parsnip.workflow_set <- function(x, id, ...) {
   check_id(x, id)
   y <- x %>% dplyr::filter(wflow_id == id[1])
   extract_spec_parsnip(y$info[[1]]$workflow[[1]])
}


#' @export
#' @rdname extract_workflow_set_result
extract_recipe.workflow_set <- function(x, id, estimated = TRUE, ...) {
   check_id(x, id)
   y <- x %>% dplyr::filter(wflow_id == id[1])
   extract_recipe(y$info[[1]]$workflow[[1]], estimated = estimated)
}

#' @export
#' @rdname extract_workflow_set_result
extract_fit_parsnip.workflow_set <- function(x, id, ...) {
   check_id(x, id)
   y <- x %>% dplyr::filter(wflow_id == id[1])
   extract_fit_parsnip(y$info[[1]]$workflow[[1]])
}

#' @export
#' @rdname extract_workflow_set_result
extract_fit_engine.workflow_set <- function(x, id, ...) {
   check_id(x, id)
   y <- x %>% dplyr::filter(wflow_id == id[1])
   extract_fit_engine(y$info[[1]]$workflow[[1]])
}

#' @export
#' @rdname extract_workflow_set_result
extract_mold.workflow_set <- function(x, id, ...) {
   check_id(x, id)
   y <- x %>% dplyr::filter(wflow_id == id[1])
   extract_mold(y$info[[1]]$workflow[[1]])
}

#' @export
#' @rdname extract_workflow_set_result
extract_preprocessor.workflow_set <- function(x, id, ...) {
   check_id(x, id)
   y <- x %>% dplyr::filter(wflow_id == id[1])
   extract_preprocessor(y$info[[1]]$workflow[[1]])
}

# ------------------------------------------------------------------------------


check_id <- function(x, id) {
   if (length(id) != 1) {
      rlang::abort("'id' should have a single value.")
   }
   y <- x %>% dplyr::filter(wflow_id == id[1])
   if (nrow(y) != 1) {
      halt("No workflow ID found for '", id[1], "'")
   }
   invisible(NULL)
}


