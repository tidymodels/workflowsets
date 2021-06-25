#' Extract elements from a workflow set
#'
#' `extract_workflow_set_result()` retrieves the results of [workflow_map()] for a
#' particular workflow while `extract_workflow()` extracts the unfitted workflow
#' from the `info` column. These functions supersede `pull_workflow_set_result()`
#' and `pull_workflow()` (since their names are more consistent with other
#' tidymodels functions).
#' @param x A workflow set.
#' @param id A single character string for a workflow ID.
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
#' @return `extract_workflow_set_result()` produces a `tune_result` or
#' `resample_results` object. `extract_workflow()` returns an unfit workflow
#' object.
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


