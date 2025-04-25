#' Update components of a workflow within a workflow set
#'
#' @description
#' Workflows can take special arguments for the recipe (e.g. a blueprint) or a
#' model (e.g. a special formula). However, when creating a workflow set, there
#' is no way to specify these extra components.
#'
#' `update_workflow_model()` and `update_workflow_recipe()` allow users to set
#' these values _after_ the workflow set is initially created. They are
#' analogous to [workflows::add_model()] or [workflows::add_recipe()].
#'
#' @inheritParams comment_add
#' @param id A single character string from the `wflow_id` column indicating
#' which workflow to update.
#' @inheritParams workflows::add_recipe
#' @inheritParams workflows::add_model
#'
#' @includeRmd man-roxygen/example_data.Rmd note
#'
#' @examples
#' library(parsnip)
#'
#' new_mod <-
#'   decision_tree() |>
#'   set_engine("rpart", method = "anova") |>
#'   set_mode("classification")
#'
#' new_set <- update_workflow_model(two_class_res, "none_cart", spec = new_mod)
#'
#' new_set
#'
#' extract_workflow(new_set, id = "none_cart")
#' @export
update_workflow_model <- function(x, id, spec, formula = NULL) {
  check_wf_set(x)
  check_string(id)
  check_formula(formula, allow_null = TRUE)

  wflow <- extract_workflow(x, id = id)
  wflow <- workflows::update_model(wflow, spec = spec, formula = formula)
  id_ind <- which(x$wflow_id == id)
  x$info[[id_ind]]$workflow[[1]] <- wflow
  # Remove any existing results since they are now inconsistent
  if (!identical(x$result[[id_ind]], list())) {
    x$result[[id_ind]] <- list()
  }
  x
}


#' @rdname update_workflow_model
#' @export
update_workflow_recipe <- function(x, id, recipe, blueprint = NULL) {
  check_wf_set(x)
  check_string(id)

  wflow <- extract_workflow(x, id = id)
  wflow <- workflows::update_recipe(
    wflow,
    recipe = recipe,
    blueprint = blueprint
  )
  id_ind <- which(x$wflow_id == id)
  x$info[[id_ind]]$workflow[[1]] <- wflow
  # Remove any existing results since they are now inconsistent
  if (!identical(x$result[[id_ind]], list())) {
    x$result[[id_ind]] <- list()
  }
  x
}
