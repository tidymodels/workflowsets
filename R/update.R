#' Update workflow componets inside a workflow set
#'
#' @param x A workflow set.
#' @param id A character string for values from the `wflow_id` column that
#' indicates which workflow to update.
#' @param spec A parsnip model specification.
#' @param formula An optional formula override to specify the terms of the
#' model. Typically, the terms are extracted from the formula or recipe
#' preprocessing methods. However, some models (like survival and bayesian
#' models) use the formula not to preprocess, but to specify the structure of
#' the model. In those cases, a formula specifying the model structure must be
#' passed unchanged into the model call itself. This argument is used for those
#' purposes.
#' @param recipe A recipe created using [recipes::recipe()].
#' @param blueprint A hardhat blueprint used for fine tuning the preprocessing.
#' If `NULL`, [hardhat::default_recipe_blueprint()] is used. Note that
#' preprocessing done here is separate from preprocessing that might be done
#' automatically by the underlying model.
#' @return An altered workflow set where the `results` column value for the
#' selected  `id` has been reset to empty.
#' @examples
#' library(parsnip)
#' new_mod <-
#'   decision_tree() %>%
#'   set_engine("rpart", method = "anova") %>%
#'   set_mode("classification")
#'
#' update_workflow_model(two_class_res, "none_cart", spec = new_mod)
#' @export
update_workflow_model <- function(x, id, spec, formula = NULL) {
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

update_workflow_recipe <- function(x, id, recipe, blueprint = NULL) {
   # stuff
}
