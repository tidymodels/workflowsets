#' Obtain and format results produced by tuning functions for workflow sets
#'
#' @param x A `workflow_set` object where all workflows have been evaluated.
#' @param summarize A logical for whether the performance estimates should be
#'  summarized via the mean (over resamples) or the raw performance values (per
#'  resample) should be returned along with the resampling identifiers.
#' @param ... Not currently used.
#' @return A tibble.
#' @examples
#' library(tbd)
#' library(modeldata)
#' library(recipes)
#' library(parsnip)
#' library(dplyr)
#' library(rsample)
#' library(tune)
#' library(yardstick)
#'
#' # ------------------------------------------------------------------------------
#'
#' data(cells)
#' cells <- cells %>% dplyr::select(-case)
#'
#' set.seed(1)
#' val_set <- validation_split(cells)
#'
#' # ------------------------------------------------------------------------------
#'
#' basic_recipe <-
#'    recipe(class ~ ., data = cells) %>%
#'    step_YeoJohnson(all_predictors()) %>%
#'    step_normalize(all_predictors())
#'
#' pca_recipe <-
#'    basic_recipe %>%
#'    step_pca(all_predictors(), num_comp = tune())
#'
#' ss_recipe <-
#'    basic_recipe %>%
#'    step_spatialsign(all_predictors())
#'
#' # ------------------------------------------------------------------------------
#'
#' knn_mod <-
#'    nearest_neighbor(neighbors = tune(), weight_func = tune()) %>%
#'    set_engine("kknn") %>%
#'    set_mode("classification")
#'
#' lr_mod <-
#'    logistic_reg() %>%
#'    set_engine("glm")
#'
#' # ------------------------------------------------------------------------------
#'
#' preproc <- list(none = basic_recipe, pca = pca_recipe, sp_sign = ss_recipe)
#' models <- list(knn = knn_mod, logistic = lr_mod)
#'
#' cell_models <- workflow_set(preproc, models, cross = TRUE)
#'
#' # ------------------------------------------------------------------------------
#'
#' \donttest{
#' cell_models <-
#'    cell_models %>%
#'    tune_grid(resamples = val_set, grid = 10, metrics = metric_set(roc_auc)) %>%
#'    fit_resamples(resamples = val_set, metrics = metric_set(roc_auc))
#' cell_models
#' collect_metrics(cell_models)
#' }
#' @export
collect_metrics.workflow_set <- function(x, summarize = TRUE, ...) {
   check_incompete(x, fail = TRUE)
   res <- purrr::map(x$results, tune::collect_metrics, summarize = summarize)
   check_consistent_metrics(res, fail = FALSE)
   res <- purrr::map2(res, x$wflow_id, add_object_name)
   res <- purrr::map2(res, x$preprocs, add_preproc_name)
   res <- purrr::map2(res, x$models,   add_model_name)
   param <- purrr::map(x$objects, ~ tune::tune_args(.x)$id)
   res <- purrr::map2(res, param, nest_cols)
   all_names <- purrr::map(res, ~ names(.x))
   all_names <- unique(unlist(all_names))
   if (any(all_names == ".iter")) {
      res <- purrr::map(res, maybe_add_iter)
   }
   res <- dplyr::bind_rows(res)
   not_ids <- names(res)[!(names(res) %in% c("wflow_id", "info", ".config"))]
   res <- dplyr::select(res, wflow_id, info, .config, !!!not_ids)
   res
}

nest_cols <- function(x, nms) {
   nest_cols <- c("preprocs", "models")
   if (length(nms) > 0) {
      nest_cols <- c(nms, nest_cols)
   }
   res <- tidyr::nest(x, info = nest_cols)
   res
}

add_object_name <- function(x, nms) {
   dplyr::mutate(x, wflow_id = nms)
}
add_preproc_name <- function(x, nms) {
   dplyr::mutate(x, preprocs = nms)
}
add_model_name <- function(x, nms) {
   dplyr::mutate(x, models = nms)
}
maybe_add_iter <- function(x) {
   if (!any(names(x) == ".iter")) {
      x <- dplyr::mutate(x, .iter = 0)
   }
   x
}
