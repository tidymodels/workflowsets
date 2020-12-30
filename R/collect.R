#' Obtain and format results produced by tuning functions for workflow sets
#'
#' @param x A `workflow_set` object where all workflows have been evaluated.
#' @param summarize A logical for whether the performance estimates should be
#'  summarized via the mean (over resamples) or the raw performance values (per
#'  resample) should be returned along with the resampling identifiers.
#' @param parameters An optional tibble of tuning parameter values that can be
#'  used to filter the predicted values before processing. This tibble should
#'  only have columns for each tuning parameter identifier (e.g. `"my_param"`
#'  if `tune("my_param")` was used).
#' @param select_best A single logical for whether the numerically best results
#' are retained. If `TRUE`, the `parameters` argument is ignored.
#' @param metric A character string for the metric that is used for
#' `select_best`.
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
#'    workflow_map(resamples = val_set, grid = 10, metrics = metric_set(roc_auc))
#' cell_models
#' collect_metrics(cell_models)
#' }
#' @export
collect_metrics.workflow_set <- function(x, summarize = TRUE, ...) {
   check_incompete(x, fail = TRUE)
   x <-
      dplyr::mutate(
         x,
         metrics = purrr::map(
            result,
            collect_metrics,
            summarize = summarize
         ),
         metrics = purrr::map2(metrics, result, remove_parameters)
      )
   x <-
      dplyr::select(x, wflow_id, preproc, model, metrics) %>%
      tidyr::unnest(cols = c(metrics)) %>%
      reorder_cols()
   check_consistent_metrics(x, fail = FALSE)
   x
}

remove_parameters <- function(x, object) {
   prm <- tune::.get_tune_parameter_names(object)
   x <- dplyr::select(x,-dplyr::one_of(prm))
   x
}

reorder_cols <- function(x) {
   if (any(names(x) == ".iter")) {
      cols <- c("wflow_id", ".config", ".iter", "preproc", "model")
   } else {
      cols <- c("wflow_id", ".config", "preproc", "model")
   }
   dplyr::relocate(x, !!!cols)
}
maybe_add_iter <- function(x) {
   if (!any(names(x) == ".iter")) {
      x <- dplyr::mutate(x, .iter = 0)
   }
   x
}

#' @export
#' @rdname collect_metrics.workflow_set
collect_predictions.workflow_set <-
   function(x, summarize = TRUE, parameters = NULL, select_best = FALSE,
            metric = NULL, ...) {
      check_incompete(x, fail = TRUE)
      if (select_best) {
         x <-
            dplyr::mutate(x,
                          predictions = purrr::map(
                             result,
                             ~ select_bare_predictions(
                                .x,
                                summarize = summarize,
                                metric = metric
                             )
                          )
            )
      } else {
         x <-
            dplyr::mutate(
               x,
               predictions = purrr::map(
                  result,
                  get_bare_predictions,
                  summarize = summarize,
                  parameters = parameters
               )
            )
      }
      x <-
         dplyr::select(x, wflow_id, preproc, model, predictions) %>%
         tidyr::unnest(cols = c(predictions)) %>%
         reorder_cols()
      x
   }

select_bare_predictions <- function(x, metric, summarize) {
   res <-
      tune::collect_predictions(x,
                                summarize = summarize,
                                parameters = tune::select_best(x, metric = metric))
   remove_parameters(res, x)
}

get_bare_predictions <- function(x, ...) {
   res <- tune::collect_predictions(x, ...)
   remove_parameters(res, x)
}
