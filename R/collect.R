#' Obtain and format results produced by tuning functions for workflow sets
#'
#' @param x A `workflow_set` object where all workflows have been evaluated.
#' @param summarize A logical for whether the performance estimates should be
#'  summarized via the mean (over resamples) or the raw performance values (per
#'  resample) should be returned along with the resampling identifiers. When
#'  collecting predictions, these are averaged if multiple assessment sets
#'  contain the same row.
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
#' @details
#'
#' When applied to a workflow set, the metrics and predictions that are returned
#' do not contain the actual tuning parameter columns and values (unlike when
#' these collect functions are run on other objects). The reason is that workflow
#' sets can contain different types of models or models with different tuning
#' parameters.
#'
#' If the columns are needed, there are two options. First, the `.config` column
#' can be used to merge the tuning parameter columns into an appropriate object.
#' Alternatively, the `map()` function can be used to get the metrics from the
#' original objects (see the example below).
#'
#' @examples
#' library(workflowsets)
#' library(modeldata)
#' library(recipes)
#' library(parsnip)
#' library(dplyr)
#' library(rsample)
#' library(tune)
#' library(yardstick)
#' library(purrr)
#' library(tidyr)
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
#'
#' # Alternatively, if the tuning parameter values are needed:
#' cell_models %>%
#'   dplyr::filter(grepl("knn", wflow_id)) %>%
#'   mutate(metrics = map(result, collect_metrics)) %>%
#'   dplyr::select(wflow_id, metrics) %>%
#'   tidyr::unnest(cols = metrics)
#'
#'
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
   info <- dplyr::bind_rows(x$info) %>% dplyr::select(-workflow)
   x <-
      dplyr::select(x, wflow_id, metrics) %>%
      dplyr::bind_cols(info) %>%
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
      info <- dplyr::bind_rows(x$info) %>% dplyr::select(-workflow)
      x <-
         dplyr::select(x, wflow_id, predictions) %>%
         dplyr::bind_cols(info) %>%
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

collect_notes <- function(x, show = 1) {
   y <- purrr::map_dfr(x$.notes, I)
   show <- min(show, nrow(y))
   y <- paste0(y$.notes[1:show])
   gsub("[\r\n]", "", y)
}

