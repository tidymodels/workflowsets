#' Example Data Sets
#'
#' @details These results are used for development purposes and will probably
#' be removed before a CRAN submissin (due to size).
#'
#' `chi_models` contains the results of the code whown in the readme file.
#'
#' `cell_models` was generated using this code:
#'
#' \preformatted{
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
#' cell_models <-
#'    cell_models %>%
#'    tune_grid(resamples = val_set, grid = 10) %>%
#'    fit_resamples(resamples = val_set)
#' }
#'
#' @name chi_models
#' @aliases chi_models
#' @docType data
#' @return A workflow set.
#' @keywords datasets
#' @examples
#' data(chi_models)
#' chi_models
NULL


#' @name cell_models
#' @rdname chi_models
#' @aliases cell_models
#' @docType data
#' @keywords datasets
NULL
