#' Example Data Sets
#'
#' @details These results are used for development purposes and will probably
#' be removed before a CRAN submissin (due to size).
#'
#' `chi_models` contains the results of the code shown in the readme file.
#'
#' `cell_models` was generated using this code:
#'
#' `two_class_set` and `two_class_res` were generated using the data in the
#' package file `example-data/two-class-set.R`
#'
#' \preformatted{
#' library(workflowsets)
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
#' cells <- cells %>% dplyr::select(-case) %>% dplyr::slice(1:200)
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
#'    workflow_map(resamples = val_set, grid = 10, seed = 1)
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

#' @name two_class_set
#' @rdname chi_models
#' @aliases two_class_set two_class_res
#' @docType data
#' @keywords datasets
NULL
