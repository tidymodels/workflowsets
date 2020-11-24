#' Generate a set of workflow objects from preprocessing and model objects
#'
#' @param preproc A list (preferably named) with preprocessing objects:
#'  formulas, recipes, or selectors.
#' @param models A list (preferably named) of `parsnip` model specifications.
#' @param cross A logical: should all combinations of the preprocessors and
#'  models be used to create the workflows? If `FALSE`, the length of `preproc`
#'  and `models` should be equal.
#' @return A tibble with extra class 'workflow_set'.
#' @examples
#' library(tbd)
#' library(modeldata)
#' library(recipes)
#' library(parsnip)
#' library(dplyr)
#' library(rsample)
#' library(tune)
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
#' cell_models
#' @export
workflow_set <- function(preproc, models, cross = FALSE) {
   if (!cross && length(preproc) == length(models)) {
      rlang::abort(
         "The lengths of 'preproc' and 'models' are different and `cross = FALSE`."
      )
   }

   preproc <- fix_list_names(preproc)
   models <- fix_list_names(models)

   if (cross) {
      res <- cross_objects(preproc, models)
   } else {
      res <- fuse_objects(preproc, models)
   }
   res <-
      res %>%
      dplyr::mutate(
         objects  = purrr::map2(preproc, models, make_workflow),
         objects  = unname(objects),
         preprocs = purrr::map_chr(objects, preproc_type),
         models   = purrr::map_chr(objects, model_type),
         results  = purrr::map(1:nrow(res), ~ list())
      ) %>%
      dplyr::select(wflow_id, preprocs, models, objects, results)
   new_workflow_set(res)
}

new_workflow_set <- function(x) {
 req_cols <- c("wflow_id", "objects", "preprocs", "models", "results")
 if (!tibble::is_tibble(x)) {
    halt("The object should be a tibble.")
 }
 if (!all(req_cols %in% names(x))) {
    halt(
       "The object should have columns: ",
       paste0("'", req_cols, "'", collapse = ", "),
       "."
    )
 }
 if (!is.list(x$objects)) {
    halt("The 'objects' column should be a list.")
 }
 if (!is.list(x$results)) {
    halt("The 'results' column should be a list.")
 }
 if (!is.character(x$wflow_id)) {
    halt("The 'wflow_id' column should be character.")
 }
 if (max(table(x$wflow_id)) > 1 | any(x$wflow_id == "") | any(is.na(x$wflow_id))) {
    halt("The 'wflow_id' column should contain unique, non-missing character strings.")
 }
 is_workflow <- purrr::map_lgl(x$objects, ~ inherits(.x, "workflow"))
 if (!all(is_workflow)) {
    bad <- x$wflow_id[!is_workflow]
    halt("The following elements of the 'objects' column are not workflow ",
         "objects: ", paste0("'", bad, "'", collapse = ", "), ".")
 }
 if (!is.character(x$preprocs)) {
    halt("The 'preprocs' column should be character.")
 }
 if (!is.character(x$models)) {
    halt("The 'models' column should be character.")
 }
 class(x) <- c("workflow_set", class(tibble::tibble()))
 x
}

preproc_type <- function(x) {
   x <- workflows::pull_workflow_preprocessor(x)
   class(x)[1]
}

model_type <- function(x) {
   x <- workflows::pull_workflow_spec(x)
   class(x)[1]
}

fix_list_names <- function(x) {
   prefix <- purrr::map_chr(x, ~ class(.x)[1])
   prefix <- vctrs::vec_as_names(prefix, repair = "unique", quiet = TRUE)
   prefix <- gsub("\\.\\.\\.", "_", prefix)
   nms <- names(x)
   if (is.null(nms)) {
      names(x) <- prefix
   } else if (any(nms == "")) {
      no_name <- which(nms == "")
      names(x)[no_name] <- prefix[no_name]
   }
   x
}


cross_objects <- function(preproc, models) {
   tidyr::crossing(preproc, models) %>%
      dplyr::mutate(pp_nm = names(preproc), mod_nm = names(models)) %>%
      dplyr::mutate(wflow_id = paste(pp_nm, mod_nm, sep = "_")) %>%
      dplyr::select(wflow_id, preproc, models)
}

fuse_objects <- function(preproc, models) {
   if (length(preproc) == 1 | length(models) == 1) {
      return(cross_objects(preproc, models))
   }
   nms <-
      tibble::tibble(wflow_id = paste(names(preproc), names(models), sep = "_"))

   tibble::tibble(preproc = preproc, models = models) %>%
      dplyr::bind_cols(nms)
}

