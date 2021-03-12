#' Generate a set of workflow objects from preprocessing and model objects
#'
#' @param preproc A list (preferably named) with preprocessing objects:
#'  formulas, recipes, or [workflows::workflow_variables()].
#' @param models A list (preferably named) of `parsnip` model specifications.
#' @param cross A logical: should all combinations of the preprocessors and
#'  models be used to create the workflows? If `FALSE`, the length of `preproc`
#'  and `models` should be equal.
#' @seealso [workflow_map()], [comment_add()], [option_add()],
#' [as_workflow_set()]
#' @details
#' The preprocessors that can be combined with the model objects can include:
#'
#'  * A basic R formula.
#'  * A recipe definition (un-prepared).
#'  * A selectors object.
#'
#' Since `preproc` is a named list column, any combination of these can be
#' used in that argument (i.e., `preproc` can be mixed types).
#'
#' @return A tibble with extra class 'workflow_set'. A new set includes four
#' columns (but others can be added):
#'
#'  * `wflow_id` contains character strings for the preprocessor/workflow
#'     combination. These can be changed but must be unique.
#'  * `info` is a list column with tibbles containing more specific information,
#'     including any comments added using [comment_add()]. This tibble also
#'     contains the workflow object (which can be easily retrieved using
#'     [pull_workflow()]).
#'  * `option` is a list column that will include a list of optional arguments
#'     passed to the functions from the `tune` package. They can be added
#'     manually via [option_add()] or automatically when options are passed to
#'     [workflow_map()].
#'  * `result` is a list column that will contain any objects produced when
#'     [workflow_map()] is used.
#' @examples
#' library(workflowsets)
#' library(workflows)
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
#' cell_set <- workflow_set(preproc, models, cross = TRUE)
#' cell_set
#'
#' # ------------------------------------------------------------------------------
#' # Using variables and formulas
#'
#' # Select predictors by their names
#' channels <- paste0("ch_", 1:4)
#' preproc <- purrr::map(channels, ~ workflow_variables(class, c(contains(!!.x))))
#' names(preproc) <- channels
#' preproc$everything <- class ~ .
#' preproc
#'
#' cell_set_by_group <- workflow_set(preproc, models["logistic"], cross = TRUE)
#' cell_set_by_group
#' @export
workflow_set <- function(preproc, models, cross = TRUE) {
   if (length(preproc) != length(models) &
       (length(preproc) != 1 & length(models) != 1 &
        !cross)
   ) {
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
         workflow  = purrr::map2(preproc, model, make_workflow),
         workflow  = unname(workflow),
         info = purrr::map(workflow, get_info),
         option  = purrr::map(1:nrow(res), ~ new_options()),
         result   = purrr::map(1:nrow(res), ~ list())
      ) %>%
      dplyr::select(wflow_id, info, option, result)
   new_workflow_set(res)
}

get_info <- function(x) {
   tibble::tibble(workflow = list(x),
                  preproc = preproc_type(x),
                  model = model_type(x),
                  comment = character(1))
}

new_workflow_set <- function(x) {
   req_cols <- c("wflow_id", "info", "option", "result")
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
   if (!is.list(x$info)) {
      halt("The 'info' column should be a list.")
   }
   if (!is.list(x$result)) {
      halt("The 'result' column should be a list.")
   }
   if (!is.list(x$option)) {
      halt("The 'option' column should be a list.")
   }
   if (!is.character(x$wflow_id)) {
      halt("The 'wflow_id' column should be character.")
   }
   if (max(table(x$wflow_id)) > 1 | any(x$wflow_id == "") | any(is.na(x$wflow_id))) {
      halt("The 'wflow_id' column should contain unique, non-missing character strings.")
   }
   is_tibble <- purrr::map_lgl(x$info, ~ inherits(.x, "tbl_df"))
   if (!all(is_tibble)) {
      bad <- x$wflow_id[!is_tibble]
      halt("The following elements of the 'info' column are not tibbles: ",
           paste0("'", bad, "'", collapse = ", "), ".")
   }
   tbl_nms <- purrr::map(x$info, names)
   exp_nms <- c("workflow", "preproc", "model", "comment")
   check_nms <- purrr::map_lgl(tbl_nms, ~ identical(.x, exp_nms))
   if (!all(check_nms)) {
      bad <- x$wflow_id[!check_nms]
      halt("The following elements of the 'info' column do not have the correct elements: ",
           paste0("'", bad, "'", collapse = ", "), ".")
   }

   check_for_tune_results(x$result)
   check_consistent_resamples(x)

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
      dplyr::select(wflow_id, preproc, model = models)
}

fuse_objects <- function(preproc, models) {
   if (length(preproc) == 1 | length(models) == 1) {
      return(cross_objects(preproc, models))
   }
   nms <-
      tibble::tibble(wflow_id = paste(names(preproc), names(models), sep = "_"))

   tibble::tibble(preproc = preproc, model = models) %>%
      dplyr::bind_cols(nms)
}


# TODO api for correlation analysis?
# TODO select_best methods (req tune changes)

#' @export
tbl_sum.workflow_set <- function(x) {
   orig <- NextMethod()
   c("A workflow set/tibble" = unname(orig))
}

