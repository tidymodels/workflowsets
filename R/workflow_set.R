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
#' # Using selector functions and formulas
#'
#' # Select predictors by their names
#' channels <- paste0("ch_", 1:4)
#' preproc <- purrr::map(channels, ~ selectors(class, c(contains(!!.x))))
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

preproc_type <- function(x) {
   x <- workflows::pull_workflow_preprocessor(x)
   res <- class(x)[1]
   if (res == "list") {
      res <- "selector"
   }
   res
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

# ------------------------------------------------------------------------------

#' @export
tbl_sum.workflow_set <- function(x) {
   orig <- NextMethod()
   c("A workflow set/tibble" = unname(orig))
}

# ------------------------------------------------------------------------------

#' @export
`[.workflow_set` <- function(x, i, j, drop = FALSE, ...) {
   out <- NextMethod()
   workflow_set_maybe_reconstruct(out)
}

# ------------------------------------------------------------------------------

#' @export
`names<-.workflow_set` <- function(x, value) {
   out <- NextMethod()
   workflow_set_maybe_reconstruct(out)
}

# ------------------------------------------------------------------------------

new_workflow_set <- function(x) {
   if (!has_required_container_type(x)) {
      halt("`x` must be a list.")
   }
   if (!has_required_container_columns(x)) {
      columns <- required_container_columns()
      halt(
         "The object should have columns: ",
         paste0("'", columns, "'", collapse = ", "),
         "."
      )
   }

   if (!has_valid_column_info_structure(x)) {
      halt("The 'info' column should be a list.")
   }
   if (!has_valid_column_info_inner_types(x)) {
      halt("All elements of 'info' must be tibbles.")
   }
   if (!has_valid_column_info_inner_names(x)) {
      columns <- required_info_inner_names()
      halt(
         "The 'info' columns should have columns: ",
         paste0("'", columns, "'", collapse = ", "),
         "."
      )
   }

   if (!has_valid_column_result_structure(x)) {
      halt("The 'result' column should be a list.")
   }
   if (!has_valid_column_result_inner_types(x)) {
      halt("Some elements of 'result' do not have class `tune_results`.")
   }
   if (!has_valid_column_result_fingerprints(x)) {
      halt(
         "Different resamples were used in the workflow 'result's. ",
         "All elements of 'result' must use the same resamples."
      )
   }

   if (!has_valid_column_option_structure(x)) {
      halt("The 'option' column should be a list.")
   }
   if (!has_valid_column_option_inner_types(x)) {
      halt("All elements of 'option' should be 'options'.")
   }

   if (!has_valid_column_wflow_id_structure(x)) {
      halt("The 'wflow_id' column should be character.")
   }
   if (!has_valid_column_wflow_id_strings(x)) {
      halt("The 'wflow_id' column should contain unique, non-missing character strings.")
   }

   new_workflow_set0(x)
}

new_workflow_set0 <- function(x) {
   new_tibble0(x, class = "workflow_set")
}
new_tibble0 <- function(x, ..., class = NULL) {
   # Handle the 0-row case correctly by using `new_data_frame()`.
   # This also correctly strips any attributes except `names` off `x`.
   x <- vctrs::new_data_frame(x)
   tibble::new_tibble(x, nrow = nrow(x), class = class)
}

has_required_container_type <- function(x) {
   rlang::is_list(x)
}
has_required_container_columns <- function(x) {
   columns <- required_container_columns()
   ok <- all(columns %in% names(x))
   ok
}
required_container_columns <- function() {
   c("wflow_id", "info", "option", "result")
}


has_valid_column_info_structure <- function(x) {
   info <- x$info
   rlang::is_list(info)
}
has_valid_column_info_inner_types <- function(x) {
   info <- x$info
   is_tibble_indicator <- purrr::map_lgl(info, tibble::is_tibble)
   all(is_tibble_indicator)
}
has_valid_column_info_inner_names <- function(x) {
   columns <- required_info_inner_names()
   info <- x$info
   list_of_names <- purrr::map(info, names)
   has_names_indicator <- purrr::map_lgl(list_of_names, identical, y = columns)
   all(has_names_indicator)
}
required_info_inner_names <- function() {
   c("workflow", "preproc", "model", "comment")
}


has_valid_column_result_structure <- function(x) {
   result <- x$result
   rlang::is_list(result)
}
has_valid_column_result_inner_types <- function(x) {
   result <- x$result
   valid_indicator <- purrr::map_lgl(result, is_valid_result_inner_type)
   all(valid_indicator)
}
has_valid_column_result_fingerprints <- function(x) {
   result <- x$result

   # Drop default results
   default_indicator <- purrr::map_lgl(result, is_default_result_element)
   result <- result[!default_indicator]

   # Not sure how to fingerprint racing objects just yet. See
   # https://github.com/tidymodels/rsample/issues/212
   racing_indicator <- purrr::map_lgl(result, inherits, "tune_race")
   result <- result[!racing_indicator]

   hashes <- purrr::map_chr(result, rsample::.get_fingerprint)

   # Drop NAs for results created before rsample 0.1.0, which won't have a hash
   pre_0.1.0 <- is.na(hashes)
   hashes <- hashes[!pre_0.1.0]
   result <- result[!pre_0.1.0]

   if (rlang::is_empty(hashes)) {
      # No hashes to check
      TRUE
   } else {
      # Should collapse to a single hash value if all resamples are the same
      uniques <- unique(hashes)
      length(uniques) == 1L
   }
}
is_valid_result_inner_type <- function(x) {
   if (is_default_result_element(x)) {
      # Default, before any results are filled
      return(TRUE)
   }
   inherits(x, "tune_results")
}
is_default_result_element <- function(x) {
   identical(x, list())
}


has_valid_column_option_structure <- function(x) {
   option <- x$option
   rlang::is_list(option)
}
has_valid_column_option_inner_types <- function(x) {
   option <- x$option
   valid_options_indicator <- purrr::map_lgl(option, inherits, "options")
   all(valid_options_indicator)
}


has_valid_column_wflow_id_structure <- function(x) {
   wflow_id <- x$wflow_id
   rlang::is_character(wflow_id)
}
has_valid_column_wflow_id_strings <- function(x) {
   wflow_id <- x$wflow_id
   uniques <- unique(wflow_id)

   if (length(wflow_id) != length(uniques)) {
      return(FALSE)
   }
   if (any(is.na(wflow_id))) {
      return(FALSE)
   }
   if (any(wflow_id == "")) {
      return(FALSE)
   }

   TRUE
}
