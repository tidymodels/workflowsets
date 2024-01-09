#' Generate a set of workflow objects from preprocessing and model objects
#'
#' Often a data practitioner needs to consider a large number of possible
#' modeling approaches for a task at hand, especially for new data sets
#' and/or when there is little knowledge about what modeling strategy
#' will work best. Workflow sets provide an expressive interface for
#' investigating multiple models or feature engineering strategies in such
#' a situation.
#'
#' @param preproc A list (preferably named) with preprocessing objects:
#'  formulas, recipes, or [workflows::workflow_variables()].
#' @param models A list (preferably named) of `parsnip` model specifications.
#' @param cross A logical: should all combinations of the preprocessors and
#'  models be used to create the workflows? If `FALSE`, the length of `preproc`
#'  and `models` should be equal.
#' @param case_weights A single unquoted column name specifying the case
#' weights for the models. This must be a classed case weights column, as
#' determined by [hardhat::is_case_weights()]. See the "Case weights" section
#' below for more information.
#' @seealso [workflow_map()], [comment_add()], [option_add()],
#' [as_workflow_set()]
#' @details
#' The preprocessors that can be combined with the model objects can be one or
#' more of:
#'
#'  * A traditional R formula.
#'  * A recipe definition (un-prepared) via [recipes::recipe()].
#'  * A selectors object created by [workflows::workflow_variables()].
#'
#' Since `preproc` is a named list column, any combination of these can be
#' used in that argument (i.e., `preproc` can be mixed types).
#'
#' @section Case weights:
#' The `case_weights` argument can be passed as a single unquoted column name
#' identifying the data column giving model case weights. For each workflow
#' in the workflow set using an engine that supports case weights, the case
#' weights will be added with [workflows::add_case_weights()]. `workflow_set()`
#' will warn if any of the workflows specify an engine that does not support
#' case weights---and ignore the case weights argument for those workflows---but
#' will not fail.
#'
#' Read more about case weights in the tidymodels at `?parsnip::case_weights`.
#'
#' @return A tibble with extra class 'workflow_set'. A new set includes four
#' columns (but others can be added):
#'
#'  * `wflow_id` contains character strings for the preprocessor/workflow
#'     combination. These can be changed but must be unique.
#'  * `info` is a list column with tibbles containing more specific information,
#'     including any comments added using [comment_add()]. This tibble also
#'     contains the workflow object (which can be easily retrieved using
#'     [extract_workflow()]).
#'  * `option` is a list column that will include a list of optional arguments
#'     passed to the functions from the `tune` package. They can be added
#'     manually via [option_add()] or automatically when options are passed to
#'     [workflow_map()].
#'  * `result` is a list column that will contain any objects produced when
#'     [workflow_map()] is used.
#'
#' @includeRmd man-roxygen/example_data.Rmd note
#'
#' @examplesIf rlang::is_installed(c("kknn", "modeldata", "recipes", "yardstick"))
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
#'   recipe(class ~ ., data = cells) %>%
#'   step_YeoJohnson(all_predictors()) %>%
#'   step_normalize(all_predictors())
#'
#' pca_recipe <-
#'   basic_recipe %>%
#'   step_pca(all_predictors(), num_comp = tune())
#'
#' ss_recipe <-
#'   basic_recipe %>%
#'   step_spatialsign(all_predictors())
#'
#' # ------------------------------------------------------------------------------
#'
#' knn_mod <-
#'   nearest_neighbor(neighbors = tune(), weight_func = tune()) %>%
#'   set_engine("kknn") %>%
#'   set_mode("classification")
#'
#' lr_mod <-
#'   logistic_reg() %>%
#'   set_engine("glm")
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
#' cell_set_by_group <- workflow_set(preproc, models["logistic"])
#' cell_set_by_group
#' @export
workflow_set <- function(preproc, models, cross = TRUE, case_weights = NULL) {
  check_bool(cross)

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
  case_weights <- enquo(case_weights)

  if (cross) {
    res <- cross_objects(preproc, models)
  } else {
    res <- fuse_objects(preproc, models)
  }

  # call set_weights outside of mutate call so that dplyr
  # doesn't prepend possible warnings with "Problem while computing..."
  wfs <-
     purrr::map2(res$preproc, res$model, make_workflow) %>%
     set_weights(case_weights) %>%
     unname()

  res <-
     res %>%
     dplyr::mutate(
      workflow = wfs,
      info = purrr::map(workflow, get_info),
      option = purrr::map(1:nrow(res), ~ new_workflow_set_options()),
      result = purrr::map(1:nrow(res), ~ list())
    ) %>%
    dplyr::select(wflow_id, info, option, result)

  new_workflow_set(res)
}

get_info <- function(x) {
  tibble::tibble(
    workflow = list(x),
    preproc = preproc_type(x),
    model = model_type(x),
    comment = character(1)
  )
}

preproc_type <- function(x) {
  x <- extract_preprocessor(x)
  class(x)[1]
}

model_type <- function(x) {
  x <- extract_spec_parsnip(x)
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

# takes in a _list_ of workflows so that we can check whether case weights
# are allowed in batch and only prompt once if so.
set_weights <- function(workflows, case_weights) {
   if (rlang::quo_is_null(case_weights)) {
      return(workflows)
   }

   allowed <-
      workflows %>%
      purrr::map(extract_spec_parsnip) %>%
      purrr::map_lgl(case_weights_allowed)

   if (any(!allowed)) {
      disallowed <-
         workflows[!allowed] %>%
         purrr::map(extract_spec_parsnip) %>%
         purrr::map(purrr::pluck, "engine") %>%
         unlist() %>%
         unique()

      rlang::warn(
         glue::glue(
            "Case weights are not enabled by the underlying model implementation ",
            "for the following engine(s): ",
            "{glue::glue_collapse(disallowed, sep = ', ')}.\n\n",
            "The `case_weights` argument will be ignored for specifications ",
            "using that engine."
         )
      )
   }

   workflows <-
      purrr::map2(
         workflows,
         allowed,
         add_case_weights_conditionally,
         case_weights
      )

   workflows
}

# copied from parsnip
case_weights_allowed <- function(spec) {
   mod_type <- class(spec)[1]
   mod_eng <- spec$engine
   mod_mode <- spec$mode

   model_info <-
      parsnip::get_from_env(paste0(mod_type, "_fit")) %>%
      dplyr::filter(engine == mod_eng & mode == mod_mode)

   # If weights are used, they are protected data arguments with the canonical
   # name 'weights' (although this may not be the model function's argument name).
   data_args <- model_info$value[[1]]$protect
   any(data_args == "weights")
}

add_case_weights_conditionally <- function(workflow, allowed, case_weights) {
   if (allowed) {
      res <- workflows::add_case_weights(workflow, !!case_weights)
   } else{
      res <- workflow
   }

   res
}

# adapted from workflows
has_case_weights <- function(x) {
   "case_weights" %in% names(x$pre$actions)
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
    halt("All elements of 'option' should have class 'workflow_set_options'.")
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


