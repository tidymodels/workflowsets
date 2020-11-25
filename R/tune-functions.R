#' Methods for processing workflow sets
#'
#' @param object A `workflow_set` object.
#' @param pick A character string of identifiers indicating which workflows should
#'  be processed. If left `NULL`, all applicable workflows without entries
#'  in the `results` column are selected.
#' @param options A list the same length as `pick` with workflow-specific
#'  arguments to the corresponding function in the `tune` package.
#' @param seed A single integer that is set before each workflow is processed.
#' @param ... Common arguments that will be passed to each `tune` function. These
#'  should not be the same arguments passed to `options`.
#' @param verbose A single logical to indicate if logging should occur. This
#'  will write a message _after_ each workflow is process and indicates if there
#'  were any issues.
#' @return An object from the corresponding `tune` package function (e.g. with
#'  class `tune_result`, etc.)
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
#' }
#' @export
tune_grid.workflow_set <- function(object, pick = NULL, options = NULL,
                                   seed = sample(1e5, 1), verbose = FALSE,
                                   ...) {
   fn_loop(object, .fn = "tune_grid", tune = TRUE, pick = pick, verbose = verbose,
           options = options, seed = seed, ...)
}

#' @export
#' @rdname tune_grid.workflow_set
tune_bayes.workflow_set <- function(object, pick = NULL, options = NULL,
                                    seed = sample(1e5, 1), verbose = FALSE, ...) {
   fn_loop(object, .fn = "tune_bayes", tune = TRUE, pick = pick, verbose = verbose,
           options = options, seed = seed, ...)
}

#' @export
#' @rdname tune_grid.workflow_set
fit_resamples.workflow_set <- function(object, pick = NULL, options = NULL,
                                       seed = sample(1e5, 1), verbose = FALSE, ...) {
   fn_loop(object, .fn = "fit_resamples", tune = FALSE, pick = pick, verbose = verbose,
           options = options, seed = seed, ...)
}

# ------------------------------------------------------------------------------

fn_loop <- function(object, .fn = "tune_grid", tune = TRUE,
                    pick = NULL, options = NULL, seed = sample(1e5, 1),
                    verbose = FALSE, ...) {
   if (is.null(pick)) {
      has_tune <- purrr::map_lgl(object$objects, ~ nrow(tune::tune_args(.x)) > 0)
      no_value <- purrr::map_lgl(object$results, ~ length(.x) == 0)
      if (tune) {
         pick <- object$wflow_id[ has_tune & no_value]
      } else {
         pick <- object$wflow_id[!has_tune & no_value]
      }
   }
   if (length(pick) == 0) {
      rlang::abort("No objects are tunable.")
   }
   num_tasks <- length(pick)
   # check to see if result is not empty

   # merge options in options with ...
   # TODO check length of options, check names
   dots <- rlang::enquos(...)
   options <- purrr::map(1:num_tasks, ~ c(options[[.x]], dots))

   iter_seq <- seq_along(pick)
   iter_chr <- format(iter_seq)
   n <- length(iter_seq)

   for (iter in seq_along(pick)) {
      obj <- which(object$wflow_id == pick[iter])
      cl <- rlang::call2(.fn,
                         .ns = "tune",
                         object = object$objects[[obj]],!!!options[[iter]])
      withr::with_seed(seed[1],
                       object$results[[obj]] <-
                          try(rlang::eval_tidy(cl), silent = TRUE))
      log_progress(verbose, object$wflow_id[[obj]], object$results[[obj]],
                   iter_chr[iter], n, .fn)
   }
   object
}

log_progress <- function(verbose, id, res, iter, n, .fn) {
   if (!verbose) {
      return(invisible(NULL))
   }
   cols <- tune::get_tune_colors()
   event <- ifelse(grepl("tune", .fn), "tuning:    ", "resampling:")
   msg <- paste0(iter, " of ", n, " ", event, " ", id)
   if (inherits(res, "try-error")) {
      message(
         cols$symbol$danger(cli::symbol$cross), " ",
         cols$message$info(msg),
         cols$message$danger(" failed with "),
         cols$message$danger(as.character(res))
      )
   } else {
      message(
         cols$symbol$success(cli::symbol$tick), " ",
         cols$message$info(msg)
      )
   }

   invisible(NULL)
}


