#' Methods for processing workflow sets
#'
#' @param object A `workflow_set` object.
#' @param which A character string of identifiers indicating which workflows should
#'  be processed. If left `NULL`, all applicable workflows without entries
#'  in the `results` column are selected.
#' @param seed A single integer that is set before each workflow is processed.
#' @param ... Common arguments that will be passed to each `tune` function. These
#'  should not be the same arguments passed to `options`.
#' @param verbose A single logical to indicate if logging should occur. This
#'  will write a message _after_ each workflow is process and indicates if there
#'  were any issues.
#' @return An object from the corresponding `tune` package function (e.g. with
#'  class `tune_result`, etc.)
#' @examples
#' library(workflowsets)
#' library(modeldata)
#' library(recipes)
#' library(parsnip)
#' library(dplyr)
#' library(rsample)
#' library(tune)
#' library(yardstick)
#' library(dials)
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
#' cell_model_results <-
#'    cell_models %>%
#'    tune_grid(resamples = val_set, grid = 10, metrics = metric_set(roc_auc)) %>%
#'    fit_resamples(resamples = val_set, metrics = metric_set(roc_auc))
#' cell_model_results
#' }
#'
#' # ------------------------------------------------------------------------------
#'
#' # An example of setting options. Let's change the range for `num_comp` by
#' # passing a specific parameter set.
#
#' # pca_knn_mod <-
#' #   cell_models %>%
#' #   dplyr::filter(wflow_id == "pca_knn")
#' # pca_knn_param <-
#' #   pca_knn_mod$object[[1]] %>%
#' #   parameters() %>%
#' #   update(num_comp = num_comp(c(0, 20)))
#' #
#' # new_opts <- list(pca_knn = list(param_info = pca_knn_param))
#' # new_opts
#' # \donttest{
#' # cell_model_results <-
#' #    cell_models %>%
#' #    tune_grid(resamples = val_set, grid = 10, which = "pca_knn", options = new_opts)
#' # }
#' @export
tune_grid.workflow_set <- function(object, ..., which = NULL,
                                   seed = sample(1e5, 1), verbose = FALSE) {
   fn_loop(object, .fn = "tune_grid", tune = TRUE, which = which, verbose = verbose,
           seed = seed, ...)
}

# TODO ... will which up unnamed arguments to tune_grid()

#' @export
#' @rdname tune_grid.workflow_set
tune_bayes.workflow_set <- function(object, ..., which = NULL,
                                    seed = sample(1e5, 1), verbose = FALSE) {
   fn_loop(object, .fn = "tune_bayes", tune = TRUE, which = which, verbose = verbose,
           seed = seed, ...)
}

#' @export
#' @rdname tune_grid.workflow_set
fit_resamples.workflow_set <- function(object, ..., which = NULL,
                                       seed = sample(1e5, 1), verbose = FALSE) {
   fn_loop(object, .fn = "fit_resamples", tune = FALSE, which = which, verbose = verbose,
           seed = seed, ...)
}

# ------------------------------------------------------------------------------

fn_loop <- function(object, .fn = "tune_grid", tune = TRUE,
                    which = NULL,  seed = sample(1e5, 1),
                    verbose = FALSE, ...) {
   if (is.null(which)) {
      has_tune <- purrr::map_lgl(object$object, ~ nrow(tune::tune_args(.x)) > 0)
      no_value <- purrr::map_lgl(object$result, ~ length(.x) == 0)
      if (tune) {
         which <- object$wflow_id[ has_tune & no_value]
      } else {
         which <- object$wflow_id[!has_tune & no_value]
      }
   }
   if (length(which) == 0) {
      rlang::abort("No objects are tunable.")
   }
   num_tasks <- length(which)
   which_ind <- which(object$wflow_id %in% which)

   # check to see if result is not empty

   dots <- rlang::list2(...)
   # check and add options to options column
   if (length(dots) > 0) {
      tmp <- rlang::exec("add_options", object %>% dplyr::slice(which_ind), !!!dots)
      object$options[which_ind] <- tmp$options
   }



   iter_seq <- seq_along(which)
   iter_chr <- format(iter_seq)
   n <- length(iter_seq)

   for (iter in seq_along(which)) {
      obj <- which(object$wflow_id == which[iter])
      opt <- object$options[[obj]]
      cl <- rlang::call2(.fn,
                         .ns = "tune",
                         object = object$object[[obj]],
                         !!!opt)
      withr::with_seed(seed[1],
                       object$result[[obj]] <-
                          try(rlang::eval_tidy(cl), silent = TRUE))
      log_progress(verbose, object$wflow_id[[obj]], object$result[[obj]],
                   iter_chr[iter], n, .fn)
   }
   object
}

log_progress <- function(verbose, id, res, iter, n, .fn, elapsed) {
   if (!verbose) {
      return(invisible(NULL))
   }
   cols <- tune::get_tune_colors()
   event <- ifelse(grepl("tune", .fn), "tuning:    ", "resampling:")
   msg <- paste0(iter, " of ", n, " ", event, " ", id)
   if (is.null(res)) {
      message(
         cols$symbol$info(cli::symbol$info), " ",
         cols$message$info(msg)
      )
   } else {
      if (inherits(res, "try-error")) {
         message(
            cols$symbol$danger(cli::symbol$cross), " ",
            cols$message$info(msg),
            cols$message$danger(" failed with "),
            cols$message$danger(as.character(res))
         )
      } else {
         time_msg <- paste0(" (", prettyunits::pretty_sec(elapsed[3]), ")")
         message(
            cols$symbol$success(cli::symbol$tick), " ",
            cols$message$info(msg),
            cols$message$info(time_msg)
         )
      }
   }

   invisible(NULL)
}


