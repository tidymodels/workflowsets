
make_workflow <- function(x, y, weight) {
  exp_classes <- c("formula", "recipe", "workflow_variables")
  w <-
    workflows::workflow() %>%
    workflows::add_model(y)
  if (inherits(x, "formula")) {
    w <- workflows::add_formula(w, x)
  } else if (inherits(x, "recipe")) {
    w <- workflows::add_recipe(w, x)
  } else if (inherits(x, "workflow_variables")) {
    w <- workflows::add_variables(w, variables = x)
  } else {
    halt(
      "The preprocessor must be an object with one of the ",
      "following classes: ", paste0("'", exp_classes, "'", collapse = ", ")
    )
  }
  w
}

# copied from parsnip
case_weights_allowed <- function(spec) {
   mod_type <- class(spec)[1]
   mod_eng <- spec$engine
   mod_mode <- spec$mode

   model_info <-
      get_from_env(paste0(mod_type, "_fit")) %>%
      dplyr::filter(engine == mod_eng & mode == mod_mode)

   # If weights are used, they are protected data arguments with the canonical
   # name 'weights' (although this may not be the model function's argument name).
   data_args <- model_info$value[[1]]$protect
   any(data_args == "weights")
}

halt <- function(...) {
  rlang::abort(paste0(...))
}

# ------------------------------------------------------------------------------


metric_to_df <- function(x, ...) {
  metrics <- attributes(x)$metrics
  names <- names(metrics)
  metrics <- unname(metrics)
  classes <- purrr::map_chr(metrics, ~ class(.x)[[1]])
  directions <- purrr::map_chr(metrics, ~ attr(.x, "direction"))
  info <- data.frame(metric = names, class = classes, direction = directions)
  info
}


collate_metrics <- function(x) {
  metrics <-
    x$result %>%
    purrr::map(tune::.get_tune_metrics) %>%
    purrr::map(metric_to_df) %>%
    purrr::map_dfr(~ dplyr::mutate(.x, order = 1:nrow(.x)))

  mean_order <-
    metrics %>%
    dplyr::group_by(metric) %>%
    dplyr::summarize(
      order = mean(order, na.rm = TRUE), n = dplyr::n(),
      .groups = "drop"
    )

  dplyr::full_join(
    dplyr::distinct(metrics) %>% dplyr::select(-order),
    mean_order,
    by = "metric"
  ) %>%
    dplyr::arrange(order)
}

pick_metric <- function(x, rank_metric, select_metrics = NULL) {
  # mostly to check for completeness and consistency:
  tmp <- collect_metrics(x)
  metrics <- collate_metrics(x)

  if (!is.null(select_metrics)) {
    tmp <- dplyr::filter(tmp, .metric %in% select_metrics)
    metrics <- dplyr::filter(metrics, metric %in% select_metrics)
  }

  if (is.null(rank_metric)) {
    rank_metric <- metrics$metric[1]
    direction <- metrics$direction[1]
  } else {
    if (!any(metrics$metric == rank_metric)) {
      halt("Metric '", rank_metric, "' was not in the results.")
    }
    direction <- metrics$direction[metrics$metric == rank_metric]
  }
  list(metric = as.character(rank_metric), direction = as.character(direction))
}
