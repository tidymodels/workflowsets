make_workflow <- function(x, y, call = caller_env()) {
  exp_classes <- c("formula", "recipe", "workflow_variables")
  w <-
    workflows::workflow() |>
    workflows::add_model(y)
  if (inherits(x, "formula")) {
    w <- workflows::add_formula(w, x)
  } else if (inherits(x, "recipe")) {
    w <- workflows::add_recipe(w, x)
  } else if (inherits(x, "workflow_variables")) {
    w <- workflows::add_variables(w, variables = x)
  } else {
    cli::cli_abort(
      "The preprocessor must be an object with one of the
       following classes: {.or {.cls {exp_classes}}}.",
      call = call
    )
  }
  w
}

# ------------------------------------------------------------------------------

metric_to_df <- function(x, ...) {
  metrics <- attributes(x)$metrics
  names <- names(metrics)
  metrics <- unname(metrics)
  classes <- purrr::map_chr(metrics, \(.x) class(.x)[[1]])
  directions <- purrr::map_chr(metrics, \(.x) attr(.x, "direction"))
  info <- data.frame(metric = names, class = classes, direction = directions)
  info
}


collate_metrics <- function(x) {
  metrics <-
    x$result |>
    purrr::map(tune::.get_tune_metrics) |>
    purrr::map(metric_to_df) |>
    purrr::map_dfr(\(.x) dplyr::mutate(.x, order = 1:nrow(.x)))

  mean_order <-
    metrics |>
    dplyr::group_by(metric) |>
    dplyr::summarize(
      order = mean(order, na.rm = TRUE),
      n = dplyr::n(),
      .groups = "drop"
    )

  dplyr::full_join(
    dplyr::distinct(metrics) |> dplyr::select(-order),
    mean_order,
    by = "metric"
  ) |>
    dplyr::arrange(order)
}

pick_metric <- function(
  x,
  rank_metric,
  select_metrics = NULL,
  call = caller_env()
) {
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
      cli::cli_abort(
        "Metric {.val {rank_metric}} was not in the results.",
        call = call
      )
    }
    direction <- metrics$direction[metrics$metric == rank_metric]
  }
  list(metric = as.character(rank_metric), direction = as.character(direction))
}
