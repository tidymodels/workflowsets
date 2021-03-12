#' Plot the results of a workflow set
#'
#' This `autoplot()` method can performance metrics that have been ranked using
#' a metric. It can also run `autoplot()` on the individual results (per
#' `wflow_id`).
#'
#' @param object A `workflow_set` whose elements have results.
#' @param rank_metric A character string for which metric should be used to rank
#' the results.
#' @param id A character string for what to plot. If a value of
#' `"workflow_set"` is used, the results of each model (and sub-model) are ordered
#' and plotted. Alternatively, a value of the workflow set's `wflow_id` can be
#' given and the `autoplot()` method is executed on that workflow's results.
#' @param select_best A logical; should the results only contain the numerically
#' best submodel per workflow?
#' @param metric A character vector for which metrics (apart from `rank_metric`)
#' to be included in the visualization.
#' @param std_errs The number of standard errors to plot (if the standard error
#' exists).
#' @param ... Other options to pass to `autoplot()`.
#' @details
#' The x-axis is the workflow rank in the set (a value of one being the best)
#' versus the performance metric(s) on the y-axis. With multiple metrics, there
#' will be facets for each metric.
#'
#' If multiple resamples are used, confidence bounds are shown for each result.
#' @return A ggplot object.
#' @examples
#' autoplot(two_class_res)
#' autoplot(two_class_res, select_best = TRUE)
#' autoplot(two_class_res, id = "yj_trans_cart", metric = "roc_auc")
#' @export
autoplot.workflow_set <- function(object, rank_metric = NULL, metric = NULL,
                                  id = "workflow_set",
                                  select_best = FALSE,
                                  std_errs = qnorm(0.95), ...) {
   if (id == "workflow_set") {
      p <- rank_plot(object, rank_metric = rank_metric, metric = metric,
                     select_best = select_best, std_errs = std_errs)
   } else {
      p <- autoplot(object$result[[which(object$wflow_id == id)]], metric = metric, ...)
   }
   p
}

rank_plot <- function(object, rank_metric = NULL, metric = NULL,
                      select_best = FALSE, std_errs = 1, ...) {
   metric_info <- pick_metric(object, rank_metric)
   metrics <- collate_metrics(object)
   res <- rank_results(object, rank_metric = rank_metric, select_best = select_best)

   if (!is.null(metric)) {
      keep_metrics <- unique(c(rank_metric, metric))
      res <- dplyr::filter(res, .metric %in% keep_metrics)
   }

   num_metrics <- length(unique(res$.metric))
   has_std_error <- !all(is.na(res$std_err))

   p <-
      ggplot(res, aes(x = rank, y = mean, col = model)) +
      geom_point(aes(shape = preprocessor))

   if (num_metrics > 1) {
      res$.metric <- factor(as.character(res$.metric), levels = metrics$metric)
      p <-
         p +
         facet_wrap(~ .metric, scales = "free_y") +
         labs(x = "Workflow Rank", y = "Metric")
   } else {
      p <- p + labs(x = "Workflow Rank", y = metric_info$metric)
   }

   if (has_std_error) {
      p <-
         p +
         geom_errorbar(
            aes(ymin = mean - std_errs * std_err,
                ymax = mean + std_errs * std_err),
            width = diff(range(res$rank))/75
         )
   }

   p

}
