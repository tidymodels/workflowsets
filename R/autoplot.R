#' Plot the results of a workflow set
#'
#' @param object A `workflow_set` whose elements have results.
#' @param rank_metric A character string for which metric should be used to rank
#' the results.
#' @param which A character string for what to plot. If a value of
#' `"workflow_set"` is used, the results of each model (and sub-model) are ordered
#' and plotted. Alternatively, a value of the workflow set's `wflow_id` can be
#' given and the `autoplot()` method is executed on that workflow's results.
#' @param std_errs The number of standard errors to plot (if the standard error
#' exists).
#' @param ... Other options to pass to `autoplot()`.
#' @examples
#' autoplot(chi_models)
#' autoplot(chi_models, which = "pca_knn")
#' @export
autoplot.workflow_set <- function(object, rank_metric = NULL,
                                  which = "workflow_set", std_errs = 1, ...) {
   if (which == "workflow_set") {
      p <- rank_plot(object, rank_metric = rank_metric, std_errs = std_errs)
   } else {
      p <- autoplot(object$results[[which(object$wflow_id == which)]], ...)
   }
   p
}

rank_plot <- function(object, metric = NULL, std_errs = 1, ...) {
   metric_info <- pick_metric(object, metric)
   res <- rank_results(object, rank_metric = metric)
   num_metrics <- length(unique(res$.metric))
   has_std_error <- !all(is.na(res$std_err))

   p <-
      ggplot(res, aes(x = rank, y = mean, col = model)) +
      geom_point(aes(shape = preprocessor))

   if (num_metrics > 1) {
      p <- p + facet_wrap(~ .metric) + labs(x = "Workflow Rank", y = "Metric")
   } else {
      p <- p + labs(x = "Workflow Rank", y = metric_info$metric)
   }

   if (has_std_error) {
      p <-
         p +
         geom_errorbar(aes(ymin = mean - std_errs * std_err, ymax = mean + std_errs * std_err))
   }

   p

}
