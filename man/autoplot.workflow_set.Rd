% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/autoplot.R
\name{autoplot.workflow_set}
\alias{autoplot.workflow_set}
\title{Plot the results of a workflow set}
\usage{
\method{autoplot}{workflow_set}(
  object,
  rank_metric = NULL,
  metric = NULL,
  id = "workflow_set",
  select_best = FALSE,
  std_errs = qnorm(0.95),
  ...
)
}
\arguments{
\item{object}{A \code{workflow_set} whose elements have results.}

\item{rank_metric}{A character string for which metric should be used to rank
the results.}

\item{metric}{A character vector for which metrics (apart from \code{rank_metric})
to be included in the visualization.}

\item{id}{A character string for what to plot. If a value of
\code{"workflow_set"} is used, the results of each model (and sub-model) are ordered
and plotted. Alternatively, a value of the workflow set's \code{wflow_id} can be
given and the \code{autoplot()} method is executed on that workflow's results.}

\item{select_best}{A logical; should the results only contain the numerically
best submodel per workflow?}

\item{std_errs}{The number of standard errors to plot (if the standard error
exists).}

\item{...}{Other options to pass to \code{autoplot()}.}
}
\value{
A ggplot object.
}
\description{
This \code{autoplot()} method can performance metrics that have been ranked using
a metric. It can also run \code{autoplot()} on the individual results (per
\code{wflow_id}).
}
\details{
The x-axis is the workflow rank in the set (a value of one being the best)
versus the performance metric(s) on the y-axis. With multiple metrics, there
will be facets for each metric.

If multiple resamples are used, confidence bounds are shown for each result.
}
\examples{
autoplot(two_class_res)
autoplot(two_class_res, select_best = TRUE)
autoplot(two_class_res, id = "yj_trans_cart", metric = "roc_auc")
}
