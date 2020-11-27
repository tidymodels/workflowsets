#' @import ggplot2

utils::globalVariables(
   c(".config", ".estimator", ".metric", "info", "metric", "mod_nm",
     "model", "models", "n", "pp_nm", "preprocessor", "preprocs",
     "results", "std_err", "wflow_id")
)

# ------------------------------------------------------------------------------



#' @importFrom tune tune_grid
#' @export
tune::tune_grid

#' @importFrom tune tune_bayes
#' @export
tune::tune_bayes

#' @importFrom tune fit_resamples
#' @export
tune::fit_resamples

#' @importFrom tune collect_metrics
#' @export
tune::collect_metrics

#' @importFrom dplyr %>%
#' @export
dplyr::`%>%`

#' @importFrom ggplot2 autoplot
#' @export
ggplot2::autoplot
