

utils::globalVariables(
   c(".config", "mod_nm", "models", "pp_nm", "preprocs", "results", "wflow_id",
     "info", ".metric", ".estimator")
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
