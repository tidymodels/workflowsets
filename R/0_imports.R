#' @import ggplot2
#' @import vctrs
#' @importFrom stats qnorm as.formula model.frame
#' @importFrom tibble obj_sum type_sum
#' @importFrom dplyr dplyr_reconstruct
#'
utils::globalVariables(
   c(".config", ".estimator", ".metric", "info", "metric", "mod_nm",
     "model", "n", "pp_nm", "preprocessor", "preproc", "object",
     "result", "std_err", "wflow_id", "func", "is_race", "num_rs", "option",
     "metrics", "predictions", "hash", "id", "workflow", "comment")
)

# ------------------------------------------------------------------------------

#' @importFrom tibble tbl_sum
#' @export
tibble::tbl_sum

#' @importFrom tibble type_sum
#' @export
tibble::type_sum

#' @importFrom tibble size_sum
#' @export
tibble::size_sum

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

#' @importFrom tune collect_predictions
#' @export
tune::collect_predictions

#' @importFrom dplyr %>%
#' @export
dplyr::`%>%`

#' @importFrom ggplot2 autoplot
#' @export
ggplot2::autoplot
