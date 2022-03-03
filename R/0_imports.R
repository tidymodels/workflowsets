#' @keywords internal
"_PACKAGE"

## usethis namespace: start
## usethis namespace: end
NULL

#' @import ggplot2
#' @import vctrs
#' @importFrom stats qnorm as.formula model.frame
#' @importFrom tibble obj_sum type_sum tbl_sum size_sum
#' @importFrom dplyr dplyr_reconstruct
#' @importFrom lifecycle deprecated

utils::globalVariables(
  c(
    ".config", ".estimator", ".metric", "info", "metric", "mod_nm",
    "model", "n", "pp_nm", "preprocessor", "preproc", "object",
    "result", "std_err", "wflow_id", "func", "is_race", "num_rs", "option",
    "metrics", "predictions", "hash", "id", "workflow", "comment"
  )
)

# ------------------------------------------------------------------------------

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

#' @importFrom hardhat extract_spec_parsnip
#' @export
hardhat::extract_spec_parsnip
#'
#' @importFrom hardhat extract_recipe
#' @export
hardhat::extract_recipe
#'
#' @importFrom hardhat extract_fit_parsnip
#' @export
hardhat::extract_fit_parsnip
#'
#' @importFrom hardhat extract_fit_engine
#' @export
hardhat::extract_fit_engine
#'
#' @importFrom hardhat extract_mold
#' @export
hardhat::extract_mold
#'
#' @importFrom hardhat extract_preprocessor
#' @export
hardhat::extract_preprocessor
#'
#' @importFrom hardhat extract_workflow
#' @export
hardhat::extract_workflow
