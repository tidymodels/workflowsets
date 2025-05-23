#' @keywords internal
"_PACKAGE"

## usethis namespace: start
## usethis namespace: end
NULL

#' @import ggplot2
#' @import vctrs
#' @import rlang
#' @importFrom stats qnorm as.formula model.frame
#' @importFrom pillar obj_sum type_sum tbl_sum size_sum
#' @importFrom dplyr dplyr_reconstruct
#' @importFrom lifecycle deprecated

utils::globalVariables(
  c(
    ".config",
    ".estimator",
    ".metric",
    "info",
    "metric",
    "mod_nm",
    "model",
    "n",
    "pp_nm",
    "preprocessor",
    "preproc",
    "object",
    "engine",
    "result",
    "std_err",
    "wflow_id",
    "func",
    "is_race",
    "num_rs",
    "option",
    "metrics",
    "predictions",
    "hash",
    "id",
    "workflow",
    "comment",
    "get_from_env",
    ".get_tune_metric_names",
    "select_best",
    "notes",
    "extracts"
  )
)

# ------------------------------------------------------------------------------

#' @importFrom tune collect_metrics
#' @export
tune::collect_metrics

#' @importFrom tune collect_predictions
#' @export
tune::collect_predictions

#' @importFrom tune collect_notes
#' @export
tune::collect_notes

#' @importFrom tune collect_extracts
#' @export
tune::collect_extracts

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
#'
#' @importFrom hardhat extract_parameter_set_dials
#' @export
hardhat::extract_parameter_set_dials
#'
#' @importFrom hardhat extract_parameter_dials
#' @export
hardhat::extract_parameter_dials
