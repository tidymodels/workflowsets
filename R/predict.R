#' @importFrom stats predict

#' @noRd
#' @method predict workflow_set
#' @export
predict.workflow_set <- function(object, ...) {
  cli::cli_abort(c(
    "`predict()` is not well-defined for workflow sets.",
    "i" = "To predict with the optimal model configuration from a workflow \\
           set, ensure that the workflow set was fitted with the \\
           {.help [control option](workflowsets::option_add)} \\
           {.help [{.code save_workflow = TRUE}](tune::control_grid)}, run \\
           {.help [{.fun fit_best}](tune::fit_best)}, and then predict using \\
           {.help [{.fun predict}](workflows::predict.workflow)} on its output.",
    "i" = "To collect predictions from a workflow set, ensure that \\
           the workflow set was fitted with the \\
           {.help [control option](workflowsets::option_add)} \\
           {.help [{.code save_pred = TRUE}](tune::control_grid)} and run \\
           {.help [{.fun collect_predictions}](tune::collect_predictions)}."
  ))
}
