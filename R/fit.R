#' @importFrom generics fit

#' @noRd
#' @method fit workflow_set
#' @export
fit.workflow_set <- function(object, ...) {
  msg <- "`fit()` is not well-defined for workflow sets."

  # supply a different message depending on whether the
  # workflow set has been (attempted to have been) fitted or not
  if (!all(purrr::map_lgl(object$result, ~ identical(.x, list())))) {
    # if fitted:
    msg <-
      c(
        msg,
        "i" = "Please see {.help [{.fun fit_best}](workflowsets::fit_best.workflow_set)}."
      )
  } else {
    # if not fitted:
    msg <-
      c(
        msg,
        "i" = "Please see {.help [{.fun workflow_map}](workflowsets::workflow_map)}."
      )
  }

  cli::cli_abort(msg)
}
