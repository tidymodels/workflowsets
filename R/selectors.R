#' Raw column selectors for modeling
#'
#' @param outcomes,predictors Tidyselect expressions specifying the terms of
#' the model. `outcomes` is evaluated first, and then all outcome columns are
#' removed from the data before `predictors` is evaluated. See
#' [workflows::add_variables()] for more information. The selectors are not
#' evaluated until the model is fit.
#' @param blueprint A hardhat blueprint used for fine tuning the preprocessing.
#' @return A object with class `'selectors'`.
#' @examples
#' selectors(mpg, c(cyl, disp))
#' @export
selectors <- function(outcomes, predictors, blueprint = NULL) {
   res <-
      list(
         outcomes = rlang::enquo(outcomes),
         predictors = rlang::enquo(predictors),
         bp = blueprint
      )
   structure(res, class = "selectors")
}


#' @export
print.selectors <- function(x, ...) {
   cat("A set of tidy selectors for model columns.\n")
}
