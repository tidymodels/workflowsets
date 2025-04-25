#' Create formulas without each predictor
#'
#' From an initial model formula, create a list of formulas that exclude
#' each predictor.
#' @param formula A model formula that contains at least two predictors.
#' @param data A data frame.
#' @param full_model A logical; should the list include the original formula?
#' @param ... Options to pass to [stats::model.frame()]
#' @seealso [workflow_set()]
#' @return A named list of formulas
#' @details The new formulas obey the hierarchy rule so that interactions
#'  without main effects are not included (unless the original formula contains
#'  such terms).
#'
#' Factor predictors are left as-is (i.e., no indicator variables are created).
#'
#' @examplesIf rlang::is_installed("modeldata")
#' data(penguins, package = "modeldata")
#'
#' leave_var_out_formulas(
#'   bill_length_mm ~ .,
#'   data = penguins
#' )
#'
#' leave_var_out_formulas(
#'   bill_length_mm ~ (island + sex)^2 + flipper_length_mm,
#'   data = penguins
#' )
#'
#' leave_var_out_formulas(
#'   bill_length_mm ~ (island + sex)^2 + flipper_length_mm +
#'     I(flipper_length_mm^2),
#'   data = penguins
#' )
#' @export
leave_var_out_formulas <- function(formula, data, full_model = TRUE, ...) {
  check_formula(formula)
  check_bool(full_model)

  trms <- attr(model.frame(formula, data, ...), "terms")
  x_vars <- attr(trms, "term.labels")
  if (length(x_vars) < 2) {
    cli::cli_abort("There should be at least 2 predictors in the formula.")
  }
  y_vars <- as.character(formula[[2]])

  form_terms <- purrr::map(x_vars, rm_vars, lst = x_vars)
  form <- purrr::map_chr(
    form_terms,
    \(.x) paste(y_vars, "~", paste(.x, collapse = " + "))
  )
  form <- purrr::map(form, as.formula)
  form <- purrr::map(form, rm_formula_env)
  names(form) <- x_vars
  if (full_model) {
    form$everything <- formula
  }
  form
}

rm_vars <- function(x, lst) {
  remaining_terms(x, lst)
}

remaining_terms <- function(x, lst) {
  has_x <- purrr::map_lgl(lst, \(.x) x %in% all_terms(.x))
  is_x <- lst == x
  lst[!has_x & !is_x]
}

rm_formula_env <- function(x) {
  attr(x, ".Environment") <- rlang::base_env()
  x
}

all_terms <- function(x) {
  y <- paste("~", x)
  y <- as.formula(y)
  all.vars(y)
}
