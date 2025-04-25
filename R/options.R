#' Add and edit options saved in a workflow set
#'
#' @description
#' The `option` column controls options for the functions that are used to
#' _evaluate_ the workflow set, such as [tune::fit_resamples()] or
#' [tune::tune_grid()]. Examples of common options to set for these functions
#' include `param_info` and `grid`.
#'
#' These functions are helpful for manipulating the information in the `option`
#' column.
#'
#' @export
#' @inheritParams comment_add
#' @param ... Arguments to pass to the `tune_*()` functions (e.g.
#' [tune::tune_grid()]) or [tune::fit_resamples()]. For `option_remove()` this
#' can be a series of unquoted option names.
#' @param id A character string of one or more values from the `wflow_id`
#' column that indicates which options to update. By default, all workflows
#' are updated.
#' @param strict A logical; should execution stop if existing options are being
#' replaced?
#' @return An updated workflow set.
#' @details
#' `option_add()` is used to update all of the options in a workflow set.
#'
#' `option_remove()` will eliminate specific options across rows.
#'
#' `option_add_parameters()` adds a parameter object to the `option` column
#' (if parameters are being tuned).
#'
#' Note that executing a function on the workflow set, such as `tune_grid()`,
#' will add any options given to that function to the `option` column.
#'
#' These functions do _not_ control options for the individual workflows, such as
#' the recipe blueprint. When creating a workflow manually, use
#' [workflows::add_model()] or [workflows::add_recipe()] to specify
#' extra options. To alter these in a workflow set, use
#' [update_workflow_model()] or [update_workflow_recipe()].
#'
#' @examples
#' library(tune)
#'
#' two_class_set
#'
#' two_class_set |>
#'   option_add(grid = 10)
#'
#' two_class_set |>
#'   option_add(grid = 10) |>
#'   option_add(grid = 50, id = "none_cart")
#'
#' two_class_set |>
#'   option_add_parameters()
option_add <- function(x, ..., id = NULL, strict = FALSE) {
  check_wf_set(x)
  dots <- list(...)
  if (length(dots) == 0) {
    return(x)
  }

  if (strict) {
    act <- "fail"
  } else {
    act <- "warn"
  }

  check_tune_args(names(dots))

  check_string(id, allow_null = TRUE)
  check_bool(strict)

  if (!is.null(id)) {
    for (i in id) {
      ind <- which(x$wflow_id == i)
      if (length(ind) == 0) {
        cli::cli_warn("Don't have an {.arg id} value {i}")
      } else {
        check_options(x$option[[ind]], x$wflow_id[[ind]], dots, action = act)
        x$option[[ind]] <- append_options(x$option[[ind]], dots)
      }
    }
  } else {
    check_options(x$option, x$wflow_id, dots, action = act)
    x <- dplyr::mutate(x, option = purrr::map(option, append_options, dots))
  }
  x
}


#' @export
#' @rdname option_add
option_remove <- function(x, ...) {
  dots <- rlang::enexprs(...)
  if (length(dots) == 0) {
    return(x)
  }
  dots <- purrr::map_chr(dots, rlang::expr_text)

  x <- dplyr::mutate(x, option = purrr::map(option, rm_elem, dots))
  x
}


maybe_param <- function(x) {
  prm <- hardhat::extract_parameter_set_dials(x)
  if (nrow(prm) == 0) {
    x <- list()
  } else {
    x <- list(param_info = prm)
  }
  x
}
#' @export
#' @rdname option_add
option_add_parameters <- function(x, id = NULL, strict = FALSE) {
  prm <- purrr::map(x$info, \(.x) maybe_param(.x$workflow[[1]]))
  num <- purrr::map_int(prm, length)
  if (all(num == 0)) {
    return(x)
  }

  if (strict) {
    act <- "fail"
  } else {
    act <- "warn"
  }

  if (!is.null(id)) {
    for (i in id) {
      ind <- which(x$wflow_id == i)
      if (length(ind) == 0) {
        cli::cli_warn("Don't have an {.arg id} value {i}")
      } else {
        check_options(
          x$option[[ind]],
          x$wflow_id[[ind]],
          prm[[ind]],
          action = act
        )
        x$option[[ind]] <- append_options(x$option[[ind]], prm[[ind]])
      }
    }
  } else {
    check_options(x$option, x$wflow_id, prm[1], action = act)
    x <- dplyr::mutate(x, option = purrr::map2(option, prm, append_options))
  }
  x
}

rm_elem <- function(x, nms) {
  x <- x[!(names(x) %in% nms)]
  new_workflow_set_options(!!!x)
}

append_options <- function(model, global) {
  old_names <- names(model)
  new_names <- names(global)
  common_names <- intersect(old_names, new_names)

  if (length(common_names) > 0) {
    model <- rm_elem(model, common_names)
  }

  all_opt <- c(model, global)
  new_workflow_set_options(!!!all_opt)
}

#' @export
print.workflow_set_options <- function(x, ...) {
  if (length(x) > 0) {
    cat(
      "a list of options with names: ",
      paste0("'", names(x), "'", collapse = ", ")
    )
  } else {
    cat("an empty container for options")
  }
  cat("\n")
}


#' Make a classed list of options
#'
#' This function returns a named list with an extra class of
#' `"workflow_set_options"` that has corresponding formatting methods for
#' printing inside of tibbles.
#' @param ... A set of named options (or nothing)
#' @return A classed list.
#' @examples
#' option_list(a = 1, b = 2)
#' option_list()
#' @export
option_list <- function(...) new_workflow_set_options(...)

new_workflow_set_options <- function(..., call = caller_env()) {
  res <- rlang::list2(...)
  if (any(names(res) == "")) {
    cli::cli_abort("All options should be named.", call = call)
  }
  structure(res, class = c("workflow_set_options", "list"))
}

#' @export
type_sum.workflow_set_options <- function(x) {
  paste0("opts[", length(x), "]")
}
#' @export
size_sum.workflow_set_options <- function(x) {
  ""
}
#' @export
obj_sum.workflow_set_options <- function(x) {
  paste0("opts[", length(x), "]")
}
