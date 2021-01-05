#' Functions for options in a workflow set
#' @export
#' @param x A workflow set
#' @param ... A list of named options. For `remove_options()` this can be a
#' series of unquoted option names.
#' @param strict A logical; show execution stop if existing options are being
#' replaced?
#' @return An updated workflow set.
#' @details
#' `add_options()` is used to update all of the options in a workflow set.
#'
#' `remove_options()` will eliminate specific options across rows.
#'
#' `add_option_parameters()` adds a parameter object to the `option` column
#' (if parameters are being tuned).
#'
#' Note that executing a function on the workflow set, such as `tune_grid()`,
#' will add any options given to that funciton to the `option` column.
add_options <- function(x, ..., strict = FALSE) {
   dots <- list(...)
   if (length(dots) == 0) {
      return(x)
   }

   if (strict) {
      act <- "fail"
   } else {
      act <- "warn"
   }
   check_options(x$option, x$wflow_id, dots, action = act)
   x <- dplyr::mutate(x, option = purrr::map(option, append_options, dots))
   x
}


#' @export
#' @rdname add_options
remove_options <- function(x, ...) {
   dots <- rlang::enexprs(...)
   if (length(dots) == 0) {
      return(x)
   }
   dots <- purrr::map_chr(dots, rlang::expr_text)

   x <- dplyr::mutate(x, option = purrr::map(option, rm_elem, dots))
   x
}



maybe_param <- function(x) {
   prm <-  dials::parameters(x)
   if (nrow(prm) == 0) {
      x <- list()
   } else {
      x <- list(param_info = prm)
   }
   x
}
#' @export
#' @rdname add_options
add_option_parameters <- function(x, strict = FALSE) {
   prm <- purrr::map(x$object, maybe_param)
   num <- purrr::map_int(prm, length)
   if (all(num == 0)) {
      return(x)
   }

   if (strict) {
      act <- "fail"
   } else {
      act <- "warn"
   }
   check_options(x$option, x$wflow_id, prm[1], action = act)
   x <- dplyr::mutate(x, option = purrr::map2(option, prm, append_options))
   x
}




rm_elem <- function(x, nms) {
   x[!(names(x) %in% nms)]
}

append_options <- function(model, global) {
   old_names <- names(model)
   new_names <- names(global)
   common_names <- intersect(old_names, new_names)

   if (length(common_names) > 0) {
      model <- rm_elem(model, common_names)
   }

   c(model, global)
}
