#' Add annotations and comments for workflows
#'
#' `comment_add()` can be used to log important information about the workflow or
#' its results as you work. Comments can be appended or removed.
#' @param x A workflow set outputted by [workflow_set()] or [workflow_map()].
#' @param id A single character string for a value in the `wflow_id` column. For
#' `comment_print()`, `id` can be a vector or `NULL` (and this indicates that
#' all comments are printed).
#' @param ... One or more character strings.
#' @param append A logical value to determine if the new comment should be added
#' to the existing values.
#' @param collapse A character string that separates the comments.
#' @return `comment_add()` and `comment_reset()` return an updated workflow set.
#' `comment_get()` returns a character string. `comment_print()` returns `NULL`
#' invisibly.
#' @export
#' @examples
#' two_class_set
#'
#' two_class_set |> comment_get("none_cart")
#'
#' new_set <-
#'   two_class_set |>
#'   comment_add("none_cart", "What does 'cart' stand for\u2753") |>
#'   comment_add("none_cart", "Classification And Regression Trees.")
#'
#' comment_print(new_set)
#'
#' new_set |> comment_get("none_cart")
#'
#' new_set |>
#'   comment_reset("none_cart") |>
#'   comment_get("none_cart")
comment_add <- function(x, id, ..., append = TRUE, collapse = "\n") {
  check_wf_set(x)
  check_bool(append)
  check_string(collapse)
  dots <- list(...)
  if (length(dots) == 0) {
    return(x)
  } else {
    is_chr <- purrr::map_lgl(dots, is.character)
    if (any(!is_chr)) {
      cli::cli_abort("The comments should be character strings.")
    }
  }

  check_string(id)
  has_id <- id == x$wflow_id
  if (!any(has_id)) {
    cli::cli_abort("The {.arg id} value is not in {.arg wflow_id}.")
  }
  id_index <- which(has_id)
  current_val <- x$info[[id_index]]$comment
  if (!is.na(current_val) && !append) {
    cli::cli_abort(
      "There is already a comment for this id and {.code append = FALSE}."
    )
  }
  new_value <- c(x$info[[id_index]]$comment, unlist(dots))
  new_value <- new_value[!is.na(new_value) & nchar(new_value) > 0]
  new_value <- paste0(new_value, collapse = "\n")
  x$info[[id_index]]$comment <- new_value
  x
}

#' @export
#' @rdname comment_add
comment_get <- function(x, id) {
  check_wf_set(x)
  if (length(id) > 1) {
    cli::cli_abort("{.arg id} should be a single character value.")
  }
  has_id <- id == x$wflow_id
  if (!any(has_id)) {
    cli::cli_abort("The {.arg id} value is not in {.arg wflow_id}.")
  }
  id_index <- which(has_id)
  x$info[[id_index]]$comment
}


#' @export
#' @rdname comment_add
comment_reset <- function(x, id) {
  check_wf_set(x)
  if (length(id) > 1) {
    cli::cli_abort("{.arg id} should be a single character value.")
  }
  has_id <- id == x$wflow_id
  if (!any(has_id)) {
    cli::cli_abort("The {.arg id} value is not in {.arg wflow_id}.")
  }
  id_index <- which(has_id)
  x$info[[id_index]]$comment <- character(1)
  x
}

#' @export
#' @rdname comment_add
comment_print <- function(x, id = NULL, ...) {
  check_wf_set(x)
  if (is.null(id)) {
    id <- x$wflow_id
  }

  x <- dplyr::filter(x, wflow_id %in% id)
  chr_x <- purrr::map(x$wflow_id, \(.x) comment_get(x, id = .x))
  has_comment <- purrr::map_lgl(chr_x, \(.x) nchar(.x) > 0)
  chr_x <- chr_x[which(has_comment)]
  id <- x$wflow_id[which(has_comment)]

  for (i in seq_along(chr_x)) {
    cat(cli::rule(id[i]), "\n\n")
    tmp_chr <- comment_format(chr_x[[i]])
    n_comments <- length(tmp_chr)

    for (j in 1:n_comments) {
      cat(tmp_chr[j], "\n\n")
    }
  }
  invisible(NULL)
}

comment_format <- function(x, id, ...) {
  x <- strsplit(x, "\n")[[1]]
  x <- purrr::map(x, \(.x) strwrap(.x))
  x <- purrr::map(x, \(.x) add_returns(.x))
  paste0(x, collapse = "\n\n")
}

add_returns <- function(x) {
  paste0(x, collapse = "\n")
}
