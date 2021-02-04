#' Work with notes in a workflow
#'
#' `note_add()` can be used to log important information about the workflow or
#' its results as you work. Notes can be appended or removed.
#' @param x A workflow set
#' @param id A single character string for a value in the `wflow_id` column.
#' @param ... One or more character strings.
#' @param append A logical value to determine if the new note should be added
#' to the existing values.
#' @param collapse A character string that separates the notes.
#' @return `note_add()` and `note_reset()` return an updated workflow set.
#' `note_get()` returns a character string.
#' @export
#' @examples
#' two_class_set %>% note_get("none_cart")
#'
#' new_set <-
#'    two_class_set %>%
#'    note_add("none_cart", "What does 'cart' stand for\u2753") %>%
#'    note_add("none_cart", "Classification And Regression Trees.")
#'
#' new_set %>% note_get("none_cart") %>% cat("\n")
#'
#' new_set %>% note_reset("none_cart") %>% note_get("none_cart")
note_add <- function(x, id, ..., append = TRUE, collapse = "\n") {
   dots <- list(...)
   if (length(dots) == 0) {
      return(x)
   } else {
      is_chr <- purrr::map_lgl(dots, is.character)
      if (any(!is_chr)) {
         rlang::abort("The notes should be character strings.")
      }
   }

   if (length(id) > 1) {
      rlang::abort("'id' should be a single character value.")
   }
   has_id <- id == x$wflow_id
   if (!any(has_id)) {
      rlang::abort("The 'id' value is not in wflow_id.")
   }
   id_index <- which(has_id)
   current_val <- x$info[[id_index]]$note
   if (!is.na(current_val) && !append) {
      rlang::abort("There is already a note for this id and `append = FALSE`.")
   }
   new_value <- c(x$info[[id_index]]$note, unlist(dots))
   new_value <- new_value[!is.na(new_value) & nchar(new_value) > 0]
   new_value <- paste0(new_value, collapse = "\n")
   x$info[[id_index]]$note <- new_value
   x
}

#' @export
#' @rdname note_add
note_get <- function(x, id) {
   if (length(id) > 1) {
      rlang::abort("'id' should be a single character value.")
   }
   has_id <- id == x$wflow_id
   if (!any(has_id)) {
      rlang::abort("The 'id' value is not in wflow_id.")
   }
   id_index <- which(has_id)
   x$info[[id_index]]$note
}


#' @export
#' @rdname note_add
note_reset <- function(x, id) {
   if (length(id) > 1) {
      rlang::abort("'id' should be a single character value.")
   }
   has_id <- id == x$wflow_id
   if (!any(has_id)) {
      rlang::abort("The 'id' value is not in wflow_id.")
   }
   id_index <- which(has_id)
   x$info[[id_index]]$note <- character(1)
   x
}

