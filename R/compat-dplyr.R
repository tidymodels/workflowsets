#' Extending objects with new workflowset subclass
#'
#' ` wflowset_reconstruct()` encapsulates the logic for allowing new workflow
#' set subclass to work properly with vctrs (through `vctrs::vec_restore()`) and
#' dplyr (through `dplyr::dplyr_reconstruct()`). It is intended to be a
#' developer tool, and is not required for normal usage of `workflowsets`.
#'
#' workflow set objects are considered "reconstructable" after a vctrs/dplyr '
#' operation if:
#'
#' - `x` and `to` both have an columns named '`wflow_id`', '`info`', '`option`',
#' '`result`' (column and row order do not matter) and that those columns are
#' the correct type. "Correct type" is defined by the internal function
#' `new_workflow_set()`.
#'
#' Adding new columns and row additions/deletions still result in workflow sets
#' as long as the new rows adhere to the definition of a workflow set.
#'
#' @param x A data frame to restore to a `workflow_set` subclass.
#' @return `x` as a `workflow_set`.
#' @export
#' @examples
#' to <- two_class_set
#'
#' # Imitate a vctrs/dplyr operation,
#' # where the class might be lost along the way
#' x <- tibble::as_tibble(to)
#'
#' # Say we added a new column to `x`. Here we mock a `mutate()`.
#' x$foo <- "bar"
#'
#' # This is still reconstructable to `to`
#'  wflowset_reconstruct(x)
#'
#' # Say we lose the first column, we get a bare tibble:
#' x <- x[, 1]
#' wflowset_reconstruct(x)
#'
wflowset_reconstruct <- function(x) {
   new_x <- try(new_workflow_set(x), silent = TRUE)
   if (inherits(new_x, "try-error")) {
      new_x <- tibble::as_tibble(x)
   }
   new_x
}

# Registered in `.onLoad()`
# `template` is not used because new_workflow_set() has all of the logic to
# determine if it is reconstructable.
dplyr_reconstruct_wflowset <- function(data, template) {
   wflowset_reconstruct(data)
}

# ------------------------------------------------------------------------------

#' @export
vec_restore.workflow_sets <- function(x, to, ...) {
   browser()
   wflowset_reconstruct(x)
}

# ------------------------------------------------------------------------------

#' @export
vec_ptype2.workflow_sets.workflow_sets <- function(x, y, ..., x_arg = "", y_arg = "") {
   z <- vctrs::df_ptype2(x, y, ..., x_arg = x_arg, y_arg = y_arg)
   wflowset_reconstruct(z)
}
#' @export
vec_ptype2.workflow_sets.tbl_df <- function(x, y, ..., x_arg = "", y_arg = "") {
   z <- vctrs::df_ptype2(x, y, ..., x_arg = x_arg, y_arg = y_arg)
   wflowset_reconstruct(z)
}
#' @export
vec_ptype2.tbl_df.workflow_sets <- function(x, y, ..., x_arg = "", y_arg = "") {
   z <- vctrs::df_ptype2(x, y, ..., x_arg = x_arg, y_arg = y_arg)
   wflowset_reconstruct(z)
}
#' @export
vec_ptype2.workflow_sets.data.frame <- function(x, y, ..., x_arg = "", y_arg = "") {
   z <- vctrs::df_ptype2(x, y, ..., x_arg = x_arg, y_arg = y_arg)
   wflowset_reconstruct(z)
}
#' @export
vec_ptype2.data.frame.workflow_sets <- function(x, y, ..., x_arg = "", y_arg = "") {
   z <- vctrs::df_ptype2(x, y, ..., x_arg = x_arg, y_arg = y_arg)
   wflowset_reconstruct(z)
}

# ------------------------------------------------------------------------------

#' @export
vec_cast.workflow_sets.workflow_sets <- function(x, to, ..., x_arg = "", to_arg = "") {
   z <- vctrs::df_cast(x, to, ..., x_arg = x_arg, to_arg = to_arg)
   wflowset_reconstruct(z)
}
#' @export
vec_cast.workflow_sets.tbl_df <- function(x, to, ..., x_arg = "", to_arg = "") {
   stop_incompatible_cast_wflowset(x, to, x_arg = x_arg, to_arg = to_arg)
}
#' @export
vec_cast.tbl_df.workflow_sets <- function(x, to, ..., x_arg = "", to_arg = "") {
   tib_cast(x, to, ..., x_arg = x_arg, to_arg = to_arg)
}
#' @export
vec_cast.workflow_sets.data.frame <- function(x, to, ..., x_arg = "", to_arg = "") {
   stop_incompatible_cast_wflowset(x, to, x_arg = x_arg, to_arg = to_arg)
}
#' @export
vec_cast.data.frame.workflow_sets <- function(x, to, ..., x_arg = "", to_arg = "") {
   df_cast(x, to, ..., x_arg = x_arg, to_arg = to_arg)
}

# ------------------------------------------------------------------------------

stop_incompatible_cast_wflowset <- function(x, to, ..., x_arg, to_arg) {
   details <- "Can't cast to a workflow set because attributes are likely incompatible."
   stop_incompatible_cast(x, to, x_arg = x_arg, to_arg = to_arg, details = details)
}
