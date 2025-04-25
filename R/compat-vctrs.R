# ------------------------------------------------------------------------------

# `vec_restore()`
#
# Called at the end of `vec_slice()` and `vec_ptype()` after all slicing has
# been done on the proxy object.
#
# If all invariants still hold after modifying the proxy, then we can restore
# to a workflow_set object. Otherwise, it will fall back to a bare tibble.
#
# Unlike rsample classes, `vec_ptype()` returns a workflow_set object here.
# This allows `vec_ptype.workflow_set.workflow_set()` to be called.

#' @export
vec_restore.workflow_set <- function(x, to, ...) {
  workflow_set_maybe_reconstruct(x)
}

# ------------------------------------------------------------------------------

# `vec_ptype2()`
#
# When combining two workflow_sets together, `x` and `y` will be zero-row slices
# which should always result in a new workflow_set object, as long as
# `df_ptype2()` can compute a common type.
#
# Combining a workflow_set with a tibble/data.frame will only ever happen if
# the user calls `vec_c()` or `vec_rbind()` with one of each of those inputs.
# I think that it would be very difficult to expect that this returns a new
# workflow_set, so instead we always return a tibble.

#' @export
vec_ptype2.workflow_set.workflow_set <- function(
  x,
  y,
  ...,
  x_arg = "",
  y_arg = ""
) {
  out <- vctrs::df_ptype2(x, y, ..., x_arg = x_arg, y_arg = y_arg)
  workflow_set_maybe_reconstruct(out)
}
#' @export
vec_ptype2.workflow_set.tbl_df <- function(x, y, ..., x_arg = "", y_arg = "") {
  vctrs::tib_ptype2(x, y, ..., x_arg = x_arg, y_arg = y_arg)
}
#' @export
vec_ptype2.tbl_df.workflow_set <- function(x, y, ..., x_arg = "", y_arg = "") {
  vctrs::tib_ptype2(x, y, ..., x_arg = x_arg, y_arg = y_arg)
}
#' @export
vec_ptype2.workflow_set.data.frame <- function(
  x,
  y,
  ...,
  x_arg = "",
  y_arg = ""
) {
  vctrs::tib_ptype2(x, y, ..., x_arg = x_arg, y_arg = y_arg)
}
#' @export
vec_ptype2.data.frame.workflow_set <- function(
  x,
  y,
  ...,
  x_arg = "",
  y_arg = ""
) {
  vctrs::tib_ptype2(x, y, ..., x_arg = x_arg, y_arg = y_arg)
}

# ------------------------------------------------------------------------------

# `vec_cast()`
#
# These methods are designed with `vec_ptype2()` in mind.
#
# Casting from one workflow_set to another will happen "automatically" when
# two workflow_sets are combined with `vec_c()`. The common type will be
# computed with `vec_ptype2()`, then each input will be `vec_cast()` to that
# common type. It should always be possible to reconstruct the workflow_set
# if `df_cast()` is able to cast the underlying data frames successfully.
#
# Casting a tibble or data.frame to a workflow_set should never happen
# automatically, because the ptype2 methods always push towards
# tibble / data.frame. Since it is so unlikely that this will be done
# correctly, we don't ever allow it.
#
# Casting a workflow_set to a tibble or data.frame is easy, the underlying
# vctrs function does the work for us. This is used when doing
# `vec_c(<workflow_set>, <tbl>)`, as the `vec_ptype2()` method will compute
# a common type of tibble, and then each input will be cast to tibble.

#' @export
vec_cast.workflow_set.workflow_set <- function(
  x,
  to,
  ...,
  x_arg = "",
  to_arg = ""
) {
  out <- vctrs::df_cast(x, to, ..., x_arg = x_arg, to_arg = to_arg)
  workflow_set_maybe_reconstruct(out)
}
#' @export
vec_cast.workflow_set.tbl_df <- function(x, to, ..., x_arg = "", to_arg = "") {
  stop_incompatible_cast_workflow_set(x, to, x_arg = x_arg, to_arg = to_arg)
}
#' @export
vec_cast.tbl_df.workflow_set <- function(x, to, ..., x_arg = "", to_arg = "") {
  vctrs::tib_cast(x, to, ..., x_arg = x_arg, to_arg = to_arg)
}
#' @export
vec_cast.workflow_set.data.frame <- function(
  x,
  to,
  ...,
  x_arg = "",
  to_arg = ""
) {
  stop_incompatible_cast_workflow_set(x, to, x_arg = x_arg, to_arg = to_arg)
}
#' @export
vec_cast.data.frame.workflow_set <- function(
  x,
  to,
  ...,
  x_arg = "",
  to_arg = ""
) {
  vctrs::df_cast(x, to, ..., x_arg = x_arg, to_arg = to_arg)
}

# ------------------------------------------------------------------------------

stop_incompatible_cast_workflow_set <- function(x, to, ..., x_arg, to_arg) {
  details <- "Can't cast to a <workflow_set> because the resulting structure is likely invalid."
  vctrs::stop_incompatible_cast(
    x,
    to,
    x_arg = x_arg,
    to_arg = to_arg,
    details = details
  )
}
