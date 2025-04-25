library(dplyr)

# ------------------------------------------------------------------------------
# dplyr_reconstruct()

test_that("dplyr_reconstruct() returns an workflow_set subclass if `x` retains workflow_set structure", {
  for (x in workflow_set_objects) {
    expect_identical(dplyr_reconstruct(x, x), x)
    expect_s3_class_workflow_set(dplyr_reconstruct(x, x))
  }
})

test_that("dplyr_reconstruct() returns workflow_set when row slicing", {
  for (x in workflow_set_objects) {
    row1 <- x[1, ]
    row0 <- x[0, ]

    expect_s3_class_workflow_set(dplyr_reconstruct(row1, x))
    expect_s3_class_workflow_set(dplyr_reconstruct(row0, x))
  }
})

test_that("dplyr_reconstruct() returns bare tibble if `x` loses workflow_set structure", {
  for (x in workflow_set_objects) {
    col <- x[1]
    expect_s3_class_bare_tibble(dplyr_reconstruct(col, x))
  }
})

# ------------------------------------------------------------------------------
# dplyr_col_modify()

test_that("can add columns and retain workflow_set class", {
  for (x in workflow_set_objects) {
    cols <- list(x = rep(1, vec_size(x)))

    result <- dplyr_col_modify(x, cols)

    expect_s3_class_workflow_set(result)
    expect_identical(result$x, cols$x)
  }
})

test_that("modifying workflow_set columns removes workflow_set class", {
  for (x in workflow_set_objects) {
    cols <- list(wflow_id = rep(1, vec_size(x)))

    result <- dplyr_col_modify(x, cols)

    expect_s3_class_bare_tibble(result)
    expect_identical(result$wflow_id, cols$wflow_id)
  }

  for (x in workflow_set_objects) {
    cols <- list(info = rep(1, vec_size(x)))

    result <- dplyr_col_modify(x, cols)

    expect_s3_class_bare_tibble(result)
    expect_identical(result$info, cols$info)
  }
})

test_that("replacing workflow_set columns with the exact same column retains workflow_set class", {
  for (x in workflow_set_objects) {
    cols <- list(wflow_id = x$wflow_id)

    result <- dplyr_col_modify(x, cols)

    expect_s3_class_workflow_set(result)
    expect_identical(result, x)
  }
})

# ------------------------------------------------------------------------------
# dplyr_row_slice()

test_that("row slicing generally keeps the workflow_set subclass", {
  for (x in workflow_set_objects) {
    expect_s3_class_workflow_set(dplyr_row_slice(x, 0))
  }
})

test_that("row slicing and duplicating any rows removes the workflow_set subclass", {
  for (x in workflow_set_objects) {
    expect_s3_class_bare_tibble(dplyr_row_slice(x, c(1, 1)))
  }
})

test_that("workflow_set subclass is kept if row order is changed", {
  for (x in workflow_set_objects) {
    loc <- rev(seq_len(nrow(x)))
    expect_s3_class_workflow_set(dplyr_row_slice(x, loc))
  }
})

# ------------------------------------------------------------------------------
# summarise()

test_that("summarise() always drops the workflow_set class", {
  for (x in workflow_set_objects) {
    expect_s3_class_bare_tibble(summarise(x, y = 1))
    expect_s3_class_bare_tibble(summarise(
      x,
      wflow_id = wflow_id[1],
      info = info[1],
      option = option[1],
      result = result[1]
    ))
  }
})

# ------------------------------------------------------------------------------
# group_by()

test_that("group_by() always returns a bare grouped-df or bare tibble", {
  for (x in workflow_set_objects) {
    expect_s3_class_bare_tibble(group_by(x))
    expect_s3_class(
      group_by(x, wflow_id),
      c("grouped_df", "tbl_df", "tbl", "data.frame"),
      exact = TRUE
    )
  }
})

# ------------------------------------------------------------------------------
# ungroup()

test_that("ungroup() returns a workflow_set", {
  for (x in workflow_set_objects) {
    expect_s3_class_workflow_set(ungroup(x))
  }
})

# ------------------------------------------------------------------------------
# relocate()

test_that("can relocate() and keep the class", {
  for (x in workflow_set_objects) {
    x <- relocate(x, info)
    expect_s3_class_workflow_set(x)
  }
})

# ------------------------------------------------------------------------------
# distinct()

test_that("distinct() keeps the class if everything is intact", {
  for (x in workflow_set_objects) {
    expect_s3_class_workflow_set(distinct(x))
  }
})

test_that("distinct() drops the class if any workflow_set columns are lost", {
  for (x in workflow_set_objects) {
    expect_s3_class_bare_tibble(distinct(x, option))
  }
})

# ------------------------------------------------------------------------------
# left_join()

test_that("left_join() can keep workflow_set class if workflow_set structure is intact", {
  for (x in workflow_set_objects) {
    expect_s3_class_workflow_set(left_join(x, x, by = names(x)))

    y <- tibble(wflow_id = x$wflow_id[[1]], x = 1)
    expect_s3_class_workflow_set(left_join(x, y, by = "wflow_id"))
  }
})

test_that("left_join() can lose workflow_set class if rows are duplicated", {
  for (x in workflow_set_objects) {
    y <- tibble(wflow_id = x$wflow_id[[1]], x = 1:2)
    expect_s3_class_bare_tibble(left_join(x, y, by = "wflow_id"))
  }
})

# ------------------------------------------------------------------------------
# right_join()

test_that("right_join() can keep workflow_set class if workflow_set structure is intact", {
  for (x in workflow_set_objects) {
    expect_s3_class_workflow_set(right_join(x, x, by = names(x)))

    y <- mutate(select(x, wflow_id), x = 1)
    expect_s3_class_workflow_set(right_join(x, y, by = "wflow_id"))
  }
})

test_that("right_join() can lose workflow_set class if rows are duplicated", {
  for (x in workflow_set_objects) {
    y <- tibble(wflow_id = x$wflow_id[[1]], x = 1:2)
    expect_s3_class_bare_tibble(right_join(x, y, by = "wflow_id"))
  }
})

test_that("right_join() restores to the type of first input", {
  for (x in workflow_set_objects) {
    y <- tibble(wflow_id = x$wflow_id[[1]], x = 1)
    # technically workflow_set structure is intact, but `y` is a bare tibble!
    expect_s3_class_bare_tibble(right_join(y, x, by = "wflow_id"))
  }
})

# ------------------------------------------------------------------------------
# full_join()

test_that("full_join() can keep workflow_set class if workflow_set structure is intact", {
  for (x in workflow_set_objects) {
    expect_s3_class_workflow_set(full_join(x, x, by = names(x)))
  }
})

test_that("full_join() can lose workflow_set class if rows are added", {
  for (x in workflow_set_objects) {
    y <- tibble(wflow_id = "foo", x = 1)
    expect_s3_class_bare_tibble(full_join(x, y, by = "wflow_id"))
  }
})

# ------------------------------------------------------------------------------
# anti_join()

test_that("anti_join() can keep workflow_set class if workflow_set structure is intact", {
  for (x in workflow_set_objects) {
    y <- tibble(wflow_id = "foo")
    expect_s3_class_workflow_set(anti_join(x, y, by = "wflow_id"))
  }
})

test_that("anti_join() can keep workflow_set class if rows are removed", {
  for (x in workflow_set_objects) {
    y <- tibble(wflow_id = x$wflow_id[[1]], x = 1)
    expect_s3_class_workflow_set(anti_join(x, y, by = "wflow_id"))
  }
})

# ------------------------------------------------------------------------------
# semi_join()

test_that("semi_join() can keep workflow_set class if workflow_set structure is intact", {
  for (x in workflow_set_objects) {
    expect_s3_class_workflow_set(semi_join(x, x, by = names(x)))
  }
})

test_that("semi_join() can keep workflow_set class if rows are removed", {
  for (x in workflow_set_objects) {
    y <- tibble(wflow_id = "foo", x = 1)
    expect_s3_class_workflow_set(semi_join(x, y, by = "wflow_id"))
  }
})

# ------------------------------------------------------------------------------
# nest_join()

test_that("nest_join() can keep workflow_set class if workflow_set structure is intact", {
  for (x in workflow_set_objects) {
    y <- mutate(x, foo = "bar")
    expect_s3_class_workflow_set(nest_join(x, y, by = names(x)))
  }
})

# ------------------------------------------------------------------------------
# bind_rows()

test_that("bind_rows() drops the class with duplicate rows", {
  for (x in workflow_set_objects) {
    expect_s3_class_bare_tibble(bind_rows(x, x))
  }
})

test_that("bind_rows() keeps the class with new non-duplicate rows", {
  for (x in workflow_set_objects) {
    expect_s3_class_workflow_set(bind_rows(x[1:2, ], x[3, ]))
  }
})

# ------------------------------------------------------------------------------
# bind_cols()

test_that("bind_cols() keeps the class with new duplicated columns", {
  x <- workflow_set_objects$unfit
  y <- tibble(x = rep(1, vec_size(x)))
  expect_s3_class_workflow_set(bind_cols(x, y))
})

test_that("bind_cols() drops the class with new duplicated rows", {
  # Use workflow_set with 1 row, these get recycled
  x <- workflow_set_objects$unfit[1, ]
  expect_s3_class_bare_tibble(bind_cols(x, tibble(x = 1:2)))
})
