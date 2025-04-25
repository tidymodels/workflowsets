# ------------------------------------------------------------------------------
# vec_restore()

test_that("vec_restore() returns a workflow_set subclass if `x` retains structure", {
  for (x in workflow_set_objects) {
    expect_identical(vec_restore(x, x), x)
    expect_s3_class_workflow_set(vec_restore(x, x))
  }
})

test_that("vec_restore() returns workflow_set when row slicing", {
  for (x in workflow_set_objects) {
    row1 <- x[1, ]
    row0 <- x[0, ]

    expect_s3_class_workflow_set(vec_restore(row1, x))
    expect_s3_class_workflow_set(vec_restore(row0, x))
  }
})

test_that("vec_restore() returns bare tibble if `x` loses column structure", {
  for (x in workflow_set_objects) {
    col <- x[1]
    expect_s3_class_bare_tibble(vec_restore(col, x))
  }
})

# ------------------------------------------------------------------------------
# vec_ptype2()

test_that("vec_ptype2() is working", {
  for (x in workflow_set_objects) {
    x2 <- x
    x2$y <- 1
    x3 <- x
    x3$z <- 2

    tbl <- tibble::tibble(x = 1)
    df <- data.frame(x = 1)

    # workflow_set-workflow_set
    expect_identical(vec_ptype2(x, x), vec_slice(x, NULL))
    expect_identical(vec_ptype2(x2, x3), new_workflow_set0(df_ptype2(x2, x3)))

    # workflow_set-tbl_df
    expect_identical(vec_ptype2(x, tbl), vec_ptype2(new_tibble0(x), tbl))
    expect_identical(vec_ptype2(tbl, x), vec_ptype2(tbl, new_tibble0(x)))

    # workflow_set-df
    expect_identical(vec_ptype2(x, df), vec_ptype2(new_tibble0(x), df))
    expect_identical(vec_ptype2(df, x), vec_ptype2(df, new_tibble0(x)))
  }
})

# ------------------------------------------------------------------------------
# vec_cast()

test_that("vec_cast() is working", {
  for (x in workflow_set_objects) {
    x2 <- x
    x2$y <- 1
    x3 <- x
    x3$z <- 2

    tbl <- new_tibble0(x)
    df <- as.data.frame(tbl)

    # workflow_set-workflow_set
    expect_identical(vec_cast(x, x), x)

    x2_expect <- x
    x2_expect$y <- NA_real_
    expect_identical(vec_cast(x, x2), x2_expect)

    expect_error(vec_cast(x2, x3), class = "vctrs_error_cast_lossy_dropped")

    # workflow_set-tbl_df
    expect_identical(vec_cast(x, tbl), tbl)
    expect_error(vec_cast(tbl, x), class = "vctrs_error_incompatible_type")

    # workflow_set-df
    expect_identical(vec_cast(x, df), df)
    expect_error(vec_cast(df, x), class = "vctrs_error_incompatible_type")
  }
})

# ------------------------------------------------------------------------------
# vctrs methods

test_that("vec_ptype() returns a workflow_set", {
  for (x in workflow_set_objects) {
    expect_s3_class_workflow_set(vec_ptype(x))
  }
})

test_that("vec_slice() generally returns a workflow_set", {
  for (x in workflow_set_objects) {
    expect_s3_class_workflow_set(vec_slice(x, 0))
    expect_s3_class_workflow_set(vec_slice(x, 1:2))
  }
})

test_that("vec_slice() can return a tibble if wflow_ids are duplicated", {
  for (x in workflow_set_objects) {
    expect_identical(vec_slice(x, c(1, 1)), vec_slice(new_tibble0(x), c(1, 1)))
  }
})

test_that("vec_c() works", {
  for (x in workflow_set_objects) {
    tbl <- new_tibble0(x)

    expect_identical(vec_c(x), x)
    expect_identical(vec_c(x, x), vec_c(tbl, tbl))
    expect_identical(vec_c(x[1:2, ], x[3, ]), x)
  }
})

test_that("vec_rbind() works", {
  for (x in workflow_set_objects) {
    tbl <- new_tibble0(x)

    expect_identical(vec_rbind(x), x)
    expect_identical(vec_rbind(x, x), vec_rbind(tbl, tbl))
    expect_identical(vec_rbind(x[1:2, ], x[3, ]), x)
  }
})

test_that("vec_cbind() returns a bare tibble", {
  for (x in workflow_set_objects) {
    tbl <- new_tibble0(x)

    # Unlike vec_c() and vec_rbind(), the prototype of the output comes
    # from doing `x[0]`, which will drop the workflow_set class
    expect_identical(vec_cbind(x), vec_cbind(tbl))
    expect_identical(
      vec_cbind(x, x, .name_repair = "minimal"),
      vec_cbind(tbl, tbl, .name_repair = "minimal")
    )
    expect_identical(
      vec_cbind(x, tbl, .name_repair = "minimal"),
      vec_cbind(tbl, tbl, .name_repair = "minimal")
    )
  }
})
