# ------------------------------------------------------------------------------

test_that("option management", {
  expect_no_error(
    set_1 <- two_class_set |> option_add(grid = 1)
  )
  for (i in 1:nrow(set_1)) {
    expect_equal(unclass(set_1$option[[i]]), list(grid = 1))
  }
  expect_no_error(
    set_2 <- two_class_set |> option_remove(grid)
  )
  for (i in 1:nrow(set_2)) {
    expect_equal(unclass(set_2$option[[i]]), list())
  }
  expect_no_error(
    set_3 <- two_class_set |> option_add(grid = 1, id = "none_cart")
  )
  expect_equal(unclass(set_3$option[[1]]), list(grid = 1))
  for (i in 2:nrow(set_3)) {
    expect_equal(unclass(set_3$option[[i]]), list())
  }
  expect_no_error(
    set_4 <- two_class_set |> option_add_parameters()
  )
  for (i in which(!grepl("glm", set_4$wflow_id))) {
    expect_true(all(names(set_4$option[[i]]) == "param_info"))
    expect_true(inherits(set_4$option[[i]]$param_info, "parameters"))
  }
  for (i in which(grepl("glm", set_4$wflow_id))) {
    expect_equal(unclass(set_4$option[[i]]), list())
  }
  expect_no_error(
    set_5 <- two_class_set |> option_add_parameters(id = "none_cart")
  )
  expect_true(all(names(set_5$option[[1]]) == "param_info"))
  expect_true(inherits(set_5$option[[1]]$param_info, "parameters"))
  for (i in 2:nrow(set_5)) {
    expect_equal(unclass(set_5$option[[i]]), list())
  }
})


test_that("option printing", {
  expect_output(
    print(two_class_res$option[[1]]),
    "a list of options with names:  'resamples', 'grid'"
  )
  expect_equal(
    pillar::type_sum(two_class_res$option[[1]]),
    "opts[3]"
  )
})


test_that("check for bad options", {
  expect_snapshot_error(
    two_class_set |> option_add(grid2 = 1)
  )
  expect_snapshot_error(
    two_class_set |> option_add(grid = 1, blueprint = 2)
  )
})
