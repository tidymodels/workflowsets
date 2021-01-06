test_that("autoplot with error bars", {
  p_1 <- autoplot(chi_models)
  expect_s3_class(p_1, "ggplot")
  expect_equal(
     names(p_1$data),
     c("wflow_id", ".config", ".metric", "mean", "std_err", "n", "model",
       "preprocessor", "rank")
  )
  expect_equal(rlang::get_expr(p_1$mapping$x), expr(rank))
  expect_equal(rlang::get_expr(p_1$mapping$y), expr(mean))
  expect_equal(rlang::get_expr(p_1$mapping$colour), expr(model))
  expect_equal(as.list(p_1$facet)$params, list())
  expect_equal(
     rlang::get_expr(as.list(p_1$layers[[2]])$mapping$ymin),
     expr(mean - std_errs * std_err)
  )
  expect_equal(
     rlang::get_expr(as.list(p_1$layers[[2]])$mapping$ymax),
     expr(mean + std_errs * std_err)
  )
  expect_equal(as.character(p_1$labels$y), "mae")
  expect_equal(as.character(p_1$labels$x), "Workflow Rank")
})

test_that("autoplot with without error bars", {
   p_2 <- autoplot(cell_models)
   expect_s3_class(p_2, "ggplot")
   expect_equal(
      names(p_2$data),
      c("wflow_id", ".config", ".metric", "mean", "std_err", "n", "model",
        "preprocessor", "rank")
   )
   expect_equal(rlang::get_expr(p_2$mapping$x), expr(rank))
   expect_equal(rlang::get_expr(p_2$mapping$y), expr(mean))
   expect_equal(rlang::get_expr(p_2$mapping$colour), expr(model))
   expect_equal(length(p_2$layers), 1)
   expect_equal(names(as.list(p_2$facet)$params$facet), ".metric")
   expect_equal(p_2$labels$y, "Metric")
   expect_equal(p_2$labels$x, "Workflow Rank")
})

test_that("autoplot for specific workflow result", {
   p_3 <- autoplot(cell_models, which = cell_models$wflow_id[1])
   p_4 <- autoplot(cell_models$result[[1]])
   expect_equal(p_3$data, p_4$data)
   expect_equal(p_3$labels, p_4$labels)
   expect_equal(
      purrr::map(as.list(p_3$mapping), rlang::get_expr),
      purrr::map(as.list(p_4$mapping), rlang::get_expr)
   )

})
