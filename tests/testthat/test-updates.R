skip_if_not_installed("kknn")
skip_if_not_installed("modeldata")

library(parsnip)
library(recipes)
library(hardhat)

data(two_class_dat, package = "modeldata")

xgb <- boost_tree(trees = 3) |> set_mode("classification")
rec <-
  recipe(Class ~ A + B, two_class_dat) |>
  step_normalize(A) |>
  step_normalize(B)

sparse_bp <- default_recipe_blueprint(composition = "dgCMatrix")


test_that("update model", {
  expect_no_error(
    new_set <- update_workflow_model(two_class_res, "none_cart", spec = xgb)
  )
  expect_true(
    inherits(
      extract_spec_parsnip(new_set, id = "none_cart"),
      "boost_tree"
    )
  )
  expect_equal(new_set$result[[1]], list())

  expect_no_error(
    new_new_set <-
      update_workflow_model(
        new_set,
        "none_glm",
        spec = xgb,
        formula = Class ~ log(A) + B
      )
  )
  new_wflow <- extract_workflow(new_new_set, "none_glm")
  expect_equal(
    new_wflow$fit$actions$model$formula,
    Class ~ log(A) + B
  )
})

test_that("update recipe", {
  expect_no_error(
    new_set <- update_workflow_recipe(
      two_class_res,
      "yj_trans_cart",
      recipe = rec
    )
  )
  new_rec <- extract_recipe(new_set, id = "yj_trans_cart", estimated = FALSE)

  expect_true(all(tidy(new_rec)$type == "normalize"))
  expect_equal(new_set$result[[4]], list())

  expect_no_error(
    new_new_set <-
      update_workflow_recipe(
        new_set,
        "yj_trans_cart",
        recipe = rec,
        blueprint = sparse_bp
      )
  )
  new_wflow <- extract_workflow(new_new_set, "yj_trans_cart")
  expect_equal(new_wflow$pre$actions$recipe$blueprint, sparse_bp)
})
