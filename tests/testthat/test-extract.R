skip_if_not_installed(c("kknn", "modeldata"))

library(parsnip)
library(rsample)
library(recipes)
data(Chicago, package = "modeldata")

lr_spec <- linear_reg() |> set_engine("lm")

set.seed(1)
car_set_1 <-
  workflow_set(
    list(
      reg = recipe(mpg ~ ., data = mtcars) |> step_log(disp),
      nonlin = mpg ~ wt + 1 / sqrt(disp)
    ),
    list(lm = lr_spec)
  ) |>
  workflow_map(
    "fit_resamples",
    resamples = vfold_cv(mtcars, v = 3),
    control = tune::control_resamples(save_pred = TRUE)
  )

# ------------------------------------------------------------------------------

test_that("extracts", {
  # workflows specific errors, so we don't capture their messages
  expect_snapshot(
    error = TRUE,
    extract_fit_engine(car_set_1, id = "reg_lm")
  )
  expect_snapshot(
    error = TRUE,
    extract_fit_parsnip(car_set_1, id = "reg_lm")
  )
  expect_snapshot(
    error = TRUE,
    extract_mold(car_set_1, id = "reg_lm")
  )
  expect_snapshot(
    error = TRUE,
    extract_recipe(car_set_1, id = "reg_lm")
  )

  expect_s3_class(
    extract_preprocessor(car_set_1, id = "reg_lm"),
    "recipe"
  )
  expect_s3_class(
    extract_spec_parsnip(car_set_1, id = "reg_lm"),
    "model_spec"
  )
  expect_s3_class(
    extract_workflow(car_set_1, id = "reg_lm"),
    "workflow"
  )
  expect_s3_class(
    extract_recipe(car_set_1, id = "reg_lm", estimated = FALSE),
    "recipe"
  )

  expect_equal(
    car_set_1 |> extract_workflow("reg_lm"),
    car_set_1$info[[1]]$workflow[[1]]
  )

  expect_equal(
    car_set_1 |> extract_workflow_set_result("reg_lm"),
    car_set_1$result[[1]]
  )

  expect_snapshot(error = TRUE, {
    car_set_1 |> extract_workflow_set_result("Gideon Nav")
  })

  expect_snapshot(error = TRUE, {
    car_set_1 |> extract_workflow("Coronabeth Tridentarius")
  })
})


test_that("extract parameter set from workflow set with untunable workflow", {
  rm_rec <- recipes::recipe(ridership ~ ., data = head(Chicago)) |>
    recipes::step_rm(date, ends_with("away"))
  lm_model <- parsnip::linear_reg() |>
    parsnip::set_engine("lm")
  bst_model <-
    parsnip::boost_tree(
      mode = "classification",
      trees = hardhat::tune("funky name \n")
    ) |>
    parsnip::set_engine("C5.0", rules = hardhat::tune(), noGlobalPruning = TRUE)
  wf_set <- workflow_set(
    list(reg = rm_rec),
    list(lm = lm_model, bst = bst_model)
  )

  lm_info <- hardhat::extract_parameter_set_dials(wf_set, id = "reg_lm")
  check_parameter_set_tibble(lm_info)
  expect_equal(nrow(lm_info), 0)
})

test_that("extract parameter set from workflow set with tunable workflow", {
  rm_rec <- recipes::recipe(ridership ~ ., data = head(Chicago)) |>
    recipes::step_rm(date, ends_with("away"))
  lm_model <- parsnip::linear_reg() |>
    parsnip::set_engine("lm")
  bst_model <-
    parsnip::boost_tree(
      mode = "classification",
      trees = hardhat::tune("funky name \n")
    ) |>
    parsnip::set_engine("C5.0", rules = hardhat::tune(), noGlobalPruning = TRUE)
  wf_set <- workflow_set(
    list(reg = rm_rec),
    list(lm = lm_model, bst = bst_model)
  )

  c5_info <- extract_parameter_set_dials(wf_set, id = "reg_bst")
  expect_equal(
    c5_info,
    extract_parameter_set_dials(bst_model)
  )
  check_parameter_set_tibble(c5_info)
  expect_equal(nrow(c5_info), 2)
  expect_true(all(c5_info$source == "model_spec"))
  expect_true(all(c5_info$component == "boost_tree"))
  expect_equal(c5_info$component_id, c("main", "engine"))
  nms <- c("trees", "rules")
  expect_equal(c5_info$name, nms)
  ids <- c("funky name \n", "rules")
  expect_equal(c5_info$id, ids)

  expect_equal(c5_info$object[[1]], dials::trees(c(1, 100)))
  expect_equal(c5_info$object[[2]], NA)

  c5_new_info <-
    c5_info |>
    update(
      rules = dials::new_qual_param(
        "logical",
        values = c(TRUE, FALSE),
        label = c(rules = "Rules")
      )
    )

  wf_set_2 <-
    wf_set |>
    option_add(id = "reg_bst", param_info = c5_new_info)

  check_parameter_set_tibble(c5_new_info)
  expect_s3_class(c5_new_info$object[[2]], "qual_param")
  expect_equal(
    c5_new_info,
    extract_parameter_set_dials(wf_set_2, "reg_bst")
  )
})


test_that("extract single parameter from workflow set with untunable workflow", {
  rm_rec <- recipes::recipe(ridership ~ ., data = head(Chicago)) |>
    recipes::step_rm(date, ends_with("away"))
  lm_model <- parsnip::linear_reg() |>
    parsnip::set_engine("lm")
  bst_model <-
    parsnip::boost_tree(
      mode = "classification",
      trees = hardhat::tune("funky name \n")
    ) |>
    parsnip::set_engine("C5.0", rules = hardhat::tune(), noGlobalPruning = TRUE)
  wf_set <- workflow_set(
    list(reg = rm_rec),
    list(lm = lm_model, bst = bst_model)
  )

  expect_snapshot(
    error = TRUE,
    hardhat::extract_parameter_dials(
      wf_set,
      id = "reg_lm",
      parameter = "non there"
    )
  )
})

test_that("extract single parameter from workflow set with tunable workflow", {
  rm_rec <- recipes::recipe(ridership ~ ., data = head(Chicago)) |>
    recipes::step_rm(date, ends_with("away"))
  lm_model <- parsnip::linear_reg() |>
    parsnip::set_engine("lm")
  bst_model <-
    parsnip::boost_tree(
      mode = "classification",
      trees = hardhat::tune("funky name \n")
    ) |>
    parsnip::set_engine("C5.0", rules = hardhat::tune(), noGlobalPruning = TRUE)
  wf_set <- workflow_set(
    list(reg = rm_rec),
    list(lm = lm_model, bst = bst_model)
  )

  expect_equal(
    hardhat::extract_parameter_dials(
      wf_set,
      id = "reg_bst",
      parameter = "funky name \n"
    ),
    dials::trees(c(1, 100))
  )
  expect_equal(
    extract_parameter_dials(wf_set, id = "reg_bst", parameter = "rules"),
    NA
  )
})
