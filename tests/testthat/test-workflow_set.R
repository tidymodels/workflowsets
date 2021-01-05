library(parsnip)
library(rsample)
library(rlang)

lr_spec <- linear_reg() %>% set_engine("lm")

# ------------------------------------------------------------------------------


test_that("creating workflow sets", {
   expect_error({
      set.seed(1)
      car_set_1 <-
         workflow_set(
            list(reg = mpg ~ ., nonlin = mpg ~ wt + 1/sqrt(disp)),
            list(lm = lr_spec)
         ) %>%
         workflow_map("fit_resamples", resamples = vfold_cv(mtcars, v = 3),
                      control = tune::control_resamples(save_pred = TRUE, save_workflow = TRUE))
   },
   regex = NA
   )

   expect_s3_class(car_set_1, c("workflow_set", "tbl_df", "tbl", "data.frame"))
   expect_equal(
      names(car_set_1),
      c("wflow_id", "preproc", "model", "object", "option", "result")
   )

   expect_true(
      all(purrr::map_lgl(car_set_1$wflow_id, is.character))
   )
   expect_true(
      all(purrr::map_lgl(car_set_1$preproc, is.character))
   )
   expect_true(
      all(purrr::map_lgl(car_set_1$model, is.character))
   )

   expect_true(
      all(purrr::map_lgl(car_set_1$object, is.list))
   )
   expect_true(
      all(purrr::map_lgl(car_set_1$option, is.list))
   )
   expect_true(
      all(purrr::map_lgl(car_set_1$result, is.list))
   )

   expect_true(
      all(purrr::map_lgl(car_set_1$object, ~ inherits(.x, "workflow")))
   )
   expect_true(
      all(purrr::map_lgl(car_set_1$option, ~ inherits(.x, "list")))
   )
   expect_true(
      all(purrr::map_lgl(car_set_1$result, ~ inherits(.x, "resample_results")))
   )
   expect_true(
      all(purrr::map_lgl(car_set_1$result, ~ any(names(.x) == ".predictions")))
   )

   # ---------------------------------------------------------------------------

   model_list <- car_set_1$result
   names(model_list) <- car_set_1$wflow_id

   expect_error(
      car_set_2 <- as_workflow_set(!!!model_list),
      regex = NA
   )

   expect_true(
      all(purrr::map_lgl(car_set_2$wflow_id, is.character))
   )
   expect_true(
      all(purrr::map_lgl(car_set_2$preproc, is.character))
   )
   expect_true(
      all(purrr::map_lgl(car_set_2$model, is.character))
   )

   expect_true(
      all(purrr::map_lgl(car_set_2$object, is.list))
   )
   expect_true(
      all(purrr::map_lgl(car_set_2$option, is.list))
   )
   expect_true(
      all(purrr::map_lgl(car_set_2$result, is.list))
   )

   expect_true(
      all(purrr::map_lgl(car_set_2$object, ~ inherits(.x, "workflow")))
   )
   expect_true(
      all(purrr::map_lgl(car_set_2$option, ~ inherits(.x, "list")))
   )
   expect_true(
      all(purrr::map_lgl(car_set_2$result, ~ inherits(.x, "resample_results")))
   )
   expect_true(
      all(purrr::map_lgl(car_set_2$result, ~ any(names(.x) == ".predictions")))
   )

})

