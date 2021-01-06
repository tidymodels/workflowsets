library(parsnip)
library(rsample)
library(rlang)

lr_spec <- linear_reg() %>% set_engine("lm")
knn_spec <- nearest_neighbor() %>% set_engine("kknn") %>% set_mode("regression")

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

# ------------------------------------------------------------------------------

test_that("crossing", {
   expect_equal(
      nrow(
         workflow_set(
            list(reg = mpg ~ ., nonlin = mpg ~ wt + 1/sqrt(disp)),
            list(lm = lr_spec, knn = knn_spec), cross = FALSE
         )
      ),
      2
   )
   expect_equal(
      nrow(
         workflow_set(
            list(reg = mpg ~ ., nonlin = mpg ~ wt + 1/sqrt(disp)),
            list(lm = lr_spec, knn = knn_spec), cross = TRUE
         )
      ),
      4
   )
   expect_equal(
      nrow(
         workflow_set(
            list(reg = mpg ~ ., nonlin = mpg ~ wt + 1/sqrt(disp)),
            list(lm = lr_spec),
         )
      ),
      2
   )
   expect_equal(
      nrow(
         workflow_set(
            list(reg = mpg ~ .),
            list(lm = lr_spec, knn = knn_spec)
         )
      ),
      2
   )
   expect_error(
      nrow(
         workflow_set(
            list(reg = mpg ~ ., nonlin = mpg ~ wt + 1/sqrt(disp), two = mpg ~ wt + disp),
            list(lm = lr_spec, knn = knn_spec), cross = FALSE
         )
      ),
      "The lengths of 'preproc' and 'models' are different"
   )
})

# ------------------------------------------------------------------------------

test_that("constructor", {

   set.seed(1)
   car_set_1 <-
      workflow_set(
         list(reg = mpg ~ ., nonlin = mpg ~ wt + 1/sqrt(disp)),
         list(lm = lr_spec)
      ) %>%
      workflow_map("fit_resamples", resamples = vfold_cv(mtcars, v = 3),
                   control = tune::control_resamples(save_pred = TRUE, save_workflow = TRUE))

   expect_error(
      workflowsets:::new_workflow_set(car_set_1 %>% dplyr::select(-object)),
      "The object should have columns"
   )

   expect_error(
      workflowsets:::new_workflow_set(car_set_1 %>% mutate(object = "a")),
      "The 'object' column should be a list."
   )
   expect_error(
      workflowsets:::new_workflow_set(car_set_1 %>% mutate(result = "a")),
      "The 'result' column should be a list."
   )
   expect_error(
      workflowsets:::new_workflow_set(car_set_1 %>% mutate(option = "a")),
      "The 'option' column should be a list."
   )
   expect_error(
      workflowsets:::new_workflow_set(car_set_1 %>% mutate(wflow_id = 1)),
      "The 'wflow_id' column should be character."
   )
   expect_error(
      workflowsets:::new_workflow_set(car_set_1 %>% mutate(wflow_id = "a")),
      "The 'wflow_id' column should contain unique, non-missing character strings"
   )
   expect_error(
      workflowsets:::new_workflow_set(car_set_1 %>% mutate(object = map(wflow_id, ~ list(1)))),
      "column are not workflow object"
   )
   expect_error(
      workflowsets:::new_workflow_set(car_set_1 %>% mutate(preproc = 1)),
      "The 'preproc' column should be character."
   )
   expect_error(
      workflowsets:::new_workflow_set(car_set_1 %>% mutate(model = 1)),
      "The 'model' column should be character."
   )








})
