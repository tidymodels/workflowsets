library(parsnip)
library(rsample)
library(recipes)

lr_spec <- linear_reg() %>% set_engine("lm")

set.seed(1)
car_set_1 <-
   workflow_set(
      list(reg = recipe(mpg ~ ., data = mtcars) %>% step_log(disp),
           nonlin = mpg ~ wt + 1/sqrt(disp)),
      list(lm = lr_spec)
   ) %>%
   workflow_map("fit_resamples", resamples = vfold_cv(mtcars, v = 3),
                control = tune::control_resamples(save_pred = TRUE))

# ------------------------------------------------------------------------------

test_that('extracts', {
   expect_error(
      extract_fit_engine(car_set_1, id = "reg_lm"),
      "The workflow does not have a model fit"
   )
   expect_error(
      extract_fit_parsnip(car_set_1, id = "reg_lm"),
      "The workflow does not have a model fit"
   )
   expect_error(
      extract_mold(car_set_1, id = "reg_lm"),
      "workflow does not have a mold"
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
   expect_error(
      extract_recipe(car_set_1, id = "reg_lm"),
      "workflow does not have a mold"
   )
   expect_s3_class(
      extract_recipe(car_set_1, id = "reg_lm", estimated = FALSE),
      "recipe"
   )

   expect_equal(
      car_set_1 %>% extract_workflow("reg_lm"),
      car_set_1$info[[1]]$workflow[[1]]
   )

   expect_equal(
      car_set_1 %>% extract_workflow_set_result("reg_lm"),
      car_set_1$result[[1]]
   )

   expect_error(
      car_set_1 %>% extract_workflow_set_result("Gideon Nav"),
      "`id` must correspond to a single row in `x`"
   )

   expect_error(
      car_set_1 %>% extract_workflow("Coronabeth Tridentarius"),
      "`id` must correspond to a single row in `x`"
   )
})