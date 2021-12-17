library(parsnip)
suppressPackageStartupMessages(library(rsample))
suppressPackageStartupMessages(library(tune))
library(kknn)

# ------------------------------------------------------------------------------

lr_spec <- linear_reg() %>% set_engine("lm")
knn_spec <-
   nearest_neighbor(neighbors = tune()) %>%
   set_engine("kknn") %>%
   set_mode("regression")
glmn_spec <-
   linear_reg(penalty = tune()) %>%
   set_engine("glmnet")

set.seed(1)
folds <- vfold_cv(mtcars, v = 3)

car_set_1 <-
   workflow_set(
      list(reg = mpg ~ ., nonlin = mpg ~ wt + 1/sqrt(disp)),
      list(lm = lr_spec, knn = knn_spec)
   ) %>%
   dplyr::slice(-4)

# ------------------------------------------------------------------------------

test_that("basic mapping", {
   expect_error({
      res_1 <-
         car_set_1 %>%
         workflow_map(resamples = folds, seed = 2, grid = 2)
   },
   regexp = NA)

   # check reproducibility
   expect_error({
      res_2 <-
         car_set_1 %>%
         workflow_map(resamples = folds, seed = 2, grid = 2)
   },
   regexp = NA)
   expect_equal(collect_metrics(res_1), collect_metrics(res_2))

   # ---------------------------------------------------------------------------

   expect_error({
      two_class_set %>%
         workflow_map("foo", seed = 1, resamples = folds, grid = 2)
   },
   regexp = "Allowable values are")

})


test_that("map logging", {
   # since the logging prints execution times, we capture output then make a
   # snapshot without those lines
   expect_error({
      logging_res <-
         capture.output(
            res <-
               car_set_1 %>%
               workflow_map(resamples = folds, seed = 2, verbose = TRUE),
            type = "message"
         )
   },
   regex = NA
   )
   logging_res <- logging_res[!grepl("s\\)$", logging_res)]
   expect_snapshot(
      cat(logging_res, sep = "\n")
   )
})



test_that("missing packages", {
   skip_if(rlang::is_installed("rlang"))
   car_set_2 <-
      workflow_set(
         list(reg = mpg ~ .),
         list(glmn = glmn_spec)
      )

   expect_message({
      res <-
         car_set_2 %>%
         workflow_map(resamples = folds, seed = 2, verbose = FALSE)
   },
   regex = "glmnet"
   )
   expect_true(inherits(res, "workflow_set"))
   expect_equal(res$result[[1]], list())
})



test_that("failers", {
   skip_on_cran()
   car_set_3 <-
      workflow_set(
         list(reg = mpg ~ .),
         list(knn = knn_spec, lm = lr_spec)
      )

   expect_error({
      res_quiet <-
         car_set_3 %>%
         workflow_map(resamples = folds, seed = 2, verbose = FALSE, grid = "a")
   },
   regex = NA
   )
   expect_true(inherits(res_quiet, "workflow_set"))
   expect_true(inherits(res_quiet$result[[1]], "try-error"))

   expect_message(
      expect_error({
         res_loud <-
            car_set_3 %>%
            workflow_map(resamples = folds, seed = 2, verbose = TRUE, grid = "a")
      },
      regex = NA
      ),
      regex = "should be a positive integer or a data frame"
   )
   expect_true(inherits(res_loud, "workflow_set"))
   expect_true(inherits(res_loud$result[[1]], "try-error"))

})
