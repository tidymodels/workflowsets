suppressPackageStartupMessages(library(rsample))

data(two_class_dat, package = "modeldata")

# ------------------------------------------------------------------------------

set.seed(1)
folds <- vfold_cv(two_class_dat, v = 3)

# ------------------------------------------------------------------------------


test_that("basic mapping", {
   expect_message(
      expect_message(
         expect_error({
            res_1 <-
               two_class_set %>%
               workflow_map(seed = 1, resamples = folds, grid = 2)
         },
         regexp = NA),
         "No tuning parameters"
      ),
      "No tuning parameters"
   )

   # check reproducability
   expect_message(
      expect_message(
         expect_error({
            res_2 <-
               two_class_set %>%
               workflow_map(seed = 1, resamples = folds, grid = 2)
         },
         regexp = NA),
         "No tuning parameters"
      ),
      "No tuning parameters"
   )
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
               two_class_set %>%
               workflow_map(seed = 1, resamples = folds, grid = 2, verbose = TRUE),
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

