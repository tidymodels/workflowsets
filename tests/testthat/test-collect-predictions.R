
library(parsnip)
library(rsample)

lr_spec <- linear_reg() %>% set_engine("lm")

set.seed(1)
car_set_1 <-
   workflow_set(
      list(reg = mpg ~ ., nonlin = mpg ~ wt + 1/sqrt(disp)),
      list(lm = lr_spec)
   ) %>%
   workflow_map("fit_resamples", resamples = vfold_cv(mtcars, v = 3),
                control = tune::control_resamples(save_pred = TRUE))

set.seed(1)
car_set_2 <-
   workflow_set(
      list(reg = mpg ~ ., nonlin = mpg ~ wt + 1/sqrt(disp)),
      list(lm = lr_spec)
   ) %>%
   workflow_map("fit_resamples", resamples = vfold_cv(mtcars, v = 3, repeats = 2),
                control = tune::control_resamples(save_pred = TRUE))

# ------------------------------------------------------------------------------


check_prediction_results <- function(ind, x, summarize = FALSE, ...) {
   id_val <- x$wflow_id[ind]

   cols <- c(".row", "mpg", ".config", ".pred")

   orig <-
      collect_predictions(x$result[[ind]], summarize = summarize, ...) %>%
      dplyr::select(dplyr::all_of(cols))

   if (any(names(list(...)) == "summarize")) {
      cols <- c(grep("^id", names(orig), value = TRUE), cols)
   }

   everythng <-
      collect_predictions(x, summarize = summarize, ...) %>%
      dplyr::filter(wflow_id == id_val) %>%
      dplyr::select(dplyr::all_of(cols))
   dplyr::all_equal(orig, everythng)
}

# ------------------------------------------------------------------------------

test_that("collect predictions", {

   for (i in 1:nrow(car_set_1)) {
      expect_true(check_prediction_results(i, car_set_1))
   }
   for (i in 1:nrow(car_set_2)) {
      expect_true(check_prediction_results(i, car_set_2))
   }

   for (i in 1:nrow(car_set_1)) {
      expect_true(check_prediction_results(i, car_set_1, summarize = FALSE))
   }
   for (i in 1:nrow(car_set_2)) {
      expect_true(check_prediction_results(i, car_set_2, summarize = FALSE))
   }

})


test_that("dropping tuning parameter columns", {

   expect_equal(
      names(collect_predictions(car_set_1)),
      c("wflow_id", ".config", "preproc", "model", ".row", "mpg", ".pred")
   )
   expect_equal(
      names(collect_predictions(car_set_2)),
      c("wflow_id", ".config", "preproc", "model", ".row", "mpg", ".pred")
   )

   expect_equal(
      names(collect_predictions(car_set_1, summarize = FALSE)),
      c("wflow_id", ".config", "preproc", "model", "id", ".pred", ".row", "mpg")
   )
   expect_equal(
      names(collect_predictions(car_set_2, summarize = FALSE)),
      c("wflow_id", ".config", "preproc", "model", "id", "id2", ".pred", ".row", "mpg")
   )

})


