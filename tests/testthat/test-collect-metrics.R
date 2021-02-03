
check_metric_results <- function(ind, x, ...) {
   id_val <- x$wflow_id[ind]

   if (any(names(list(...)) == "summarize")) {
      cols <- c(".metric", ".estimator", ".estimate", ".config", "id")
   } else {

      cols <- c(".metric", ".estimator", "mean", "n", "std_err", ".config")
   }

   orig <-
      collect_metrics(x$result[[ind]], ...) %>%
      dplyr::select(dplyr::all_of(cols))

   everythng <-
      collect_metrics(x, ...) %>%
      dplyr::filter(wflow_id == id_val) %>%
      dplyr::select(dplyr::all_of(cols))
   dplyr::all_equal(orig, everythng)
}

# ------------------------------------------------------------------------------

test_that("collect summarized metrics", {

   for (i in 1:nrow(two_class_res)) {
      expect_true(check_metric_results(i, two_class_res))
   }
   for (i in 1:nrow(cell_models)) {
      expect_true(check_metric_results(i, cell_models))
   }

   for (i in 1:nrow(two_class_res)) {
      expect_true(check_metric_results(i, two_class_res, summarize = FALSE))
   }
   for (i in 1:nrow(cell_models)) {
      expect_true(check_metric_results(i, cell_models, summarize = FALSE))
   }

})


