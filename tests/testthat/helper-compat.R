workflow_set_objects <- list(
  unfit = chi_features_set,
  fit = chi_features_res
)

expect_s3_class_workflow_set <- function(x) {
  expect_s3_class(x, "workflow_set")
}

expect_s3_class_bare_tibble <- function(x) {
  expect_s3_class(x, c("tbl_df", "tbl", "data.frame"), exact = TRUE)
}
