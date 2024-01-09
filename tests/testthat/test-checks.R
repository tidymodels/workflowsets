test_that("wf set check works", {
  expect_true(check_wf_set(chi_features_set))
  expect_true(check_wf_set(chi_features_res))

  expect_snapshot(error = TRUE, check_wf_set("no"))
  expect_snapshot(error = TRUE, check_wf_set(data.frame()))
  expect_snapshot(error = TRUE, rank_results("beeEeEEp!"))
})
