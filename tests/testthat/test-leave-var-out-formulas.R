form_data <- data.frame(
  a = 1:10,
  b = seq(1, 7, length.out = 10),
  c = factor(rep(letters[1:2], 5)),
  y = (1:10) * 2
)


num_pred <- function(f) {
  length(all.vars(f[-2]))
}

num_terms <- function(f) {
  length(strsplit(deparse(f[-2]), "+", fixed = TRUE)[[1]])
}

# ------------------------------------------------------------------------------

test_that("LOO var formulas", {
  expect_snapshot(
    error = TRUE,
    leave_var_out_formulas(y ~ 1, data = form_data)
  )
  expect_snapshot(
    error = TRUE,
    leave_var_out_formulas(y ~ a, data = form_data)
  )

  f_1 <- leave_var_out_formulas(y ~ ., data = form_data)
  expect_true(length(f_1) == 4)
  expect_equal(names(f_1), c(letters[1:3], "everything"))
  expect_equal(
    purrr::map_int(f_1, num_pred),
    c(a = 2L, b = 2L, c = 2L, everything = 1L)
  )

  f_2 <- leave_var_out_formulas(y ~ (.)^2, data = form_data, FALSE)
  expect_true(length(f_2) == 6)
  expect_equal(names(f_2), c("a", "b", "c", "a:b", "a:c", "b:c"))
  expect_equal(unname(purrr::map_int(f_2, num_pred)), rep(2:3, each = 3))

  f_3 <- leave_var_out_formulas(y ~ . + I(a^3), data = form_data, FALSE)
  expect_true(length(f_3) == 4)
  expect_equal(names(f_3), c("a", "b", "c", "I(a^3)"))
  expect_equal(unname(purrr::map_int(f_3, num_pred)), c(2, 2, 2, 3))
  expect_equal(unname(purrr::map_int(f_3, num_terms)), c(2, 3, 3, 3))
})
