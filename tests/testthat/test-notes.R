
# ------------------------------------------------------------------------------

test_that("test comments", {
   comments_1 <-
      two_class_set %>%
      comment_add("none_cart", "What does 'cart' stand for\u2753")

   expect_equal(
      comment_get(comments_1, id = "none_cart"),
      "What does 'cart' stand for\u2753"
   )

   for(i in 2:nrow(comments_1)) {
      expect_equal(comments_1$info[[i]]$comment, character(1))
   }
   comments_2 <-
      comments_1 %>%
      comment_add("none_cart", "Stuff.")
   expect_equal(
      comment_get(comments_2, id = "none_cart") %>% paste0(collapse = "\n"),
      "What does 'cart' stand for\u2753\nStuff."
   )
   comments_3 <-
      comments_2 %>%
      comment_reset("none_cart")
   expect_equal(
      comments_3$info[[1]]$comment,
      character(1)
   )
   expect_equal(
      two_class_set %>% comment_add(),
      two_class_set
   )
   expect_equal(
      two_class_set %>% comment_add("none_cart"),
      two_class_set
   )
   expect_error(
      two_class_set %>% comment_add("toe", "foot"),
      "The 'id' value is not in wflow_id."
   )
   expect_error(
      two_class_set %>% comment_add(letters, "foot"),
      "'id' should be a single character value."
   )
   expect_error(
      two_class_set %>% comment_add(1:2, "foot"),
      "'id' should be a single character value."
   )
   expect_error(
      two_class_set %>% comment_add("none_cart", 1:2),
      "The comments should be character strings."
   )
   expect_error(
      comments_1 %>% comment_add("none_cart", "Stuff.", append = FALSE),
      "There is already a comment for this id and"
   )
   expect_error(
      comment_get(comments_1, id = letters),
      "should be a single character value"
   )
   expect_error(
      comment_get(comments_1, id = letters[1]),
      "value is not in wflow_id"
   )
   expect_error(
      comments_1 %>% comment_reset(letters),
      "should be a single character value"
   )
   expect_error(
      comments_1 %>% comment_reset("none_carts"),
      "value is not in wflow_id"
   )

})
