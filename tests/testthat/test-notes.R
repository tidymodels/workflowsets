
# ------------------------------------------------------------------------------

test_that("test notes", {
   notes_1 <-
      two_class_set %>%
      note_add("none_cart", "What does 'cart' stand for\u2753")

   expect_equal(
      notes_1$info[[1]]$note,
      "What does 'cart' stand for\u2753"
   )

   for(i in 2:nrow(notes_1)) {
      expect_equal(notes_1$info[[i]]$note, character(1))
   }
   notes_2 <-
      notes_1 %>%
      note_add("none_cart", "Stuff.")
   expect_equal(
      notes_2$info[[1]]$note,
      "What does 'cart' stand for\u2753\nStuff."
   )
   notes_3 <-
      notes_2 %>%
      note_reset("none_cart")
   expect_equal(
      notes_3$info[[1]]$note,
      character(1)
   )
   expect_equal(
      two_class_set %>% note_add(),
      two_class_set
   )
   expect_equal(
      two_class_set %>% note_add("none_cart"),
      two_class_set
   )
   expect_error(
      two_class_set %>% note_add("toe", "foot"),
      "The 'id' value is not in wflow_id."
   )
   expect_error(
      two_class_set %>% note_add(letters, "foot"),
      "'id' should be a single character value."
   )
   expect_error(
      two_class_set %>% note_add(1:2, "foot"),
      "'id' should be a single character value."
   )
   expect_error(
      two_class_set %>% note_add("none_cart", 1:2),
      "The notes should be character strings."
   )
   expect_error(
      notes_1 %>% note_add("none_cart", "Stuff.", append = FALSE),
      "There is already a note for this id and"
   )
})
