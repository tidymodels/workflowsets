# ------------------------------------------------------------------------------

test_that("test comments", {
  comments_1 <-
    two_class_set |>
    comment_add("none_cart", "What does 'cart' stand for\u2753")

  expect_equal(
    comment_get(comments_1, id = "none_cart"),
    "What does 'cart' stand for\u2753"
  )

  for (i in 2:nrow(comments_1)) {
    expect_equal(comments_1$info[[i]]$comment, character(1))
  }
  comments_2 <-
    comments_1 |>
    comment_add("none_cart", "Stuff.")
  expect_equal(
    comment_get(comments_2, id = "none_cart") |> paste0(collapse = "\n"),
    "What does 'cart' stand for\u2753\nStuff."
  )
  comments_3 <-
    comments_2 |>
    comment_reset("none_cart")
  expect_equal(
    comments_3$info[[1]]$comment,
    character(1)
  )
  expect_equal(
    two_class_set |> comment_add(),
    two_class_set
  )
  expect_equal(
    two_class_set |> comment_add("none_cart"),
    two_class_set
  )
  expect_snapshot(
    error = TRUE,
    two_class_set |> comment_add("toe", "foot")
  )
  expect_snapshot(
    error = TRUE,
    two_class_set |> comment_add(letters, "foot")
  )
  expect_snapshot(
    error = TRUE,
    two_class_set |> comment_add(1:2, "foot")
  )
  expect_snapshot(
    error = TRUE,
    two_class_set |> comment_add("none_cart", 1:2)
  )
  expect_snapshot(
    error = TRUE,
    comments_1 |> comment_add("none_cart", "Stuff.", append = FALSE)
  )
  expect_snapshot(
    error = TRUE,
    comment_get(comments_1, id = letters)
  )
  expect_snapshot(
    error = TRUE,
    comment_get(comments_1, id = letters[1])
  )
  expect_snapshot(
    error = TRUE,
    comments_1 |> comment_reset(letters)
  )
  expect_snapshot(
    error = TRUE,
    comments_1 |> comment_reset("none_carts")
  )
})

test_that("print comments", {
  gatsby_1 <- "\"Whenever you feel like criticizing any one,\" he told me, \"just remember that all the people in this world haven't had the advantages that you've had.\""
  gatsby_2 <- "My family have been prominent, well-to-do people in this middle-western city for three generations. The Carraways are something of a clan and we have a tradition that we're descended from the Dukes of Buccleuch, but the actual founder of my line was my grandfather's brother who came here in fifty-one, sent a substitute to the Civil War and started the wholesale hardware business that my father carries on today."
  gatsby_3 <- "Across the courtesy bay the white palaces of fashionable East Egg glittered along the water, and the history of the summer really begins on the evening I drove over there to have dinner with the Tom Buchanans. Daisy was my second cousin once removed and I'd known Tom in college. And just after the war I spent two days with them in Chicago."

  test <-
    two_class_res |>
    comment_add("none_cart", gatsby_1) |>
    comment_add("none_cart", gatsby_2) |>
    comment_add("none_glm", gatsby_3)

  expect_snapshot(comment_print(test))
  expect_snapshot(comment_print(test, id = "none_glm"))
})
