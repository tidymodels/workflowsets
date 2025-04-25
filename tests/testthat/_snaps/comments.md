# test comments

    Code
      comment_add(two_class_set, "toe", "foot")
    Condition
      Error in `comment_add()`:
      ! The `id` value is not in `wflow_id`.

---

    Code
      comment_add(two_class_set, letters, "foot")
    Condition
      Error in `comment_add()`:
      ! `id` must be a single string, not a character vector.

---

    Code
      comment_add(two_class_set, 1:2, "foot")
    Condition
      Error in `comment_add()`:
      ! `id` must be a single string, not an integer vector.

---

    Code
      comment_add(two_class_set, "none_cart", 1:2)
    Condition
      Error in `comment_add()`:
      ! The comments should be character strings.

---

    Code
      comment_add(comments_1, "none_cart", "Stuff.", append = FALSE)
    Condition
      Error in `comment_add()`:
      ! There is already a comment for this id and `append = FALSE`.

---

    Code
      comment_get(comments_1, id = letters)
    Condition
      Error in `comment_get()`:
      ! `id` should be a single character value.

---

    Code
      comment_get(comments_1, id = letters[1])
    Condition
      Error in `comment_get()`:
      ! The `id` value is not in `wflow_id`.

---

    Code
      comment_reset(comments_1, letters)
    Condition
      Error in `comment_reset()`:
      ! `id` should be a single character value.

---

    Code
      comment_reset(comments_1, "none_carts")
    Condition
      Error in `comment_reset()`:
      ! The `id` value is not in `wflow_id`.

# print comments

    Code
      comment_print(test)
    Output
      -- none_cart ------------------------------------------------------------------- 
      
      "Whenever you feel like criticizing any one," he told me, "just
      remember that all the people in this world haven't had the advantages
      that you've had."
      
      My family have been prominent, well-to-do people in this middle-western
      city for three generations. The Carraways are something of a clan and
      we have a tradition that we're descended from the Dukes of Buccleuch,
      but the actual founder of my line was my grandfather's brother who came
      here in fifty-one, sent a substitute to the Civil War and started the
      wholesale hardware business that my father carries on today. 
      
      -- none_glm -------------------------------------------------------------------- 
      
      Across the courtesy bay the white palaces of fashionable East Egg
      glittered along the water, and the history of the summer really begins
      on the evening I drove over there to have dinner with the Tom
      Buchanans. Daisy was my second cousin once removed and I'd known Tom in
      college. And just after the war I spent two days with them in Chicago. 
      

---

    Code
      comment_print(test, id = "none_glm")
    Output
      -- none_glm -------------------------------------------------------------------- 
      
      Across the courtesy bay the white palaces of fashionable East Egg
      glittered along the water, and the history of the summer really begins
      on the evening I drove over there to have dinner with the Tom
      Buchanans. Daisy was my second cousin once removed and I'd known Tom in
      college. And just after the war I spent two days with them in Chicago. 
      

