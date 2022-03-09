# extracts

    Code
      extract_fit_engine(car_set_1, id = "reg_lm")
    Error <rlang_error>
      Can't extract a model fit from an untrained workflow.
      i Do you need to call `fit()`?

---

    Code
      extract_fit_parsnip(car_set_1, id = "reg_lm")
    Error <rlang_error>
      Can't extract a model fit from an untrained workflow.
      i Do you need to call `fit()`?

---

    Code
      extract_mold(car_set_1, id = "reg_lm")
    Error <rlang_error>
      Can't extract a mold from an untrained workflow.
      i Do you need to call `fit()`?

---

    Code
      extract_recipe(car_set_1, id = "reg_lm")
    Error <rlang_error>
      Can't extract a mold from an untrained workflow.
      i Do you need to call `fit()`?

---

    Code
      car_set_1 %>% extract_workflow_set_result("Gideon Nav")
    Error <rlang_error>
      `id` must correspond to a single row in `x`.

---

    Code
      car_set_1 %>% extract_workflow("Coronabeth Tridentarius")
    Error <rlang_error>
      `id` must correspond to a single row in `x`.

