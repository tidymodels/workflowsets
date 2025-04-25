# extracts

    Code
      extract_fit_engine(car_set_1, id = "reg_lm")
    Condition
      Error in `extract_fit_parsnip()`:
      ! Can't extract a model fit from an untrained workflow.
      i Do you need to call `fit()`?

---

    Code
      extract_fit_parsnip(car_set_1, id = "reg_lm")
    Condition
      Error in `extract_fit_parsnip()`:
      ! Can't extract a model fit from an untrained workflow.
      i Do you need to call `fit()`?

---

    Code
      extract_mold(car_set_1, id = "reg_lm")
    Condition
      Error in `extract_mold()`:
      ! Can't extract a mold from an untrained workflow.
      i Do you need to call `fit()`?

---

    Code
      extract_recipe(car_set_1, id = "reg_lm")
    Condition
      Error in `extract_mold()`:
      ! Can't extract a mold from an untrained workflow.
      i Do you need to call `fit()`?

---

    Code
      extract_workflow_set_result(car_set_1, "Gideon Nav")
    Condition
      Error in `extract_workflow_set_result()`:
      ! `id` must correspond to a single row in `x`.

---

    Code
      extract_workflow(car_set_1, "Coronabeth Tridentarius")
    Condition
      Error in `extract_workflow()`:
      ! `id` must correspond to a single row in `x`.

# extract single parameter from workflow set with untunable workflow

    Code
      hardhat::extract_parameter_dials(wf_set, id = "reg_lm", parameter = "non there")
    Condition
      Error in `extract_parameter_dials()`:
      ! No parameter exists with id "non there".

