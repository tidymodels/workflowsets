# pulling objects

    Code
      res <- car_set_1 %>% pull_workflow("reg_lm")
    Condition
      Warning:
      `pull_workflow()` was deprecated in workflowsets 0.1.0.
      i Please use `extract_workflow()` instead.

---

    Code
      res <- car_set_1 %>% pull_workflow_set_result("reg_lm")
    Condition
      Warning:
      `pull_workflow_set_result()` was deprecated in workflowsets 0.1.0.
      i Please use `extract_workflow_set_result()` instead.

---

    Code
      car_set_1 %>% pull_workflow_set_result("Gideon Nav")
    Condition
      Warning:
      `pull_workflow_set_result()` was deprecated in workflowsets 0.1.0.
      i Please use `extract_workflow_set_result()` instead.
      Error in `halt()`:
      ! No workflow ID found for 'Gideon Nav'

---

    Code
      car_set_1 %>% pull_workflow("Coronabeth Tridentarius")
    Condition
      Warning:
      `pull_workflow()` was deprecated in workflowsets 0.1.0.
      i Please use `extract_workflow()` instead.
      Error in `halt()`:
      ! No workflow ID found for 'Coronabeth Tridentarius'

