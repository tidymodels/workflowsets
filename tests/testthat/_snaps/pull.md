# pulling objects

    Code
      res <- pull_workflow(car_set_1, "reg_lm")
    Condition
      Warning:
      `pull_workflow()` was deprecated in workflowsets 0.1.0.
      i Please use `extract_workflow()` instead.

---

    Code
      res <- pull_workflow_set_result(car_set_1, "reg_lm")
    Condition
      Warning:
      `pull_workflow_set_result()` was deprecated in workflowsets 0.1.0.
      i Please use `extract_workflow_set_result()` instead.

---

    Code
      pull_workflow_set_result(car_set_1, "Gideon Nav")
    Condition
      Warning:
      `pull_workflow_set_result()` was deprecated in workflowsets 0.1.0.
      i Please use `extract_workflow_set_result()` instead.
      Error in `pull_workflow_set_result()`:
      ! No workflow ID found for "Gideon Nav".

---

    Code
      pull_workflow(car_set_1, "Coronabeth Tridentarius")
    Condition
      Warning:
      `pull_workflow()` was deprecated in workflowsets 0.1.0.
      i Please use `extract_workflow()` instead.
      Error in `pull_workflow()`:
      ! No workflow ID found for "Coronabeth Tridentarius".

