# predict() errors informatively with workflow sets

    Code
      predict(car_set_1)
    Condition
      Error in `predict()`:
      ! `predict()` is not well-defined for workflow sets.
      i To predict with the optimal model configuration from a workflow set, ensure that the workflow set was fitted with the control option (`?workflowsets::option_add()`) `save_workflow = TRUE` (`?tune::control_grid()`), run `fit_best()` (`?tune::fit_best()`), and then predict using `predict()` (`?workflows::predict.workflow()`) on its output.
      i To collect predictions from a workflow set, ensure that the workflow set was fitted with the control option (`?workflowsets::option_add()`) `save_pred = TRUE` (`?tune::control_grid()`) and run `collect_predictions()` (`?tune::collect_predictions()`).

---

    Code
      predict(car_set_2)
    Condition
      Error in `predict()`:
      ! `predict()` is not well-defined for workflow sets.
      i To predict with the optimal model configuration from a workflow set, ensure that the workflow set was fitted with the control option (`?workflowsets::option_add()`) `save_workflow = TRUE` (`?tune::control_grid()`), run `fit_best()` (`?tune::fit_best()`), and then predict using `predict()` (`?workflows::predict.workflow()`) on its output.
      i To collect predictions from a workflow set, ensure that the workflow set was fitted with the control option (`?workflowsets::option_add()`) `save_pred = TRUE` (`?tune::control_grid()`) and run `collect_predictions()` (`?tune::collect_predictions()`).

