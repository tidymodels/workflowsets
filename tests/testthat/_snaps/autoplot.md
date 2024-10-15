# autoplot with bad type input

    Code
      autoplot(two_class_res, metric = "roc_auc", type = "banana")
    Condition
      Error in `autoplot()`:
      ! `type` must be one of "class" or "wflow_id", not "banana".

# automatic selection of rank metric

    Code
      pick_metric(two_class_res, "roc_auc", "accuracy")
    Condition
      Error:
      ! Metric "roc_auc" was not in the results.

