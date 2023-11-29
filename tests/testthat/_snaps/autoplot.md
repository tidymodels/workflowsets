# autoplot with bad type input

    Code
      autoplot(two_class_res, metric = "roc_auc", type = "banana")
    Condition
      Error in `autoplot()`:
      ! `type` must be one of "class" or "wflow_id", not "banana".

