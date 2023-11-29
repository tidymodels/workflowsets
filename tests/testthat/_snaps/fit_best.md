# fit_best errors informatively with bad inputs

    Code
      fit_best(chi_features_res)
    Condition
      Error in `fit_best()`:
      x The control option `save_workflow = TRUE` should be used when tuning.

---

    Code
      fit_best(chi_features_map, metric = "boop")
    Condition
      Error in `halt()`:
      ! Metric 'boop' was not in the results.

---

    Code
      fit_best(chi_features_map, boop = "bop")
    Condition
      Error in `fit_best()`:
      x The `...` are not used by this function.

