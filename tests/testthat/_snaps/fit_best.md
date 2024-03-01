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
      Error in `fit_best()`:
      ! "boop" was not in the metric set. Please choose from: "rmse" and "rsq".

---

    Code
      fit_best(chi_features_map, boop = "bop")
    Condition
      Error in `fit_best()`:
      ! `...` must be empty.
      x Problematic argument:
      * boop = "bop"

