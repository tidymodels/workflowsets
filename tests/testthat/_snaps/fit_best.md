# fit_best errors informatively with bad inputs

    Code
      fit_best(chi_features_res)
    Error <rlang_error>
      x The control option `save_workflow = TRUE` should be used when tuning.

---

    Code
      fit_best(chi_features_map, metric = "boop")
    Error <rlang_error>
      Metric 'boop' was not in the results.

---

    Code
      fit_best(chi_features_map, boop = "bop")
    Error <rlang_error>
      x The `...` are not used by this function.

