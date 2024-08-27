# collect_extracts fails gracefully without .extracts column

    Code
      collect_extracts(wflow_set_trained)
    Condition
      Error in `dplyr::mutate()`:
      i In argument: `extracts = list(collect_extracts(result))`.
      i In row 1.
      Caused by error in `collect_extracts()`:
      ! The `.extracts` column does not exist.
      i Please supply a control object (`?tune::control_grid()`) with a non-`NULL` `extract` argument during resample fitting.

