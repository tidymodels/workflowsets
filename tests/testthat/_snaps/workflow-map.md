# basic mapping

    Code
      two_class_set %>% workflow_map("foo", seed = 1, resamples = folds, grid = 2)
    Error <rlang_error>
      `fn` must be one of "tune_grid", "tune_bayes", "fit_resamples", "tune_race_anova", "tune_race_win_loss", or "tune_sim_anneal", not "foo".

---

    Code
      two_class_set %>% workflow_map(fn = 1L, seed = 1, resamples = folds, grid = 2)
    Error <rlang_error>
      `fn` must be a character vector, not the number 1.

---

    Code
      two_class_set %>% workflow_map(fn = tune::tune_grid, seed = 1, resamples = folds,
      grid = 2)
    Error <rlang_error>
      `fn` must be a character vector, not a function.

# map logging

    Code
      cat(logging_res, sep = "\n")
    Output
      i	No tuning parameters. `fit_resamples()` will be attempted
      i 1 of 3 resampling: reg_lm
      i 2 of 3 tuning:     reg_knn
      i	No tuning parameters. `fit_resamples()` will be attempted
      i 3 of 3 resampling: nonlin_lm

