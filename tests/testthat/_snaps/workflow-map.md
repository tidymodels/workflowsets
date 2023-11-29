# basic mapping

    Code
      two_class_set %>% workflow_map("foo", seed = 1, resamples = folds, grid = 2)
    Condition
      Error in `workflow_map()`:
      ! `fn` must be one of "tune_grid", "tune_bayes", "fit_resamples", "tune_race_anova", "tune_race_win_loss", "tune_sim_anneal", or "tune_cluster", not "foo".

---

    Code
      two_class_set %>% workflow_map(fn = 1L, seed = 1, resamples = folds, grid = 2)
    Condition
      Error in `workflow_map()`:
      ! `fn` must be a character vector, not the number 1.

---

    Code
      two_class_set %>% workflow_map(fn = tune::tune_grid, seed = 1, resamples = folds,
      grid = 2)
    Condition
      Error in `workflow_map()`:
      ! `fn` must be a character vector, not a function.

# map logging

    Code
      cat(logging_res, sep = "\n")
    Output
      i	No tuning parameters. `fit_resamples()` will be attempted
      i 1 of 3 resampling: reg_lm
      i 2 of 3 tuning:     reg_knn
      i	No tuning parameters. `fit_resamples()` will be attempted
      i 3 of 3 resampling: nonlin_lm

# fail informatively on mismatched spec/tuning function

    Code
      workflow_map(wf_set_1, resamples = folds)
    Condition
      Error in `workflow_map()`:
      ! To tune with `tune_grid()`, each workflow's model specification must inherit from <model_spec>, but `reg_km` does not.
      i The workflow `reg_km` is a cluster specification. Did you intend to set `fn = 'tune_cluster'`?

---

    Code
      workflow_map(wf_set_2, resamples = folds)
    Condition
      Error in `workflow_map()`:
      ! To tune with `tune_grid()`, each workflow's model specification must inherit from <model_spec>, but `reg_km` and `reg_hc` do not.
      i The workflows `reg_km` and `reg_hc` are cluster specifications. Did you intend to set `fn = 'tune_cluster'`?

---

    Code
      workflow_map(wf_set_1, resamples = folds, fn = "tune_cluster")
    Condition
      Error in `workflow_map()`:
      ! To tune with `tune_cluster()`, each workflow's model specification must inherit from <cluster_spec>, but `reg_dt` does not.

---

    Code
      workflow_map(wf_set_3, resamples = folds, fn = "tune_cluster")
    Condition
      Error in `workflow_map()`:
      ! To tune with `tune_cluster()`, each workflow's model specification must inherit from <cluster_spec>, but `reg_dt` and `reg_nn` do not.

