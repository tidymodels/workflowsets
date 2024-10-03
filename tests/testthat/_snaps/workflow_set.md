# specifying a column that is not case weights

    Code
      car_set_2 <- workflow_set(list(reg = mpg ~ ., nonlin = mpg ~ wt + 1 / sqrt(disp)),
      list(lm = lr_spec), case_weights = non_wts) %>% workflow_map("fit_resamples",
        resamples = vfold_cv(cars, v = 5))
    Message
      x Fold1: preprocessor 1/1:
        Error in `fit()`:
        ! `col` must select a classed case weights column, as determined by `h...
      x Fold2: preprocessor 1/1:
        Error in `fit()`:
        ! `col` must select a classed case weights column, as determined by `h...
      x Fold3: preprocessor 1/1:
        Error in `fit()`:
        ! `col` must select a classed case weights column, as determined by `h...
      x Fold4: preprocessor 1/1:
        Error in `fit()`:
        ! `col` must select a classed case weights column, as determined by `h...
      x Fold5: preprocessor 1/1:
        Error in `fit()`:
        ! `col` must select a classed case weights column, as determined by `h...
    Condition
      Warning:
      All models failed. Run `show_notes(.Last.tune.result)` for more information.
    Message
      x Fold1: preprocessor 1/1:
        Error in `fit()`:
        ! `col` must select a classed case weights column, as determined by `h...
      x Fold2: preprocessor 1/1:
        Error in `fit()`:
        ! `col` must select a classed case weights column, as determined by `h...
      x Fold3: preprocessor 1/1:
        Error in `fit()`:
        ! `col` must select a classed case weights column, as determined by `h...
      x Fold4: preprocessor 1/1:
        Error in `fit()`:
        ! `col` must select a classed case weights column, as determined by `h...
      x Fold5: preprocessor 1/1:
        Error in `fit()`:
        ! `col` must select a classed case weights column, as determined by `h...
    Condition
      Warning:
      All models failed. Run `show_notes(.Last.tune.result)` for more information.

---

    Code
      class_note$note[1]
    Output
      [1] "Error in `fit()`:\n! `col` must select a classed case weights column, as determined by `hardhat::is_case_weights()`. For example, it could be a column created by `hardhat::frequency_weights()` or `hardhat::importance_weights()`."

# specifying an engine that does not allow case weights

    Code
      car_set_3 <- workflow_set(list(reg = mpg ~ ., nonlin = mpg ~ wt + 1 / sqrt(disp)),
      list(lm = lr_spec, knn = knn_spec), case_weights = wts)
    Condition
      Warning:
      Case weights are not enabled by the underlying model implementation for the following engine(s): kknn.
      
      The `case_weights` argument will be ignored for specifications using that engine.

# specifying a case weight column that isn't in the resamples

    Code
      car_set_4 <- workflow_set(list(reg = mpg ~ ., nonlin = mpg ~ wt + 1 / sqrt(disp)),
      list(lm = lr_spec), case_weights = boop) %>% workflow_map("fit_resamples",
        resamples = vfold_cv(cars, v = 5))
    Message
      x Fold1: preprocessor 1/1:
        Error in `fit()`:
        ! Can't select columns that don't exist.
        x Column `boop` doesn't exist.
      x Fold2: preprocessor 1/1:
        Error in `fit()`:
        ! Can't select columns that don't exist.
        x Column `boop` doesn't exist.
      x Fold3: preprocessor 1/1:
        Error in `fit()`:
        ! Can't select columns that don't exist.
        x Column `boop` doesn't exist.
      x Fold4: preprocessor 1/1:
        Error in `fit()`:
        ! Can't select columns that don't exist.
        x Column `boop` doesn't exist.
      x Fold5: preprocessor 1/1:
        Error in `fit()`:
        ! Can't select columns that don't exist.
        x Column `boop` doesn't exist.
    Condition
      Warning:
      All models failed. Run `show_notes(.Last.tune.result)` for more information.
    Message
      x Fold1: preprocessor 1/1:
        Error in `fit()`:
        ! Can't select columns that don't exist.
        x Column `boop` doesn't exist.
      x Fold2: preprocessor 1/1:
        Error in `fit()`:
        ! Can't select columns that don't exist.
        x Column `boop` doesn't exist.
      x Fold3: preprocessor 1/1:
        Error in `fit()`:
        ! Can't select columns that don't exist.
        x Column `boop` doesn't exist.
      x Fold4: preprocessor 1/1:
        Error in `fit()`:
        ! Can't select columns that don't exist.
        x Column `boop` doesn't exist.
      x Fold5: preprocessor 1/1:
        Error in `fit()`:
        ! Can't select columns that don't exist.
        x Column `boop` doesn't exist.
    Condition
      Warning:
      All models failed. Run `show_notes(.Last.tune.result)` for more information.

---

    Code
      class_note$note[1]
    Output
      [1] "Error in `fit()`:\n! Can't select columns that don't exist.\nx Column `boop` doesn't exist."

# crossing

    Code
      nrow(workflow_set(list(reg = mpg ~ ., nonlin = mpg ~ wt + 1 / sqrt(disp), two = mpg ~
        wt + disp), list(lm = lr_spec, knn = knn_spec), cross = FALSE))
    Condition
      Error in `workflow_set()`:
      ! The lengths of 'preproc' and 'models' are different and `cross = FALSE`.

# checking resamples

    Code
      as_workflow_set(wt = f_1, disp = f_2)
    Condition
      Error in `halt()`:
      ! Different resamples were used in the workflow 'result's. All elements of 'result' must use the same resamples.

# constructor

    Code
      workflowsets:::new_workflow_set(car_set_1 %>% dplyr::select(-info))
    Condition
      Error in `halt()`:
      ! The object should have columns: 'wflow_id', 'info', 'option', 'result'.

---

    Code
      workflowsets:::new_workflow_set(car_set_1 %>% dplyr::mutate(info = "a"))
    Condition
      Error in `halt()`:
      ! The 'info' column should be a list.

---

    Code
      workflowsets:::new_workflow_set(car_set_1 %>% dplyr::mutate(result = "a"))
    Condition
      Error in `halt()`:
      ! The 'result' column should be a list.

---

    Code
      workflowsets:::new_workflow_set(car_set_1 %>% dplyr::mutate(option = "a"))
    Condition
      Error in `halt()`:
      ! The 'option' column should be a list.

---

    Code
      workflowsets:::new_workflow_set(car_set_1 %>% dplyr::mutate(wflow_id = 1))
    Condition
      Error in `halt()`:
      ! The 'wflow_id' column should be character.

---

    Code
      workflowsets:::new_workflow_set(car_set_1 %>% dplyr::mutate(wflow_id = "a"))
    Condition
      Error in `halt()`:
      ! The 'wflow_id' column should contain unique, non-missing character strings.

# pillar formatting

    # A workflow set/tibble: 3 x 4
      wflow_id         info             option    result    
      <chr>            <list>           <list>    <list>    
    1 date_lm          <tibble [1 x 4]> <opts[0]> <list [0]>
    2 plus_holidays_lm <tibble [1 x 4]> <opts[0]> <list [0]>
    3 plus_pca_lm      <tibble [1 x 4]> <opts[0]> <list [0]>

---

    # A workflow set/tibble: 3 x 4
      wflow_id         info             option    result   
      <chr>            <list>           <list>    <list>   
    1 date_lm          <tibble [1 x 4]> <opts[2]> <rsmp[+]>
    2 plus_holidays_lm <tibble [1 x 4]> <opts[2]> <rsmp[+]>
    3 plus_pca_lm      <tibble [1 x 4]> <opts[3]> <tune[+]>

