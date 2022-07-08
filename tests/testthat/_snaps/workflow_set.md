# specifying a column that is not case weights

    Code
      car_set_2 <- workflow_set(list(reg = mpg ~ ., nonlin = mpg ~ wt + 1 / sqrt(disp)),
      list(lm = lr_spec), case_weights = non_wts) %>% workflow_map("fit_resamples",
        resamples = vfold_cv(cars, v = 5))
    Message <simpleMessage>
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
    Warning <rlang_warning>
      All models failed. Run `show_notes(.Last.tune.result)` for more information.
    Message <simpleMessage>
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
    Warning <rlang_warning>
      All models failed. Run `show_notes(.Last.tune.result)` for more information.

---

    Code
      class_note$note[1]
    Output
      [1] "Error in `fit()`:\n! `col` must select a classed case weights column, as determined by `hardhat::is_case_weights()`. For example, it could be a column created by `hardhat::frequency_weights()` or `hardhat::importance_weights()`."

# specifying a case weight column that isn't in the resamples

    Code
      car_set_4 <- workflow_set(list(reg = mpg ~ ., nonlin = mpg ~ wt + 1 / sqrt(disp)),
      list(lm = lr_spec), case_weights = boop) %>% workflow_map("fit_resamples",
        resamples = vfold_cv(cars, v = 5))
    Message <simpleMessage>
      x Fold1: preprocessor 1/1:
        Error in `chr_as_locations()`:
        ! Can't subset columns that don't exist.
        x Column `boop` doesn't exist.
      x Fold2: preprocessor 1/1:
        Error in `chr_as_locations()`:
        ! Can't subset columns that don't exist.
        x Column `boop` doesn't exist.
      x Fold3: preprocessor 1/1:
        Error in `chr_as_locations()`:
        ! Can't subset columns that don't exist.
        x Column `boop` doesn't exist.
      x Fold4: preprocessor 1/1:
        Error in `chr_as_locations()`:
        ! Can't subset columns that don't exist.
        x Column `boop` doesn't exist.
      x Fold5: preprocessor 1/1:
        Error in `chr_as_locations()`:
        ! Can't subset columns that don't exist.
        x Column `boop` doesn't exist.
    Warning <rlang_warning>
      All models failed. Run `show_notes(.Last.tune.result)` for more information.
    Message <simpleMessage>
      x Fold1: preprocessor 1/1:
        Error in `chr_as_locations()`:
        ! Can't subset columns that don't exist.
        x Column `boop` doesn't exist.
      x Fold2: preprocessor 1/1:
        Error in `chr_as_locations()`:
        ! Can't subset columns that don't exist.
        x Column `boop` doesn't exist.
      x Fold3: preprocessor 1/1:
        Error in `chr_as_locations()`:
        ! Can't subset columns that don't exist.
        x Column `boop` doesn't exist.
      x Fold4: preprocessor 1/1:
        Error in `chr_as_locations()`:
        ! Can't subset columns that don't exist.
        x Column `boop` doesn't exist.
      x Fold5: preprocessor 1/1:
        Error in `chr_as_locations()`:
        ! Can't subset columns that don't exist.
        x Column `boop` doesn't exist.
    Warning <rlang_warning>
      All models failed. Run `show_notes(.Last.tune.result)` for more information.

---

    Code
      class_note$note[1]
    Output
      [1] "Error in `chr_as_locations()`:\n! Can't subset columns that don't exist.\nx Column `boop` doesn't exist."

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

