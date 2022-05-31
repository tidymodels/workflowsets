# specifying a column that is not case weights

    Code
      car_set_2 <- workflow_set(list(reg = mpg ~ ., nonlin = mpg ~ wt + 1 / sqrt(disp)),
      list(lm = lr_spec), case_weights = non_wts) %>% workflow_map("fit_resamples",
        resamples = vfold_cv(cars, v = 5))
    Message <simpleMessage>
      x Fold1: preprocessor 1/1: Error in `fit()`:
      ! `col` must select a classed case w...
      x Fold2: preprocessor 1/1: Error in `fit()`:
      ! `col` must select a classed case w...
      x Fold3: preprocessor 1/1: Error in `fit()`:
      ! `col` must select a classed case w...
      x Fold4: preprocessor 1/1: Error in `fit()`:
      ! `col` must select a classed case w...
      x Fold5: preprocessor 1/1: Error in `fit()`:
      ! `col` must select a classed case w...
    Warning <rlang_warning>
      All models failed. See the `.notes` column.
    Message <simpleMessage>
      x Fold1: preprocessor 1/1: Error in `fit()`:
      ! `col` must select a classed case w...
      x Fold2: preprocessor 1/1: Error in `fit()`:
      ! `col` must select a classed case w...
      x Fold3: preprocessor 1/1: Error in `fit()`:
      ! `col` must select a classed case w...
      x Fold4: preprocessor 1/1: Error in `fit()`:
      ! `col` must select a classed case w...
      x Fold5: preprocessor 1/1: Error in `fit()`:
      ! `col` must select a classed case w...
    Warning <rlang_warning>
      All models failed. See the `.notes` column.

# specifying a case weight column that isn't in the resamples

    Code
      car_set_4 <- workflow_set(list(reg = mpg ~ ., nonlin = mpg ~ wt + 1 / sqrt(disp)),
      list(lm = lr_spec), case_weights = boop) %>% workflow_map("fit_resamples",
        resamples = vfold_cv(cars, v = 5))
    Message <simpleMessage>
      x Fold1: preprocessor 1/1: Error in `chr_as_locations()`:
      ! Can't subset columns ...
      x Fold2: preprocessor 1/1: Error in `chr_as_locations()`:
      ! Can't subset columns ...
      x Fold3: preprocessor 1/1: Error in `chr_as_locations()`:
      ! Can't subset columns ...
      x Fold4: preprocessor 1/1: Error in `chr_as_locations()`:
      ! Can't subset columns ...
      x Fold5: preprocessor 1/1: Error in `chr_as_locations()`:
      ! Can't subset columns ...
    Warning <rlang_warning>
      All models failed. See the `.notes` column.
    Message <simpleMessage>
      x Fold1: preprocessor 1/1: Error in `chr_as_locations()`:
      ! Can't subset columns ...
      x Fold2: preprocessor 1/1: Error in `chr_as_locations()`:
      ! Can't subset columns ...
      x Fold3: preprocessor 1/1: Error in `chr_as_locations()`:
      ! Can't subset columns ...
      x Fold4: preprocessor 1/1: Error in `chr_as_locations()`:
      ! Can't subset columns ...
      x Fold5: preprocessor 1/1: Error in `chr_as_locations()`:
      ! Can't subset columns ...
    Warning <rlang_warning>
      All models failed. See the `.notes` column.

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

