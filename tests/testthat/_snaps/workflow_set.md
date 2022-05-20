# workflow_set can handle case weights

    Code
      car_set_2 <- workflow_set(list(reg = mpg ~ ., nonlin = mpg ~ wt + 1 / sqrt(disp)),
      list(lm = lr_spec), case_weights = non_wts) %>% workflow_map("fit_resamples",
        resamples = vfold_cv(cars, v = 5))
    Error <rlang_error>
      The supplied `case_weights` argument 'non_wts' is not a case weights column. See `?workflow_set` for more information.
    Message <simpleMessage>
      Execution stopped; returning current results

---

    Code
      car_set_4 <- workflow_set(list(reg = mpg ~ ., nonlin = mpg ~ wt + 1 / sqrt(disp)),
      list(lm = lr_spec), case_weights = boop) %>% workflow_map("fit_resamples",
        resamples = vfold_cv(cars, v = 5))
    Error <rlang_error>
      The supplied `case_weights` argument 'boop' is not a column in the data passed via `resamples`. See `workflow_set` for more information.
    Message <simpleMessage>
      Execution stopped; returning current results

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

