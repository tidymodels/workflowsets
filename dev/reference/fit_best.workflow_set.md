# Fit a model to the numerically optimal configuration

[`fit_best()`](https://tune.tidymodels.org/reference/fit_best.html)
takes results from tuning many models and fits the workflow
configuration associated with the best performance to the training set.

## Usage

``` r
# S3 method for class 'workflow_set'
fit_best(x, metric = NULL, eval_time = NULL, ...)
```

## Arguments

- x:

  A
  [`workflow_set`](https://workflowsets.tidymodels.org/dev/reference/workflow_set.md)
  object that has been evaluated with
  [`workflow_map()`](https://workflowsets.tidymodels.org/dev/reference/workflow_map.md).
  Note that the workflow set must have been fitted with the [control
  option](https://workflowsets.tidymodels.org/dev/reference/option_add.md)
  `save_workflow = TRUE`.

- metric:

  A character string giving the metric to rank results by.

- eval_time:

  A single numeric time point where dynamic event time metrics should be
  chosen (e.g., the time-dependent ROC curve, etc). The values should be
  consistent with the values used to create `x`. The `NULL` default will
  automatically use the first evaluation time used by `x`.

- ...:

  Additional options to pass to
  [tune::fit_best](https://tune.tidymodels.org/reference/fit_best.html).

## Details

This function is a shortcut for the steps needed to fit the numerically
optimal configuration in a fitted workflow set. The function ranks
results, extracts the tuning result pertaining to the best result, and
then again calls
[`fit_best()`](https://tune.tidymodels.org/reference/fit_best.html)
(itself a wrapper) on the tuning result containing the best result.

In pseudocode:

    rankings <- rank_results(wf_set, metric, select_best = TRUE)
    tune_res <- extract_workflow_set_result(wf_set, rankings$wflow_id[1])
    fit_best(tune_res, metric)

## Note

The package supplies two pre-generated workflow sets, `two_class_set`
and `chi_features_set`, and associated sets of model fits
`two_class_res` and `chi_features_res`.

The `two_class_*` objects are based on a binary classification problem
using the `two_class_dat` data from the modeldata package. The six
models utilize either a bare formula or a basic recipe utilizing
[`recipes::step_YeoJohnson()`](https://recipes.tidymodels.org/reference/step_YeoJohnson.html)
as a preprocessor, and a decision tree, logistic regression, or MARS
model specification. See
[`?two_class_set`](https://workflowsets.tidymodels.org/dev/reference/two_class_set.md)
for source code.

The `chi_features_*` objects are based on a regression problem using the
`Chicago` data from the modeldata package. Each of the three models
utilize a linear regression model specification, with three different
recipes of varying complexity. The objects are meant to approximate the
sequence of models built in Section 1.3 of Kuhn and Johnson (2019). See
[`?chi_features_set`](https://workflowsets.tidymodels.org/dev/reference/chi_features_set.md)
for source code.

## Examples

``` r
library(tune)
library(modeldata)
#> 
#> Attaching package: ‘modeldata’
#> The following object is masked from ‘package:datasets’:
#> 
#>     penguins
library(rsample)
library(recipes)
#> 
#> Attaching package: ‘recipes’
#> The following object is masked from ‘package:stats’:
#> 
#>     step

data(Chicago)
Chicago <- Chicago[1:1195, ]

time_val_split <-
  sliding_period(
    Chicago,
    date,
    "month",
    lookback = 38,
    assess_stop = 1
  )

chi_features_set
#> # A workflow set/tibble: 3 × 4
#>   wflow_id         info             option    result    
#>   <chr>            <list>           <list>    <list>    
#> 1 date_lm          <tibble [1 × 4]> <opts[0]> <list [0]>
#> 2 plus_holidays_lm <tibble [1 × 4]> <opts[0]> <list [0]>
#> 3 plus_pca_lm      <tibble [1 × 4]> <opts[0]> <list [0]>

chi_features_res_new <-
  chi_features_set |>
  # note: must set `save_workflow = TRUE` to use `fit_best()`
  option_add(control = control_grid(save_workflow = TRUE)) |>
  # evaluate with resamples
  workflow_map(resamples = time_val_split, grid = 21, seed = 1, verbose = TRUE)
#> i   No tuning parameters. `fit_resamples()` will be attempted
#> i 1 of 3 resampling: date_lm
#> → A | warning: prediction from rank-deficient fit; consider predict(., rankdeficient="NA")
#> There were issues with some computations   A: x1
#> There were issues with some computations   A: x1
#> 
#> ✔ 1 of 3 resampling: date_lm (270ms)
#> i   No tuning parameters. `fit_resamples()` will be attempted
#> i 2 of 3 resampling: plus_holidays_lm
#> → A | warning: prediction from rank-deficient fit; consider predict(., rankdeficient="NA")
#> ✔ 2 of 3 resampling: plus_holidays_lm (272ms)
#> i 3 of 3 tuning:     plus_pca_lm
#> → A | warning: prediction from rank-deficient fit; consider predict(., rankdeficient="NA")
#> ✔ 3 of 3 tuning:     plus_pca_lm (1.3s)

chi_features_res_new
#> # A workflow set/tibble: 3 × 4
#>   wflow_id         info             option    result   
#>   <chr>            <list>           <list>    <list>   
#> 1 date_lm          <tibble [1 × 4]> <opts[3]> <rsmp[+]>
#> 2 plus_holidays_lm <tibble [1 × 4]> <opts[3]> <rsmp[+]>
#> 3 plus_pca_lm      <tibble [1 × 4]> <opts[3]> <tune[+]>

# sort models by performance metrics
rank_results(chi_features_res_new)
#> # A tibble: 12 × 9
#>    wflow_id      .config .metric  mean std_err     n preprocessor model
#>    <chr>         <chr>   <chr>   <dbl>   <dbl> <int> <chr>        <chr>
#>  1 plus_pca_lm   pre3_m… rmse    0.586      NA     1 recipe       line…
#>  2 plus_pca_lm   pre3_m… rsq     0.989      NA     1 recipe       line…
#>  3 plus_pca_lm   pre2_m… rmse    0.590      NA     1 recipe       line…
#>  4 plus_pca_lm   pre2_m… rsq     0.988      NA     1 recipe       line…
#>  5 plus_pca_lm   pre1_m… rmse    0.591      NA     1 recipe       line…
#>  6 plus_pca_lm   pre1_m… rsq     0.988      NA     1 recipe       line…
#>  7 plus_pca_lm   pre4_m… rmse    0.594      NA     1 recipe       line…
#>  8 plus_pca_lm   pre4_m… rsq     0.989      NA     1 recipe       line…
#>  9 plus_holiday… pre0_m… rmse    0.646      NA     1 recipe       line…
#> 10 plus_holiday… pre0_m… rsq     0.986      NA     1 recipe       line…
#> 11 date_lm       pre0_m… rmse    0.733      NA     1 recipe       line…
#> 12 date_lm       pre0_m… rsq     0.982      NA     1 recipe       line…
#> # ℹ 1 more variable: rank <int>

# fit the numerically optimal configuration to the training set
chi_features_wf <- fit_best(chi_features_res_new)

chi_features_wf
#> ══ Workflow [trained] ═════════════════════════════════════════════════
#> Preprocessor: Recipe
#> Model: linear_reg()
#> 
#> ── Preprocessor ───────────────────────────────────────────────────────
#> 5 Recipe Steps
#> 
#> • step_date()
#> • step_holiday()
#> • step_dummy()
#> • step_zv()
#> • step_pca()
#> 
#> ── Model ──────────────────────────────────────────────────────────────
#> 
#> Call:
#> stats::lm(formula = ..y ~ ., data = data)
#> 
#> Coefficients:
#>       (Intercept)           temp_min               temp  
#>         5.067e+02         -4.811e-04          6.885e-02  
#>          temp_max        temp_change                dew  
#>         9.511e-04                 NA         -5.110e-02  
#>          humidity           pressure    pressure_change  
#>         2.516e-02          6.921e-01          2.230e-02  
#>              wind           wind_max               gust  
#>        -1.642e-02          1.409e-04          3.146e-03  
#>          gust_max             percip         percip_max  
#>         7.870e-03         -7.111e+00          2.199e-01  
#>      weather_rain       weather_snow      weather_cloud  
#>        -6.168e-01         -2.689e-01         -9.951e-02  
#>     weather_storm    Blackhawks_Away    Blackhawks_Home  
#>         2.603e-01         -1.245e-01         -1.114e-01  
#>        Bulls_Away         Bulls_Home         Bears_Away  
#>         9.407e-02          1.833e-01          3.306e-01  
#>        Bears_Home      WhiteSox_Away      WhiteSox_Home  
#>         3.531e-01         -5.198e-01                 NA  
#>         Cubs_Away          Cubs_Home          date_year  
#>                NA                 NA         -2.638e-01  
#>     date_LaborDay   date_NewYearsDay  date_ChristmasDay  
#>         5.166e-01         -1.275e+01         -1.308e+01  
#>      date_dow_Mon       date_dow_Tue       date_dow_Wed  
#>         1.232e+01          1.345e+01          1.348e+01  
#>      date_dow_Thu       date_dow_Fri       date_dow_Sat  
#>         1.325e+01          1.281e+01          9.855e-01  
#>    date_month_Feb     date_month_Mar     date_month_Apr  
#>         4.218e-02          3.897e-01          5.472e-01  
#>    date_month_May     date_month_Jun     date_month_Jul  
#>         2.842e-01          9.032e-01          3.897e-01  
#>    date_month_Aug     date_month_Sep     date_month_Oct  
#>         4.855e-01          1.588e-01          6.197e-01  
#>    date_month_Nov     date_month_Dec                PC1  
#>        -4.350e-01         -8.359e-01          2.979e-02  
#>               PC2                PC3  
#>         1.225e-01         -1.722e-01  
#> 

# to select optimal value based on a specific metric:
fit_best(chi_features_res_new, metric = "rmse")
#> ══ Workflow [trained] ═════════════════════════════════════════════════
#> Preprocessor: Recipe
#> Model: linear_reg()
#> 
#> ── Preprocessor ───────────────────────────────────────────────────────
#> 5 Recipe Steps
#> 
#> • step_date()
#> • step_holiday()
#> • step_dummy()
#> • step_zv()
#> • step_pca()
#> 
#> ── Model ──────────────────────────────────────────────────────────────
#> 
#> Call:
#> stats::lm(formula = ..y ~ ., data = data)
#> 
#> Coefficients:
#>       (Intercept)           temp_min               temp  
#>         5.067e+02         -4.811e-04          6.885e-02  
#>          temp_max        temp_change                dew  
#>         9.511e-04                 NA         -5.110e-02  
#>          humidity           pressure    pressure_change  
#>         2.516e-02          6.921e-01          2.230e-02  
#>              wind           wind_max               gust  
#>        -1.642e-02          1.409e-04          3.146e-03  
#>          gust_max             percip         percip_max  
#>         7.870e-03         -7.111e+00          2.199e-01  
#>      weather_rain       weather_snow      weather_cloud  
#>        -6.168e-01         -2.689e-01         -9.951e-02  
#>     weather_storm    Blackhawks_Away    Blackhawks_Home  
#>         2.603e-01         -1.245e-01         -1.114e-01  
#>        Bulls_Away         Bulls_Home         Bears_Away  
#>         9.407e-02          1.833e-01          3.306e-01  
#>        Bears_Home      WhiteSox_Away      WhiteSox_Home  
#>         3.531e-01         -5.198e-01                 NA  
#>         Cubs_Away          Cubs_Home          date_year  
#>                NA                 NA         -2.638e-01  
#>     date_LaborDay   date_NewYearsDay  date_ChristmasDay  
#>         5.166e-01         -1.275e+01         -1.308e+01  
#>      date_dow_Mon       date_dow_Tue       date_dow_Wed  
#>         1.232e+01          1.345e+01          1.348e+01  
#>      date_dow_Thu       date_dow_Fri       date_dow_Sat  
#>         1.325e+01          1.281e+01          9.855e-01  
#>    date_month_Feb     date_month_Mar     date_month_Apr  
#>         4.218e-02          3.897e-01          5.472e-01  
#>    date_month_May     date_month_Jun     date_month_Jul  
#>         2.842e-01          9.032e-01          3.897e-01  
#>    date_month_Aug     date_month_Sep     date_month_Oct  
#>         4.855e-01          1.588e-01          6.197e-01  
#>    date_month_Nov     date_month_Dec                PC1  
#>        -4.350e-01         -8.359e-01          2.979e-02  
#>               PC2                PC3  
#>         1.225e-01         -1.722e-01  
#> 
```
