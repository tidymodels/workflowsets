# Rank the results by a metric

This function sorts the results by a specific performance metric.

## Usage

``` r
rank_results(x, rank_metric = NULL, eval_time = NULL, select_best = FALSE)
```

## Arguments

- x:

  A
  [`workflow_set`](https://workflowsets.tidymodels.org/dev/reference/workflow_set.md)
  object that has been evaluated with
  [`workflow_map()`](https://workflowsets.tidymodels.org/dev/reference/workflow_map.md).

- rank_metric:

  A character string for a metric.

- eval_time:

  A single numeric time point where dynamic event time metrics should be
  chosen (e.g., the time-dependent ROC curve, etc). The values should be
  consistent with the values used to create `x`. The `NULL` default will
  automatically use the first evaluation time used by `x`.

- select_best:

  A logical giving whether the results should only contain the
  numerically best submodel per workflow.

## Value

A tibble with columns: `wflow_id`, `.config`, `.metric`, `mean`,
`std_err`, `n`, `preprocessor`, `model`, and `rank`.

## Details

If some models have the exact same performance,
`rank(value, ties.method = "random")` is used (with a reproducible seed)
so that all ranks are integers.

No columns are returned for the tuning parameters since they are likely
to be different (or not exist) for some models. The `wflow_id` and
`.config` columns can be used to determine the corresponding parameter
values.

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
chi_features_res
#> # A workflow set/tibble: 3 × 4
#>   wflow_id         info             option    result   
#>   <chr>            <list>           <list>    <list>   
#> 1 date_lm          <tibble [1 × 4]> <opts[2]> <rsmp[+]>
#> 2 plus_holidays_lm <tibble [1 × 4]> <opts[2]> <rsmp[+]>
#> 3 plus_pca_lm      <tibble [1 × 4]> <opts[3]> <tune[+]>

rank_results(chi_features_res)
#> # A tibble: 40 × 9
#>    wflow_id    .config   .metric  mean std_err     n preprocessor model
#>    <chr>       <chr>     <chr>   <dbl>   <dbl> <int> <chr>        <chr>
#>  1 plus_pca_lm Preproce… rmse    0.574      NA     1 recipe       line…
#>  2 plus_pca_lm Preproce… rsq     0.989      NA     1 recipe       line…
#>  3 plus_pca_lm Preproce… rmse    0.586      NA     1 recipe       line…
#>  4 plus_pca_lm Preproce… rsq     0.989      NA     1 recipe       line…
#>  5 plus_pca_lm Preproce… rmse    0.590      NA     1 recipe       line…
#>  6 plus_pca_lm Preproce… rsq     0.988      NA     1 recipe       line…
#>  7 plus_pca_lm Preproce… rmse    0.591      NA     1 recipe       line…
#>  8 plus_pca_lm Preproce… rsq     0.988      NA     1 recipe       line…
#>  9 plus_pca_lm Preproce… rmse    0.594      NA     1 recipe       line…
#> 10 plus_pca_lm Preproce… rsq     0.989      NA     1 recipe       line…
#> # ℹ 30 more rows
#> # ℹ 1 more variable: rank <int>
rank_results(chi_features_res, select_best = TRUE)
#> # A tibble: 6 × 9
#>   wflow_id .config .metric  mean std_err     n preprocessor model  rank
#>   <chr>    <chr>   <chr>   <dbl>   <dbl> <int> <chr>        <chr> <int>
#> 1 plus_pc… Prepro… rmse    0.574      NA     1 recipe       line…     1
#> 2 plus_pc… Prepro… rsq     0.989      NA     1 recipe       line…     1
#> 3 plus_ho… Prepro… rmse    0.646      NA     1 recipe       line…     2
#> 4 plus_ho… Prepro… rsq     0.986      NA     1 recipe       line…     2
#> 5 date_lm  Prepro… rmse    0.733      NA     1 recipe       line…     3
#> 6 date_lm  Prepro… rsq     0.982      NA     1 recipe       line…     3
rank_results(chi_features_res, rank_metric = "rsq")
#> # A tibble: 40 × 9
#>    wflow_id    .config   .metric  mean std_err     n preprocessor model
#>    <chr>       <chr>     <chr>   <dbl>   <dbl> <int> <chr>        <chr>
#>  1 plus_pca_lm Preproce… rmse    0.594      NA     1 recipe       line…
#>  2 plus_pca_lm Preproce… rsq     0.989      NA     1 recipe       line…
#>  3 plus_pca_lm Preproce… rmse    0.574      NA     1 recipe       line…
#>  4 plus_pca_lm Preproce… rsq     0.989      NA     1 recipe       line…
#>  5 plus_pca_lm Preproce… rmse    0.586      NA     1 recipe       line…
#>  6 plus_pca_lm Preproce… rsq     0.989      NA     1 recipe       line…
#>  7 plus_pca_lm Preproce… rmse    0.591      NA     1 recipe       line…
#>  8 plus_pca_lm Preproce… rsq     0.988      NA     1 recipe       line…
#>  9 plus_pca_lm Preproce… rmse    0.590      NA     1 recipe       line…
#> 10 plus_pca_lm Preproce… rsq     0.988      NA     1 recipe       line…
#> # ℹ 30 more rows
#> # ℹ 1 more variable: rank <int>
```
