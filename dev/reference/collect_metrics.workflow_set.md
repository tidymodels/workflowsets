# Obtain and format results produced by tuning functions for workflow sets

Return a tibble of performance metrics for all models or submodels.

## Usage

``` r
# S3 method for class 'workflow_set'
collect_metrics(x, ..., summarize = TRUE)

# S3 method for class 'workflow_set'
collect_predictions(
  x,
  ...,
  summarize = TRUE,
  parameters = NULL,
  select_best = FALSE,
  metric = NULL
)

# S3 method for class 'workflow_set'
collect_notes(x, ...)

# S3 method for class 'workflow_set'
collect_extracts(x, ...)
```

## Arguments

- x:

  A
  [`workflow_set`](https://workflowsets.tidymodels.org/dev/reference/workflow_set.md)
  object that has been evaluated with
  [`workflow_map()`](https://workflowsets.tidymodels.org/dev/reference/workflow_map.md).

- ...:

  Not currently used.

- summarize:

  A logical for whether the performance estimates should be summarized
  via the mean (over resamples) or the raw performance values (per
  resample) should be returned along with the resampling identifiers.
  When collecting predictions, these are averaged if multiple assessment
  sets contain the same row.

- parameters:

  An optional tibble of tuning parameter values that can be used to
  filter the predicted values before processing. This tibble should only
  have columns for each tuning parameter identifier (e.g. `"my_param"`
  if `tune("my_param")` was used).

- select_best:

  A single logical for whether the numerically best results are
  retained. If `TRUE`, the `parameters` argument is ignored.

- metric:

  A character string for the metric that is used for `select_best`.

## Value

A tibble.

## Details

When applied to a workflow set, the metrics and predictions that are
returned do not contain the actual tuning parameter columns and values
(unlike when these collect functions are run on other objects). The
reason is that workflow sets can contain different types of models or
models with different tuning parameters.

If the columns are needed, there are two options. First, the `.config`
column can be used to merge the tuning parameter columns into an
appropriate object. Alternatively, the
[`map()`](https://purrr.tidyverse.org/reference/map.html) function can
be used to get the metrics from the original objects (see the example
below).

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

## See also

[`tune::collect_metrics()`](https://tune.tidymodels.org/reference/collect_predictions.html),
[`rank_results()`](https://workflowsets.tidymodels.org/dev/reference/rank_results.md)

## Examples

``` r
library(dplyr)
#> 
#> Attaching package: ‘dplyr’
#> The following objects are masked from ‘package:stats’:
#> 
#>     filter, lag
#> The following objects are masked from ‘package:base’:
#> 
#>     intersect, setdiff, setequal, union
library(purrr)
library(tidyr)

two_class_res
#> # A workflow set/tibble: 6 × 4
#>   wflow_id      info             option    result   
#>   <chr>         <list>           <list>    <list>   
#> 1 none_cart     <tibble [1 × 4]> <opts[3]> <tune[+]>
#> 2 none_glm      <tibble [1 × 4]> <opts[3]> <rsmp[+]>
#> 3 none_mars     <tibble [1 × 4]> <opts[3]> <tune[+]>
#> 4 yj_trans_cart <tibble [1 × 4]> <opts[3]> <tune[+]>
#> 5 yj_trans_glm  <tibble [1 × 4]> <opts[3]> <rsmp[+]>
#> 6 yj_trans_mars <tibble [1 × 4]> <opts[3]> <tune[+]>

# ------------------------------------------------------------------------------
# \donttest{
collect_metrics(two_class_res)
#> # A tibble: 52 × 9
#>    wflow_id  .config       preproc model .metric .estimator  mean     n
#>    <chr>     <chr>         <chr>   <chr> <chr>   <chr>      <dbl> <int>
#>  1 none_cart Preprocessor… formula deci… accura… binary     0.777     5
#>  2 none_cart Preprocessor… formula deci… roc_auc binary     0.839     5
#>  3 none_cart Preprocessor… formula deci… accura… binary     0.784     5
#>  4 none_cart Preprocessor… formula deci… roc_auc binary     0.841     5
#>  5 none_cart Preprocessor… formula deci… accura… binary     0.791     5
#>  6 none_cart Preprocessor… formula deci… roc_auc binary     0.853     5
#>  7 none_cart Preprocessor… formula deci… accura… binary     0.814     5
#>  8 none_cart Preprocessor… formula deci… roc_auc binary     0.829     5
#>  9 none_cart Preprocessor… formula deci… accura… binary     0.788     5
#> 10 none_cart Preprocessor… formula deci… roc_auc binary     0.852     5
#> # ℹ 42 more rows
#> # ℹ 1 more variable: std_err <dbl>

# Alternatively, if the tuning parameter values are needed:
two_class_res |>
  dplyr::filter(grepl("cart", wflow_id)) |>
  mutate(metrics = map(result, collect_metrics)) |>
  dplyr::select(wflow_id, metrics) |>
  tidyr::unnest(cols = metrics)
#> # A tibble: 40 × 9
#>    wflow_id  cost_complexity min_n .metric  .estimator  mean     n
#>    <chr>               <dbl> <int> <chr>    <chr>      <dbl> <int>
#>  1 none_cart        2.28e- 7    13 accuracy binary     0.777     5
#>  2 none_cart        2.28e- 7    13 roc_auc  binary     0.839     5
#>  3 none_cart        1.32e- 5    16 accuracy binary     0.784     5
#>  4 none_cart        1.32e- 5    16 roc_auc  binary     0.841     5
#>  5 none_cart        8.19e-10    26 accuracy binary     0.791     5
#>  6 none_cart        8.19e-10    26 roc_auc  binary     0.853     5
#>  7 none_cart        9.52e- 3     9 accuracy binary     0.814     5
#>  8 none_cart        9.52e- 3     9 roc_auc  binary     0.829     5
#>  9 none_cart        4.76e- 7    20 accuracy binary     0.788     5
#> 10 none_cart        4.76e- 7    20 roc_auc  binary     0.852     5
#> # ℹ 30 more rows
#> # ℹ 2 more variables: std_err <dbl>, .config <chr>
# }

collect_metrics(two_class_res, summarize = FALSE)
#> # A tibble: 260 × 8
#>    wflow_id  .config   preproc model id    .metric .estimator .estimate
#>    <chr>     <chr>     <chr>   <chr> <chr> <chr>   <chr>          <dbl>
#>  1 none_cart Preproce… formula deci… Fold1 accura… binary         0.786
#>  2 none_cart Preproce… formula deci… Fold1 roc_auc binary         0.875
#>  3 none_cart Preproce… formula deci… Fold2 accura… binary         0.766
#>  4 none_cart Preproce… formula deci… Fold2 roc_auc binary         0.851
#>  5 none_cart Preproce… formula deci… Fold3 accura… binary         0.778
#>  6 none_cart Preproce… formula deci… Fold3 roc_auc binary         0.848
#>  7 none_cart Preproce… formula deci… Fold4 accura… binary         0.766
#>  8 none_cart Preproce… formula deci… Fold4 roc_auc binary         0.779
#>  9 none_cart Preproce… formula deci… Fold5 accura… binary         0.791
#> 10 none_cart Preproce… formula deci… Fold5 roc_auc binary         0.841
#> # ℹ 250 more rows
```
