# Process a series of workflows

`workflow_map()` will execute the same function across the workflows in
the set. The various `tune_*()` functions can be used as well as
[`tune::fit_resamples()`](https://tune.tidymodels.org/reference/fit_resamples.html).

## Usage

``` r
workflow_map(
  object,
  fn = "tune_grid",
  verbose = FALSE,
  seed = sample.int(10^4, 1),
  ...
)
```

## Arguments

- object:

  A workflow set.

- fn:

  The name of the function to run, as a character. Acceptable values
  are:
  ["tune_grid"](https://tune.tidymodels.org/reference/tune_grid.html),
  ["tune_bayes"](https://tune.tidymodels.org/reference/tune_bayes.html),
  ["fit_resamples"](https://tune.tidymodels.org/reference/fit_resamples.html),
  ["tune_race_anova"](https://finetune.tidymodels.org/reference/tune_race_anova.html),
  ["tune_race_win_loss"](https://finetune.tidymodels.org/reference/tune_race_win_loss.html),
  or
  ["tune_sim_anneal"](https://finetune.tidymodels.org/reference/tune_sim_anneal.html).
  Note that users need not provide the namespace or parentheses in this
  argument, e.g. provide `"tune_grid"` rather than `"tune::tune_grid"`
  or `"tune_grid()"`.

- verbose:

  A logical for logging progress.

- seed:

  A single integer that is set prior to each function execution.

- ...:

  Options to pass to the modeling function. See details below.

## Value

An updated workflow set. The `option` column will be updated with any
options for the `tune` package functions given to `workflow_map()`.
Also, the results will be added to the `result` column. If the
computations for a workflow fail, a `try-catch` object will be saved in
place of the results (without stopping execution).

## Details

When passing options, anything passed in the `...` will be combined with
any values in the `option` column. The values in `...` will override
that column's values and the new options are added to the `options`
column.

Any failures in execution result in the corresponding row of `results`
to contain a `try-error` object.

In cases where a model has no tuning parameters is mapped to one of the
tuning functions,
[`tune::fit_resamples()`](https://tune.tidymodels.org/reference/fit_resamples.html)
will be used instead and a warning is issued if `verbose = TRUE`.

If a workflow requires packages that are not installed, a message is
printed and `workflow_map()` continues with the next workflow (if any).

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

[`workflow_set()`](https://workflowsets.tidymodels.org/dev/reference/workflow_set.md),
[`as_workflow_set()`](https://workflowsets.tidymodels.org/dev/reference/as_workflow_set.md),
[`extract_workflow_set_result()`](https://workflowsets.tidymodels.org/dev/reference/extract_workflow_set_result.md)

## Examples

``` r
library(workflowsets)
library(workflows)
library(modeldata)
library(recipes)
library(parsnip)
library(dplyr)
library(rsample)
library(tune)
library(yardstick)
library(dials)
#> Loading required package: scales
#> 
#> Attaching package: ‘scales’
#> The following object is masked from ‘package:purrr’:
#> 
#>     discard

# An example of processed results
chi_features_res
#> # A workflow set/tibble: 3 × 4
#>   wflow_id         info             option    result   
#>   <chr>            <list>           <list>    <list>   
#> 1 date_lm          <tibble [1 × 4]> <opts[2]> <rsmp[+]>
#> 2 plus_holidays_lm <tibble [1 × 4]> <opts[2]> <rsmp[+]>
#> 3 plus_pca_lm      <tibble [1 × 4]> <opts[3]> <tune[+]>

# Recreating them:

# ---------------------------------------------------------------------------
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

# ---------------------------------------------------------------------------

base_recipe <-
  recipe(ridership ~ ., data = Chicago) |>
  # create date features
  step_date(date) |>
  step_holiday(date) |>
  # remove date from the list of predictors
  update_role(date, new_role = "id") |>
  # create dummy variables from factor columns
  step_dummy(all_nominal()) |>
  # remove any columns with a single unique value
  step_zv(all_predictors()) |>
  step_normalize(all_predictors())

date_only <-
  recipe(ridership ~ ., data = Chicago) |>
  # create date features
  step_date(date) |>
  update_role(date, new_role = "id") |>
  # create dummy variables from factor columns
  step_dummy(all_nominal()) |>
  # remove any columns with a single unique value
  step_zv(all_predictors())

date_and_holidays <-
  recipe(ridership ~ ., data = Chicago) |>
  # create date features
  step_date(date) |>
  step_holiday(date) |>
  # remove date from the list of predictors
  update_role(date, new_role = "id") |>
  # create dummy variables from factor columns
  step_dummy(all_nominal()) |>
  # remove any columns with a single unique value
  step_zv(all_predictors())

date_and_holidays_and_pca <-
  recipe(ridership ~ ., data = Chicago) |>
  # create date features
  step_date(date) |>
  step_holiday(date) |>
  # remove date from the list of predictors
  update_role(date, new_role = "id") |>
  # create dummy variables from factor columns
  step_dummy(all_nominal()) |>
  # remove any columns with a single unique value
  step_zv(all_predictors()) |>
  step_pca(!!stations, num_comp = tune())

# ---------------------------------------------------------------------------

lm_spec <- linear_reg() |> set_engine("lm")

# ---------------------------------------------------------------------------

pca_param <-
  parameters(num_comp()) |>
  update(num_comp = num_comp(c(0, 20)))

# ---------------------------------------------------------------------------

chi_features_set <-
  workflow_set(
    preproc = list(
      date = date_only,
      plus_holidays = date_and_holidays,
      plus_pca = date_and_holidays_and_pca
    ),
    models = list(lm = lm_spec),
    cross = TRUE
  )

# ---------------------------------------------------------------------------

chi_features_res_new <-
  chi_features_set |>
  option_add(param_info = pca_param, id = "plus_pca_lm") |>
  workflow_map(resamples = time_val_split, grid = 21, seed = 1, verbose = TRUE)
#> i   No tuning parameters. `fit_resamples()` will be attempted
#> i 1 of 3 resampling: date_lm
#> → A | warning: prediction from rank-deficient fit; consider predict(., rankdeficient="NA")
#> There were issues with some computations   A: x1
#> There were issues with some computations   A: x1
#> 
#> ✔ 1 of 3 resampling: date_lm (206ms)
#> i   No tuning parameters. `fit_resamples()` will be attempted
#> i 2 of 3 resampling: plus_holidays_lm
#> → A | warning: prediction from rank-deficient fit; consider predict(., rankdeficient="NA")
#> ✔ 2 of 3 resampling: plus_holidays_lm (237ms)
#> i 3 of 3 tuning:     plus_pca_lm
#> → A | warning: prediction from rank-deficient fit; consider predict(., rankdeficient="NA")
#> There were issues with some computations   A: x6
#> There were issues with some computations   A: x18
#> There were issues with some computations   A: x18
#> 
#> ✔ 3 of 3 tuning:     plus_pca_lm (5s)

chi_features_res_new
#> # A workflow set/tibble: 3 × 4
#>   wflow_id         info             option    result   
#>   <chr>            <list>           <list>    <list>   
#> 1 date_lm          <tibble [1 × 4]> <opts[2]> <rsmp[+]>
#> 2 plus_holidays_lm <tibble [1 × 4]> <opts[2]> <rsmp[+]>
#> 3 plus_pca_lm      <tibble [1 × 4]> <opts[3]> <tune[+]>
```
