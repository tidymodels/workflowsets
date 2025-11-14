# Chicago Features Example Data

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
`?chi_features_set` for source code.

## Details

See below for the source code to generate the Chicago Features example
workflow sets:

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

    # ------------------------------------------------------------------------------
    # Slightly smaller data size
    data(Chicago)
    Chicago <- Chicago[1:1195,]

    time_val_split <-
       sliding_period(
          Chicago,
          date,
          "month",
          lookback = 38,
          assess_stop = 1
       )

    # ------------------------------------------------------------------------------

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

    # ------------------------------------------------------------------------------

    lm_spec <- linear_reg() |> set_engine("lm")

    # ------------------------------------------------------------------------------

    pca_param <-
       parameters(num_comp()) |>
       update(num_comp = num_comp(c(0, 20)))

    # ------------------------------------------------------------------------------

    chi_features_set <-
       workflow_set(
          preproc = list(date = date_only,
                         plus_holidays = date_and_holidays,
                         plus_pca = date_and_holidays_and_pca),
          models = list(lm = lm_spec),
          cross = TRUE
       )

    # ------------------------------------------------------------------------------

    chi_features_res <-
       chi_features_set |>
       option_add(param_info = pca_param, id = "plus_pca_lm") |>
       workflow_map(resamples = time_val_split, grid = 21, seed = 1, verbose = TRUE)

## References

Max Kuhn and Kjell Johnson (2019) *Feature Engineering and Selection*,
<https://bookdown.org/max/FES/a-more-complex-example.html>

## Examples

``` r
data(chi_features_set)

chi_features_set
#> # A workflow set/tibble: 3 × 4
#>   wflow_id         info             option    result    
#>   <chr>            <list>           <list>    <list>    
#> 1 date_lm          <tibble [1 × 4]> <opts[0]> <list [0]>
#> 2 plus_holidays_lm <tibble [1 × 4]> <opts[0]> <list [0]>
#> 3 plus_pca_lm      <tibble [1 × 4]> <opts[0]> <list [0]>
```
