# Convert existing objects to a workflow set

Use existing objects to create a workflow set. A list of objects that
are either simple workflows or objects that have class `"tune_results"`
can be converted into a workflow set.

## Usage

``` r
as_workflow_set(...)
```

## Arguments

- ...:

  One or more named objects. Names should be unique and the objects
  should have at least one of the following classes: `workflow`,
  `iteration_results`, `tune_results`, `resample_results`, or
  `tune_race`. Each `tune_results` element should also contain the
  original workflow (accomplished using the `save_workflow` option in
  the control function).

## Value

A workflow set. Note that the `option` column will not reflect the
options that were used to create each object.

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
# ------------------------------------------------------------------------------
# Existing results

# Use the already worked example to show how to add tuned
# objects to a workflow set
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

results <- two_class_res |> purrr::pluck("result")
names(results) <- two_class_res$wflow_id

# These are all objects that have been resampled or tuned:
purrr::map_chr(results, \(x) class(x)[1])
#>          none_cart           none_glm          none_mars 
#>     "tune_results" "resample_results"     "tune_results" 
#>      yj_trans_cart       yj_trans_glm      yj_trans_mars 
#>     "tune_results" "resample_results"     "tune_results" 

# Use rlang's !!! operator to splice in the elements of the list
new_set <- as_workflow_set(!!!results)

# ------------------------------------------------------------------------------
# Make a set from unfit workflows

library(parsnip)
library(workflows)

lr_spec <- logistic_reg()

main_effects <-
  workflow() |>
  add_model(lr_spec) |>
  add_formula(Class ~ .)

interactions <-
  workflow() |>
  add_model(lr_spec) |>
  add_formula(Class ~ (.)^2)

as_workflow_set(main = main_effects, int = interactions)
#> # A workflow set/tibble: 2 × 4
#>   wflow_id info             option    result
#>   <chr>    <list>           <list>    <list>
#> 1 main     <tibble [1 × 4]> <opts[0]> <NULL>
#> 2 int      <tibble [1 × 4]> <opts[0]> <NULL>
```
