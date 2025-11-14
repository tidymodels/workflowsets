# Two Class Example Data

The package supplies two pre-generated workflow sets, `two_class_set`
and `chi_features_set`, and associated sets of model fits
`two_class_res` and `chi_features_res`.

The `two_class_*` objects are based on a binary classification problem
using the `two_class_dat` data from the modeldata package. The six
models utilize either a bare formula or a basic recipe utilizing
[`recipes::step_YeoJohnson()`](https://recipes.tidymodels.org/reference/step_YeoJohnson.html)
as a preprocessor, and a decision tree, logistic regression, or MARS
model specification. See `?two_class_set` for source code.

The `chi_features_*` objects are based on a regression problem using the
`Chicago` data from the modeldata package. Each of the three models
utilize a linear regression model specification, with three different
recipes of varying complexity. The objects are meant to approximate the
sequence of models built in Section 1.3 of Kuhn and Johnson (2019). See
[`?chi_features_set`](https://workflowsets.tidymodels.org/dev/reference/chi_features_set.md)
for source code.

## Details

See below for the source code to generate the Two Class example workflow
sets:

    library(workflowsets)
    library(workflows)
    library(modeldata)
    library(recipes)
    library(parsnip)
    library(dplyr)
    library(rsample)
    library(tune)
    library(yardstick)

    # ------------------------------------------------------------------------------

    data(two_class_dat, package = "modeldata")

    set.seed(1)
    folds <- vfold_cv(two_class_dat, v = 5)

    # ------------------------------------------------------------------------------

    decision_tree_rpart_spec <-
      decision_tree(min_n = tune(), cost_complexity = tune()) |>
      set_engine('rpart') |>
      set_mode('classification')

    logistic_reg_glm_spec <-
      logistic_reg() |>
      set_engine('glm')

    mars_earth_spec <-
      mars(prod_degree = tune()) |>
      set_engine('earth') |>
      set_mode('classification')

    # ------------------------------------------------------------------------------

    yj_recipe <-
       recipe(Class ~ ., data = two_class_dat) |>
       step_YeoJohnson(A, B)

    # ------------------------------------------------------------------------------

    two_class_set <-
       workflow_set(
          preproc = list(none = Class ~ A + B, yj_trans = yj_recipe),
          models = list(cart = decision_tree_rpart_spec, glm = logistic_reg_glm_spec,
                        mars = mars_earth_spec)
       )

    # ------------------------------------------------------------------------------

    two_class_res <-
      two_class_set |>
      workflow_map(
        resamples = folds,
        grid = 10,
        seed = 2,
        verbose = TRUE,
        control = control_grid(save_workflow = TRUE)
      )

## Examples

``` r
data(two_class_set)

two_class_set
#> # A workflow set/tibble: 6 × 4
#>   wflow_id      info             option    result    
#>   <chr>         <list>           <list>    <list>    
#> 1 none_cart     <tibble [1 × 4]> <opts[0]> <list [0]>
#> 2 none_glm      <tibble [1 × 4]> <opts[0]> <list [0]>
#> 3 none_mars     <tibble [1 × 4]> <opts[0]> <list [0]>
#> 4 yj_trans_cart <tibble [1 × 4]> <opts[0]> <list [0]>
#> 5 yj_trans_glm  <tibble [1 × 4]> <opts[0]> <list [0]>
#> 6 yj_trans_mars <tibble [1 × 4]> <opts[0]> <list [0]>
```
