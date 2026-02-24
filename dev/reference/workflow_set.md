# Generate a set of workflow objects from preprocessing and model objects

Often a data practitioner needs to consider a large number of possible
modeling approaches for a task at hand, especially for new data sets
and/or when there is little knowledge about what modeling strategy will
work best. Workflow sets provide an expressive interface for
investigating multiple models or feature engineering strategies in such
a situation.

## Usage

``` r
workflow_set(preproc, models, cross = TRUE, case_weights = NULL)
```

## Arguments

- preproc:

  A list (preferably named) with preprocessing objects: formulas,
  recipes, or
  [`workflows::workflow_variables()`](https://workflows.tidymodels.org/reference/add_variables.html).

- models:

  A list (preferably named) of `parsnip` model specifications.

- cross:

  A logical: should all combinations of the preprocessors and models be
  used to create the workflows? If `FALSE`, the length of `preproc` and
  `models` should be equal.

- case_weights:

  A single unquoted column name specifying the case weights for the
  models. This must be a classed case weights column, as determined by
  [`hardhat::is_case_weights()`](https://hardhat.tidymodels.org/reference/is_case_weights.html).
  See the "Case weights" section below for more information.

## Value

A tibble with extra class 'workflow_set'. A new set includes four
columns (but others can be added):

- `wflow_id` contains character strings for the preprocessor/workflow
  combination. These can be changed but must be unique.

- `info` is a list column with tibbles containing more specific
  information, including any comments added using
  [`comment_add()`](https://workflowsets.tidymodels.org/dev/reference/comment_add.md).
  This tibble also contains the workflow object (which can be easily
  retrieved using
  [`extract_workflow()`](https://hardhat.tidymodels.org/reference/hardhat-extract.html)).

- `option` is a list column that will include a list of optional
  arguments passed to the functions from the `tune` package. They can be
  added manually via
  [`option_add()`](https://workflowsets.tidymodels.org/dev/reference/option_add.md)
  or automatically when options are passed to
  [`workflow_map()`](https://workflowsets.tidymodels.org/dev/reference/workflow_map.md).

- `result` is a list column that will contain any objects produced when
  [`workflow_map()`](https://workflowsets.tidymodels.org/dev/reference/workflow_map.md)
  is used.

## Details

The preprocessors that can be combined with the model objects can be one
or more of:

- A traditional R formula.

- A recipe definition (un-prepared) via
  [`recipes::recipe()`](https://recipes.tidymodels.org/reference/recipe.html).

- A selectors object created by
  [`workflows::workflow_variables()`](https://workflows.tidymodels.org/reference/add_variables.html).

Since `preproc` is a named list column, any combination of these can be
used in that argument (i.e., `preproc` can be mixed types).

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

## Case weights

The `case_weights` argument can be passed as a single unquoted column
name identifying the data column giving model case weights. For each
workflow in the workflow set using an engine that supports case weights,
the case weights will be added with
[`workflows::add_case_weights()`](https://workflows.tidymodels.org/reference/add_case_weights.html).
`workflow_set()` will warn if any of the workflows specify an engine
that does not support case weights—and ignore the case weights argument
for those workflows—but will not fail.

Read more about case weights in the tidymodels at
[`?parsnip::case_weights`](https://parsnip.tidymodels.org/reference/case_weights.html).

## See also

[`workflow_map()`](https://workflowsets.tidymodels.org/dev/reference/workflow_map.md),
[`comment_add()`](https://workflowsets.tidymodels.org/dev/reference/comment_add.md),
[`option_add()`](https://workflowsets.tidymodels.org/dev/reference/option_add.md),
[`as_workflow_set()`](https://workflowsets.tidymodels.org/dev/reference/as_workflow_set.md)

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

# ------------------------------------------------------------------------------

data(cells)
cells <- cells |> dplyr::select(-case)

set.seed(1)
val_set <- validation_split(cells)
#> Warning: `validation_split()` was deprecated in rsample 1.2.0.
#> ℹ Please use `initial_validation_split()` instead.

# ------------------------------------------------------------------------------

basic_recipe <-
  recipe(class ~ ., data = cells) |>
  step_YeoJohnson(all_predictors()) |>
  step_normalize(all_predictors())

pca_recipe <-
  basic_recipe |>
  step_pca(all_predictors(), num_comp = tune())

ss_recipe <-
  basic_recipe |>
  step_spatialsign(all_predictors())

# ------------------------------------------------------------------------------

knn_mod <-
  nearest_neighbor(neighbors = tune(), weight_func = tune()) |>
  set_engine("kknn") |>
  set_mode("classification")

lr_mod <-
  logistic_reg() |>
  set_engine("glm")

# ------------------------------------------------------------------------------

preproc <- list(none = basic_recipe, pca = pca_recipe, sp_sign = ss_recipe)
models <- list(knn = knn_mod, logistic = lr_mod)

cell_set <- workflow_set(preproc, models, cross = TRUE)
cell_set
#> # A workflow set/tibble: 6 × 4
#>   wflow_id         info             option    result    
#>   <chr>            <list>           <list>    <list>    
#> 1 none_knn         <tibble [1 × 4]> <opts[0]> <list [0]>
#> 2 none_logistic    <tibble [1 × 4]> <opts[0]> <list [0]>
#> 3 pca_knn          <tibble [1 × 4]> <opts[0]> <list [0]>
#> 4 pca_logistic     <tibble [1 × 4]> <opts[0]> <list [0]>
#> 5 sp_sign_knn      <tibble [1 × 4]> <opts[0]> <list [0]>
#> 6 sp_sign_logistic <tibble [1 × 4]> <opts[0]> <list [0]>

# ------------------------------------------------------------------------------
# Using variables and formulas

# Select predictors by their names
channels <- paste0("ch_", 1:4)
preproc <- purrr::map(channels, \(.x) workflow_variables(class, c(contains(!!.x))))
names(preproc) <- channels
preproc$everything <- class ~ .
preproc
#> $ch_1
#> $outcomes
#> <quosure>
#> expr: ^class
#> env:  0x55a3147150c8
#> 
#> $predictors
#> <quosure>
#> expr: ^c(contains("ch_1"))
#> env:  0x55a3147150c8
#> 
#> attr(,"class")
#> [1] "workflow_variables"
#> 
#> $ch_2
#> $outcomes
#> <quosure>
#> expr: ^class
#> env:  0x55a314712360
#> 
#> $predictors
#> <quosure>
#> expr: ^c(contains("ch_2"))
#> env:  0x55a314712360
#> 
#> attr(,"class")
#> [1] "workflow_variables"
#> 
#> $ch_3
#> $outcomes
#> <quosure>
#> expr: ^class
#> env:  0x55a31470d4f0
#> 
#> $predictors
#> <quosure>
#> expr: ^c(contains("ch_3"))
#> env:  0x55a31470d4f0
#> 
#> attr(,"class")
#> [1] "workflow_variables"
#> 
#> $ch_4
#> $outcomes
#> <quosure>
#> expr: ^class
#> env:  0x55a3147096c0
#> 
#> $predictors
#> <quosure>
#> expr: ^c(contains("ch_4"))
#> env:  0x55a3147096c0
#> 
#> attr(,"class")
#> [1] "workflow_variables"
#> 
#> $everything
#> class ~ .
#> <environment: 0x55a319b70e50>
#> 

cell_set_by_group <- workflow_set(preproc, models["logistic"])
cell_set_by_group
#> # A workflow set/tibble: 5 × 4
#>   wflow_id            info             option    result    
#>   <chr>               <list>           <list>    <list>    
#> 1 ch_1_logistic       <tibble [1 × 4]> <opts[0]> <list [0]>
#> 2 ch_2_logistic       <tibble [1 × 4]> <opts[0]> <list [0]>
#> 3 ch_3_logistic       <tibble [1 × 4]> <opts[0]> <list [0]>
#> 4 ch_4_logistic       <tibble [1 × 4]> <opts[0]> <list [0]>
#> 5 everything_logistic <tibble [1 × 4]> <opts[0]> <list [0]>
```
