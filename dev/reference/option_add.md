# Add and edit options saved in a workflow set

The `option` column controls options for the functions that are used to
*evaluate* the workflow set, such as
[`tune::fit_resamples()`](https://tune.tidymodels.org/reference/fit_resamples.html)
or
[`tune::tune_grid()`](https://tune.tidymodels.org/reference/tune_grid.html).
Examples of common options to set for these functions include
`param_info` and `grid`.

These functions are helpful for manipulating the information in the
`option` column.

## Usage

``` r
option_add(x, ..., id = NULL, strict = FALSE)

option_remove(x, ...)

option_add_parameters(x, id = NULL, strict = FALSE)
```

## Arguments

- x:

  A workflow set outputted by
  [`workflow_set()`](https://workflowsets.tidymodels.org/dev/reference/workflow_set.md)
  or
  [`workflow_map()`](https://workflowsets.tidymodels.org/dev/reference/workflow_map.md).

- ...:

  Arguments to pass to the `tune_*()` functions (e.g.
  [`tune::tune_grid()`](https://tune.tidymodels.org/reference/tune_grid.html))
  or
  [`tune::fit_resamples()`](https://tune.tidymodels.org/reference/fit_resamples.html).
  For `option_remove()` this can be a series of unquoted option names.

- id:

  A character string of one or more values from the `wflow_id` column
  that indicates which options to update. By default, all workflows are
  updated.

- strict:

  A logical; should execution stop if existing options are being
  replaced?

## Value

An updated workflow set.

## Details

`option_add()` is used to update all of the options in a workflow set.

`option_remove()` will eliminate specific options across rows.

`option_add_parameters()` adds a parameter object to the `option` column
(if parameters are being tuned).

Note that executing a function on the workflow set, such as
[`tune_grid()`](https://tune.tidymodels.org/reference/tune_grid.html),
will add any options given to that function to the `option` column.

These functions do *not* control options for the individual workflows,
such as the recipe blueprint. When creating a workflow manually, use
[`workflows::add_model()`](https://workflows.tidymodels.org/reference/add_model.html)
or
[`workflows::add_recipe()`](https://workflows.tidymodels.org/reference/add_recipe.html)
to specify extra options. To alter these in a workflow set, use
[`update_workflow_model()`](https://workflowsets.tidymodels.org/dev/reference/update_workflow_model.md)
or
[`update_workflow_recipe()`](https://workflowsets.tidymodels.org/dev/reference/update_workflow_model.md).

## Examples

``` r
library(tune)

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

two_class_set |>
  option_add(grid = 10)
#> # A workflow set/tibble: 6 × 4
#>   wflow_id      info             option    result    
#>   <chr>         <list>           <list>    <list>    
#> 1 none_cart     <tibble [1 × 4]> <opts[1]> <list [0]>
#> 2 none_glm      <tibble [1 × 4]> <opts[1]> <list [0]>
#> 3 none_mars     <tibble [1 × 4]> <opts[1]> <list [0]>
#> 4 yj_trans_cart <tibble [1 × 4]> <opts[1]> <list [0]>
#> 5 yj_trans_glm  <tibble [1 × 4]> <opts[1]> <list [0]>
#> 6 yj_trans_mars <tibble [1 × 4]> <opts[1]> <list [0]>

two_class_set |>
  option_add(grid = 10) |>
  option_add(grid = 50, id = "none_cart")
#> # A workflow set/tibble: 6 × 4
#>   wflow_id      info             option    result    
#>   <chr>         <list>           <list>    <list>    
#> 1 none_cart     <tibble [1 × 4]> <opts[1]> <list [0]>
#> 2 none_glm      <tibble [1 × 4]> <opts[1]> <list [0]>
#> 3 none_mars     <tibble [1 × 4]> <opts[1]> <list [0]>
#> 4 yj_trans_cart <tibble [1 × 4]> <opts[1]> <list [0]>
#> 5 yj_trans_glm  <tibble [1 × 4]> <opts[1]> <list [0]>
#> 6 yj_trans_mars <tibble [1 × 4]> <opts[1]> <list [0]>

two_class_set |>
  option_add_parameters()
#> # A workflow set/tibble: 6 × 4
#>   wflow_id      info             option    result    
#>   <chr>         <list>           <list>    <list>    
#> 1 none_cart     <tibble [1 × 4]> <opts[1]> <list [0]>
#> 2 none_glm      <tibble [1 × 4]> <opts[0]> <list [0]>
#> 3 none_mars     <tibble [1 × 4]> <opts[1]> <list [0]>
#> 4 yj_trans_cart <tibble [1 × 4]> <opts[1]> <list [0]>
#> 5 yj_trans_glm  <tibble [1 × 4]> <opts[0]> <list [0]>
#> 6 yj_trans_mars <tibble [1 × 4]> <opts[1]> <list [0]>
```
