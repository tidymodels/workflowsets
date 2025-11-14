# Add annotations and comments for workflows

`comment_add()` can be used to log important information about the
workflow or its results as you work. Comments can be appended or
removed.

## Usage

``` r
comment_add(x, id, ..., append = TRUE, collapse = "\n")

comment_get(x, id)

comment_reset(x, id)

comment_print(x, id = NULL, ...)
```

## Arguments

- x:

  A workflow set outputted by
  [`workflow_set()`](https://workflowsets.tidymodels.org/dev/reference/workflow_set.md)
  or
  [`workflow_map()`](https://workflowsets.tidymodels.org/dev/reference/workflow_map.md).

- id:

  A single character string for a value in the `wflow_id` column. For
  `comment_print()`, `id` can be a vector or `NULL` (and this indicates
  that all comments are printed).

- ...:

  One or more character strings.

- append:

  A logical value to determine if the new comment should be added to the
  existing values.

- collapse:

  A character string that separates the comments.

## Value

`comment_add()` and `comment_reset()` return an updated workflow set.
`comment_get()` returns a character string. `comment_print()` returns
`NULL` invisibly.

## Examples

``` r
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

two_class_set |> comment_get("none_cart")
#> [1] ""

new_set <-
  two_class_set |>
  comment_add("none_cart", "What does 'cart' stand for\u2753") |>
  comment_add("none_cart", "Classification And Regression Trees.")

comment_print(new_set)
#> ── none_cart ────────────────────────────────────────────────────────── 
#> 
#> What does 'cart' stand for❓
#> 
#> Classification And Regression Trees. 
#> 

new_set |> comment_get("none_cart")
#> [1] "What does 'cart' stand for❓\nClassification And Regression Trees."

new_set |>
  comment_reset("none_cart") |>
  comment_get("none_cart")
#> [1] ""
```
