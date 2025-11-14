# Extract elements from a workflow set

**\[deprecated\]**

## Usage

``` r
pull_workflow_set_result(x, id)

pull_workflow(x, id)
```

## Arguments

- x:

  A workflow set outputted by
  [`workflow_set()`](https://workflowsets.tidymodels.org/dev/reference/workflow_set.md)
  or
  [`workflow_map()`](https://workflowsets.tidymodels.org/dev/reference/workflow_map.md).

- id:

  A single character string for a workflow ID.

## Value

`pull_workflow_set_result()` produces a `tune_result` or
`resample_results` object. `pull_workflow()` returns an unfit workflow
object.

## Details

`pull_workflow_set_result()` retrieves the results of
[`workflow_map()`](https://workflowsets.tidymodels.org/dev/reference/workflow_map.md)
for a particular workflow while `pull_workflow()` extracts the unfitted
workflow from the `info` column.

The
[`extract_workflow_set_result()`](https://workflowsets.tidymodels.org/dev/reference/extract_workflow_set_result.md)
and
[`extract_workflow()`](https://hardhat.tidymodels.org/reference/hardhat-extract.html)
functions should be used instead of these functions.
