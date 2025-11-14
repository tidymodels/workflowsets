# Extract elements of workflow sets

These functions extract various elements from a workflow set object. If
they do not exist yet, an error is thrown.

- [`extract_preprocessor()`](https://hardhat.tidymodels.org/reference/hardhat-extract.html)
  returns the formula, recipe, or variable expressions used for
  preprocessing.

- [`extract_spec_parsnip()`](https://hardhat.tidymodels.org/reference/hardhat-extract.html)
  returns the parsnip model specification.

- [`extract_fit_parsnip()`](https://hardhat.tidymodels.org/reference/hardhat-extract.html)
  returns the parsnip model fit object.

- [`extract_fit_engine()`](https://hardhat.tidymodels.org/reference/hardhat-extract.html)
  returns the engine specific fit embedded within a parsnip model fit.
  For example, when using
  [`parsnip::linear_reg()`](https://parsnip.tidymodels.org/reference/linear_reg.html)
  with the `"lm"` engine, this returns the underlying `lm` object.

- [`extract_mold()`](https://hardhat.tidymodels.org/reference/hardhat-extract.html)
  returns the preprocessed "mold" object returned from
  [`hardhat::mold()`](https://hardhat.tidymodels.org/reference/mold.html).
  It contains information about the preprocessing, including either the
  prepped recipe, the formula terms object, or variable selectors.

- [`extract_recipe()`](https://hardhat.tidymodels.org/reference/hardhat-extract.html)
  returns the recipe. The `estimated` argument specifies whether the
  fitted or original recipe is returned.

- `extract_workflow_set_result()` returns the results of
  [`workflow_map()`](https://workflowsets.tidymodels.org/dev/reference/workflow_map.md)
  for a particular workflow.

- [`extract_workflow()`](https://hardhat.tidymodels.org/reference/hardhat-extract.html)
  returns the workflow object. The workflow will not have been
  estimated.

- [`extract_parameter_set_dials()`](https://hardhat.tidymodels.org/reference/hardhat-extract.html)
  returns the parameter set *that will be used to fit* the supplied row
  `id` of the workflow set. Note that workflow sets reference a
  parameter set associated with the `workflow` contained in the `info`
  column by default, but can be fitted with a modified parameter set via
  the
  [`option_add()`](https://workflowsets.tidymodels.org/dev/reference/option_add.md)
  interface. This extractor returns the latter, if it exists, and
  returns the former if not, mirroring the process that
  [`workflow_map()`](https://workflowsets.tidymodels.org/dev/reference/workflow_map.md)
  follows to provide tuning functions a parameter set.

- [`extract_parameter_dials()`](https://hardhat.tidymodels.org/reference/hardhat-extract.html)
  returns the `parameters` object *that will be used to fit* the
  supplied tuning `parameter` in the supplied row `id` of the workflow
  set. See the above notes in
  [`extract_parameter_set_dials()`](https://hardhat.tidymodels.org/reference/hardhat-extract.html)
  on precedence.

## Usage

``` r
extract_workflow_set_result(x, id, ...)

# S3 method for class 'workflow_set'
extract_workflow(x, id, ...)

# S3 method for class 'workflow_set'
extract_spec_parsnip(x, id, ...)

# S3 method for class 'workflow_set'
extract_recipe(x, id, ..., estimated = TRUE)

# S3 method for class 'workflow_set'
extract_fit_parsnip(x, id, ...)

# S3 method for class 'workflow_set'
extract_fit_engine(x, id, ...)

# S3 method for class 'workflow_set'
extract_mold(x, id, ...)

# S3 method for class 'workflow_set'
extract_preprocessor(x, id, ...)

# S3 method for class 'workflow_set'
extract_parameter_set_dials(x, id, ...)

# S3 method for class 'workflow_set'
extract_parameter_dials(x, id, parameter, ...)
```

## Arguments

- x:

  A workflow set outputted by
  [`workflow_set()`](https://workflowsets.tidymodels.org/dev/reference/workflow_set.md)
  or
  [`workflow_map()`](https://workflowsets.tidymodels.org/dev/reference/workflow_map.md).

- id:

  A single character string for a workflow ID.

- ...:

  Other options (not currently used).

- estimated:

  A logical for whether the original (unfit) recipe or the fitted recipe
  should be returned.

- parameter:

  A single string for the parameter ID.

## Value

The extracted value from the object, `x`, as described in the
description section.

## Details

These functions supersede the `pull_*()` functions (e.g.,
`extract_workflow_set_result()`).

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
library(tune)

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

extract_workflow_set_result(two_class_res, "none_cart")
#> # Tuning results
#> # 5-fold cross-validation 
#> # A tibble: 5 × 4
#>   splits            id    .metrics          .notes          
#>   <list>            <chr> <list>            <list>          
#> 1 <split [632/159]> Fold1 <tibble [20 × 6]> <tibble [0 × 1]>
#> 2 <split [633/158]> Fold2 <tibble [20 × 6]> <tibble [0 × 1]>
#> 3 <split [633/158]> Fold3 <tibble [20 × 6]> <tibble [0 × 1]>
#> 4 <split [633/158]> Fold4 <tibble [20 × 6]> <tibble [0 × 1]>
#> 5 <split [633/158]> Fold5 <tibble [20 × 6]> <tibble [0 × 1]>

extract_workflow(two_class_res, "none_cart")
#> ══ Workflow ═══════════════════════════════════════════════════════════
#> Preprocessor: Formula
#> Model: decision_tree()
#> 
#> ── Preprocessor ───────────────────────────────────────────────────────
#> Class ~ A + B
#> 
#> ── Model ──────────────────────────────────────────────────────────────
#> Decision Tree Model Specification (classification)
#> 
#> Main Arguments:
#>   cost_complexity = tune()
#>   min_n = tune()
#> 
#> Computational engine: rpart 
#> 
```
