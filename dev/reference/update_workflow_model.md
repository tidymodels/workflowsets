# Update components of a workflow within a workflow set

Workflows can take special arguments for the recipe (e.g. a blueprint)
or a model (e.g. a special formula). However, when creating a workflow
set, there is no way to specify these extra components.

`update_workflow_model()` and `update_workflow_recipe()` allow users to
set these values *after* the workflow set is initially created. They are
analogous to
[`workflows::add_model()`](https://workflows.tidymodels.org/reference/add_model.html)
or
[`workflows::add_recipe()`](https://workflows.tidymodels.org/reference/add_recipe.html).

## Usage

``` r
update_workflow_model(x, id, spec, formula = NULL)

update_workflow_recipe(x, id, recipe, blueprint = NULL)
```

## Arguments

- x:

  A workflow set outputted by
  [`workflow_set()`](https://workflowsets.tidymodels.org/dev/reference/workflow_set.md)
  or
  [`workflow_map()`](https://workflowsets.tidymodels.org/dev/reference/workflow_map.md).

- id:

  A single character string from the `wflow_id` column indicating which
  workflow to update.

- spec:

  A parsnip model specification.

- formula:

  An optional formula override to specify the terms of the model.
  Typically, the terms are extracted from the formula or recipe
  preprocessing methods. However, some models (like survival and
  bayesian models) use the formula not to preprocess, but to specify the
  structure of the model. In those cases, a formula specifying the model
  structure must be passed unchanged into the model call itself. This
  argument is used for those purposes.

- recipe:

  A recipe created using
  [`recipes::recipe()`](https://recipes.tidymodels.org/reference/recipe.html).
  The recipe should not have been trained already with
  [`recipes::prep()`](https://recipes.tidymodels.org/reference/prep.html);
  workflows will handle training internally.

- blueprint:

  A hardhat blueprint used for fine tuning the preprocessing.

  If `NULL`,
  [`hardhat::default_recipe_blueprint()`](https://hardhat.tidymodels.org/reference/default_recipe_blueprint.html)
  is used.

  Note that preprocessing done here is separate from preprocessing that
  might be done automatically by the underlying model.

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
library(parsnip)

new_mod <-
  decision_tree() |>
  set_engine("rpart", method = "anova") |>
  set_mode("classification")

new_set <- update_workflow_model(two_class_res, "none_cart", spec = new_mod)

new_set
#> # A workflow set/tibble: 6 × 4
#>   wflow_id      info             option    result    
#>   <chr>         <list>           <list>    <list>    
#> 1 none_cart     <tibble [1 × 4]> <opts[3]> <list [0]>
#> 2 none_glm      <tibble [1 × 4]> <opts[3]> <rsmp[+]> 
#> 3 none_mars     <tibble [1 × 4]> <opts[3]> <tune[+]> 
#> 4 yj_trans_cart <tibble [1 × 4]> <opts[3]> <tune[+]> 
#> 5 yj_trans_glm  <tibble [1 × 4]> <opts[3]> <rsmp[+]> 
#> 6 yj_trans_mars <tibble [1 × 4]> <opts[3]> <tune[+]> 

extract_workflow(new_set, id = "none_cart")
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
#> Engine-Specific Arguments:
#>   method = anova
#> 
#> Computational engine: rpart 
#> 
```
