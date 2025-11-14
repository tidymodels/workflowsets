# Changelog

## workflowsets (development version)

## workflowsets 1.1.1

CRAN release: 2025-05-27

- Added a
  [`collect_extracts()`](https://tune.tidymodels.org/reference/collect_predictions.html)
  method for workflow sets ([@jrosell](https://github.com/jrosell),
  [\#156](https://github.com/tidymodels/workflowsets/issues/156)).

- The deprecation of the `pull_*()` functions has been moved forward.
  These functions now error. Please use the `extract_*()` functions
  instead
  ([\#178](https://github.com/tidymodels/workflowsets/issues/178)).

- Increased the minimum required R version to R 4.1.

## workflowsets 1.1.0

CRAN release: 2024-03-21

- Ellipses (…) are now used consistently in the package to require
  optional arguments to be named;
  [`collect_metrics()`](https://tune.tidymodels.org/reference/collect_predictions.html)
  and
  [`collect_predictions()`](https://tune.tidymodels.org/reference/collect_predictions.html)
  are the only functions that received changes
  ([\#151](https://github.com/tidymodels/workflowsets/issues/151),
  tidymodels/tune#863).
- Enabled evaluating censored regression models
  ([\#139](https://github.com/tidymodels/workflowsets/issues/139),
  [\#144](https://github.com/tidymodels/workflowsets/issues/144)). The
  placement of the new `eval_time` argument to
  [`rank_results()`](https://workflowsets.tidymodels.org/dev/reference/rank_results.md)
  breaks passing-by-position for the `select_best` argument.
- Added a
  [`collect_notes()`](https://tune.tidymodels.org/reference/collect_predictions.html)
  method for workflow sets
  ([\#135](https://github.com/tidymodels/workflowsets/issues/135)).
- Added methods to improve error messages when workflow sets are
  mistakenly passed to unsupported functions like
  [`fit()`](https://generics.r-lib.org/reference/fit.html) and
  [`predict()`](https://rdrr.io/r/stats/predict.html)
  ([\#137](https://github.com/tidymodels/workflowsets/issues/137)).
- Added a new argument, `type`, to the `workflow_set`
  [`autoplot()`](https://ggplot2.tidyverse.org/reference/autoplot.html)
  method. The default, `"class"`, retains the existing behavior of
  mapping model type to color and preprocessor type to shape, while the
  new `"wflow_id"` type maps the workflow IDs to color
  ([\#134](https://github.com/tidymodels/workflowsets/issues/134)).
- Added type checking for inputted arguments
  ([\#136](https://github.com/tidymodels/workflowsets/issues/136),
  [\#131](https://github.com/tidymodels/workflowsets/issues/131)).

## workflowsets 1.0.1

CRAN release: 2023-04-06

- The
  [`extract_parameter_dials()`](https://hardhat.tidymodels.org/reference/hardhat-extract.html)
  and
  [`extract_parameter_set_dials()`](https://hardhat.tidymodels.org/reference/hardhat-extract.html)
  extractors will now return the parameter or parameter set *that will
  be used by the tuning function utilized in
  [`workflow_map()`](https://workflowsets.tidymodels.org/dev/reference/workflow_map.md)*.
  The extractors previously always returned the parameter or parameter
  set associated with the workflow contained in the `info` column, which
  can be overridden by passing a `param_info` argument to
  [`option_add()`](https://workflowsets.tidymodels.org/dev/reference/option_add.md).
  The extractors will now first look to the added options before
  extracting from workflows
  ([\#106](https://github.com/tidymodels/workflowsets/issues/106)).
- Introduces support for clustering model specifications via the
  tidyclust package. Supplying clustering models to
  [`workflow_set()`](https://workflowsets.tidymodels.org/dev/reference/workflow_set.md)
  and set `fn = "tune_cluster"` in
  [`workflow_map()`](https://workflowsets.tidymodels.org/dev/reference/workflow_map.md)
  to use this feature
  ([\#125](https://github.com/tidymodels/workflowsets/issues/125))!
- Introduces a
  [`fit_best()`](https://tune.tidymodels.org/reference/fit_best.html)
  method for workflowsets that takes in a workflow set evaluated with
  [`workflow_map()`](https://workflowsets.tidymodels.org/dev/reference/workflow_map.md)
  and returns a workflow fitted with the model configuration associated
  with the best performance
  ([\#126](https://github.com/tidymodels/workflowsets/issues/126)).
- Transitions deprecations of `pull_*()` functions to now warn on every
  usage
  ([\#123](https://github.com/tidymodels/workflowsets/issues/123)).
- Various bug fixes and improvements to documentation.

## workflowsets 1.0.0

CRAN release: 2022-07-12

- New
  [`extract_parameter_set_dials()`](https://hardhat.tidymodels.org/reference/hardhat-extract.html)
  and
  [`extract_parameter_dials()`](https://hardhat.tidymodels.org/reference/hardhat-extract.html)
  methods to extract parameter sets and single parameters from
  `workflow_set` objects.

- Added support for case weights via a new `case_weights` argument to
  [`workflow_set()`](https://workflowsets.tidymodels.org/dev/reference/workflow_set.md)
  ([\#82](https://github.com/tidymodels/workflowsets/issues/82)).

## workflowsets 0.2.1

CRAN release: 2022-03-15

- [`update_workflow_model()`](https://workflowsets.tidymodels.org/dev/reference/update_workflow_model.md)
  and
  [`update_workflow_recipe()`](https://workflowsets.tidymodels.org/dev/reference/update_workflow_model.md)
  were added. These are analogous to
  [`workflows::add_model()`](https://workflows.tidymodels.org/reference/add_model.html)
  or
  [`workflows::add_recipe()`](https://workflows.tidymodels.org/reference/add_recipe.html)
  ([\#64](https://github.com/tidymodels/workflowsets/issues/64)).

- Updated tests related to changes in workflows 0.2.5
  ([\#75](https://github.com/tidymodels/workflowsets/issues/75)).

- [`as_workflow_set()`](https://workflowsets.tidymodels.org/dev/reference/as_workflow_set.md)
  can now take a mixture of workflows or `tune_results` objects.

- [`option_add()`](https://workflowsets.tidymodels.org/dev/reference/option_add.md)
  now checks the names of the options to see if they are valid names for
  the functions that receive them
  ([\#66](https://github.com/tidymodels/workflowsets/issues/66))

## workflowsets 0.1.0

CRAN release: 2021-07-22

- Fixed an
  [`autoplot()`](https://ggplot2.tidyverse.org/reference/autoplot.html)
  bug where, if one metric is selected but a ranking metric is not
  specified, the wrong metric is used to order the workflows
  ([\#52](https://github.com/tidymodels/workflowsets/issues/52))

- Updated pillar formatting for options objects.

- New `extract_*()` functions have been added that supersede the
  existing `pull_*()` functions. This is part of a larger move across
  the tidymodels packages towards a family of generic `extract_*()`
  functions. The `pull_*()` functions have been soft-deprecated, and
  will eventually be removed

## workflowsets 0.0.2

CRAN release: 2021-04-16

- Ensured that
  [`workflow_map()`](https://workflowsets.tidymodels.org/dev/reference/workflow_map.md)
  does not fail if there are missing packages or if the function being
  mapped fails.

## workflowsets 0.0.1

CRAN release: 2021-03-18

- First CRAN version
