# Package index

## Core functions

- [`workflow_set()`](https://workflowsets.tidymodels.org/dev/reference/workflow_set.md)
  : Generate a set of workflow objects from preprocessing and model
  objects
- [`workflow_map()`](https://workflowsets.tidymodels.org/dev/reference/workflow_map.md)
  : Process a series of workflows

## Interface with workflow sets

- [`option_add()`](https://workflowsets.tidymodels.org/dev/reference/option_add.md)
  [`option_remove()`](https://workflowsets.tidymodels.org/dev/reference/option_add.md)
  [`option_add_parameters()`](https://workflowsets.tidymodels.org/dev/reference/option_add.md)
  : Add and edit options saved in a workflow set
- [`option_list()`](https://workflowsets.tidymodels.org/dev/reference/option_list.md)
  : Make a classed list of options
- [`extract_workflow_set_result()`](https://workflowsets.tidymodels.org/dev/reference/extract_workflow_set_result.md)
  [`extract_workflow(`*`<workflow_set>`*`)`](https://workflowsets.tidymodels.org/dev/reference/extract_workflow_set_result.md)
  [`extract_spec_parsnip(`*`<workflow_set>`*`)`](https://workflowsets.tidymodels.org/dev/reference/extract_workflow_set_result.md)
  [`extract_recipe(`*`<workflow_set>`*`)`](https://workflowsets.tidymodels.org/dev/reference/extract_workflow_set_result.md)
  [`extract_fit_parsnip(`*`<workflow_set>`*`)`](https://workflowsets.tidymodels.org/dev/reference/extract_workflow_set_result.md)
  [`extract_fit_engine(`*`<workflow_set>`*`)`](https://workflowsets.tidymodels.org/dev/reference/extract_workflow_set_result.md)
  [`extract_mold(`*`<workflow_set>`*`)`](https://workflowsets.tidymodels.org/dev/reference/extract_workflow_set_result.md)
  [`extract_preprocessor(`*`<workflow_set>`*`)`](https://workflowsets.tidymodels.org/dev/reference/extract_workflow_set_result.md)
  [`extract_parameter_set_dials(`*`<workflow_set>`*`)`](https://workflowsets.tidymodels.org/dev/reference/extract_workflow_set_result.md)
  [`extract_parameter_dials(`*`<workflow_set>`*`)`](https://workflowsets.tidymodels.org/dev/reference/extract_workflow_set_result.md)
  : Extract elements of workflow sets
- [`comment_add()`](https://workflowsets.tidymodels.org/dev/reference/comment_add.md)
  [`comment_get()`](https://workflowsets.tidymodels.org/dev/reference/comment_add.md)
  [`comment_reset()`](https://workflowsets.tidymodels.org/dev/reference/comment_add.md)
  [`comment_print()`](https://workflowsets.tidymodels.org/dev/reference/comment_add.md)
  : Add annotations and comments for workflows
- [`update_workflow_model()`](https://workflowsets.tidymodels.org/dev/reference/update_workflow_model.md)
  [`update_workflow_recipe()`](https://workflowsets.tidymodels.org/dev/reference/update_workflow_model.md)
  : Update components of a workflow within a workflow set

## Process workflow set results

- [`rank_results()`](https://workflowsets.tidymodels.org/dev/reference/rank_results.md)
  : Rank the results by a metric
- [`autoplot(`*`<workflow_set>`*`)`](https://workflowsets.tidymodels.org/dev/reference/autoplot.workflow_set.md)
  : Plot the results of a workflow set
- [`fit_best(`*`<workflow_set>`*`)`](https://workflowsets.tidymodels.org/dev/reference/fit_best.workflow_set.md)
  : Fit a model to the numerically optimal configuration
- [`collect_metrics(`*`<workflow_set>`*`)`](https://workflowsets.tidymodels.org/dev/reference/collect_metrics.workflow_set.md)
  [`collect_predictions(`*`<workflow_set>`*`)`](https://workflowsets.tidymodels.org/dev/reference/collect_metrics.workflow_set.md)
  [`collect_notes(`*`<workflow_set>`*`)`](https://workflowsets.tidymodels.org/dev/reference/collect_metrics.workflow_set.md)
  [`collect_extracts(`*`<workflow_set>`*`)`](https://workflowsets.tidymodels.org/dev/reference/collect_metrics.workflow_set.md)
  : Obtain and format results produced by tuning functions for workflow
  sets

## Miscellanous

- [`as_workflow_set()`](https://workflowsets.tidymodels.org/dev/reference/as_workflow_set.md)
  : Convert existing objects to a workflow set
- [`autoplot(`*`<workflow_set>`*`)`](https://workflowsets.tidymodels.org/dev/reference/autoplot.workflow_set.md)
  : Plot the results of a workflow set
- [`chi_features_set`](https://workflowsets.tidymodels.org/dev/reference/chi_features_set.md)
  [`chi_features_res`](https://workflowsets.tidymodels.org/dev/reference/chi_features_set.md)
  : Chicago Features Example Data
- [`collect_metrics(`*`<workflow_set>`*`)`](https://workflowsets.tidymodels.org/dev/reference/collect_metrics.workflow_set.md)
  [`collect_predictions(`*`<workflow_set>`*`)`](https://workflowsets.tidymodels.org/dev/reference/collect_metrics.workflow_set.md)
  [`collect_notes(`*`<workflow_set>`*`)`](https://workflowsets.tidymodels.org/dev/reference/collect_metrics.workflow_set.md)
  [`collect_extracts(`*`<workflow_set>`*`)`](https://workflowsets.tidymodels.org/dev/reference/collect_metrics.workflow_set.md)
  : Obtain and format results produced by tuning functions for workflow
  sets
- [`comment_add()`](https://workflowsets.tidymodels.org/dev/reference/comment_add.md)
  [`comment_get()`](https://workflowsets.tidymodels.org/dev/reference/comment_add.md)
  [`comment_reset()`](https://workflowsets.tidymodels.org/dev/reference/comment_add.md)
  [`comment_print()`](https://workflowsets.tidymodels.org/dev/reference/comment_add.md)
  : Add annotations and comments for workflows
- [`extract_workflow_set_result()`](https://workflowsets.tidymodels.org/dev/reference/extract_workflow_set_result.md)
  [`extract_workflow(`*`<workflow_set>`*`)`](https://workflowsets.tidymodels.org/dev/reference/extract_workflow_set_result.md)
  [`extract_spec_parsnip(`*`<workflow_set>`*`)`](https://workflowsets.tidymodels.org/dev/reference/extract_workflow_set_result.md)
  [`extract_recipe(`*`<workflow_set>`*`)`](https://workflowsets.tidymodels.org/dev/reference/extract_workflow_set_result.md)
  [`extract_fit_parsnip(`*`<workflow_set>`*`)`](https://workflowsets.tidymodels.org/dev/reference/extract_workflow_set_result.md)
  [`extract_fit_engine(`*`<workflow_set>`*`)`](https://workflowsets.tidymodels.org/dev/reference/extract_workflow_set_result.md)
  [`extract_mold(`*`<workflow_set>`*`)`](https://workflowsets.tidymodels.org/dev/reference/extract_workflow_set_result.md)
  [`extract_preprocessor(`*`<workflow_set>`*`)`](https://workflowsets.tidymodels.org/dev/reference/extract_workflow_set_result.md)
  [`extract_parameter_set_dials(`*`<workflow_set>`*`)`](https://workflowsets.tidymodels.org/dev/reference/extract_workflow_set_result.md)
  [`extract_parameter_dials(`*`<workflow_set>`*`)`](https://workflowsets.tidymodels.org/dev/reference/extract_workflow_set_result.md)
  : Extract elements of workflow sets
- [`fit_best(`*`<workflow_set>`*`)`](https://workflowsets.tidymodels.org/dev/reference/fit_best.workflow_set.md)
  : Fit a model to the numerically optimal configuration
- [`leave_var_out_formulas()`](https://workflowsets.tidymodels.org/dev/reference/leave_var_out_formulas.md)
  : Create formulas without each predictor
- [`option_add()`](https://workflowsets.tidymodels.org/dev/reference/option_add.md)
  [`option_remove()`](https://workflowsets.tidymodels.org/dev/reference/option_add.md)
  [`option_add_parameters()`](https://workflowsets.tidymodels.org/dev/reference/option_add.md)
  : Add and edit options saved in a workflow set
- [`option_list()`](https://workflowsets.tidymodels.org/dev/reference/option_list.md)
  : Make a classed list of options
- [`pull_workflow_set_result()`](https://workflowsets.tidymodels.org/dev/reference/pull_workflow_set_result.md)
  [`pull_workflow()`](https://workflowsets.tidymodels.org/dev/reference/pull_workflow_set_result.md)
  **\[deprecated\]** : Extract elements from a workflow set
- [`rank_results()`](https://workflowsets.tidymodels.org/dev/reference/rank_results.md)
  : Rank the results by a metric
- [`two_class_set`](https://workflowsets.tidymodels.org/dev/reference/two_class_set.md)
  [`two_class_res`](https://workflowsets.tidymodels.org/dev/reference/two_class_set.md)
  : Two Class Example Data
- [`update_workflow_model()`](https://workflowsets.tidymodels.org/dev/reference/update_workflow_model.md)
  [`update_workflow_recipe()`](https://workflowsets.tidymodels.org/dev/reference/update_workflow_model.md)
  : Update components of a workflow within a workflow set
- [`workflow_map()`](https://workflowsets.tidymodels.org/dev/reference/workflow_map.md)
  : Process a series of workflows
- [`workflow_set()`](https://workflowsets.tidymodels.org/dev/reference/workflow_set.md)
  : Generate a set of workflow objects from preprocessing and model
  objects
