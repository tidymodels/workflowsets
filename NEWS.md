# workflowsets 1.1.0

* Ellipses (...) are now used consistently in the package to require optional arguments to be named; `collect_metrics()` and `collect_predictions()` are the only functions that received changes (#151, tidymodels/tune#863).
* Enabled evaluating censored regression models (#139, #144). The placement of 
  the new `eval_time` argument to `rank_results()` breaks passing-by-position 
  for the `select_best` argument.
* Added a `collect_notes()` method for workflow sets (#135).
* Added methods to improve error messages when workflow sets are mistakenly
  passed to unsupported functions like `fit()` and `predict()` (#137).
* Added a new argument, `type`, to the `workflow_set` `autoplot()` method. The
  default, `"class"`, retains the existing behavior of mapping model type to 
  color and preprocessor type to shape, while the new `"wflow_id"`
  type maps the workflow IDs to color (#134).
* Added type checking for inputted arguments (#136, #131).

# workflowsets 1.0.1

* The `extract_parameter_dials()` and `extract_parameter_set_dials()` extractors
  will now return the parameter or parameter set 
  _that will be used by the tuning function utilized in `workflow_map()`_. 
  The extractors previously always returned the parameter or parameter set
  associated with the workflow contained in the `info` column, which can be
  overridden by passing a `param_info` argument to `option_add()`. The 
  extractors will now first look to the added options before extracting from
  workflows (#106).
* Introduces support for clustering model specifications via the tidyclust 
  package. Supplying clustering models to `workflow_set()` and set
  `fn = "tune_cluster"` in `workflow_map()` to use this feature (#125)!
* Introduces a `fit_best()` method for workflowsets that takes in a workflow set
  evaluated with `workflow_map()` and returns a workflow fitted with the model
  configuration associated with the best performance (#126).
* Transitions deprecations of `pull_*()` functions to now warn on every usage 
  (#123).
* Various bug fixes and improvements to documentation.

# workflowsets 1.0.0

* New `extract_parameter_set_dials()` and `extract_parameter_dials()` methods to 
  extract parameter sets and single parameters from `workflow_set` objects.
  
* Added support for case weights via a new `case_weights` argument
  to `workflow_set()` (#82).

# workflowsets 0.2.1

* `update_workflow_model()` and `update_workflow_recipe()` were added. These are analogous to `workflows::add_model()` or `workflows::add_recipe()` (#64).

* Updated tests related to changes in workflows 0.2.5 (#75).

* `as_workflow_set()` can now take a mixture of workflows or `tune_results` objects. 

* `option_add()` now checks the names of the options to see if they are valid names for the functions that receive them (#66)

# workflowsets 0.1.0

* Fixed an `autoplot()` bug where, if one metric is selected but a ranking metric is not specified, the wrong metric is used to order the workflows (#52)

* Updated pillar formatting for options objects. 

* New `extract_*()` functions have been added that supersede the existing `pull_*()` functions. This is part of a larger move across the tidymodels packages towards a family of generic `extract_*()` functions. The `pull_*()` functions have been soft-deprecated, and will eventually be removed

# workflowsets 0.0.2

* Ensured that `workflow_map()` does not fail if there are missing packages or if the function being mapped fails. 

# workflowsets 0.0.1

* First CRAN version
