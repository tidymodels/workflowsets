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
