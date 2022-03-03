workflow_set_maybe_reconstruct <- function(x) {
  if (workflow_set_is_reconstructable(x)) {
    new_workflow_set0(x)
  } else {
    new_tibble0(x)
  }
}

workflow_set_is_reconstructable <- function(x) {
  has_required_container_type(x) &&
    has_required_container_columns(x) &&
    has_valid_column_info_structure(x) &&
    has_valid_column_info_inner_types(x) &&
    has_valid_column_info_inner_names(x) &&
    has_valid_column_result_structure(x) &&
    has_valid_column_result_inner_types(x) &&
    has_valid_column_result_fingerprints(x) &&
    has_valid_column_option_structure(x) &&
    has_valid_column_option_inner_types(x) &&
    has_valid_column_wflow_id_structure(x) &&
    has_valid_column_wflow_id_strings(x)
}
