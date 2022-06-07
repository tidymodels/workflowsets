.onLoad <- function(libname, pkgname) {
  vctrs::s3_register("pillar::obj_sum", "workflow_set_options")
  vctrs::s3_register("pillar::size_sum", "workflow_set_options")
  vctrs::s3_register("pillar::type_sum", "workflow_set_options")
  vctrs::s3_register("pillar::tbl_sum", "workflow_set")
  vctrs::s3_register("tune::collect_metrics", "workflow_set")
  vctrs::s3_register("tune::collect_predictions", "workflow_set")
  vctrs::s3_register("ggplot2::autoplot", "workflow_set")
  invisible()
}
