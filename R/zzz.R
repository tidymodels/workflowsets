.onLoad <- function(libname, pkgname) {
   vctrs::s3_register("tibble::obj_sum",           "workflow_set_options")
   vctrs::s3_register("tibble::size_sum",          "workflow_set_options")
   vctrs::s3_register("tibble::type_sum",          "workflow_set_options")
   vctrs::s3_register("tibble::tbl_sum",           "workflow_set")
   vctrs::s3_register("tune::collect_metrics",     "workflow_set")
   vctrs::s3_register("tune::collect_predictions", "workflow_set")
   vctrs::s3_register("ggplot2::autoplot",         "workflow_set")
   invisible()
}

