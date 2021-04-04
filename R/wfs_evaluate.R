#' Model evaluation of a workflow set
#' 
#' `wfs_evaluate()` takes as input a tibble that corresponds to the output of 
#' the `get_best_wf()` function, it allows to extract the trained workflows 
#' and the performance metrics in the train and test partitions.
#'
#' @param .best_wfs a tibble that corresponds to the output of the 
#' `get_best_wf()` function.
#' @param metrics_cv_test A logical to get the evaluation metrics on the train 
#' and test partitions, it defaults to TRUE.
#' @param fit_models A logical to obtain the trained workflows on the train 
#' partition, by default it is TRUE.
#'
#' @return A list with two elements, the first is named as "metrics_cv_test" 
#' and stores "n" tibbles, according to the "top n" requested workflows, where
#' the evaluation metrics are shown in the train and test partitions.
#' 
#' The second element is named "fit_models" and stores the "top n" requested 
#' workflows trained on the train partition.
#' 
#' @details
#' 
#' Depending on the way the object that enters the parameter ".best_wfs" was 
#' generated, the outputs of this function can be obtained. If the input of 
#' “.best_wfs” does not have the evaluation metrics in train and test, the 
#' element “metrics_cv_test” will not be generated. If the input of “.best_wfs”
#' does not have the workflows trained, the element "fit_models" cannot be 
#' generated.
#' 
#' @export
#'
#' @examples
#' #library(workflowsets)
#' 
#' # Slightly smaller data size
#' data(Chicago, package = "modeldata")
#' Chicago <- Chicago[1:1195,]
#' 
#' time_val_split <-
#'    rsample::sliding_period(
#'       Chicago,
#'       date,
#'       "month",
#'       lookback = 38,
#'       assess_stop = 1
#'    )
#' 
#' chi_features_res
#' chi_features_res %>% rank_results(select_best = TRUE)
#' 
#' best_wf <- get_best_wf(.wfs = chi_features_res,
#'                        .model = "top 1",
#'                        .rank_metric = "rsq",
#'                        .split = time_val_split)
#' 
#' best_wf
#' 
#' wfs_evaluate(best_wf)
wfs_evaluate <- function(.best_wfs, metrics_cv_test = TRUE, fit_models = TRUE) {
   
   if(metrics_cv_test == FALSE & fit_models == FALSE) stop(message("At least one of the parameters metrics_cv_test or fit_models must be  TRUE"))
   
   if(".model_name" %in% names(.best_wfs) == FALSE) stop("Column .model_name not found in .best_wfs object")
   
   #wfsets list
   list_wfsets <- .best_wfs %>% split(.best_wfs$.model_name)
   
   #metrics cv / test
   if(metrics_cv_test == TRUE){
      
      if(any(c(!"test_metrics" %in% names(.best_wfs), !"cv_metrics" %in% names(.best_wfs)))){
         
         metrics_cv_test <- NULL 
         message("Columns test_metrics or cv_metrics are not in .best_wfs object. Cannot compare test metrics with train metrics.")
         
      }
      
      tryCatch({
         
         metrics_cv_test <- purrr::map(list_wfsets,
                                       ~ .x %>%
                                          dplyr::mutate(.evaluate = purrr::map2(cv_metrics, test_metrics,
                                                                                ~ dplyr::bind_rows(.x, .y) %>%
                                                                                   dplyr::select(.metric, .estimate, n, std_err,.evaluate))
                                          ) %>%
                                          dplyr::select(workflow_set_name, .model_name, .evaluate) %>%
                                          tidyr::unnest(cols = c(.evaluate)))
         
      }, error = function(x){ message("Ignore the metrics_cv_test element")}
      )
      } else {
      
      metrics_cv_test <- NULL
      
   }
   #fit models
   if(fit_models == TRUE){
      
      if(".last_fit_model" %in% names(.best_wfs) == FALSE) message("Column .last_fit_model is not in .best_wfs object. cannot extract fitted workflows")
         
         fit_models <- purrr::map(list_wfsets,
                                  ~ .x %>% 
                                     purrr::pluck(".last_fit_model",1,".workflow",1))
         
         if(purrr::map(fit_models, is.null) %>% unlist() %>% all() == TRUE){
            
            fit_models <- NULL
            message("Ignore the fit_models element")
             
         } 
      
   } else {
      
      fit_models <- NULL
      
   }
   
   output_list <- list(metrics_cv_test = metrics_cv_test,
                       fit_models = fit_models) %>%
      purrr::compact()
   
   if(is.null(fit_models)  & is.null(metrics_cv_test)) output_list <- NULL
   
   output_list
   
}
