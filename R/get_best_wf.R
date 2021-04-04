#' Extract models from a workflow set
#'  
#' `get_best_wf()` takes the updated workflow set that comes from the function
#' `workflow_map()`. It allows to obtain a top number of workflows based on a
#' evaluation metric, obtaining the finalized, entered workflows
#' on the test aprticion and the metrics in train / test.
#'  
#' @param .wfs A updated workflow set.
#' @param .model the top number of workflows to extract. By default it is NULL
#' (extracts the best workflow). It admits 3 ways to specify the extraction of
#' the workflows, a numerical value that represents the "n" best workflows, a
#' character string with the word "top" (it has a regex that admits some 
#' flexibility) followed by a numerical value ("Top 3" , "Top 5"), a string or
#' a vector of stings with the specific names of the workflows to be extracted
#' in reference to the column "wflow_id" of the workflows set supplied.
#' @param .rank_metric A character string for a metric. Represents the metric 
#' on which the top models will be selected. If not specified, the first metric
#' of the workflow set is selected.
#' @param .set_metrics A "metric_set" object from the `metric_set` function 
#' where a set of evaluation metrics are passed. It is used to calculate extra
#' metrics to those estimated in the supplied workflow set.
#' @param .resamples A resample rset created from an rsample function such as 
#' [rsample::vfold_cv()].Necessary to estimate the performance of workflows
#' with metrics not specified in the workflow set training.
#' @param .split An rsplit object. Necessary to estimate the performance of the 
#' model in the test partition and also to obtain the trained workflows on the 
#' test partition.
#' @param ctrl A [control_resamples()] object used to fine tune the resampling
#' process.
#' @return a tibble. Saves the name of the supplied workflow set (useful if 
#' you want to compare multiple workflow sets), the requested workflows
#' completed with the update of hyperparameters (both models and recipes).
#' Finally, according to the parameters supplied, it returns the workflows 
#' trained on the train partitions and the adjustment metrics on train and 
#' test. It contains the following columns "workflow_set_name", ".model_name",
#' "wf_finalize", "cv_metrics", ".last_fit_model", "test_metrics".
#'  
#' @details
#' 
#' The workflows are selected automatically by ranking and then filtering it by
#' the metric provided. The ".split" parameter is only necessary if you want to
#' obtain the trained workflows on the train partition and the metrics on test.
#' 
#' The ".resamples" parameter does not need to be specified, since the 
#' performance of the model on train can be extracted from the supplied 
#' workflow set. However, if you want to obtain evaluation metrics other than
#' those of the workflow set, it is necessary to specify it to obtain these 
#' metrics from the supplied folds.
#' 
#' The ".set_metrics" parameter does not need to be specified, however it is
#' necessary to supply it to calculate non-estimated metrics in the workflow 
#' set object. Also to estimate metrics on tests other than those that the 
#' `last_fit()` function of the tune package has by default.
#' 
#' @export
#'
#' @examples
#' library(workflowsets)
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
get_best_wf <- function(.wfs, .model = NULL, .rank_metric = NULL, .set_metrics = NULL, .resamples = NULL, .split = NULL, ctrl = tune::control_resamples()){
   
   #Capture name of workflowset (util for multiple workflowsets)
   workflow_set_name <- match.call()
   
   workflow_set_name <- as.list(workflow_set_name[-1])$.wfs %>% as.character()
   
   #Rank models
   rank_models <- rank_results(.wfs, rank_metric = .rank_metric, select_best = TRUE)
   
   #Metrics
   ##select wf in rank_results
   if(is.null(.rank_metric)) .rank_metric <- (rank_models$.metric %>% unique())[1]
   
   #Filter Rank models by unique metrics
   rank_models <- rank_models %>% dplyr::filter(.metric == .rank_metric)
   
   #Select model
   if(is.null(.model)){
      
      best_model <- rank_models %>% dplyr::slice(1)
      
      .model <- best_model["wflow_id"] %>% unlist() %>% as.character()
      
   }
   
   #All models
   if(.model == "all") .model <- nrow(rank_models)
   
   #Select number top models
   if(is.numeric(.model)){
      
      if(.model > nrow(rank_models)) stop('The number of top models requested is higher than the number of models supplied')
      
      best_model <- rank_models %>% dplyr::slice(1:.model)
      
      .model <- best_model["wflow_id"] %>% unlist() %>% as.character()
      
   }
   
   #Select top models with top string
   top_str_val <- tolower(.model)
   top_str_val <- trimws(top_str_val)
   top_str_val <- gsub("\\s+"," ",top_str_val)
   top_str_val <- strsplit(top_str_val, " ") %>% unlist()
   
   if(length(.model) == 1 & top_str_val[1] == "top") {
      
      if(is.na(top_str_val[2])) stop('Enter a number that accompanies the word "top"')
      
      if(is.na(top_str_val[2] %>% as.numeric())) stop('the word that accompanies the word "top" is not a number')
      
      if(top_str_val[2] %>% as.numeric() > nrow(rank_models)) stop('The number of top models requested is higher than the number of models supplied')
      
      best_model <- rank_models %>% dplyr::slice(1:top_str_val[2] %>% as.numeric())
      
      .model <- best_model["wflow_id"] %>% unlist() %>% as.character()
      
   }
   
   #Validation of models names
   if(any(!.model %in% rank_models$wflow_id)) stop('some of the model names passed in the ".model" argument do not match the model names in the supplied workflow set object')
   
   #Finalize workflowsets
   F_get_workflows <- function(models , wfstes, .metric){
      
      best_parameteres <- wfstes %>% pull_workflow_set_result(models) %>% tune::show_best(metric = .metric) %>% dplyr::slice(1)
      
      best_workflow <- wfstes %>% pull_workflow(models)
      
      workflow_finalize <- best_workflow  %>% tune::finalize_workflow(best_parameteres)
      
      workflow_tibble <- tibble::tibble(.model_name = models,
                                        wf_finalize = list(workflow_finalize))
      
   }
   
   wfsets_tibble <- purrr::map(.model,
                               ~ F_get_workflows(models = .x,
                                                 wfstes = .wfs,
                                                 .metric = .rank_metric)) %>%
      
      dplyr::bind_rows() %>%
      
      dplyr::mutate(workflow_set_name = workflow_set_name) %>%
      
      dplyr::relocate(workflow_set_name)
   
   # evaluation CV
   if(!is.null(.resamples)){
      
      cli::cli_h1(paste0(cli::symbol$info,' The workflow performance is calculated in the training partition through the .resamples input'))
      
      wfsets_tibble <- wfsets_tibble %>%
         
         dplyr::mutate(cv_metrics = purrr::map(wf_finalize, 
                                               ~fit_resamples(object = .x,
                                                              resamples = .resamples,
                                                              metrics = .set_metrics,
                                                              control =  ctrl) %>%
                                                  collect_metrics() %>%
                                                  dplyr::rename(.estimate = mean) %>%
                                                  dplyr::mutate(.evaluate = "cv"))
         )
      
   } else {
      
      cli::cli_h1(paste0(cli::symbol$info,' The performance of the workflow in the training partition is extracted from the workflow set input'))
      
      wfsets_tibble <- wfsets_tibble %>%
         
         dplyr::mutate(cv_metrics = purrr::map(.model_name, 
                                               ~ rank_models %>% 
                                                  dplyr::filter(wflow_id == .x) %>%
                                                  dplyr::rename(.estimate = mean) %>%
                                                  dplyr::mutate(.evaluate = "cv"))
         )
      
   }
   # evaluation test
   if(!is.null(.split)){
      
      if('rsplit' %in% class(.split) == FALSE) .split <-.split %>% purrr::pluck("splits",1)
      
      wfsets_tibble <- wfsets_tibble %>%
         
         dplyr::mutate(.last_fit_model = purrr::map(wf_finalize,
                                                   ~ tune::last_fit(object = .x ,
                                                                    split = .split, 
                                                                    metrics = .set_metrics))
         ) %>%
         
         dplyr::mutate(test_metrics = purrr::map(.last_fit_model, 
                                                 ~ collect_metrics(.x) %>% 
                                                    dplyr::mutate(.evaluate = "test"))
         )
   }
   
   wfsets_tibble
   
}