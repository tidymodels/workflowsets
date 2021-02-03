library(tidymodels)
library(workflowsets)

# ------------------------------------------------------------------------------
# Slightly smaller data size
data(Chicago)
Chicago <- Chicago[1:1195,]

time_val_split <-
   sliding_period(
      Chicago,
      date,
      "month",
      lookback = 38,
      assess_stop = 1
   )

# ------------------------------------------------------------------------------

base_recipe <-
   recipe(ridership ~ ., data = Chicago) %>%
   # create date features
   step_date(date) %>%
   step_holiday(date) %>%
   # remove date from the list of predictors
   update_role(date, new_role = "id") %>%
   # create dummy variables from factor columns
   step_dummy(all_nominal()) %>%
   # remove any columns with a single unique value
   step_zv(all_predictors()) %>%
   step_normalize(all_predictors())

date_only <-
   recipe(ridership ~ ., data = Chicago) %>%
   # create date features
   step_date(date) %>%
   update_role(date, new_role = "id") %>%
   # create dummy variables from factor columns
   step_dummy(all_nominal()) %>%
   # remove any columns with a single unique value
   step_zv(all_predictors())

date_and_holidays <-
   recipe(ridership ~ ., data = Chicago) %>%
   # create date features
   step_date(date) %>%
   step_holiday(date) %>%
   # remove date from the list of predictors
   update_role(date, new_role = "id") %>%
   # create dummy variables from factor columns
   step_dummy(all_nominal()) %>%
   # remove any columns with a single unique value
   step_zv(all_predictors())

date_and_holidays_and_pca <-
   recipe(ridership ~ ., data = Chicago) %>%
   # create date features
   step_date(date) %>%
   step_holiday(date) %>%
   # remove date from the list of predictors
   update_role(date, new_role = "id") %>%
   # create dummy variables from factor columns
   step_dummy(all_nominal()) %>%
   # remove any columns with a single unique value
   step_zv(all_predictors()) %>%
   step_pca(!!stations, num_comp = tune())

# ------------------------------------------------------------------------------

lm_spec <- linear_reg() %>% set_engine("lm")

# ------------------------------------------------------------------------------

pca_param <-
   parameters(num_comp()) %>%
   update(num_comp = num_comp(c(0, 20)))

# ------------------------------------------------------------------------------

chi_features_set <-
   workflow_set(
      preproc = list(date = date_only,
                     plus_holidays = date_and_holidays,
                     plus_pca = date_and_holidays_and_pca),
      models = list(lm = lm_spec),
      cross = TRUE
   )

# ------------------------------------------------------------------------------

chi_features_res <-
   chi_features_set %>%
   options_add(param_info = pca_param, id = "plus_pca_lm") %>%
   workflow_map(resamples = time_val_split, grid = 21, seed = 1, verbose = TRUE)

# ------------------------------------------------------------------------------

save(chi_features_set, chi_features_res,
     file = "data/chi_features_set.rda",
     version = 2, compress = "xz")
