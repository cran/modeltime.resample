## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
    message = FALSE,
    warning = FALSE,
    fig.width = 8, 
    fig.height = 4.5,
    fig.align = 'center',
    out.width='95%', 
    dpi = 100
)

## ----setup--------------------------------------------------------------------
library(tidymodels)
library(modeltime)
library(modeltime.resample)
library(timetk)
library(tidyverse)
library(tidyquant)

## -----------------------------------------------------------------------------
walmart_sales_weekly %>%
  group_by(id) %>%
  plot_time_series(Date, Weekly_Sales, .facet_ncol = 3, .interactive = FALSE)

## -----------------------------------------------------------------------------
# Full = Training + Forecast Datasets
full_data_tbl <- walmart_sales_weekly %>%
  select(id, Date, Weekly_Sales) %>%
  
  # Apply Group-wise Time Series Manipulations
  group_by(id) %>%
  future_frame(
    .date_var   = Date,
    .length_out = "3 months",
    .bind_data  = TRUE
  ) %>%
  ungroup() %>%
  
  # Consolidate IDs
  mutate(id = fct_drop(id))

# Training Data
data_prepared_tbl <- full_data_tbl %>%
  filter(!is.na(Weekly_Sales))

# Forecast Data
future_tbl <- full_data_tbl %>%
  filter(is.na(Weekly_Sales))

## -----------------------------------------------------------------------------
walmart_tscv <- data_prepared_tbl %>%
  time_series_cv(
    date_var    = Date, 
    assess      = "3 months",
    skip        = "3 months",
    cumulative  = TRUE,
    slice_limit = 6
  )

walmart_tscv

## -----------------------------------------------------------------------------
walmart_tscv %>%
  tk_time_series_cv_plan() %>%
  plot_time_series_cv_plan(Date, Weekly_Sales, 
                           .facet_ncol = 2, .interactive = F)

## -----------------------------------------------------------------------------
recipe_spec <- recipe(Weekly_Sales ~ ., 
                      data = training(walmart_tscv$splits[[1]])) %>%
  step_timeseries_signature(Date) %>%
  step_rm(matches("(.iso$)|(.xts$)|(day)|(hour)|(minute)|(second)|(am.pm)")) %>%
  step_mutate(Date_week = factor(Date_week, ordered = TRUE)) %>%
  step_dummy(all_nominal(), one_hot = TRUE)

## -----------------------------------------------------------------------------
wflw_fit_prophet <- workflow() %>%
  add_model(
    prophet_reg() %>% set_engine("prophet") 
  ) %>%
  add_recipe(recipe_spec) %>%
  fit(training(walmart_tscv$splits[[1]]))

## -----------------------------------------------------------------------------
wflw_fit_xgboost <- workflow() %>%
  add_model(
    boost_tree("regression") %>% set_engine("xgboost") 
  ) %>%
  add_recipe(recipe_spec %>% step_rm(Date)) %>%
  fit(training(walmart_tscv$splits[[1]]))

## -----------------------------------------------------------------------------
wflw_fit_prophet_boost <- workflow() %>%
  add_model(
    prophet_boost(
      seasonality_daily  = FALSE, 
      seasonality_weekly = FALSE,
      seasonality_yearly = FALSE
    ) %>% 
      set_engine("prophet_xgboost") 
  ) %>%
  add_recipe(recipe_spec) %>%
  fit(training(walmart_tscv$splits[[1]]))

## -----------------------------------------------------------------------------
model_tbl <- modeltime_table(
  wflw_fit_prophet,
  wflw_fit_xgboost,
  wflw_fit_prophet_boost
)

model_tbl

## -----------------------------------------------------------------------------
# Calibrate using the Test Sample
calibration_tbl <- model_tbl %>%
  modeltime_calibrate(testing(walmart_tscv$splits[[1]]))

# Forecast the Test Sample
forecast_panel_tbl <- calibration_tbl %>%
  modeltime_forecast(
    new_data      = testing(walmart_tscv$splits[[1]]),
    actual_data   = data_prepared_tbl,
    # Keep data allows us keep the ID feature for the time series groups
    keep_data = TRUE
  ) 

## ---- fig.cap="Panel Forecasting | 7 Time Series Groups"----------------------
forecast_panel_tbl %>%
  group_by(id) %>%
  plot_modeltime_forecast(
    .facet_ncol  = 3,
    .y_intercept = 0, 
    .interactive = FALSE,
    .title       = "Panel Forecasting | 7 Time Series Groups"
  )

## ---- eval = FALSE------------------------------------------------------------
#  resample_results <- model_tbl %>%
#    modeltime_fit_resamples(
#      resamples = walmart_tscv,
#      control   = control_resamples(verbose = FALSE)
#    )

## ---- echo=FALSE--------------------------------------------------------------
# write_rds(resample_results, "resample_results.rds")
resample_results <- read_rds("resample_results.rds")

## -----------------------------------------------------------------------------
resample_results

## -----------------------------------------------------------------------------
resample_results %>%
  plot_modeltime_resamples(
    .summary_fn  = mean, 
    .point_size  = 3,
    .interactive = FALSE
  )

## -----------------------------------------------------------------------------
resample_results %>%
  modeltime_resample_accuracy(summary_fns = list(mean = mean, sd = sd)) %>%
  table_modeltime_accuracy(.interactive = FALSE)

