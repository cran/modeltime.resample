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

## -----------------------------------------------------------------------------
library(tidymodels)
library(modeltime)
library(modeltime.resample)
library(tidyverse)
library(timetk)

## -----------------------------------------------------------------------------
m750 %>%
  plot_time_series(date, value, .interactive = FALSE)

## -----------------------------------------------------------------------------
resamples_tscv <- time_series_cv(
    data        = m750,
    assess      = "2 years",
    initial     = "5 years",
    skip        = "2 years",
    slice_limit = 4
)

resamples_tscv

## -----------------------------------------------------------------------------
# Begin with a Cross Validation Strategy
resamples_tscv %>%
    tk_time_series_cv_plan() %>%
    plot_time_series_cv_plan(date, value, .facet_ncol = 2, .interactive = FALSE)

## -----------------------------------------------------------------------------
m750_models

## -----------------------------------------------------------------------------
resamples_fitted <- m750_models %>%
    modeltime_fit_resamples(
        resamples = resamples_tscv,
        control   = control_resamples(verbose = FALSE)
    )

resamples_fitted

## -----------------------------------------------------------------------------
resamples_fitted %>%
    plot_modeltime_resamples(
      .point_size  = 3, 
      .point_alpha = 0.8,
      .interactive = FALSE
    )

## -----------------------------------------------------------------------------
resamples_fitted %>%
    modeltime_resample_accuracy(summary_fns = mean) %>%
    table_modeltime_accuracy(.interactive = FALSE)

