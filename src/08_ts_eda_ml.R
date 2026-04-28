# --------------------------------------------
# Script Name: timeseries and forecasting
# Purpose: The script will illustrate how to create
#          a ts object and to build a ml model for 
#          forecasting. 
# 
# Author:     Fanglin Liu
# Email:      flliu315@163.com
# Date:       2026-04-23
# --------------------------------------------
cat("\014") # Clears the console
rm(list = ls()) # Remove all variables

##############################################
# 01-creating a time series and representing it
#############################################
# A) use ts() to create a time series

data=read.table('data/tsdata/DOUBS_fishBiomassData.txt',h=TRUE)
head(data)
data_clean <- data |>
  dplyr::select(-YEAR) |>
  distinct() # Identify and Remove Duplicate Data

unique(data_clean$STATION) # check stations
table(data_clean$STATION)
unique(data_clean$SP) # check species
table(data_clean$SP)
CHE_VOL_data <- data_clean |>
  subset(STATION=="VOLPla" & SP == "CHE")

CHE_ts = ts(data = CHE_VOL_data[, -c(1:5)], # creating ts
             start = c(1994), # Start Year 1994
             frequency = 1)  # freq = 1

# Plot data with faceting
library(forecast) # work with ggplot2 for autoplot()
library(ggplot2)
autoplot(CHE_ts, facets = TRUE) +
  ggtitle("CHE of Doubs river") +
  ylab("Changes") + xlab("Year")

# B) use timetk() to create a time series
library(timetk)
library(tidyverse)
library(tsibble)

CHE_tk <- CHE_VOL_data |> 
  tk_tbl() |> # Convert to tibble
  select(index, DATE, BIOMASS, DENSITY) |>
  group_by(DATE) |>
  mutate(
    DATE = ymd(paste0(year(DATE), "-01-01"))
  ) |>
  arrange(DATE)

CHE_tk_long <- CHE_tk |>
  pivot_longer( # convert to to long format
  cols = c("BIOMASS", "DENSITY"),
  names_to = "variable",
  values_to = "value")

dim(CHE_tk)

# plot with timetk

CHE_tk_long |>
  group_by(variable) |> 
  plot_time_series(
    .date_var    = DATE,
    .value       = value,
    .facet_ncol  = 1,
    .facet_scale = "free",  
    .interactive = FALSE,
    .title       = "CHE of Le Doubs River"
  )


# C) representation/reduce dimensions

library(TSrepr)

CHE_biom_ts <- CHE_ts[, "BIOMASS", drop = TRUE]

p1 <- autoplot(CHE_biom_ts) +
  ggtitle("CHE biomass of Doubs river") +
  ylab("Changes") + xlab("Year")

CHE_biom_paa <- repr_paa(as.numeric(CHE_biom_ts), q = 2, 
                         func = meanC) 
CHE_biom_paa_ts <- ts(CHE_biom_paa,
                  start= c(1994,1),
                  frequency =1)

p2 <- autoplot(CHE_biom_paa_ts) +
  ggtitle("CHE biomass of Doubs river") +
  ylab("Changes") + xlab("Year")

library(patchwork)
p1 / p2

##################################################
# 02-data pre-processing and exploratory analysis
##################################################
# A) missing imputation of a times eries object
# https://www.kaggle.com/code/janiobachmann/time-series-i-an-introductory-start?scriptVersionId=53165252

library(DataExplorer)
library(ggthemes)

CHE_tk_missing <- 
  CHE_tk_long |>
  group_by(variable) |>
  summarise_by_time(
    DATE, 
    .by = "year",
    value = first(value)) |>
  pad_by_time(DATE, .by = "year")

CHE_tk_missing |>
  plot_time_series(DATE, value, 
                   .facet_ncol = 1, 
                   .facet_scale = "free",
                   .interactive = FALSE,
                   .title = "CHE of Le Doubs river"
  ) 

tail(CHE_tk_missing)

# imputation of missing data
# https://business-science.github.io/timetk/articles/TK07_Time_Series_Data_Wrangling.html

CHE_tk_imputed <- CHE_tk_missing |>
  group_by(variable) |>
  pad_by_time(DATE, .by = "year") |>
  mutate_at(vars(value), .funs = ts_impute_vec, period = 1) 

tail(CHE_tk_imputed)

CHE_tk_imputed |>
  plot_time_series(DATE, value, 
                   .facet_ncol = 1, 
                   .facet_scale = "free",
                   .interactive = FALSE,
                   .title = "CHE of Le Doubs river"
  ) 


# B) Find the outlier in timeseries
# # https://business-science.github.io/timetk/articles/TK07_Time_Series_Data_Wrangling.html

CHE_tk_imputed |>
  group_by(variable) |>
  plot_anomaly_diagnostics(
    .date = DATE,
    .value = value,
    .facet_ncol = 1,
    .interactive=FALSE,
    .title = "Anomaly Diagnostics",
    .anom_color ="#FB3029", 
    .max_anomalies = 0.07, 
    .alpha = 0.05
  )

# C) serial autocorrelation or ACF

CHE_tk_imputed |>
  group_by(variable) |>
  plot_acf_diagnostics(
    DATE, value,               # ACF & PACF
    .lags = "5 years",    
    .interactive = FALSE
  )

########################################################
# 03-the principle of a ts recursive forecast with embed
########################################################
# 1) Focusing on the time series of fishBioms

library(tidyverse)
library(randomForest)

CHE_tk_biom <- CHE_tk_imputed |>
  filter(variable == "BIOMASS")

CHE_tk_biom |>
  plot_time_series(
    DATE, value,
    .smooth = FALSE,
    .title = "BIOMASS Time Series"
  )
CHE_tk_biom_ts <- ts(CHE_tk_biom$value, start=1994, frequency=1)

# 2) splitting training/test and pre-processing
ts_train <- window(CHE_tk_biom_ts, end = 2018)
ts_train_trans <- ts_train |> log() |> diff(1)

lag_order <- 2 
horizon <- 2                                              
ts_train_mbd <- embed(ts_train_trans, lag_order + 1)
Y_train <- ts_train_mbd[, 1] 
X_train <- ts_train_mbd[, -1] 

y_test <- window(CHE_tk_biom_ts, start = 2019, end = 2020) 
x_test <- ts_train_mbd[nrow(ts_train_mbd), c(1:lag_order)]

# 3) recursive forecasting with for loop
pred_rf <- numeric(horizon)
for (i in 1:horizon){
  set.seed(1) 
  fit_rf <- randomForest(X_train, Y_train) 
  pred_rf[i] <- predict(fit_rf, t(as.matrix(x_test)))
  x_test <- c(pred_rf[i], x_test[1:(lag_order-1)])
  Y_train <- c(Y_train[-1], pred_rf[i])
  X_train <- rbind(X_train[-1, ], x_test)
}
pred_rf

# 4) back-transforming and evaluating errors
exp_term <- exp(cumsum(pred_rf)) # Undoes differencing and log-transform
last_obs <- as.vector(tail(ts_train, 1)) 
backtrans_fc <- last_obs * exp_term 
y_pred <- ts(backtrans_fc, start = 2019, frequency = 1)
forecast::accuracy(as.numeric(y_pred), as.numeric(y_test))
library(fpp2)
ts_fc <- cbind(CHE_tk_biom_ts,pred = c(rep(NA, length(ts_train)), y_pred)) 
plot_fc <- ts_fc |> autoplot() + theme_minimal() 
plot_fc

#################################################
# 04- a ts pred with timetk + recipes + workflows
#################################################
library(tidyverse)
library(tidymodels)
library(modeltime)
library(timetk)
library(lubridate)

CHE_tk_biom <- CHE_tk_imputed |>
  filter(variable == "BIOMASS") |> 
  ungroup() |>
  select(-variable)

CHE_tk_biom |>
  plot_time_series(
  DATE, value,
  .smooth = FALSE,
  .title = "BIOMASS Time Series"
)
# 1) Extracting features with timetk and perform ML

# A) Calendar-based features

biomtk_features_C <- CHE_tk_biom |>
  mutate(BIOM_log =  log1p(x = value)) |>
  mutate(BIOM_std =  standardize_vec(BIOM_log)) |>
  tk_augment_timeseries_signature(.date_var = DATE) |>
  glimpse()

biomtk_features_C

# Perform linear regression    
plot_time_series_regression(.date_var = DATE,
                            .data = biomtk_features_C,
                            .formula = BIOM_std ~ index.num + year,
                                .show_summary = TRUE)


# B) Fourier terms features

biomtk_features_F <- CHE_tk_biom |>
  mutate(BIOM_log =  log1p(x = value)) |>
  mutate(BIOM_std =  standardize_vec(BIOM_log)) |>
  tk_augment_fourier(.date_var = DATE, .periods = 5, .K=1) 

biomtk_features_F

# Perform linear regression
plot_time_series_regression(.date_var = DATE, 
                            .data = biomtk_features_F,
                            .formula = BIOM_std ~ DATE_sin5_K1 + DATE_cos5_K1,
                            .show_summary = TRUE)

# C) Lag features

biomtk_features_L <-  CHE_tk_biom  |>
  mutate(BIOM_log =  log1p(x = value)) |>
  mutate(BIOM_std =  standardize_vec(BIOM_log)) |>
  tk_augment_lags(.value = value, .lags = c(1, 2))  

biomtk_features_L 

# Perform linear regression
plot_time_series_regression(.date_var = DATE, 
                            .data = biomtk_features_L,
                            .formula = BIOM_std ~ value_lag1 + value_lag2,
                            .show_summary = TRUE)

# D) rolling window statistics

biomtk_features_R <-  CHE_tk_biom  |>
  mutate(BIOM_log =  log1p(x = value)) |>
  mutate(BIOM_std =  standardize_vec(BIOM_log)) |>
  tk_augment_slidify(.value   = contains("value"),
                     .f       = ~ mean(.x, na.rm = TRUE), 
                     .period  = c(3, 6),
                     .partial = TRUE,
                     .align   = "center")

biomtk_features_R

# Perform linear regression
plot_time_series_regression(.date_var = DATE, 
                            .data = biomtk_features_R,
                            .formula = BIOM_std ~ value_roll_3 + value_roll_6,
                            .show_summary = TRUE)


# E) put all features together 

biomtk_features_all <- CHE_tk_biom  |>
  mutate(BIOM_log =  log1p(x = value)) |>
  mutate(BIOM_std =  standardize_vec(BIOM_log)) |>
  # Calendar-based (or signature) features
  tk_augment_timeseries_signature(.date_var = DATE) |>
  select(-diff, 
         -matches("(.xts$)|(.iso$)|(hour)|(half)|(quarter)|(month)|(minute)|(second)|(day)|(week)|(am.pm)")) |>
  # Add Fourier features
  tk_augment_fourier(.date_var = DATE, .periods = 5, .K=1) |>
  # Add lag features
  tk_augment_lags(.value = value, .lags = c(1,2)) |>
  # Add rolling window statistics
  tk_augment_slidify(.value   = contains("value"),
                     .f       = ~ mean(.x, na.rm = TRUE), 
                     .period  = c(3, 6),
                     .partial = TRUE,
                     .align   = "center")

biomtk_features_all |>
  glimpse()

plot_time_series_regression(.date_var = DATE, 
                            .data = biomtk_features_all,
                            .formula = BIOM_std ~ index.num + year + 
                              # Fourier features
                              DATE_sin5_K1 + DATE_sin5_K1 + 
                              # lag features
                              value_lag1 + value_lag2 +
                              # rolling window statistics
                              value_roll_3 + value_roll_6,
                            .show_summary = TRUE)

# F) performing rf with tidymodels

library(tidymodels)

rf_spec <- rand_forest(mode = "regression") |>
  set_engine("ranger")

wf <- workflow() |>
  add_model(rf_spec) |>
  add_formula(BIOM_std ~ index.num + year + 
                DATE_sin5_K1 + DATE_sin5_K1 + 
                value_lag1 + value_lag2 +
                value_roll_3 + value_roll_6)

rf_fit <- wf |> fit(data = biomtk_features_all)

rf_pred <- predict(rf_fit, biomtk_features_all) |>
  bind_cols(biomtk_features_all)

ggplot(rf_pred, aes(x = DATE)) +
  geom_line(aes(y = BIOM_std, color = "Actual")) +
  geom_line(aes(y = .pred, color = "RF_pred")) +
  scale_color_manual(values = c("Actual" = "blue", "RF_pred" = "red")) +
  theme_minimal()


# 2) Extracting features with recipes and performing ML
# https://www.r-bloggers.com/2022/01/time-series-forecasting-lab-part-3-machine-learning-with-workflows/

# A) splitting the training/test datasets

n_rows <- nrow(CHE_tk_biom)
train_rows <- round(0.8 * n_rows)

train_data <- CHE_tk_biom |>
  slice(1:train_rows)

test_data <- CHE_tk_biom |>
  slice(train_rows:n_rows)   

# splits <- time_series_split(
#   CHE_tk_biom,
#   date_var   = DATE,
#   assess     = n_rows - round(0.8 * n_rows),
#   cumulative = TRUE
# )
# 
# train_data <- training(splits)
# test_data <- testing(splits) 

ggplot() +
  geom_line(data = train_data, 
            aes(x = DATE, y = value, color = "Training"), 
            linewidth = 1) +
  geom_line(data = test_data, 
            aes(x = DATE, y = value, color = "Test"), 
            linewidth = 1) +
  scale_color_manual(values = c("Training" = "blue", 
                                "Test" = "red")) +
  labs(title = "Training and Test Sets", 
       x = "DATE", y = "BIOM") +
  theme_minimal()

# 2) creating features with recipes
library(recipes)

recipe_spec <- recipe(value ~ ., train_data) |>
  step_timeseries_signature(DATE) |>
  step_rm(DATE) |>
  step_zv(all_predictors()) |>
  step_dummy(all_nominal_predictors(), one_hot = TRUE) |>
  step_naomit(all_predictors())

summary(prep(recipe_spec))

rec <- recipe(value ~., data = train_data) |>
  step_lag(index, DATE, lag = 2:3) |>
  prep(df) |>
  bake(df)

# C) training and evaluating models
# a. Training a boosted tree model
xgb_model <- boost_tree(mode = "regression") |>
  set_engine("xgboost")

xgb_wf <- workflow() |>
  add_model(xgb_model) |>
  add_recipe(recipe_spec)

xgb_fit <- xgb_wf |> fit(train_data)
xgb_fit 

# evaluating model performance

xgb_pred <- predict(xgb_fit, test_data) |>
  bind_cols(test_data)

# Calculating forecast error
xgb_pred |>
  metrics(value, .pred)

xgb_plot <- ggplot() +
  geom_line(data = train_data, 
            aes(x = DATE, y = value, color = "Train"), 
            linewidth = 1) +
  geom_line(data = xgb_pred, 
            aes(x = DATE, y = value, color = "Test"), 
            linewidth = 1) +
  geom_line(data = xgb_pred, 
            aes(x = DATE, y = .pred, color = "Test_pred"), 
            linewidth = 1) +
  scale_color_manual(values = c("Train" = "blue", 
                                "Test" = "red",
                                "Test_pred" ="black")) +
  labs(title = "bt-Train/Test and validation", 
       x = "DATE", y = "BIOMASS") +
  theme_minimal()

xgb_plot

# B) training a random forest model

library(tidymodels)

rf_model <- rand_forest(mode = "regression") |>
  set_engine("ranger")

rf_wf <- workflow() |>
  add_model(rf_model) |>
  add_recipe(recipe_spec)

rf_fit <- rf_wf |> fit(train_data)

# evaluating model performance

rf_pred <- predict(rf_fit, test_data) |>
  bind_cols(test_data)

# Calculating forecast error
rf_pred |> metrics(value, .pred)

rf_plot <- ggplot() +
  geom_line(data = train_data, 
            aes(x = DATE, y = value, color = "Train"), 
            linewidth = 1) +
  geom_line(data = rf_pred, 
            aes(x = DATE, y = value, color = "Test"), 
            linewidth = 1) +
  geom_line(data = rf_pred, 
            aes(x = DATE, y = .pred, color = "Test_pred"), 
            linewidth = 1) +
  scale_color_manual(values = c("Train" = "blue", 
                                "Test" = "red",
                                "Test_pred" ="black")) +
  labs(title = "rf-Train/Test and validation", 
       x = "DATE", y = "BIOMASS") +
  theme_minimal()

rf_plot

library(patchwork)
xgb_plot / rf_plot

# C) comparing among different algorithms

# create a Modeltime Table

model_tbl <- modeltime_table(
  xgb_fit,
  rf_fit
)

model_tbl

# Calibration table

calibrated_tbl <- model_tbl |>
  modeltime_calibrate(new_data = test_data)

calibrated_tbl 

# Model Evaluation

calibrated_tbl |>
  modeltime_accuracy(test_data) |>
  arrange(rmse)

# Forecast Plot

calibrated_tbl |>
  modeltime_forecast(
    new_data    = test_data,
    actual_data = CHE_tk_biom,
    keep_data   = TRUE 
  ) |>
  plot_modeltime_forecast(
    .facet_ncol         = 2, 
    .conf_interval_show = FALSE,
    .interactive        = TRUE
  )

# D) save the work

workflow_Doubs <- list(
  
  workflows = list(
    
    wflw_random_forest = rf_fit,
    wflw_xgboost = xgb_fit
    
  ),
  
  calibration = list(calibration_tbl = calibrated_tbl)
  
)

workflow_Doubs |>
  write_rds("data/tsdata/workflows_Doubs_list.rds")
