# --------------------------------------------
# Script Name: timeseries and forecasting
# Purpose: The script will illustrate how to create
#          a ts object and to build a ml model for 
#          forecasting. 
# 
# Author:     Fanglin Liu
# Email:      flliu315@163.com
# Date:       2026-04-23
#
# --------------------------------------------
cat("\014") # Clears the console
rm(list = ls()) # Remove all variables

############################################
# 01-creating time series and representation
############################################
# https://www.kaggle.com/datasets/vipullrathod/daily-min-temperatures

# A) Load the Minimum Daily Temperatures Dataset
min_temp <- read.csv("data/tsdata/daily-min-temperatures.csv")
head(min_temp) 

min_temp$Date <- as.Date(min_temp$Date) # creating ts
temp_vec <- min_temp$Temp
min_temp_ts <- ts(
  temp_vec,
  start = c(1981, 1),
  frequency = 365
)
plot(min_temp_ts)

# library(ggplot2)
# ggplot(min_temp, aes(x = Date, y = Temp)) +
#   geom_line() +
#   theme_minimal() +
#   labs(
#     title = "Daily Minimum Temperature",
#     x = "Date",
#     y = "Temperature")

library(TSrepr)
temp_vec <- as.numeric(min_temp_ts)
min_temp_ts_paa <- repr_paa(
  temp_vec,
  q = 8,        # 8 segments
  func = mean   # mean for each segment
)
plot(min_temp_ts_paa, type = "l",
     main = "PAA Representation (q = 8)",
     xlab = "Segment",
     ylab = "Mean Temperature")

plot(temp_vec, type = "l", col = "grey",
     main = "Original vs PAA")

##############################################
## 02- the data pre-process of time series 
##############################################
# https://figshare.com/articles/dataset/Data_for_Contemporary_loss_of_genetic_diversity_in_wild_fish_populations_reduces_biomass_stability_over_time_/13095380/9
# download Prunier et al._RawBiomassData.txt (141.03 kB)
# A) raw data of a time series

library(dplyr)
library(tidyr)
library(lubridate)
library(timetk)
library(anomalize)

fishBiom <- read.table("data/tsdata/DOUBS_fishBiomassData.txt", 
                       header = TRUE)
head(fishBiom)

CHE_data <- fishBiom %>% # select station and species
  filter(STATION == "VOLPla", SP == "CHE") %>%
  select(YEAR, BIOMASS) %>%
  arrange(YEAR)

CHE_data

CHE_data_ts <- CHE_data %>% # creating time series
  complete(YEAR = full_seq(YEAR, 1)) %>%
  mutate(DATE = ymd(paste0(YEAR, "-01-01"))) %>%
  arrange(DATE)

CHE_data_ts %>%
  plot_time_series(DATE, BIOMASS,
                   .title = "CHE biomass (raw)",
                   .legend_show = FALSE)

# B) imputing missing data

CHE_data_imputed_ts <- CHE_data_ts %>%
  arrange(DATE) %>%
  mutate(BIOMASS = ts_impute_vec(BIOMASS, period = 1))

CHE_data_imputed_ts %>%
  plot_time_series(DATE, BIOMASS,
                   .title = "CHE Biomass (imputed)")

# C) detecting abnormal values

CHE_data_anomalized_ts <- CHE_data_imputed_ts %>%
  time_decompose(BIOMASS, method = "stl") %>%
  anomalize(remainder) %>%
  time_recompose()

CHE_data_anomalized_ts %>%
  plot_anomalies()

# D) detecting the ACF, PACF and CCF

CHE_data_imputed_ts %>%
  tk_acf_diagnostics(
    .date_var = DATE,
    .value = BIOMASS
  )

CHE_data_imputed_ts %>%
  plot_acf_diagnostics(
    .date_var = DATE,
    .value = BIOMASS
  )

###############################################
## 03- building a model using the rf algorithm
###############################################

library(randomForest)

ts_org <- window(ts, end = 2018)
ts_trf <- ts_org %>% log() %>% diff(1)
lag_order <- 2 # how many past observations
horizon <- 2                                              
ts_mbd <- embed(ts_trf, lag_order + 1)
Y_train <- ts_mbd[, 1] 
X_train <- ts_mbd[, -1] 
y_test <- window(ts, start = 2019, end = 2020) 
x_test <- ts_mbd[nrow(ts_mbd), c(1:lag_order)]
pred_rf <- numeric(horizon)
for (i in 1:horizon){
  set.seed(1) 
  fit_rf <- randomForest(X_train, Y_train) 
  pred_rf[i] <- predict(fit_rf, x_test) 
  Y_train <- Y_train[-1] # training data update
  X_train <- X_train[-nrow(X_train), ] 
}
pred_rf
exp_term <- exp(cumsum(pred_rf)) # Undoes differencing and log-transform
last_obs <- as.vector(tail(ts_org, 1)) 
backtrans_fc <- last_obs * exp_term 
y_pred <- ts(backtrans_fc, start = 2019, frequency = 1)
forecast::accuracy(as.numeric(y_pred), as.numeric(y_test))
library(fpp2)
ts_fc <- cbind(ts,pred = c(rep(NA, length(ts_org)), y_pred)) 
plot_fc <- ts_fc |> autoplot() + theme_minimal() 
plot_fc

