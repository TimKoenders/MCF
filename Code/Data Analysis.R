#### Set-up and loading data ----------------------------------------------------------------
rm(list = ls())

ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}

packages <- c("dplyr",
              "plyr",                          
              "readr",
              "readxl",
              "tidyverse",
              "ggplot2",
              "stringr",
              "tokenizers",
              "NLP",
              "tm",
              "wordcloud",
              "RColorBrewer",
              "SnowballC",
              "dplyr",
              "malaytextr",
              "vader",
              "schrute",
              "data.table",
              "var"
)

ipak(packages)

setwd("C:/Users/koend/OneDrive/Bureaublad/WU 2023-2024/Courses/Seminar MCF/Project_MCF/MCF/CSV")
list.files()

## Select between raw or imputed data
# df <- read.csv("df_overall.csv")
df <- read.csv("df_overall_imp_sen.csv")


#### VAR and IRF's with baseline sentiment score -------------------------------------------------------

## Cumulative results for h = 1
ts_data <- ts(df[, c("Cum_Res_2d", "returns", "CommodityIndex", "VIX", "sentiment_score", "sentiment_score_ws", "art")])
# Standardize specific variables in ts_data
variables_to_standardize <- c("Cum_Res_2d", "returns", "CommodityIndex", "VIX", "art")
ts_data[, variables_to_standardize] <- scale(ts_data[, variables_to_standardize])
# Fit VAR model for h = 1
var_model <- VAR(ts_data, p = 1, type = "const")
# Print coefficients
coefficients <- coef(var_model)
residuals <- resid(var_model)
# Print the coefficients 
print(coefficients$Cum_Res_2d)
# Compute IRF's
irf_result_1 <- irf(var_model, impulse = "sentiment_score", response = "Cum_Res_2d", n.ahead = 5)
# Plot the impulse responses
plot(irf_result_1, main = "IRF of cumulative returns (h=1) to sentiment score shock", ylab = "Response", xlab = "Time")

## Cumulative results for h = 2
ts_data <- ts(df[, c("Cum_Res_3d", "returns", "CommodityIndex", "VIX", "sentiment_score", "sentiment_score_ws", "art")])
# Standardize specific variables in ts_data
variables_to_standardize <- c("Cum_Res_3d", "returns", "CommodityIndex", "VIX", "art")
ts_data[, variables_to_standardize] <- scale(ts_data[, variables_to_standardize])
# Fit VAR model for h = 2
var_model <- VAR(ts_data, p = 1, type = "const")
# Print coefficients
coefficients <- coef(var_model)
residuals <- resid(var_model)
# Print the coefficients 
print(coefficients$Cum_Res_3d)
# Compute IRF's
irf_result_2 <- irf(var_model, impulse = "sentiment_score", response = "Cum_Res_3d", n.ahead = 5)
# Plot the impulse responses
plot(irf_result_2, main = "IRF of cumulative returns (h=2) to sentiment score shock", ylab = "Response", xlab = "Time")


#### VAR and IRF's with weighted sentiment score -------------------------------------------------------

## Cumulative results for h = 1
ts_data <- ts(df[, c("Cum_Res_2d", "returns", "CommodityIndex", "VIX", "sentiment_score", "sentiment_score_ws", "art")])
# Standardize specific variables in ts_data
variables_to_standardize <- c("Cum_Res_2d", "returns", "CommodityIndex", "VIX", "art")
ts_data[, variables_to_standardize] <- scale(ts_data[, variables_to_standardize])
# Fit VAR model for h = 1
var_model <- VAR(ts_data, p = 1, type = "const")
# Print coefficients
coefficients <- coef(var_model)
residuals <- resid(var_model)
# Print the coefficients 
print(coefficients$Cum_Res_2d)
# Compute IRF's
irf_result_1 <- irf(var_model, impulse = "sentiment_score_ws", response = "Cum_Res_2d", n.ahead = 5)
# Plot the impulse responses
plot(irf_result_1, main = "IRF of cumulative returns (h=1) to weighted sentiment score shock", ylab = "Response", xlab = "Time")

## Cumulative results for h = 2
ts_data <- ts(df[, c("Cum_Res_3d", "returns", "CommodityIndex", "VIX", "sentiment_score", "sentiment_score_ws", "art")])
# Standardize specific variables in ts_data
variables_to_standardize <- c("Cum_Res_3d", "returns", "CommodityIndex", "VIX", "art")
ts_data[, variables_to_standardize] <- scale(ts_data[, variables_to_standardize])
# Fit VAR model for h = 2
var_model <- VAR(ts_data, p = 1, type = "const")
# Print coefficients
coefficients <- coef(var_model)
residuals <- resid(var_model)
# Print the coefficients 
print(coefficients$Cum_Res_3d)
# Compute IRF's
irf_result_2 <- irf(var_model, impulse = "sentiment_score_ws", response = "Cum_Res_3d", n.ahead = 5)
# Plot the impulse responses
plot(irf_result_2, main = "IRF of cumulative returns (h=2) to sentiment score shock", ylab = "Response", xlab = "Time")

