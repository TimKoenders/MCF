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


#### Data Analysis -------------------------------------------------------

# Cumulative results for h = 0
ts_data <- ts(df[, c("Cum_Res_1d", "returns", "CommodityIndex", "VIX", "sentiment_score", "sentiment_score_ws", "art")])
# Fit VAR model
var_model <- VAR(ts_data, p = 3, type = "const")
# Print coefficients
coefficients <- coef(var_model)
residuals <- resid(var_model)
# Print the coefficients 
print(coefficients$Cum_Res_1d)

# Cumulative results for h = 1
ts_data <- ts(df[, c("Cum_Res_2d", "returns", "CommodityIndex", "VIX", "sentiment_score", "sentiment_score_ws", "art")])
# Fit VAR model
var_model <- VAR(ts_data, p = 3, type = "const")
# Print coefficients
coefficients <- coef(var_model)
residuals <- resid(var_model)
# Print the coefficients 
print(coefficients$Cum_Res_2d)

# Cumulative results for h = 2
ts_data <- ts(df[, c("Cum_Res_3d", "returns", "CommodityIndex", "VIX", "sentiment_score", "sentiment_score_ws", "art")])
# Fit VAR model
var_model <- VAR(ts_data, p = 3, type = "const")
# Print coefficients
coefficients <- coef(var_model)
residuals <- resid(var_model)
# Print the coefficients 
print(coefficients$Cum_Res_3d)

# Cumulative results for h = 3
ts_data <- ts(df[, c("Cum_Res_4d", "returns", "CommodityIndex", "VIX", "sentiment_score", "sentiment_score_ws", "art")])
# Fit VAR model
var_model <- VAR(ts_data, p = 3, type = "const")
# Print coefficients
coefficients <- coef(var_model)
residuals <- resid(var_model)
# Print the coefficients 
print(coefficients$Cum_Res_4d)

# Cumulative results for h = 4
ts_data <- ts(df[, c("Cum_Res_5d", "returns", "CommodityIndex", "VIX", "sentiment_score", "sentiment_score_ws", "art")])
# Fit VAR model
var_model <- VAR(ts_data, p = 3, type = "const")
# Print coefficients
coefficients <- coef(var_model)
residuals <- resid(var_model)
# Print the coefficients 
print(coefficients$Cum_Res_5d)


# Cumulative results for h = 5
ts_data <- ts(df[, c("Cum_Res_6d", "returns", "CommodityIndex", "VIX", "sentiment_score", "sentiment_score_ws", "art")])
# Fit VAR model
var_model <- VAR(ts_data, p = 3, type = "const")
# Print coefficients
coefficients <- coef(var_model)
residuals <- resid(var_model)
# Print the coefficients 
print(coefficients$Cum_Res_6d)

