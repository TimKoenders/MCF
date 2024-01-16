#### Set-up ----------------------------------------------------------------
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
              "zoo"
)

ipak(packages)


setwd("C:/Users/koend/OneDrive/Bureaublad/WU 2023-2024/Courses/Seminar MCF/Project_MCF/MCF/CSV")
list.files()


#### Loading data and merging  ----------------------------------------------------

## Commodity price index
CF <- read.csv("S&P.csv", sep = ",", quote = "\"")
CF$Date <- gsub("\"", "", CF$Date)
head(CF)

# Remove double quotes from character columns
CF$Price <- gsub("\"", "", CF$Price)
CF$Open <- gsub("\"", "", CF$Open)
CF$High <- gsub("\"", "", CF$High)
CF$Low <- gsub("\"", "", CF$Low)
CF$Change <- gsub("\"", "", CF$Change)

# Convert the character columns to appropriate data types
CF$Date <- as.Date(CF$Date, format="%m/%d/%Y")
CF$Price <- as.numeric(gsub(",", "", CF$Price))
CF$Open <- as.numeric(gsub(",", "", CF$Open))
CF$High <- as.numeric(gsub(",", "", CF$High))
CF$Low <- as.numeric(gsub(",", "", CF$Low))
CF$Change <- as.numeric(gsub("%", "", CF$Change)) / 100  # Convert percentage to numeric

# Remove the "Vol" column
CF <- CF[, !names(CF) %in% "Vol"]

## Asset prices 
AP <- read.csv("NASDAQCOM_daily.csv")

# Merge Asset price with commodity price and remove missing observations 
df <- merge(AP, CF, by.x = "DATE", by.y = "Date", all.x = TRUE) %>%
  subset(!is.na(Price))

## Read and harmonize VIX data for merging
vix <- read.csv("VIX_History.csv")
vix$DATE <- as.Date(vix$DATE, format = "%m/%d/%Y")

# Merge data frames
df <- merge(df, vix, by = "DATE", all.x = TRUE)

# Rename columns for clarity
colnames(df)[colnames(df) == "Price"] <- "CommodityIndex"
colnames(df)[colnames(df) == "OPEN"] <- "VIX"

# Subset the data frame to keep only specified columns
df <- df[, c("DATE", "NASDAQCOM", "CommodityIndex", "VIX")]

# View
head(df)

#### Data preparation ----------------------------------------------------
# Convert "NASDAQCOM" to numeric, handling possible commas in the data
df$NASDAQCOM <- as.numeric(gsub(",", "", df$NASDAQCOM))

# Identify and replace non-numeric values with NA
df$NASDAQCOM[!is.numeric(df$NASDAQCOM)] <- NA

## Calculate daily returns
returns <- c(NA, diff(df$NASDAQCOM) / lag(df$NASDAQCOM, default = df$NASDAQCOM[1]))

# Add daily returns to the data frame
df$returns <- returns[1:nrow(df)]

take_for_impute <- df

## Create cumulative responses of returns

# Set the number of days
days <- 8  # 

# Create cumulative response variables
for (i in 1:days) {
  df[[paste0("Cum_Res_", i, "d")]] <- sapply(1:nrow(df), function(row) {
    sum(df$returns[row:min(nrow(df), row + i - 1)], na.rm = TRUE)
  })
}

# Problem: missing days in asset price data are exactly those dates for which we have sentiment scores.
date_sequence <- seq(as.Date(min(df$DATE), format="%Y-%m-%d"), as.Date(max(df$DATE), format="%Y-%m-%d"), by="day")
# Convert 'DATE' column to Date type
df$DATE <- as.Date(df$DATE, format="%Y-%m-%d")

# Perform a left join to fill in missing dates with NA values
df <- data.frame(DATE = date_sequence) %>%
  left_join(df, by = "DATE")

# Now load sentiment scores and merge
scores <- read.csv("scores.csv")
head(scores)
head(df)
df <-merge(df, scores, by.x = "DATE", by.y = "date", all.x = TRUE)

## Create lags of other variables
# Function to create lag variables
create_lag_variables <- function(df, column_name, lags) {
  for (i in 1:lags) {
    df[[paste0(column_name, "_Lag_", i)]] <- lag(df[[column_name]], n = i)
  }
  return(df)
}

# Set the number of lags
lags <- 5

# Create lag variables for CommodityIndex
df_overall <- create_lag_variables(df, "CommodityIndex", lags)

# Create lag variables for VIX
df_overall <- create_lag_variables(df_overall, "VIX", lags)

# Create lag variables for sentiment scores and articles
df_overall <- create_lag_variables(df_overall, "sentiment_score", lags)
df_overall <- create_lag_variables(df_overall, "sentiment_score_ws", lags)
df_overall <- create_lag_variables(df_overall, "art", lags)

#### Write CSV -----------------------------------------------------------
# Define the file path
file_path <- "C:/Users/koend/OneDrive/Bureaublad/WU 2023-2024/Courses/Seminar MCF/Project_MCF/MCF/CSV/df_overall.csv"

# Write the DataFrame to a CSV file
write.csv(df_overall, file = file_path, row.names = FALSE)


#### Imputation ----------------------------------------------------------
## Impute returns and control data

df_no <- take_for_impute

# Specify the columns you want to fill
columns_to_fill <- c("NASDAQCOM", "CommodityIndex", "VIX", "returns")

# Problem: missing days in asset price data are exactly those dates for which we have sentiment scores.
date_sequence <- seq(as.Date(min(df_no$DATE), format="%Y-%m-%d"), as.Date(max(df_no$DATE), format="%Y-%m-%d"), by="day")
# Convert 'DATE' column to Date type
df_no$DATE <- as.Date(df_no$DATE, format="%Y-%m-%d")

# Here we make sure the dataframe has all days and just sets missing value to the empty cells
df_no$DATE <- as.Date(df_no$DATE)
date_df <- data.frame(DATE = as.Date(date_sequence))
df_no <- left_join(date_df, df_no, by = "DATE")

# Print the updated dataframe
head(df_no)

# Apply the zoo::na.locf function to fill missing values
df_no[, columns_to_fill] <- lapply(df_no[, columns_to_fill], function(x) zoo::na.locf(x, na.rm = FALSE))

# Set the number of days
days <- 8  # 

# Create cumulative response variables
for (i in 1:days) {
  df_no[[paste0("Cum_Res_", i, "d")]] <- sapply(1:nrow(df_no), function(row) {
    sum(df_no$returns[row:min(nrow(df_no), row + i - 1)], na.rm = TRUE)
  })
}

# Now load sentiment scores and merge
scores <- read.csv("scores.csv")
head(scores)
head(df_no)
df_no<-merge(df_no, scores, by.x = "DATE", by.y = "date", all.x = TRUE)

## Create lags of other variables
# Function to create lag variables
create_lag_variables <- function(df, column_name, lags) {
  for (i in 1:lags) {
    df[[paste0(column_name, "_Lag_", i)]] <- lag(df[[column_name]], n = i)
  }
  return(df)
}

# Set the number of lags
lags <- 5

# Create lag variables for CommodityIndex
df_overall_imp<- create_lag_variables(df_no, "CommodityIndex", lags)

# Create lag variables for VIX
df_overall_imp <- create_lag_variables(df_overall_imp, "VIX", lags)

# Create lag variables for sentiment scores and articles
df_overall_imp <- create_lag_variables(df_overall_imp, "sentiment_score", lags)
df_overall_imp <- create_lag_variables(df_overall_imp, "sentiment_score_ws", lags)
df_overall_imp <-create_lag_variables(df_overall_imp, "art", lags)

#### Write CSV for imputed values file-----------------------------------------------------------
# Define the file path
file_path <- "C:/Users/koend/OneDrive/Bureaublad/WU 2023-2024/Courses/Seminar MCF/Project_MCF/MCF/CSV/df_overall_imp.csv"

# Write the DataFrame to a CSV file
write.csv(df_overall_imp, file = file_path, row.names = FALSE)




