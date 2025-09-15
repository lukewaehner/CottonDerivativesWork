# Data utility functions for Cotton Derivatives Analysis

library(dplyr)
library(lubridate)

#' Load and preprocess cotton price data
#' @param file_path Path to the CSV file
#' @param start_year Starting year for analysis
#' @param end_year Ending year for analysis
#' @return Processed data frame with date and value columns
load_cotton_data <- function(file_path, start_year, end_year) {
  # Load data
  data <- read.csv(file_path, stringsAsFactors = FALSE)
  
  # Convert date column
  data$date <- as.Date(data$date, format = "%Y-%m-%d")
  
  # Filter by year range
  data <- data %>%
    filter(year(date) >= start_year & year(date) <= end_year) %>%
    arrange(date)
  
  # Basic data validation
  if (nrow(data) == 0) {
    stop("No data found for the specified year range")
  }
  
  if (any(is.na(data$value))) {
    warning("Missing values found in price data")
  }
  
  return(data)
}

#' Split data by year
#' @param data Data frame with date and value columns
#' @return List of data frames, one for each year
split_data_by_year <- function(data) {
  years <- unique(year(data$date))
  year_data <- list()
  
  for (yr in years) {
    year_data[[as.character(yr)]] <- data %>%
      filter(year(date) == yr) %>%
      arrange(date)
  }
  
  return(year_data)
}

#' Calculate annual volatility for each year
#' @param year_data List of data frames by year
#' @param trading_days Number of trading days per year
#' @return Data frame with year, mean_price, and annual_volatility
calculate_annual_volatility <- function(year_data, trading_days = 252) {
  results <- data.frame()
  
  for (year_name in names(year_data)) {
    year_df <- year_data[[year_name]]
    
    if (nrow(year_df) > 1) {
      # Calculate daily returns
      returns <- diff(year_df$value) / lag(year_df$value, default = year_df$value[1])
      
      # Calculate statistics
      daily_vol <- sd(returns, na.rm = TRUE)
      annual_vol <- daily_vol * sqrt(trading_days)
      mean_price <- mean(year_df$value, na.rm = TRUE)
      
      results <- rbind(results, data.frame(
        year = as.numeric(year_name),
        mean_price = mean_price,
        daily_volatility = daily_vol,
        annual_volatility = annual_vol,
        n_observations = nrow(year_df)
      ))
    }
  }
  
  return(results)
}

#' Detect outliers using IQR method
#' @param data Numeric vector
#' @param factor IQR factor for outlier detection (default 1.5)
#' @return Logical vector indicating outliers
detect_outliers <- function(data, factor = 1.5) {
  Q1 <- quantile(data, 0.25, na.rm = TRUE)
  Q3 <- quantile(data, 0.75, na.rm = TRUE)
  IQR <- Q3 - Q1
  
  lower_bound <- Q1 - factor * IQR
  upper_bound <- Q3 + factor * IQR
  
  return(data < lower_bound | data > upper_bound)
}
