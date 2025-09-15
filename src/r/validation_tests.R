# Validation and Testing Suite for Cotton Derivatives Analysis
# Comprehensive testing of statistical assumptions and model validity

library(testthat)
library(ggplot2)
library(dplyr)

# Source required functions
source("config.R")
source("data_utils.R")
source("simulation.R")

#' Test data loading and preprocessing
test_data_loading <- function() {
  cat("Testing data loading...\n")
  
  # Test data loading
  data <- load_cotton_data(RAW_DATA_PATH, DATA_START_YEAR, DATA_END_YEAR)
  
  # Validate data structure
  expect_true(nrow(data) > 0, "Data should not be empty")
  expect_true("date" %in% names(data), "Data should have date column")
  expect_true("value" %in% names(data), "Data should have value column")
  expect_true(all(!is.na(data$date)), "Date column should not have NAs")
  
  # Test year splitting
  year_data <- split_data_by_year(data)
  expect_true(length(year_data) > 0, "Year data should not be empty")
  
  cat("✓ Data loading tests passed\n")
  return(TRUE)
}

#' Test statistical assumptions
test_statistical_assumptions <- function() {
  cat("Testing statistical assumptions...\n")
  
  # Load data
  data <- load_cotton_data(RAW_DATA_PATH, DATA_START_YEAR, DATA_END_YEAR)
  returns <- diff(data$value) / lag(data$value, default = data$value[1])
  returns <- returns[!is.na(returns)]
  
  # Test normality
  shapiro_test <- shapiro.test(returns)
  cat(sprintf("Shapiro-Wilk normality test p-value: %.4f\n", shapiro_test$p.value))
  
  # Test stationarity (simplified)
  # In practice, you'd use ADF test, but for simplicity:
  mean_return <- mean(returns)
  cat(sprintf("Mean return: %.6f\n", mean_return))
  
  # Test autocorrelation
  acf_test <- Box.test(returns, lag = 10, type = "Ljung-Box")
  cat(sprintf("Ljung-Box autocorrelation test p-value: %.4f\n", acf_test$p.value))
  
  cat("✓ Statistical assumption tests completed\n")
  return(TRUE)
}

#' Test simulation parameters
test_simulation_parameters <- function() {
  cat("Testing simulation parameters...\n")
  
  # Load data
  data <- load_cotton_data(RAW_DATA_PATH, DATA_START_YEAR, DATA_END_YEAR)
  returns <- diff(data$value) / lag(data$value, default = data$value[1])
  returns <- returns[!is.na(returns)]
  
  # Calculate parameters
  drift <- mean(returns)
  volatility <- sd(returns)
  initial_price <- tail(data$value, 1)
  
  # Validate parameters
  expect_true(is.finite(drift), "Drift should be finite")
  expect_true(is.finite(volatility), "Volatility should be finite")
  expect_true(volatility > 0, "Volatility should be positive")
  expect_true(initial_price > 0, "Initial price should be positive")
  
  cat(sprintf("Parameters - Drift: %.4f, Volatility: %.4f, Initial Price: %.2f\n", 
              drift, volatility, initial_price))
  
  cat("✓ Simulation parameter tests passed\n")
  return(TRUE)
}

#' Test simulation function
test_simulation_function <- function() {
  cat("Testing simulation function...\n")
  
  # Test parameters
  initial_price <- 1.0
  drift <- 0.001
  volatility <- 0.02
  time_period <- 10
  num_simulations <- 100
  
  # Run simulation
  result <- simulate_gbm_(initial_price, drift, volatility, 
                                 time_period, num_simulations)
  
  # Validate results
  expect_true(nrow(result$simulations) == time_period, 
              "Simulation should have correct time period")
  expect_true(ncol(result$simulations) == num_simulations, 
              "Simulation should have correct number of simulations")
  expect_true(all(result$simulations > 0), 
              "All simulated prices should be positive")
  
  # Test summary statistics
  expect_true(is.numeric(result$summary$mean), "Mean should be numeric")
  expect_true(is.numeric(result$summary$median), "Median should be numeric")
  expect_true(result$summary$mean > 0, "Mean should be positive")
  
  cat("✓ Simulation function tests passed\n")
  return(TRUE)
}

#' Test data quality
test_data_quality <- function() {
  cat("Testing data quality...\n")
  
  # Load data
  data <- load_cotton_data(RAW_DATA_PATH, DATA_START_YEAR, DATA_END_YEAR)
  
  # Check for missing values
  missing_values <- sum(is.na(data$value))
  cat(sprintf("Missing values: %d\n", missing_values))
  
  # Check for outliers
  outliers <- detect_outliers(data$value)
  outlier_count <- sum(outliers, na.rm = TRUE)
  cat(sprintf("Outliers detected: %d\n", outlier_count))
  
  # Check for negative prices
  negative_prices <- sum(data$value < 0, na.rm = TRUE)
  expect_true(negative_prices == 0, "No negative prices should exist")
  
  # Check date consistency
  date_diff <- diff(data$date)
  expect_true(all(date_diff > 0, na.rm = TRUE), "Dates should be in ascending order")
  
  cat("✓ Data quality tests passed\n")
  return(TRUE)
}

#' Run comprehensive validation
run_validation_suite <- function() {
  cat("=== Running Comprehensive Validation Suite ===\n\n")
  
  tryCatch({
    test_data_loading()
    test_data_quality()
    test_statistical_assumptions()
    test_simulation_parameters()
    test_simulation_function()
    
    cat("\n=== All Tests Passed! ===\n")
    return(TRUE)
  }, error = function(e) {
    cat(sprintf("\n=== Test Failed: %s ===\n", e$message))
    return(FALSE)
  })
}

# Run validation if script is executed directly
if (!interactive()) {
  success <- run_validation_suite()
  if (!success) {
    quit(status = 1)
  }
}
