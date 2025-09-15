#  Cotton Inference Analysis
# Improved version with better statistical methods and visualizations

library(ggplot2)
library(dplyr)
library(lubridate)
library(gridExtra)

# Source configuration and utilities
source("config.R")
source("data_utils.R")

#'  cotton inference analysis
#' @param historical_data_path Path to historical data
#' @return List containing analysis results
analyze_cotton_inference <- function(historical_data_path = RAW_DATA_PATH) {
  
  cat("Starting  cotton inference analysis...\n")
  
  # Load and preprocess data
  data <- load_cotton_data(historical_data_path, DATA_START_YEAR, DATA_END_YEAR)
  
  # Split data by year using utility function
  year_data <- split_data_by_year(data)
  
  # Calculate comprehensive volatility statistics
  volatility_analysis <- calculate_comprehensive_volatility(year_data)
  
  # Perform statistical tests
  statistical_tests <- perform_statistical_tests(data, year_data)
  
  # Create  visualizations
  plots <- create_inference_visualizations(data, year_data, volatility_analysis)
  
  # Generate summary report
  generate_inference_report(volatility_analysis, statistical_tests)
  
  return(list(
    data = data,
    year_data = year_data,
    volatility = volatility_analysis,
    tests = statistical_tests,
    plots = plots
  ))
}

#' Calculate comprehensive volatility analysis
#' @param year_data List of data frames by year
#' @return Data frame with comprehensive volatility statistics
calculate_comprehensive_volatility <- function(year_data) {
  
  results <- data.frame()
  
  for (year_name in names(year_data)) {
    year_df <- year_data[[year_name]]
    
    if (nrow(year_df) > 1) {
      # Calculate returns
      returns <- diff(year_df$value) / lag(year_df$value, default = year_df$value[1])
      returns <- returns[!is.na(returns)]
      
      # Basic statistics
      mean_price <- mean(year_df$value, na.rm = TRUE)
      median_price <- median(year_df$value, na.rm = TRUE)
      daily_vol <- sd(returns, na.rm = TRUE)
      annual_vol <- daily_vol * sqrt(TRADING_DAYS_PER_YEAR)
      
      # Advanced volatility metrics
      mean_return <- mean(returns, na.rm = TRUE)
      skewness <- calculate_skewness(returns)
      kurtosis <- calculate_kurtosis(returns)
      
      # Risk metrics
      sharpe_ratio <- mean_return / daily_vol * sqrt(TRADING_DAYS_PER_YEAR)
      max_drawdown <- calculate_max_drawdown(year_df$value)
      
      # Volatility clustering (GARCH-like)
      volatility_clustering <- calculate_volatility_clustering(returns)
      
      results <- rbind(results, data.frame(
        year = as.numeric(year_name),
        mean_price = mean_price,
        median_price = median_price,
        daily_volatility = daily_vol,
        annual_volatility = annual_vol,
        mean_daily_return = mean_return,
        skewness = skewness,
        kurtosis = kurtosis,
        sharpe_ratio = sharpe_ratio,
        max_drawdown = max_drawdown,
        volatility_clustering = volatility_clustering,
        n_observations = nrow(year_df),
        positive_days = sum(returns > 0, na.rm = TRUE),
        negative_days = sum(returns < 0, na.rm = TRUE)
      ))
    }
  }
  
  return(results)
}

#' Perform comprehensive statistical tests
#' @param data Full dataset
#' @param year_data Data split by year
#' @return List of test results
perform_statistical_tests <- function(data, year_data) {
  
  # Calculate overall returns
  all_returns <- diff(data$value) / lag(data$value, default = data$value[1])
  all_returns <- all_returns[!is.na(all_returns)]
  
  tests <- list()
  
  # Normality tests
  tests$normality <- list(
    shapiro = shapiro.test(all_returns),
    jarque_bera = tryCatch({
      # Simple Jarque-Bera test implementation
      n <- length(all_returns)
      skewness <- calculate_skewness(all_returns)
      kurtosis <- calculate_kurtosis(all_returns)
      jb_stat <- n/6 * (skewness^2 + (kurtosis - 3)^2/4)
      p_value <- 1 - pchisq(jb_stat, df = 2)
      list(statistic = jb_stat, p.value = p_value)
    }, error = function(e) list(statistic = NA, p.value = NA))
  )
  
  # Stationarity tests (simplified)
  tests$stationarity <- list(
    mean_return = mean(all_returns),
    variance_return = var(all_returns),
    trend_test = cor(seq_along(all_returns), all_returns)
  )
  
  # Autocorrelation tests
  tests$autocorrelation <- list(
    ljung_box = Box.test(all_returns, lag = 10, type = "Ljung-Box"),
    durbin_watson = tryCatch({
      # Simple Durbin-Watson test
      residuals <- all_returns - mean(all_returns)
      dw_stat <- sum(diff(residuals)^2) / sum(residuals^2)
      list(statistic = dw_stat)
    }, error = function(e) list(statistic = NA))
  )
  
  # Heteroscedasticity tests
  tests$heteroscedasticity <- list(
    arch_test = tryCatch({
      squared_returns <- all_returns^2
      if (length(squared_returns) > 1) {
        lm_test <- lm(squared_returns[-1] ~ squared_returns[-length(squared_returns)])
        summary(lm_test)$r.squared
      } else NA
    }, error = function(e) NA)
  )
  
  # Year-over-year analysis
  tests$year_comparison <- compare_years_volatility(year_data)
  
  return(tests)
}

#' Compare volatility across years
#' @param year_data List of data frames by year
#' @return ANOVA results
compare_years_volatility <- function(year_data) {
  
  # Prepare data for ANOVA
  volatility_data <- data.frame()
  
  for (year_name in names(year_data)) {
    year_df <- year_data[[year_name]]
    if (nrow(year_df) > 1) {
      returns <- diff(year_df$value) / lag(year_df$value, default = year_df$value[1])
      returns <- returns[!is.na(returns)]
      
      volatility_data <- rbind(volatility_data, data.frame(
        year = as.numeric(year_name),
        volatility = abs(returns)
      ))
    }
  }
  
  if (nrow(volatility_data) > 0) {
    anova_result <- aov(volatility ~ factor(year), data = volatility_data)
    return(summary(anova_result))
  } else {
    return(NULL)
  }
}

#' Create comprehensive inference visualizations
#' @param data Full dataset
#' @param year_data Data split by year
#' @param volatility_analysis Volatility statistics
#' @return List of ggplot objects
create_inference_visualizations <- function(data, year_data, volatility_analysis) {
  
  # 1. Price time series with volatility bands
  p1 <- create_price_volatility_plot(data, volatility_analysis)
  
  # 2. Volatility comparison across years
  p2 <- create_volatility_comparison_plot(volatility_analysis)
  
  # 3. Returns distribution and Q-Q plot
  p3 <- create_returns_distribution_plot(data)
  
  # 4. Volatility clustering visualization
  p4 <- create_volatility_clustering_plot(data)
  
  # 5. Risk-return scatter by year
  p5 <- create_risk_return_plot(volatility_analysis)
  
  # 6. Statistical tests summary
  p6 <- create_statistical_tests_plot(data)
  
  return(list(
    price_volatility = p1,
    volatility_comparison = p2,
    returns_distribution = p3,
    volatility_clustering = p4,
    risk_return = p5,
    statistical_tests = p6
  ))
}

#' Create price time series with volatility bands
create_price_volatility_plot <- function(data, volatility_analysis) {
  
  # Calculate rolling volatility
  data$rolling_vol <- data$value %>%
    diff() %>%
    abs() %>%
    c(NA, .) %>%
    zoo::rollmean(k = 30, na.pad = TRUE, align = "right")
  
  # Calculate volatility bands
  overall_mean <- mean(volatility_analysis$mean_price)
  overall_vol <- mean(volatility_analysis$annual_volatility)
  
  p <- ggplot(data, aes(x = date, y = value)) +
    geom_line(size = 1, color = "blue") +
    geom_ribbon(aes(
      ymin = overall_mean - overall_vol,
      ymax = overall_mean + overall_vol
    ), alpha = 0.2, fill = "red") +
    geom_hline(yintercept = overall_mean, color = "green", linetype = "dashed") +
    labs(
      title = "Cotton Price with Volatility Bands",
      subtitle = paste("Mean: $", round(overall_mean, 2), 
                      "| Annual Vol: ", round(overall_vol * 100, 1), "%"),
      x = "Date",
      y = "Price ($)"
    ) +
    theme_minimal()
  
  return(p)
}

#' Create volatility comparison plot
create_volatility_comparison_plot <- function(volatility_analysis) {
  
  p <- ggplot(volatility_analysis, aes(x = factor(year), y = annual_volatility)) +
    geom_col(fill = "lightblue", color = "black") +
    geom_text(aes(label = round(annual_volatility, 3)), 
              vjust = -0.5, size = 3) +
    labs(
      title = "Annual Volatility Comparison",
      x = "Year",
      y = "Annual Volatility"
    ) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  return(p)
}

#' Create returns distribution plot
create_returns_distribution_plot <- function(data) {
  
  returns <- diff(data$value) / lag(data$value, default = data$value[1])
  returns <- returns[!is.na(returns)]
  
  # Create data frame for plotting
  plot_data <- data.frame(returns = returns)
  
  p <- ggplot(plot_data, aes(x = returns)) +
    geom_histogram(aes(y = ..density..), bins = 30, 
                   fill = "lightblue", color = "black", alpha = 0.7) +
    stat_function(fun = dnorm, 
                  args = list(mean = mean(returns), sd = sd(returns)),
                  color = "red", size = 1) +
    labs(
      title = "Returns Distribution vs Normal",
      x = "Daily Returns",
      y = "Density"
    ) +
    theme_minimal()
  
  return(p)
}

#' Create volatility clustering plot
create_volatility_clustering_plot <- function(data) {
  
  returns <- diff(data$value) / lag(data$value, default = data$value[1])
  returns <- returns[!is.na(returns)]
  
  # Calculate rolling volatility
  rolling_vol <- zoo::rollapply(returns, width = 30, FUN = sd, fill = NA, align = "right")
  
  plot_data <- data.frame(
    date = data$date[-1],  # Remove first date to match returns
    volatility = rolling_vol
  )
  
  p <- ggplot(plot_data, aes(x = date, y = volatility)) +
    geom_line(size = 1, color = "red") +
    labs(
      title = "Volatility Clustering (30-day rolling)",
      x = "Date",
      y = "Volatility"
    ) +
    theme_minimal()
  
  return(p)
}

#' Create risk-return scatter plot
create_risk_return_plot <- function(volatility_analysis) {
  
  p <- ggplot(volatility_analysis, aes(x = annual_volatility, y = mean_daily_return * 252)) +
    geom_point(size = 3, color = "blue") +
    geom_text(aes(label = year), vjust = -0.5, size = 3) +
    geom_smooth(method = "lm", se = TRUE, color = "red", linetype = "dashed") +
    labs(
      title = "Risk-Return Relationship by Year",
      x = "Annual Volatility",
      y = "Annualized Return"
    ) +
    theme_minimal()
  
  return(p)
}

#' Create statistical tests visualization
create_statistical_tests_plot <- function(data) {
  
  returns <- diff(data$value) / lag(data$value, default = data$value[1])
  returns <- returns[!is.na(returns)]
  
  # Q-Q plot
  qq_data <- data.frame(
    theoretical = qnorm(ppoints(length(returns))),
    sample = sort(returns)
  )
  
  p <- ggplot(qq_data, aes(x = theoretical, y = sample)) +
    geom_point(alpha = 0.6) +
    geom_qq_line(color = "red", size = 1) +
    labs(
      title = "Q-Q Plot: Returns vs Normal Distribution",
      x = "Theoretical Quantiles",
      y = "Sample Quantiles"
    ) +
    theme_minimal()
  
  return(p)
}

#' Generate comprehensive inference report
#' @param volatility_analysis Volatility statistics
#' @param statistical_tests Test results
generate_inference_report <- function(volatility_analysis, statistical_tests) {
  
  cat("\n=== COTTON INFERENCE ANALYSIS REPORT ===\n\n")
  
  # Volatility summary
  cat("VOLATILITY ANALYSIS:\n")
  cat(sprintf("  Average Annual Volatility: %.2f%%\n", 
              mean(volatility_analysis$annual_volatility) * 100))
  cat(sprintf("  Volatility Range: %.2f%% - %.2f%%\n", 
              min(volatility_analysis$annual_volatility) * 100,
              max(volatility_analysis$annual_volatility) * 100))
  cat(sprintf("  Average Sharpe Ratio: %.3f\n", 
              mean(volatility_analysis$sharpe_ratio, na.rm = TRUE)))
  
  # Statistical tests summary
  cat("\nSTATISTICAL TESTS:\n")
  cat(sprintf("  Normality (Shapiro-Wilk) p-value: %.4f\n", 
              statistical_tests$normality$shapiro$p.value))
  cat(sprintf("  Autocorrelation (Ljung-Box) p-value: %.4f\n", 
              statistical_tests$autocorrelation$ljung_box$p.value))
  cat(sprintf("  ARCH Effect R²: %.4f\n", 
              statistical_tests$heteroscedasticity$arch_test))
  
  # Interpretation
  cat("\nINTERPRETATION:\n")
  if (statistical_tests$normality$shapiro$p.value > 0.05) {
    cat("  ✓ Returns appear to be normally distributed\n")
  } else {
    cat("  ⚠ Returns may not be normally distributed\n")
  }
  
  if (statistical_tests$autocorrelation$ljung_box$p.value > 0.05) {
    cat("  ✓ No significant autocorrelation detected\n")
  } else {
    cat("  ⚠ Significant autocorrelation detected\n")
  }
  
  if (statistical_tests$heteroscedasticity$arch_test < 0.1) {
    cat("  ✓ No significant ARCH effects\n")
  } else {
    cat("  ⚠ ARCH effects detected (volatility clustering)\n")
  }
  
  cat("\n=== END REPORT ===\n\n")
}

# Utility functions
calculate_skewness <- function(x) {
  n <- length(x)
  mean_x <- mean(x, na.rm = TRUE)
  sd_x <- sd(x, na.rm = TRUE)
  sum((x - mean_x)^3) / (n * sd_x^3)
}

calculate_kurtosis <- function(x) {
  n <- length(x)
  mean_x <- mean(x, na.rm = TRUE)
  sd_x <- sd(x, na.rm = TRUE)
  sum((x - mean_x)^4) / (n * sd_x^4) - 3
}

calculate_max_drawdown <- function(prices) {
  cumulative <- cumprod(1 + c(0, diff(prices) / lag(prices, default = prices[1])))
  running_max <- cummax(cumulative)
  drawdown <- (cumulative - running_max) / running_max
  return(min(drawdown, na.rm = TRUE))
}

calculate_volatility_clustering <- function(returns) {
  # Simple measure of volatility clustering
  abs_returns <- abs(returns)
  if (length(abs_returns) > 1) {
    cor(abs_returns[-1], abs_returns[-length(abs_returns)], use = "complete.obs")
  } else {
    NA
  }
}

# Main execution
if (!interactive()) {
  result <- analyze_cotton_inference()
}
