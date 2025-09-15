#  Geometric Brownian Motion Simulation
# with improved statistical methods and validation

library(ggplot2)
library(dplyr)
library(lubridate)

# Source configuration and utilities
source("config.R")
source("data_utils.R")

#'  GBM simulation with confidence intervals
#' @param initial_price Starting price
#' @param drift Annual drift rate
#' @param volatility Annual volatility
#' @param time_period Number of time steps
#' @param num_simulations Number of Monte Carlo simulations
#' @param confidence_levels Confidence levels for intervals
#' @return List containing simulations and statistics
simulate_gbm_ <- function(initial_price, drift, volatility, 
                                 time_period, num_simulations, 
                                 confidence_levels = c(0.05, 0.25, 0.5, 0.75, 0.95)) {
  
  # Validate inputs
  if (initial_price <= 0) stop("Initial price must be positive")
  if (volatility < 0) stop("Volatility must be non-negative")
  if (num_simulations <= 0) stop("Number of simulations must be positive")
  
  dt <- 1  # Daily time step
  
  # Generate random shocks
  set.seed(42)  # For reproducibility
  random_shocks <- matrix(rnorm(time_period * num_simulations), 
                         nrow = time_period, ncol = num_simulations)
  
  # Calculate log returns
  log_returns <- drift * dt + volatility * sqrt(dt) * random_shocks
  
  # Initialize price matrix
  prices <- matrix(initial_price, nrow = time_period, ncol = num_simulations)
  
  # Simulate prices
  for (t in 2:time_period) {
    prices[t, ] <- prices[t-1, ] * exp(log_returns[t, ])
  }
  
  # Calculate statistics
  final_prices <- prices[time_period, ]
  
  # Confidence intervals
  conf_intervals <- quantile(final_prices, confidence_levels)
  
  # Summary statistics
  summary_stats <- list(
    mean = mean(final_prices),
    median = median(final_prices),
    sd = sd(final_prices),
    min = min(final_prices),
    max = max(final_prices),
    confidence_intervals = conf_intervals,
    probability_above_threshold = mean(final_prices > LOWER_THRESHOLD),
    probability_below_threshold = mean(final_prices < UPPER_THRESHOLD)
  )
  
  return(list(
    simulations = prices,
    final_prices = final_prices,
    summary = summary_stats,
    parameters = list(
      initial_price = initial_price,
      drift = drift,
      volatility = volatility,
      time_period = time_period,
      num_simulations = num_simulations
    )
  ))
}

#' Validate GBM assumptions
#' @param returns Vector of daily returns
#' @return List of test results
validate_gbm_assumptions <- function(returns) {
  # Remove NA values
  returns <- returns[!is.na(returns)]
  
  # Normality test
  normality_test <- shapiro.test(returns)
  
  # Autocorrelation test
  acf_test <- Box.test(returns, lag = 10, type = "Ljung-Box")
  
  # Heteroscedasticity test
  arch_test <- tryCatch({
    # Simple ARCH test using squared returns
    squared_returns <- returns^2
    lm_test <- lm(squared_returns[-1] ~ squared_returns[-length(squared_returns)])
    summary(lm_test)$r.squared
  }, error = function(e) NA)
  
  return(list(
    normality_pvalue = normality_test$p.value,
    autocorrelation_pvalue = acf_test$p.value,
    arch_r_squared = arch_test,
    is_normal = normality_test$p.value > 0.05,
    no_autocorrelation = acf_test$p.value > 0.05
  ))
}

#' Create  visualization
#' @param simulation_result Result from simulate_gbm_
#' @param historical_data Historical price data
#' @return ggplot object
create__plot <- function(simulation_result, historical_data) {
  # Prepare simulation data for plotting
  sim_data <- simulation_result$simulations
  time_points <- 1:nrow(sim_data)
  
  # Calculate percentiles
  percentiles <- apply(sim_data, 1, function(x) quantile(x, c(0.05, 0.25, 0.5, 0.75, 0.95)))
  
  # Create data frame for plotting
  plot_data <- data.frame(
    time = rep(time_points, 5),
    value = as.vector(percentiles),
    percentile = rep(c("5%", "25%", "50%", "75%", "95%"), each = length(time_points))
  )
  
  # Create the plot
  p <- ggplot() +
    # Add historical data
    geom_line(data = historical_data, aes(x = as.numeric(date - min(date)) + 1, y = value), 
              color = "black", alpha = 0.7, size = 1) +
    
    # Add simulation percentiles
    geom_ribbon(data = plot_data[plot_data$percentile %in% c("5%", "95%"), ], 
                aes(x = time, ymin = value, ymax = value, fill = percentile), alpha = 0.2) +
    
    geom_line(data = plot_data[plot_data$percentile == "50%", ], 
              aes(x = time, y = value), color = "red", size = 1) +
    
    # Add threshold lines
    geom_hline(yintercept = LOWER_THRESHOLD, color = "blue", linetype = "dashed", size = 1) +
    geom_hline(yintercept = UPPER_THRESHOLD, color = "green", linetype = "dashed", size = 1) +
    
    labs(
      title = "Cotton Price Simulation: Historical vs Projected",
      subtitle = paste("GBM Simulation with", simulation_result$parameters$num_simulations, "Monte Carlo runs"),
      x = "Time (Days)",
      y = "Price ($)",
      caption = paste("Drift:", round(simulation_result$parameters$drift, 4), 
                     "| Volatility:", round(simulation_result$parameters$volatility, 4))
    ) +
    
    theme_minimal() +
    theme(
      plot.title = element_text(size = 14, face = "bold"),
      plot.subtitle = element_text(size = 12),
      legend.position = "bottom"
    )
  
  return(p)
}

#' Main analysis function
run__analysis <- function() {
  cat("Starting  Cotton Derivatives Analysis...\n")
  
  # Load and preprocess data
  cat("Loading data...\n")
  data <- load_cotton_data(RAW_DATA_PATH, DATA_START_YEAR, DATA_END_YEAR)
  
  # Split by year and calculate statistics
  year_data <- split_data_by_year(data)
  volatility_stats <- calculate_annual_volatility(year_data, TRADING_DAYS_PER_YEAR)
  
  # Calculate overall parameters
  all_returns <- diff(data$value) / lag(data$value, default = data$value[1])
  all_returns <- all_returns[!is.na(all_returns)]
  
  # Validate assumptions
  cat("Validating GBM assumptions...\n")
  assumptions <- validate_gbm_assumptions(all_returns)
  print(assumptions)
  
  # Calculate parameters
  drift <- mean(all_returns)
  volatility <- sd(all_returns)
  initial_price <- tail(data$value, 1)
  
  cat(sprintf("Parameters - Drift: %.4f, Volatility: %.4f, Initial Price: %.2f\n", 
              drift, volatility, initial_price))
  
  # Run simulation
  cat("Running GBM simulation...\n")
  simulation_result <- simulate_gbm_(
    initial_price, drift, volatility, 
    DEFAULT_TIME_PERIOD, DEFAULT_SIMULATIONS, 
    CONFIDENCE_LEVELS
  )
  
  # Print summary
  cat("\nSimulation Summary:\n")
  cat(sprintf("Mean final price: $%.2f\n", simulation_result$summary$mean))
  cat(sprintf("Median final price: $%.2f\n", simulation_result$summary$median))
  cat(sprintf("Standard deviation: $%.2f\n", simulation_result$summary$sd))
  cat(sprintf("Probability above $%.2f: %.1f%%\n", 
              LOWER_THRESHOLD, simulation_result$summary$probability_above_threshold * 100))
  cat(sprintf("Probability below $%.2f: %.1f%%\n", 
              UPPER_THRESHOLD, simulation_result$summary$probability_below_threshold * 100))
  
  # Create  plot
  cat("Creating visualization...\n")
  p <- create__plot(simulation_result, data)
  
  # Save results
  if (!dir.exists(OUTPUT_PATH)) {
    dir.create(OUTPUT_PATH, recursive = TRUE)
  }
  
  # Save plot
  ggsave(file.path(OUTPUT_PATH, "_simulation_plot.png"), 
         plot = p, width = PLOT_WIDTH, height = PLOT_HEIGHT, dpi = PLOT_DPI)
  
  # Save simulation data
  write.csv(simulation_result$simulations, 
            file.path(OUTPUT_PATH, "_simulations.csv"), 
            row.names = FALSE)
  
  # Save summary
  summary_df <- data.frame(
    metric = names(simulation_result$summary)[1:6],
    value = unlist(simulation_result$summary[1:6])
  )
  write.csv(summary_df, 
            file.path(OUTPUT_PATH, "simulation_summary.csv"), 
            row.names = FALSE)
  
  cat("Analysis complete! Results saved to", OUTPUT_PATH, "\n")
  
  return(simulation_result)
}

# Run the analysis if this script is executed directly
if (!interactive()) {
  result <- run__analysis()
}
