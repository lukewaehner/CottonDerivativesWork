#  Stock Price Simulation
# Improved version with better performance, visualization, and analysis

library(ggplot2)
library(dplyr)
library(lubridate)
library(gridExtra)
library(parallel)

# Source configuration and utilities
source("config.R")
source("data_utils.R")

#'  stock price simulation with improved methodology
#' @param historical_data_path Path to historical data
#' @return List containing simulation results and analysis
run__simulation <- function(historical_data_path = RAW_DATA_PATH) {
  
  cat("Starting  stock price simulation...\n")
  
  # Load and preprocess data
  data <- load_cotton_data(historical_data_path, DATA_START_YEAR, DATA_END_YEAR)
  
  # Calculate  parameters
  parameters <- calculate__parameters(data)
  
  # Run multiple simulation scenarios
  simulation_results <- run_multiple_scenarios(parameters)
  
  # Perform comprehensive analysis
  analysis_results <- analyze_simulation_results(simulation_results, data)
  
  # Create  visualizations
  plots <- create_simulation_visualizations(simulation_results, data, analysis_results)
  
  # Generate comprehensive report
  generate_simulation_report(simulation_results, analysis_results)
  
  # Save results
  save_simulation_results(simulation_results, analysis_results)
  
  return(list(
    data = data,
    parameters = parameters,
    simulations = simulation_results,
    analysis = analysis_results,
    plots = plots
  ))
}

#' Calculate  simulation parameters
#' @param data Historical data
#' @return List of calculated parameters
calculate__parameters <- function(data) {
  
  # Calculate returns
  returns <- diff(data$value) / lag(data$value, default = data$value[1])
  returns <- returns[!is.na(returns)]
  
  # Basic parameters
  drift <- mean(returns)
  volatility <- sd(returns)
  initial_price <- tail(data$value, 1)
  
  #  parameters
  parameters <- list(
    # Basic GBM parameters
    initial_price = initial_price,
    drift = drift,
    volatility = volatility,
    
    # Time parameters
    time_period = DEFAULT_TIME_PERIOD,
    dt = 1,  # Daily time step
    
    # Simulation parameters
    num_simulations = DEFAULT_SIMULATIONS,
    
    # Risk parameters
    confidence_levels = CONFIDENCE_LEVELS,
    lower_threshold = LOWER_THRESHOLD,
    upper_threshold = UPPER_THRESHOLD,
    
    # Advanced parameters
    skewness = calculate_skewness(returns),
    kurtosis = calculate_kurtosis(returns),
    autocorrelation = calculate_autocorrelation(returns),
    
    # Risk metrics
    var_95 = quantile(returns, 0.05),
    var_99 = quantile(returns, 0.01),
    expected_shortfall_95 = calculate_expected_shortfall(returns, 0.05),
    expected_shortfall_99 = calculate_expected_shortfall(returns, 0.01)
  )
  
  return(parameters)
}

#' Run multiple simulation scenarios
#' @param parameters Simulation parameters
#' @return List of simulation results
run_multiple_scenarios <- function(parameters) {
  
  scenarios <- list()
  
  # Scenario 1: Standard GBM
  cat("Running standard GBM simulation...\n")
  scenarios$standard_gbm <- simulate__gbm(
    parameters$initial_price,
    parameters$drift,
    parameters$volatility,
    parameters$time_period,
    parameters$num_simulations,
    parameters$confidence_levels
  )
  
  # Scenario 2: Conservative (lower volatility)
  cat("Running conservative scenario...\n")
  conservative_vol <- parameters$volatility * 0.8
  scenarios$conservative <- simulate__gbm(
    parameters$initial_price,
    parameters$drift,
    conservative_vol,
    parameters$time_period,
    parameters$num_simulations,
    parameters$confidence_levels
  )
  
  # Scenario 3: Aggressive (higher volatility)
  cat("Running aggressive scenario...\n")
  aggressive_vol <- parameters$volatility * 1.2
  scenarios$aggressive <- simulate__gbm(
    parameters$initial_price,
    parameters$drift,
    aggressive_vol,
    parameters$time_period,
    parameters$num_simulations,
    parameters$confidence_levels
  )
  
  # Scenario 4: Monte Carlo with different random seeds
  cat("Running Monte Carlo sensitivity analysis...\n")
  scenarios$monte_carlo <- run_monte_carlo_sensitivity(parameters)
  
  return(scenarios)
}

#'  GBM simulation with parallel processing
#' @param initial_price Starting price
#' @param drift Drift parameter
#' @param volatility Volatility parameter
#' @param time_period Number of time steps
#' @param num_simulations Number of simulations
#' @param confidence_levels Confidence levels for intervals
#' @return List containing simulation results
simulate__gbm <- function(initial_price, drift, volatility, 
                                 time_period, num_simulations, 
                                 confidence_levels = c(0.05, 0.25, 0.5, 0.75, 0.95)) {
  
  # Validate inputs
  if (initial_price <= 0) stop("Initial price must be positive")
  if (volatility < 0) stop("Volatility must be non-negative")
  if (num_simulations <= 0) stop("Number of simulations must be positive")
  
  dt <- 1  # Daily time step
  
  # Use parallel processing for large simulations
  if (num_simulations > 500) {
    cat("Using parallel processing for", num_simulations, "simulations...\n")
    simulations <- simulate_parallel_gbm(initial_price, drift, volatility, 
                                       time_period, num_simulations)
  } else {
    simulations <- simulate_serial_gbm(initial_price, drift, volatility, 
                                     time_period, num_simulations)
  }
  
  # Calculate comprehensive statistics
  final_prices <- simulations[time_period, ]
  
  # Confidence intervals
  conf_intervals <- quantile(final_prices, confidence_levels)
  
  # Risk metrics
  risk_metrics <- calculate_risk_metrics(final_prices, initial_price)
  
  # Summary statistics
  summary_stats <- list(
    mean = mean(final_prices),
    median = median(final_prices),
    sd = sd(final_prices),
    min = min(final_prices),
    max = max(final_prices),
    confidence_intervals = conf_intervals,
    risk_metrics = risk_metrics
  )
  
  return(list(
    simulations = simulations,
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

#' Serial GBM simulation
simulate_serial_gbm <- function(initial_price, drift, volatility, time_period, num_simulations) {
  
  # Generate random shocks
  set.seed(42)  # For reproducibility
  random_shocks <- matrix(rnorm(time_period * num_simulations), 
                         nrow = time_period, ncol = num_simulations)
  
  # Calculate log returns
  dt <- 1
  log_returns <- drift * dt + volatility * sqrt(dt) * random_shocks
  
  # Initialize price matrix
  prices <- matrix(initial_price, nrow = time_period, ncol = num_simulations)
  
  # Simulate prices
  for (t in 2:time_period) {
    prices[t, ] <- prices[t-1, ] * exp(log_returns[t, ])
  }
  
  return(prices)
}

#' Parallel GBM simulation
simulate_parallel_gbm <- function(initial_price, drift, volatility, time_period, num_simulations) {
  
  # Split simulations across cores
  n_cores <- min(4, detectCores() - 1)  # Use up to 4 cores
  sims_per_core <- ceiling(num_simulations / n_cores)
  
  # Create cluster
  cl <- makeCluster(n_cores)
  
  # Export required functions and variables
  clusterExport(cl, c("initial_price", "drift", "volatility", "time_period", "sims_per_core"))
  
  # Run parallel simulations
  results <- parLapply(cl, 1:n_cores, function(core_id) {
    set.seed(42 + core_id)  # Different seed for each core
    
    start_sim <- (core_id - 1) * sims_per_core + 1
    end_sim <- min(core_id * sims_per_core, num_simulations)
    n_sims <- end_sim - start_sim + 1
    
    if (n_sims > 0) {
      random_shocks <- matrix(rnorm(time_period * n_sims), 
                             nrow = time_period, ncol = n_sims)
      
      dt <- 1
      log_returns <- drift * dt + volatility * sqrt(dt) * random_shocks
      
      prices <- matrix(initial_price, nrow = time_period, ncol = n_sims)
      
      for (t in 2:time_period) {
        prices[t, ] <- prices[t-1, ] * exp(log_returns[t, ])
      }
      
      return(prices)
    } else {
      return(matrix(nrow = time_period, ncol = 0))
    }
  })
  
  # Stop cluster
  stopCluster(cl)
  
  # Combine results
  all_simulations <- do.call(cbind, results)
  
  return(all_simulations)
}

#' Run Monte Carlo sensitivity analysis
#' @param parameters Simulation parameters
#' @return List of sensitivity results
run_monte_carlo_sensitivity <- function(parameters) {
  
  # Test different random seeds
  seeds <- c(42, 123, 456, 789, 999)
  results <- list()
  
  for (i in seq_along(seeds)) {
    set.seed(seeds[i])
    
    # Run simulation with different seed
    sim_result <- simulate__gbm(
      parameters$initial_price,
      parameters$drift,
      parameters$volatility,
      parameters$time_period,
      parameters$num_simulations,
      parameters$confidence_levels
    )
    
    results[[paste0("seed_", seeds[i])]] <- sim_result
  }
  
  return(results)
}

#' Calculate comprehensive risk metrics
#' @param final_prices Vector of final prices
#' @param initial_price Initial price
#' @return List of risk metrics
calculate_risk_metrics <- function(final_prices, initial_price) {
  
  returns <- (final_prices - initial_price) / initial_price
  
  return(list(
    var_95 = quantile(returns, 0.05),
    var_99 = quantile(returns, 0.01),
    expected_shortfall_95 = calculate_expected_shortfall(returns, 0.05),
    expected_shortfall_99 = calculate_expected_shortfall(returns, 0.01),
    probability_positive = mean(returns > 0),
    probability_negative = mean(returns < 0),
    max_gain = max(returns),
    max_loss = min(returns),
    sharpe_ratio = mean(returns) / sd(returns) * sqrt(252)
  ))
}

#' Calculate expected shortfall (Conditional VaR)
#' @param returns Vector of returns
#' @param confidence_level Confidence level (e.g., 0.05 for 95% VaR)
#' @return Expected shortfall
calculate_expected_shortfall <- function(returns, confidence_level) {
  var_level <- quantile(returns, confidence_level)
  shortfall_returns <- returns[returns <= var_level]
  return(mean(shortfall_returns, na.rm = TRUE))
}

#' Analyze simulation results
#' @param simulation_results List of simulation results
#' @param historical_data Historical data for comparison
#' @return List of analysis results
analyze_simulation_results <- function(simulation_results, historical_data) {
  
  analysis <- list()
  
  # Compare scenarios
  analysis$scenario_comparison <- compare_scenarios(simulation_results)
  
  # Historical vs simulated comparison
  analysis$historical_comparison <- compare_historical_simulated(
    simulation_results$standard_gbm, historical_data
  )
  
  # Risk analysis
  analysis$risk_analysis <- analyze_risk_metrics(simulation_results)
  
  # Sensitivity analysis
  analysis$sensitivity <- analyze_sensitivity(simulation_results$monte_carlo)
  
  return(analysis)
}

#' Compare different scenarios
#' @param simulation_results List of simulation results
#' @return Data frame with scenario comparison
compare_scenarios <- function(simulation_results) {
  
  scenarios <- c("standard_gbm", "conservative", "aggressive")
  comparison <- data.frame()
  
  for (scenario in scenarios) {
    if (scenario %in% names(simulation_results)) {
      result <- simulation_results[[scenario]]
      final_prices <- result$final_prices
      
      comparison <- rbind(comparison, data.frame(
        scenario = scenario,
        mean_price = mean(final_prices),
        median_price = median(final_prices),
        sd_price = sd(final_prices),
        var_95 = quantile(final_prices, 0.05),
        var_99 = quantile(final_prices, 0.01),
        probability_above_threshold = mean(final_prices > LOWER_THRESHOLD),
        probability_below_threshold = mean(final_prices < UPPER_THRESHOLD)
      ))
    }
  }
  
  return(comparison)
}

#' Compare historical vs simulated data
#' @param simulation_result Standard GBM simulation result
#' @param historical_data Historical data
#' @return List of comparison metrics
compare_historical_simulated <- function(simulation_result, historical_data) {
  
  # Calculate historical statistics
  historical_returns <- diff(historical_data$value) / lag(historical_data$value, default = historical_data$value[1])
  historical_returns <- historical_returns[!is.na(historical_returns)]
  
  # Calculate simulated statistics
  simulated_returns <- diff(simulation_result$simulations[, 1]) / lag(simulation_result$simulations[, 1], default = simulation_result$simulations[1, 1])
  simulated_returns <- simulated_returns[!is.na(simulated_returns)]
  
  return(list(
    historical_mean = mean(historical_returns),
    simulated_mean = mean(simulated_returns),
    historical_vol = sd(historical_returns),
    simulated_vol = sd(simulated_returns),
    correlation = cor(historical_returns[1:min(length(historical_returns), length(simulated_returns))], 
                     simulated_returns[1:min(length(historical_returns), length(simulated_returns))])
  ))
}

#' Analyze risk metrics across scenarios
#' @param simulation_results List of simulation results
#' @return Data frame with risk analysis
analyze_risk_metrics <- function(simulation_results) {
  
  risk_analysis <- data.frame()
  
  for (scenario_name in names(simulation_results)) {
    if (scenario_name != "monte_carlo") {  # Skip Monte Carlo sensitivity
      result <- simulation_results[[scenario_name]]
      risk_metrics <- result$summary$risk_metrics
      
      risk_analysis <- rbind(risk_analysis, data.frame(
        scenario = scenario_name,
        var_95 = risk_metrics$var_95,
        var_99 = risk_metrics$var_99,
        expected_shortfall_95 = risk_metrics$expected_shortfall_95,
        expected_shortfall_99 = risk_metrics$expected_shortfall_99,
        probability_positive = risk_metrics$probability_positive,
        max_gain = risk_metrics$max_gain,
        max_loss = risk_metrics$max_loss,
        sharpe_ratio = risk_metrics$sharpe_ratio
      ))
    }
  }
  
  return(risk_analysis)
}

#' Analyze sensitivity to random seeds
#' @param monte_carlo_results Monte Carlo sensitivity results
#' @return Data frame with sensitivity analysis
analyze_sensitivity <- function(monte_carlo_results) {
  
  sensitivity <- data.frame()
  
  for (seed_name in names(monte_carlo_results)) {
    result <- monte_carlo_results[[seed_name]]
    final_prices <- result$final_prices
    
    sensitivity <- rbind(sensitivity, data.frame(
      seed = seed_name,
      mean_price = mean(final_prices),
      sd_price = sd(final_prices),
      var_95 = quantile(final_prices, 0.05),
      var_99 = quantile(final_prices, 0.01)
    ))
  }
  
  return(sensitivity)
}

#' Create comprehensive simulation visualizations
#' @param simulation_results List of simulation results
#' @param historical_data Historical data
#' @param analysis_results Analysis results
#' @return List of ggplot objects
create_simulation_visualizations <- function(simulation_results, historical_data, analysis_results) {
  
  plots <- list()
  
  # 1. Scenario comparison
  plots$scenario_comparison <- create_scenario_comparison_plot(simulation_results)
  
  # 2. Price paths with confidence intervals
  plots$price_paths <- create_price_paths_plot(simulation_results$standard_gbm, historical_data)
  
  # 3. Risk analysis
  plots$risk_analysis <- create_risk_analysis_plot(analysis_results$risk_analysis)
  
  # 4. Sensitivity analysis
  plots$sensitivity <- create_sensitivity_plot(analysis_results$sensitivity)
  
  # 5. Distribution comparison
  plots$distribution <- create_distribution_plot(simulation_results)
  
  # 6. Monte Carlo convergence
  plots$convergence <- create_convergence_plot(simulation_results$standard_gbm)
  
  return(plots)
}

#' Create scenario comparison plot
create_scenario_comparison_plot <- function(simulation_results) {
  
  # Prepare data for plotting
  plot_data <- data.frame()
  
  for (scenario_name in names(simulation_results)) {
    if (scenario_name != "monte_carlo") {
      result <- simulation_results[[scenario_name]]
      final_prices <- result$final_prices
      
      plot_data <- rbind(plot_data, data.frame(
        scenario = scenario_name,
        price = final_prices
      ))
    }
  }
  
  p <- ggplot(plot_data, aes(x = price, fill = scenario)) +
    geom_density(alpha = 0.7) +
    geom_vline(xintercept = LOWER_THRESHOLD, color = "red", linetype = "dashed") +
    geom_vline(xintercept = UPPER_THRESHOLD, color = "green", linetype = "dashed") +
    labs(
      title = "Final Price Distribution by Scenario",
      x = "Final Price ($)",
      y = "Density",
      fill = "Scenario"
    ) +
    theme_minimal() +
    theme(legend.position = "bottom")
  
  return(p)
}

#' Create price paths plot with confidence intervals
create_price_paths_plot <- function(simulation_result, historical_data) {
  
  # Calculate percentiles
  simulations <- simulation_result$simulations
  percentiles <- apply(simulations, 1, function(x) quantile(x, c(0.05, 0.25, 0.5, 0.75, 0.95)))
  
  # Create time series data
  time_points <- 1:nrow(simulations)
  plot_data <- data.frame(
    time = rep(time_points, 5),
    price = as.vector(percentiles),
    percentile = rep(c("5%", "25%", "50%", "75%", "95%"), each = length(time_points))
  )
  
  p <- ggplot() +
    # Add historical data
    geom_line(data = historical_data, 
              aes(x = as.numeric(date - min(date)) + 1, y = value), 
              color = "black", size = 1, alpha = 0.7) +
    
    # Add simulation percentiles
    geom_ribbon(data = plot_data[plot_data$percentile %in% c("5%", "95%"), ], 
                aes(x = time, ymin = price, ymax = price, fill = percentile), 
                alpha = 0.2) +
    
    geom_line(data = plot_data[plot_data$percentile == "50%", ], 
              aes(x = time, y = price), color = "red", size = 1) +
    
    # Add threshold lines
    geom_hline(yintercept = LOWER_THRESHOLD, color = "blue", linetype = "dashed") +
    geom_hline(yintercept = UPPER_THRESHOLD, color = "green", linetype = "dashed") +
    
    labs(
      title = "Cotton Price Simulation: Historical vs Projected",
      subtitle = paste("GBM Simulation with", simulation_result$parameters$num_simulations, "Monte Carlo runs"),
      x = "Time (Days)",
      y = "Price ($)"
    ) +
    theme_minimal()
  
  return(p)
}

#' Create risk analysis plot
create_risk_analysis_plot <- function(risk_analysis) {
  
  p <- ggplot(risk_analysis, aes(x = scenario)) +
    geom_col(aes(y = var_95), fill = "red", alpha = 0.7) +
    geom_col(aes(y = var_99), fill = "darkred", alpha = 0.7) +
    geom_text(aes(y = var_95, label = round(var_95, 3)), vjust = -0.5) +
    labs(
      title = "Value at Risk by Scenario",
      x = "Scenario",
      y = "VaR"
    ) +
    theme_minimal()
  
  return(p)
}

#' Create sensitivity plot
create_sensitivity_plot <- function(sensitivity) {
  
  p <- ggplot(sensitivity, aes(x = seed, y = mean_price)) +
    geom_point(size = 3, color = "blue") +
    geom_errorbar(aes(ymin = mean_price - sd_price, ymax = mean_price + sd_price)) +
    labs(
      title = "Sensitivity Analysis: Mean Price by Random Seed",
      x = "Random Seed",
      y = "Mean Final Price ($)"
    ) +
    theme_minimal()
  
  return(p)
}

#' Create distribution comparison plot
create_distribution_plot <- function(simulation_results) {
  
  # Combine all scenarios
  all_prices <- c()
  all_scenarios <- c()
  
  for (scenario_name in names(simulation_results)) {
    if (scenario_name != "monte_carlo") {
      result <- simulation_results[[scenario_name]]
      all_prices <- c(all_prices, result$final_prices)
      all_scenarios <- c(all_scenarios, rep(scenario_name, length(result$final_prices)))
    }
  }
  
  plot_data <- data.frame(
    price = all_prices,
    scenario = all_scenarios
  )
  
  p <- ggplot(plot_data, aes(x = scenario, y = price)) +
    geom_boxplot(aes(fill = scenario)) +
    geom_hline(yintercept = LOWER_THRESHOLD, color = "red", linetype = "dashed") +
    geom_hline(yintercept = UPPER_THRESHOLD, color = "green", linetype = "dashed") +
    labs(
      title = "Final Price Distribution by Scenario",
      x = "Scenario",
      y = "Final Price ($)"
    ) +
    theme_minimal() +
    theme(legend.position = "none")
  
  return(p)
}

#' Create convergence plot
create_convergence_plot <- function(simulation_result) {
  
  # Calculate running statistics
  simulations <- simulation_result$simulations
  n_sims <- ncol(simulations)
  
  running_means <- sapply(1:n_sims, function(i) mean(simulations[nrow(simulations), 1:i]))
  running_sds <- sapply(1:n_sims, function(i) sd(simulations[nrow(simulations), 1:i]))
  
  plot_data <- data.frame(
    n_simulations = 1:n_sims,
    mean = running_means,
    sd = running_sds
  )
  
  p <- ggplot(plot_data, aes(x = n_simulations)) +
    geom_line(aes(y = mean), color = "blue", size = 1) +
    geom_ribbon(aes(ymin = mean - sd, ymax = mean + sd), alpha = 0.3, fill = "blue") +
    labs(
      title = "Monte Carlo Convergence",
      x = "Number of Simulations",
      y = "Mean Final Price ($)"
    ) +
    theme_minimal()
  
  return(p)
}

#' Generate comprehensive simulation report
#' @param simulation_results List of simulation results
#' @param analysis_results Analysis results
generate_simulation_report <- function(simulation_results, analysis_results) {
  
  cat("\n===  SIMULATION ANALYSIS REPORT ===\n\n")
  
  # Scenario comparison
  cat("SCENARIO COMPARISON:\n")
  print(analysis_results$scenario_comparison)
  
  # Risk analysis
  cat("\nRISK ANALYSIS:\n")
  print(analysis_results$risk_analysis)
  
  # Historical comparison
  cat("\nHISTORICAL vs SIMULATED COMPARISON:\n")
  hist_comp <- analysis_results$historical_comparison
  cat(sprintf("  Historical Mean Return: %.4f\n", hist_comp$historical_mean))
  cat(sprintf("  Simulated Mean Return: %.4f\n", hist_comp$simulated_mean))
  cat(sprintf("  Historical Volatility: %.4f\n", hist_comp$historical_vol))
  cat(sprintf("  Simulated Volatility: %.4f\n", hist_comp$simulated_vol))
  cat(sprintf("  Correlation: %.4f\n", hist_comp$correlation))
  
  # Sensitivity analysis
  cat("\nSENSITIVITY ANALYSIS:\n")
  print(analysis_results$sensitivity)
  
  cat("\n=== END REPORT ===\n\n")
}

#' Save simulation results
#' @param simulation_results List of simulation results
#' @param analysis_results Analysis results
save_simulation_results <- function(simulation_results, analysis_results) {
  
  output_dir <- "../../results/simulations/"
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }
  
  # Save scenario comparison
  write.csv(analysis_results$scenario_comparison, 
            file.path(output_dir, "scenario_comparison.csv"), 
            row.names = FALSE)
  
  # Save risk analysis
  write.csv(analysis_results$risk_analysis, 
            file.path(output_dir, "risk_analysis.csv"), 
            row.names = FALSE)
  
  # Save sensitivity analysis
  write.csv(analysis_results$sensitivity, 
            file.path(output_dir, "sensitivity_analysis.csv"), 
            row.names = FALSE)
  
  # Save standard GBM simulations
  write.csv(simulation_results$standard_gbm$simulations, 
            file.path(output_dir, "_simulations.csv"), 
            row.names = FALSE)
  
  cat("Simulation results saved to", output_dir, "\n")
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

calculate_autocorrelation <- function(x, lag = 1) {
  if (length(x) > lag) {
    cor(x[1:(length(x) - lag)], x[(lag + 1):length(x)], use = "complete.obs")
  } else {
    NA
  }
}

# Main execution
if (!interactive()) {
  result <- run__simulation()
}
