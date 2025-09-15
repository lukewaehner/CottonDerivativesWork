# Master script to run all  R analyses
# Comprehensive cotton derivatives analysis with improved methodology

library(ggplot2)
library(dplyr)
library(lubridate)
library(gridExtra)
library(parallel)

# Source all  modules
source("config.R")
source("data_utils.R")
source("validation_tests.R")
source("CottonStats.R")
source("CottonInference.R")
source("SimulatingStockPrice.R")

#' Run comprehensive  analysis
#' @return List containing all analysis results
run_comprehensive_analysis <- function() {
  
  cat("=== COMPREHENSIVE COTTON DERIVATIVES ANALYSIS ===\n")
  cat("Starting  analysis with improved methodology...\n\n")
  
  # Create output directories
  dirs_to_create <- c(
    "../../results/plots/",
    "../../results/simulations/",
    "../../results/reports/"
  )
  
  for (dir in dirs_to_create) {
    if (!dir.exists(dir)) {
      dir.create(dir, recursive = TRUE)
    }
  }
  
  # Step 1: Run validation tests
  cat("Step 1: Running validation tests...\n")
  validation_success <- run_validation_suite()
  
  if (!validation_success) {
    stop("Validation tests failed. Please check the errors above.")
  }
  cat("✓ Validation tests passed\n\n")
  
  # Step 2:  statistics analysis
  cat("Step 2: Running  statistics analysis...\n")
  stats_result <- analyze_cotton_statistics()
  save_cotton_plots(stats_result$plots)
  cat("✓ Statistics analysis completed\n\n")
  
  # Step 3:  inference analysis
  cat("Step 3: Running  inference analysis...\n")
  inference_result <- analyze_cotton_inference()
  cat("✓ Inference analysis completed\n\n")
  
  # Step 4:  simulation analysis
  cat("Step 4: Running  simulation analysis...\n")
  simulation_result <- run__simulation()
  cat("✓ Simulation analysis completed\n\n")
  
  # Step 5: Generate comprehensive report
  cat("Step 5: Generating comprehensive report...\n")
  generate_comprehensive_report(stats_result, inference_result, simulation_result)
  cat("✓ Comprehensive report generated\n\n")
  
  cat("=== ANALYSIS COMPLETE ===\n")
  cat("Results saved to results/ directory\n")
  cat("Check results/plots/ for visualizations\n")
  cat("Check results/simulations/ for simulation data\n")
  cat("Check results/reports/ for analysis reports\n")
  
  return(list(
    validation = validation_success,
    statistics = stats_result,
    inference = inference_result,
    simulation = simulation_result
  ))
}

#' Generate comprehensive analysis report
#' @param stats_result Statistics analysis results
#' @param inference_result Inference analysis results
#' @param simulation_result Simulation analysis results
generate_comprehensive_report <- function(stats_result, inference_result, simulation_result) {
  
  report_path <- "../../results/reports/comprehensive_analysis_report.md"
  
  # Create comprehensive report
  report_content <- paste0(
    "# Comprehensive Cotton Derivatives Analysis Report\n\n",
    "**Generated on:** ", Sys.time(), "\n\n",
    
    "## Executive Summary\n\n",
    "This report presents a comprehensive analysis of cotton price derivatives using  statistical methods and simulation techniques.\n\n",
    
    "## Key Findings\n\n",
    "### Price Statistics\n",
    "- Mean Close Price: $", round(stats_result$statistics$price$mean_close, 2), "\n",
    "- Annual Volatility: ", round(mean(inference_result$volatility$annual_volatility) * 100, 1), "%\n",
    "- Sharpe Ratio: ", round(mean(inference_result$volatility$sharpe_ratio, na.rm = TRUE), 3), "\n\n",
    
    "### Simulation Results\n",
    "- Standard GBM Mean Final Price: $", round(simulation_result$simulations$standard_gbm$summary$mean, 2), "\n",
    "- Probability Above Lower Threshold: ", round(simulation_result$simulations$standard_gbm$summary$risk_metrics$probability_positive * 100, 1), "%\n",
    "- Value at Risk (95%): ", round(simulation_result$simulations$standard_gbm$summary$risk_metrics$var_95, 3), "\n\n",
    
    "### Risk Assessment\n",
    "- Maximum Drawdown: ", round(mean(inference_result$volatility$max_drawdown, na.rm = TRUE) * 100, 1), "%\n",
    "- Expected Shortfall (95%): ", round(simulation_result$simulations$standard_gbm$summary$risk_metrics$expected_shortfall_95, 3), "\n\n",
    
    "## Methodology\n\n",
    "###  Statistical Methods\n",
    "1. **Monte Carlo Simulation**: Increased from 100 to 1000 simulations\n",
    "2. **Confidence Intervals**: Added 5%, 25%, 50%, 75%, 95% percentiles\n",
    "3. **Risk Metrics**: Implemented VaR, Expected Shortfall, and Sharpe ratios\n",
    "4. **Assumption Validation**: Comprehensive testing of GBM assumptions\n\n",
    
    "### Advanced Visualizations\n",
    "1. **Interactive Dashboards**: 6-panel analysis with technical indicators\n",
    "2. **Risk Analysis**: Comprehensive risk metric visualizations\n",
    "3. **Scenario Analysis**: Multiple simulation scenarios comparison\n",
    "4. **Sensitivity Analysis**: Monte Carlo sensitivity testing\n\n",
    
    "## Recommendations\n\n",
    "1. **Risk Management**: Implement appropriate hedging strategies based on VaR metrics\n",
    "2. **Portfolio Allocation**: Consider cotton derivatives as part of diversified portfolio\n",
    "3. **Monitoring**: Regular review of volatility and risk metrics\n",
    "4. **Model Validation**: Periodic validation against actual market outcomes\n\n",
    
    "## Technical Details\n\n",
    "### Data Sources\n",
    "- Historical cotton prices: 2018-2022\n",
    "- Annual summary data: 1973-2023\n",
    "- Trading days per year: 252\n\n",
    
    "### Model Parameters\n",
    "- Initial Price: $", round(simulation_result$parameters$initial_price, 2), "\n",
    "- Drift: ", round(simulation_result$parameters$drift, 4), "\n",
    "- Volatility: ", round(simulation_result$parameters$volatility, 4), "\n",
    "- Time Period: ", simulation_result$parameters$time_period, " days\n",
    "- Simulations: ", simulation_result$parameters$num_simulations, "\n\n",
    
    "### Output Files\n",
    "-  visualizations: results/plots/\n",
    "- Simulation data: results/simulations/\n",
    "- Statistical summaries: results/reports/\n\n",
    
    "---\n",
    "*Report generated by  Cotton Derivatives Analysis System*"
  )
  
  writeLines(report_content, report_path)
  cat("Comprehensive report saved to:", report_path, "\n")
}

# Main execution
if (!interactive()) {
  results <- run_comprehensive_analysis()
}
