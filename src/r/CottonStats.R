#  Cotton Statistics Analysis
# Improved version with proper data handling and advanced visualizations

library(ggplot2)
library(dplyr)
library(lubridate)
library(gridExtra)

# Source configuration and utilities
source("config.R")
source("data_utils.R")

#'  cotton statistics analysis
#' @param data_path Path to the annual cotton data
#' @return List containing statistics and plots
analyze_cotton_statistics <- function(data_path = ANNUAL_DATA_PATH) {
  
  # Load and preprocess data
  cat("Loading annual cotton data...\n")
  data <- read.csv(data_path, stringsAsFactors = FALSE)
  
  # Clean and convert data
  data <- data %>%
    mutate(
      Year.Open = as.numeric(gsub("\\$", "", Year.Open)),
      Year.Close = as.numeric(gsub("\\$", "", Year.Close)),
      Year.High = as.numeric(gsub("\\$", "", Year.High)),
      Year.Low = as.numeric(gsub("\\$", "", Year.Low)),
      Average.Closing.Price = as.numeric(gsub("\\$", "", Average.Closing.Price)),
      Annual.Change = as.numeric(gsub("%", "", Annual..Change)) / 100
    ) %>%
    filter(!is.na(Year.Close))  # Remove any rows with missing data
  
  # Calculate statistics
  stats <- calculate_cotton_statistics(data)
  
  # Create visualizations
  plots <- create_cotton_visualizations(data, stats)
  
  # Print summary
  print_cotton_summary(stats)
  
  return(list(
    data = data,
    statistics = stats,
    plots = plots
  ))
}

#' Calculate comprehensive cotton statistics
#' @param data Cleaned cotton data
#' @return List of calculated statistics
calculate_cotton_statistics <- function(data) {
  
  # Basic price statistics
  price_stats <- list(
    mean_close = mean(data$Year.Close, na.rm = TRUE),
    median_close = median(data$Year.Close, na.rm = TRUE),
    sd_close = sd(data$Year.Close, na.rm = TRUE),
    min_close = min(data$Year.Close, na.rm = TRUE),
    max_close = max(data$Year.Close, na.rm = TRUE),
    mean_high = mean(data$Year.High, na.rm = TRUE),
    mean_low = mean(data$Year.Low, na.rm = TRUE),
    mean_open = mean(data$Year.Open, na.rm = TRUE)
  )
  
  # Volatility statistics
  price_changes <- data$Year.Close - lag(data$Year.Close, default = data$Year.Close[1])
  volatility_stats <- list(
    mean_annual_change = mean(data$Annual.Change, na.rm = TRUE),
    sd_annual_change = sd(data$Annual.Change, na.rm = TRUE),
    max_annual_gain = max(data$Annual.Change, na.rm = TRUE),
    max_annual_loss = min(data$Annual.Change, na.rm = TRUE),
    positive_years = sum(data$Annual.Change > 0, na.rm = TRUE),
    negative_years = sum(data$Annual.Change < 0, na.rm = TRUE)
  )
  
  # Risk metrics
  risk_metrics <- list(
    sharpe_ratio = mean(data$Annual.Change, na.rm = TRUE) / sd(data$Annual.Change, na.rm = TRUE),
    max_drawdown = calculate_max_drawdown(data$Year.Close),
    volatility_ratio = sd(data$Year.Close, na.rm = TRUE) / mean(data$Year.Close, na.rm = TRUE)
  )
  
  return(list(
    price = price_stats,
    volatility = volatility_stats,
    risk = risk_metrics
  ))
}

#' Create comprehensive cotton visualizations
#' @param data Cleaned cotton data
#' @param stats Calculated statistics
#' @return List of ggplot objects
create_cotton_visualizations <- function(data, stats) {
  
  # 1. Price time series with statistics
  p1 <- ggplot(data, aes(x = Year, y = Year.Close)) +
    geom_line(size = 1.2, color = "blue") +
    geom_line(aes(y = Average.Closing.Price), size = 1, color = "red", linetype = "dashed") +
    geom_hline(yintercept = stats$price$mean_close, color = "green", linetype = "dashed", size = 1) +
    geom_hline(yintercept = stats$price$mean_close + stats$price$sd_close, 
               color = "red", linetype = "dashed", size = 1) +
    geom_hline(yintercept = stats$price$mean_close - stats$price$sd_close, 
               color = "red", linetype = "dashed", size = 1) +
    labs(
      title = "Cotton Price Analysis: Close vs Average",
      subtitle = paste("Mean: $", round(stats$price$mean_close, 2), 
                      "| Std Dev: $", round(stats$price$sd_close, 2)),
      x = "Year",
      y = "Price ($)"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 14, face = "bold"),
      plot.subtitle = element_text(size = 12)
    )
  
  # 2. Candlestick chart
  p2 <- ggplot(data, aes(x = Year)) +
    geom_segment(aes(xend = Year, y = Year.Low, yend = Year.High), 
                 color = "black", size = 1) +
    geom_rect(aes(
      xmin = Year - 0.3, xmax = Year + 0.3,
      ymin = pmin(Year.Open, Year.Close),
      ymax = pmax(Year.Open, Year.Close),
      fill = Year.Close > Year.Open
    ), color = "black", size = 0.5) +
    scale_fill_manual(values = c("red", "green"), 
                     labels = c("Down", "Up"), name = "Daily Change") +
    labs(
      title = "Cotton Price Candlestick Chart",
      x = "Year",
      y = "Price ($)"
    ) +
    theme_minimal() +
    theme(legend.position = "bottom")
  
  # 3. Annual returns distribution
  p3 <- ggplot(data, aes(x = Annual.Change)) +
    geom_histogram(bins = 15, fill = "lightblue", color = "black", alpha = 0.7) +
    geom_vline(xintercept = stats$volatility$mean_annual_change, 
               color = "red", linetype = "dashed", size = 1) +
    geom_vline(xintercept = 0, color = "black", linetype = "solid", size = 1) +
    labs(
      title = "Distribution of Annual Returns",
      subtitle = paste("Mean Return:", round(stats$volatility$mean_annual_change * 100, 1), "%"),
      x = "Annual Return",
      y = "Frequency"
    ) +
    theme_minimal()
  
  # 4. Risk-return scatter plot
  p4 <- ggplot(data, aes(x = Year, y = Annual.Change)) +
    geom_point(size = 3, color = "blue", alpha = 0.7) +
    geom_line(size = 1, color = "blue", alpha = 0.5) +
    geom_hline(yintercept = 0, color = "black", linetype = "dashed") +
    geom_hline(yintercept = stats$volatility$mean_annual_change, 
               color = "red", linetype = "dashed") +
    labs(
      title = "Annual Returns Over Time",
      x = "Year",
      y = "Annual Return"
    ) +
    theme_minimal()
  
  return(list(
    price_analysis = p1,
    candlestick = p2,
    returns_distribution = p3,
    returns_timeline = p4
  ))
}

#' Calculate maximum drawdown
#' @param prices Vector of prices
#' @return Maximum drawdown percentage
calculate_max_drawdown <- function(prices) {
  cumulative <- cumprod(1 + c(0, diff(prices) / lag(prices, default = prices[1])))
  running_max <- cummax(cumulative)
  drawdown <- (cumulative - running_max) / running_max
  return(min(drawdown, na.rm = TRUE))
}

#' Print comprehensive summary
#' @param stats Calculated statistics
print_cotton_summary <- function(stats) {
  cat("\n=== COTTON PRICE ANALYSIS SUMMARY ===\n\n")
  
  cat("PRICE STATISTICS:\n")
  cat(sprintf("  Mean Close Price: $%.2f\n", stats$price$mean_close))
  cat(sprintf("  Median Close Price: $%.2f\n", stats$price$median_close))
  cat(sprintf("  Standard Deviation: $%.2f\n", stats$price$sd_close))
  cat(sprintf("  Min Price: $%.2f\n", stats$price$min_close))
  cat(sprintf("  Max Price: $%.2f\n", stats$price$max_close))
  
  cat("\nVOLATILITY STATISTICS:\n")
  cat(sprintf("  Mean Annual Return: %.2f%%\n", stats$volatility$mean_annual_change * 100))
  cat(sprintf("  Annual Return Std Dev: %.2f%%\n", stats$volatility$sd_annual_change * 100))
  cat(sprintf("  Best Year: %.2f%%\n", stats$volatility$max_annual_gain * 100))
  cat(sprintf("  Worst Year: %.2f%%\n", stats$volatility$max_annual_loss * 100))
  cat(sprintf("  Positive Years: %d\n", stats$volatility$positive_years))
  cat(sprintf("  Negative Years: %d\n", stats$volatility$negative_years))
  
  cat("\nRISK METRICS:\n")
  cat(sprintf("  Sharpe Ratio: %.3f\n", stats$risk$sharpe_ratio))
  cat(sprintf("  Max Drawdown: %.2f%%\n", stats$risk$max_drawdown * 100))
  cat(sprintf("  Volatility Ratio: %.3f\n", stats$risk$volatility_ratio))
  
  cat("\n=== END SUMMARY ===\n\n")
}

#' Save plots to file
#' @param plots List of ggplot objects
#' @param output_dir Output directory
save_cotton_plots <- function(plots, output_dir = "../../results/plots/") {
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }
  
  # Save individual plots
  ggsave(file.path(output_dir, "cotton_price_analysis.png"), 
         plots$price_analysis, width = 12, height = 8, dpi = 300)
  ggsave(file.path(output_dir, "cotton_candlestick.png"), 
         plots$candlestick, width = 12, height = 8, dpi = 300)
  ggsave(file.path(output_dir, "cotton_returns_distribution.png"), 
         plots$returns_distribution, width = 10, height = 6, dpi = 300)
  ggsave(file.path(output_dir, "cotton_returns_timeline.png"), 
         plots$returns_timeline, width = 12, height = 6, dpi = 300)
  
  # Create combined plot
  combined_plot <- grid.arrange(
    plots$price_analysis, plots$candlestick,
    plots$returns_distribution, plots$returns_timeline,
    ncol = 2, nrow = 2
  )
  
  ggsave(file.path(output_dir, "cotton_analysis_combined.png"), 
         combined_plot, width = 16, height = 12, dpi = 300)
  
  cat("Plots saved to", output_dir, "\n")
}

# Main execution
if (!interactive()) {
  result <- analyze_cotton_statistics()
  save_cotton_plots(result$plots)
}
