data <- read.csv("~/Desktop/CottonDerivativesWork/cotton-prices-historical-chart-data.csv")
library(ggplot2)
library(dplyr)
data$date <- as.Date(data$date, format = "%Y-%m-%d")

#separate data
cull_data <- as.Date("2018-01-01")
ndata <- data %>% filter(date >= cull_data)
data2018 <- head(ndata, 252)
data2019 <- tail(head(ndata, 508),256)
data2020 <- tail(head(ndata, 766), 258)
data2021 <- tail(head(ndata, 1026), 260)
data2022 <- head(tail(ndata, 470), 259)

returnFrame <- rbind(data2018, data2019, data2020, data2021, data2022)
save(returnFrame, file="~/Desktop/CottonFiveYearFrame.rda")


simulate_stock_prices <- function(initial_price, drift, volatility, time_period, num_simulations) {
  dt <- 1  
  
  # Generate simulations
  simulations <- matrix(0, nrow = time_period, ncol = num_simulations)
  
  for (i in 1:num_simulations) {
    prices <- rep(NA, time_period)
    prices[1] <- initial_price
    
    for (t in 2:time_period) {
      daily_return <- drift * dt + volatility * sqrt(dt) * rnorm(1)
      prices[t] <- prices[t - 1] * exp(daily_return)
    }
    
    simulations[, i] <- prices
  }
  
  colnames(simulations) <- paste("Simulation", 1:num_simulations, sep = "_")
  return(simulations)
}
#end func


#manipulate the data
#cull lower vals
filter_simulations_above_threshold <- function(data, threshold) {
  keep_simulations <- apply(data, 2, function(sim) tail(sim, 1) > threshold)
    filtered_data <- data[, keep_simulations, drop = FALSE]
  
  return(filtered_data)
}

#cull upper val
filter_simulations_below_threshold <- function(data, threshold) {
  keep_simulations <- apply(data, 2, function(sim) tail(sim, 1) < threshold)
  filtered_data <- data[, keep_simulations, drop = FALSE]
  
  return(filtered_data)
}

#editable vals
threshold_value <- 1.16
threshold_value1 <- 1.4
#drift and vol calcs
daily_returns <- diff(returnFrame$value) / lag(returnFrame$value, default = returnFrame$value[1])

# Func Parameters
initial_price <- returnFrame$value[length(returnFrame$value)-1]  
drift <- mean(daily_returns)
volatility <- sd(daily_returns)
time_period <- 259    
num_simulations <- 100

simulated_prices <- simulate_stock_prices(initial_price, drift, volatility, time_period, num_simulations)

filtered_simulations <- filter_simulations_above_threshold(simulated_prices, threshold_value)
filtered_simulations <- filter_simulations_below_threshold(filtered_simulations, threshold_value1)

matplot(simulated_prices, type = "l", col = 1:num_simulations, lty = 1, xlab = "Time", ylab = "Stock Price", main = "Simulated Stock Prices")
legend("topright", legend = colnames(simulated_prices), col = 1:num_simulations, lty = 1)
matplot(filtered_simulations, type = "l", col = 1:num_simulations, lty = 1, xlab = "Time", ylab = "Stock Price", main = "Simulated Stock Prices")
print(ncol(filtered_simulations))
#saving
  #year offset
replace_year_to_2023 <- function(date_list) {
  # Parse the dates
  parsed_dates <- ymd(date_list)
  
  # Replace the year with 2023
  updated_dates <- parsed_dates + years(2023 - year(parsed_dates))
  
  # Format the dates as character
  updated_dates_str <- as.character(updated_dates)
  
  return(updated_dates_str)
}
printYr <- replace_year_to_2023(data2022$date)
print(length(printYr))
finalizedFrame <- cbind(Date = printYr, filtered_simulations)
print(finalizedFrame)
write.csv(finalizedFrame, "~/Desktop/Simulations.csv", row.names=TRUE)


