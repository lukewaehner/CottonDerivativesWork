data <- read.csv("~/Desktop/Cotton Stuff/cotton-prices-historical-chart-data.csv")
library(ggplot2)
library(dplyr)
data$date <- as.Date(data$date, format = "%Y-%m-%d")
hanesred <- rgba(255,0,0,255)
#separate data
cull_data <- as.Date("2018-01-01")
ndata <- data %>% filter(date >= cull_data)
data2018 <- head(ndata, 252)
data2019 <- tail(head(ndata, 508),256)
data2020 <- tail(head(ndata, 766), 258)
data2021 <- tail(head(ndata, 1026), 260)
data2022 <- head(tail(ndata, 470), 259)

returns2022 <- diff(data2022$value)/ lag(data2022$value, default = first(data2022$value))
daily_volatility2022 <- sd(returns2022)
annual_volatility2022 <- daily_volatility2022 * sqrt(252)

returns2021 <- diff(data2021$value)/ lag(data2021$value, default = first(data2021$value))
daily_volatility2021 <- sd(returns2021)
annual_volatility2021 <- daily_volatility2021 * sqrt(252)

returns2020 <- diff(data2020$value)/ lag(data2020$value, default = first(data2020$value))
daily_volatility2020 <- sd(returns2020)
annual_volatility2020 <- daily_volatility2020 * sqrt(252)

returns2019 <- diff(data2019$value)/ lag(data2019$value, default = first(data2019$value))
daily_volatility2019 <- sd(returns2019)
annual_volatility2019 <- daily_volatility2019 * sqrt(252)

returns2018 <- diff(data2018$value)/ lag(data2018$value, default = first(data2018$value))
daily_volatility2018 <- sd(returns2018)
annual_volatility2018 <- daily_volatility2018 * sqrt(252)

years <- c(2018,2019,2020,2021,2022)
average_price <- c(mean(data2018$value),mean(data2019$value),mean(data2020$value), 
           + mean(data2021$value), mean(data2022$value))
price_deviation <- c(annual_volatility2018, annual_volatility2019, annual_volatility2020, 
         + annual_volatility2021, annual_volatility2022)
fiveyearframe <- data.frame(years, average_price, price_deviation)
print(fiveyearframe)
data
plot <- ggplot(data, aes(x = date, y = value, group=1)) +
  geom_line() +
  labs(title = "Cotton Price Over Time",
       x = "Date",
       y = "Closing Price") +
        theme_minimal()
y1 <- (mean(fiveyearframe$average_price))
y2 <- (mean(fiveyearframe$average_price)-mean(fiveyearframe$price_deviation))
y3 <- (mean(fiveyearframe$average_price)+mean(fiveyearframe$price_deviation))
y4 <- (mean(fiveyearframe$price_deviation))
print(y1) #mean
print(y2) #lower bound
print(y3) #upper bound
print(y4)
y_values <- c(y1,y2,y3) #vector
plot + geom_hline(yintercept = y_values, linetype="dashed", color = "red") + 
  scale_x_date(date_labels = "%Y", date_breaks = "5 year")

# norm distr code & visualization
CTmean <- y1
CTsd <- mean(fiveyearframe$price_deviation)

#rank normal, quasi-normal 
