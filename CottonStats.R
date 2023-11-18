library(ggplot2)

# Type Conversion
data$Year.Open <- as.numeric(sub("\\$", "", data$Year.Open))
data$Year.Close <- as.numeric(sub("\\$", "", data$Year.Close))
data$Year.High <- as.numeric(sub("\\$", "", data$Year.High))
data$Year.Low <- as.numeric(sub("\\$", "", data$Year.Low))
data$Average.Closing.Price <- as.numeric(sub("\\$", "", data$Average.Closing.Price))
# STDEV & Mean
yr_close_stdev <- sd(data$Year.Close)
average_price <- mean(data$Year.Close)

# candlestick
# gg2 <- ggplot(data = data, aes(x = Year)) +
#   geom_segment(aes(xend = Year, y = Year.Low, yend = Year.High), color = "black") +
#   geom_rect(aes(x = Year, xmin = Year - 0.4, xmax = Year + 0.4, ymin = pmin(Year.Open, Year.Close), ymax = pmax(Year.Open, Year.Close), fill = Year.Close > Year.Open), color = "black") +
#   scale_fill_manual(values = c("red", "green")) +
#   theme_minimal()

gg <- ggplot(data = data, aes(x = Year, y = Year.Close)) +
  geom_line()
gg <- gg + geom_line(data = data, aes(x = Year, y= Average.Closing.Price))
gg <- gg + geom_hline(yintercept = average_price+yr_close_stdev, color = "red") +
  geom_hline(yintercept = average_price-yr_close_stdev, color = "red")
# Print the chart
print(gg)

