macd <- function(data, short_period, long_period, signal_period) {
  # Check input validity
  if (length(data) < long_period) {
    stop("Data length should be greater than or equal to the long_period")
  }
  if (short_period >= long_period) {
    stop("short_period should be less than long_period")
  }
  
  # Calculate the short-term exponential moving average (EMA)
  short_ema <- ema(data, short_period)
  
  # Calculate the long-term exponential moving average (EMA)
  long_ema <- ema(data, long_period)
  
  # Calculate the MACD line (the difference between short_ema and long_ema)
  macd_line <- short_ema - long_ema
  
  # Calculate the signal line (EMA of the MACD line)
  signal_line <- ema(macd_line, signal_period)
  
  # Calculate the histogram (the difference between the MACD line and the signal line)
  histogram <- macd_line - signal_line
  
  # Return the MACD line, signal line, and histogram as a list
  result <- list(
    macd_line = macd_line,
    signal_line = signal_line,
    histogram = histogram
  )
  
  return(result)
}
# Sample data
data <- c(100, 105, 110, 115, 120, 125, 130)

# Calculate MACD with short_period = 3, long_period = 5, and signal_period = 2
macd_result <- macd(data, short_period = 3, long_period = 5, signal_period = 2)
print(macd_result)

# Access individual components
print(macd_result$macd_line)
print(macd_result$signal_line)
print(macd_result$histogram)