# Exponential Moving Average (EMA) function
ema <- function(data, period) {
  # Check if the length of data is less than the specified period
  if (length(data) < period) {
    stop("Data length should be greater than or equal to the period")
  }
  
  # Calculate the multiplier for EMA
  multiplier <- 2 / (period + 1)
  
  # Initialize an empty array to store EMA values
  ema_values <- numeric(length(data))
  
  # Loop through the data array
  for (i in 1:length(data)) {
    # Calculate EMA for the first data point
    if (i == 1) {
      ema_values[i] <- data[i]
    } else {
      # Calculate EMA for subsequent data points
      ema_values[i] <- (data[i] - ema_values[i - 1]) * multiplier + ema_values[i - 1]
    }
  }
  
  return(ema_values)
}# Sample data
data <- c(10, 12, 15, 20, 18, 22, 25, 24, 21)

# Calculate EMA with a period of 3
ema_result <- ema(data, period = 3)
print(ema_result)

# Manual calculation verification for period 3:
# multiplier = 2 / (3 + 1) = 0.5
# EMA(1) = 10
# EMA(2) = (12 - 10) * 0.5 + 10 = 11
# EMA(3) = (15 - 11) * 0.5 + 11 = 13
# EMA(4) = (20 - 13) * 0.5 + 13 = 16.5
# ... and so on
