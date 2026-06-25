rsi <- function(data, period) {
  # Check if the length of data is less than the specified period
  if (length(data) < period + 1) {
    stop("Data length should be greater than or equal to period + 1")
  }
  
  # Calculate the differences between consecutive data points
  diff_values <- diff(data)
  
  # Initialize two vectors to store the gains and losses
  gains <- numeric(length(diff_values))
  losses <- numeric(length(diff_values))
  
  # Calculate gains and losses
  for (i in 1:length(diff_values)) {
    if (diff_values[i] > 0) {
      gains[i] <- diff_values[i]
      losses[i] <- 0
    } else {
      gains[i] <- 0
      losses[i] <- abs(diff_values[i])
    }
  }
  
  # Calculate the average gains and average losses for the first 'period' data points
  avg_gain <- mean(gains[1:period])
  avg_loss <- mean(losses[1:period])
  
  # Initialize the RSI vector with NA values
  rsi_values <- rep(NA, length(data))
  
  # Set the first RSI value (optional - many implementations start with NA)
  # Some implementations set the first RSI value at index period + 1
  
  # Calculate RSI values using the Wilder's smoothing method
  for (i in (period + 1):length(data)) {
    avg_gain <- (avg_gain * (period - 1) + gains[i - 1]) / period
    avg_loss <- (avg_loss * (period - 1) + losses[i - 1]) / period
    
    # Avoid division by zero
    if (avg_loss == 0) {
      rs <- Inf
      rsi_values[i] <- 100
    } else {
      rs <- avg_gain / avg_loss
      rsi_values[i] <- 100 - (100 / (1 + rs))
    }
  }
  
  return(rsi_values)
}
data <- c(45, 50, 48, 55, 52, 49, 58, 60, 65, 62)

# Calculate RSI with a period of 5
rsi_result <- rsi(data, period = 5)
print(rsi_result)