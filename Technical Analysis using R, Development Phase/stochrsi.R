# 1. SMA Function
sma <- function(data, period) {
  if (length(data) < period) {
    stop("Data length should be greater than or equal to the period")
  }
  
  sma_values <- numeric(length(data) - period + 1)
  
  for (i in 1:(length(data) - period + 1)) {
    current_window <- data[i:(i + period - 1)]
    mean_value <- sum(current_window) / period
    sma_values[i] <- mean_value
  }
  
  return(sma_values)
}

# 2. RSI Function
rsi <- function(data, period) {
  if (length(data) < period + 1) {
    stop("Data length should be greater than or equal to period + 1")
  }
  
  diff_values <- diff(data)
  
  gains <- numeric(length(diff_values))
  losses <- numeric(length(diff_values))
  
  for (i in 1:length(diff_values)) {
    if (diff_values[i] > 0) {
      gains[i] <- diff_values[i]
      losses[i] <- 0
    } else {
      gains[i] <- 0
      losses[i] <- abs(diff_values[i])
    }
  }
  
  avg_gain <- mean(gains[1:period])
  avg_loss <- mean(losses[1:period])
  
  rsi_values <- rep(NA, length(data))
  
  for (i in (period + 1):length(data)) {
    avg_gain <- (avg_gain * (period - 1) + gains[i - 1]) / period
    avg_loss <- (avg_loss * (period - 1) + losses[i - 1]) / period
    
    if (avg_loss == 0) {
      rsi_values[i] <- 100
    } else {
      rs <- avg_gain / avg_loss
      rsi_values[i] <- 100 - (100 / (1 + rs))
    }
  }
  
  return(rsi_values)
}

# 3. StochRSI Function
stoch_rsi <- function(data, period, k_period, d_period) {
  if (length(data) < period + 1) {
    stop("Data length should be greater than or equal to period + 1")
  }
  if (k_period < 1 || d_period < 1) {
    stop("k_period and d_period must be at least 1")
  }
  
  rsi_values <- rsi(data, period)
  rsi_clean <- rsi_values[!is.na(rsi_values)]
  
  min_rsi <- min(rsi_clean)
  max_rsi <- max(rsi_clean)
  
  if (max_rsi == min_rsi) {
    k_values <- rep(0.5, length(rsi_values))
  } else {
    k_values <- (rsi_values - min_rsi) / (max_rsi - min_rsi)
  }
  
  k_line <- sma(k_values, k_period)
  d_line <- sma(k_line, d_period)
  
  result <- list(
    k_line = k_line,
    d_line = d_line
  )
  
  return(result)
}

# 4. CREATE LONG DATA (20 data points for period = 14)
long_data <- c(45, 50, 48, 55, 52, 49, 58, 60, 65, 62, 68, 70, 72, 75, 73, 71, 69, 66, 64, 62)

# Check the length
print(paste("Data length:", length(long_data)))  # Should be 20

# 5. NOW CALL WITH period = 14 (this will work!)
stoch_rsi_result <- stoch_rsi(long_data, period = 14, k_period = 3, d_period = 3)
print(stoch_rsi_result)

# 6. Access individual components
print("StochRSI %K Line:")
print(stoch_rsi_result$k_line)

print("StochRSI %D Line:")
print(stoch_rsi_result$d_line)
# Define all functions as above...

# Test with sample data
data <- c(45, 50, 48, 55, 52, 49, 58, 60, 65, 62)

# Calculate StochRSI
result <- stoch_rsi(data, period = 5, k_period = 3, d_period = 3)

# Print results
print("StochRSI %K Line:")
print(result$k_line)

print("StochRSI %D Line:")
print(result$d_line)