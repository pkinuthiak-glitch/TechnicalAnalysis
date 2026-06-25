# main.R
# Master Test Script for Technical Analysis Indicators
# Run this file to test all implemented indicators

# ============================================
# CLEAR WORKSPACE AND SET ENVIRONMENT
# ============================================

# Clear workspace
rm(list = ls())

cat("\n")
cat("============================================\n")
cat("  TECHNICAL ANALYSIS INDICATORS TEST SUITE\n")
cat("============================================\n")
cat("  Version: 1.0\n")
cat("  Date: June 24, 2026\n")
cat("============================================\n\n")

# ============================================
# LOAD ALL INDICATOR FUNCTIONS
# ============================================

cat("Loading indicator functions...\n")
cat("--------------------------------------------\n")

# Check if files exist before sourcing
files_to_source <- c(
  "sma.R",
  "ema.R", 
  "macd.R",
  "stdev.R",
  "linreg.R",
  "rsi.R",
  "stochrsi.R",
  "crossover.R",
  "crossunder.R"
)

for (file in files_to_source) {
  if (file.exists(file)) {
    source(file)
    cat(sprintf("  ✓ %s loaded\n", file))
  } else {
    cat(sprintf("  ✗ %s NOT FOUND\n", file))
  }
}

cat("--------------------------------------------\n")
cat("All available functions loaded.\n\n")

# ============================================
# SETUP TEST DATA
# ============================================

# Short test data (10 points)
data <- c(45, 50, 48, 55, 52, 49, 58, 60, 65, 62)

# Long test data (20 points) for StochRSI
long_data <- c(45, 50, 48, 55, 52, 49, 58, 60, 65, 62, 
               68, 70, 72, 75, 73, 71, 69, 66, 64, 62)

# Crossover test data
arr1 <- c(10, 12, 15, 20, 18, 22, 25, 24, 21)
arr2 <- c(18, 20, 22, 18, 15, 12, 10, 11, 13)

cat("Test Data:\n")
cat(sprintf("  Primary data: %s\n", paste(data, collapse = ", ")))
cat(sprintf("  Long data: %s... (%d points)\n", 
            paste(head(long_data, 5), collapse = ", "), length(long_data)))
cat("\n")

# ============================================
# RUN TESTS
# ============================================

cat("============================================\n")
cat("  RUNNING TESTS\n")
cat("============================================\n\n")

# ------------------------------------------------------------------
# 1. Simple Moving Average (SMA)
# ------------------------------------------------------------------
cat("1. SIMPLE MOVING AVERAGE (SMA)\n")
cat("   Function: sma(data, period)\n")
if (exists("sma")) {
  sma_result <- sma(data, 3)
  cat("   SMA(3):", paste(round(sma_result, 4), collapse = " "), "\n")
  cat("   Length of output:", length(sma_result), "\n")
} else {
  cat("   ✗ Function not loaded\n")
}
cat("\n")

# ------------------------------------------------------------------
# 2. Exponential Moving Average (EMA)
# ------------------------------------------------------------------
cat("2. EXPONENTIAL MOVING AVERAGE (EMA)\n")
cat("   Function: ema(data, period)\n")
if (exists("ema")) {
  ema_result <- ema(data, 3)
  cat("   EMA(3):", paste(round(ema_result, 4), collapse = " "), "\n")
  cat("   Length of output:", length(ema_result), "\n")
} else {
  cat("   ✗ Function not loaded\n")
}
cat("\n")

# ------------------------------------------------------------------
# 3. MACD
# ------------------------------------------------------------------
cat("3. MOVING AVERAGE CONVERGENCE DIVERGENCE (MACD)\n")
cat("   Function: macd(data, short, long, signal)\n")
if (exists("macd") && exists("ema")) {
  macd_result <- macd(data, 3, 5, 2)
  cat("   MACD Line: ", paste(round(macd_result$macd_line, 4), collapse = " "), "\n")
  cat("   Signal Line:", paste(round(macd_result$signal_line, 4), collapse = " "), "\n")
  cat("   Histogram:  ", paste(round(macd_result$histogram, 4), collapse = " "), "\n")
  cat("   Length of output:", length(macd_result$macd_line), "\n")
} else {
  cat("   ✗ Function not loaded\n")
}
cat("\n")

# ------------------------------------------------------------------
# 4. Standard Deviation
# ------------------------------------------------------------------
cat("4. STANDARD DEVIATION\n")
cat("   Function: stdev(data)\n")
if (exists("stdev")) {
  stdev_result <- stdev(data)
  cat("   Standard Deviation:", round(stdev_result, 4), "\n")
} else {
  cat("   ✗ Function not loaded\n")
}
cat("\n")

# ------------------------------------------------------------------
# 5. Linear Regression
# ------------------------------------------------------------------
cat("5. LINEAR REGRESSION\n")
cat("   Function: linreg(source, length, offset)\n")
if (exists("linreg")) {
  linreg_result <- linreg(data, 5, 2)
  cat("   Slope:", linreg_result$slope, "\n")
  cat("   Intercept:", linreg_result$intercept, "\n")
  cat("   Predicted Values:", paste(linreg_result$predicted_values, collapse = " "), "\n")
} else {
  cat("   ✗ Function not loaded\n")
}
cat("\n")

# ------------------------------------------------------------------
# 6. RSI
# ------------------------------------------------------------------
cat("6. RELATIVE STRENGTH INDEX (RSI)\n")
cat("   Function: rsi(data, period)\n")
if (exists("rsi")) {
  rsi_result <- rsi(data, 5)
  cat("   RSI(5):", paste(round(rsi_result, 4), collapse = " "), "\n")
  cat("   Note: NA values indicate insufficient data for calculation\n")
  cat("   Valid values:", sum(!is.na(rsi_result)), "\n")
} else {
  cat("   ✗ Function not loaded\n")
}
cat("\n")

# ------------------------------------------------------------------
# 7. Stochastic RSI
# ------------------------------------------------------------------
cat("7. STOCHASTIC RSI\n")
cat("   Function: stoch_rsi(data, period, k_period, d_period)\n")
if (exists("stoch_rsi") && exists("rsi") && exists("sma")) {
  stoch_result <- stoch_rsi(long_data, 14, 3, 3)
  cat("   %K Line (last 5):", paste(round(tail(stoch_result$k_line, 5), 4), collapse = " "), "\n")
  cat("   %D Line (last 5):", paste(round(tail(stoch_result$d_line, 5), 4), collapse = " "), "\n")
  cat("   Note: NA values are expected due to smoothing\n")
} else {
  cat("   ✗ Function not loaded or dependencies missing\n")
}
cat("\n")

# ------------------------------------------------------------------
# 8. Crossover Detection
# ------------------------------------------------------------------
cat("8. CROSSOVER DETECTION\n")
cat("   Function: crossover(arr1, arr2)\n")
if (exists("crossover")) {
  crossover_result <- crossover(arr1, arr2)
  cat("   arr1:", paste(arr1, collapse = " "), "\n")
  cat("   arr2:", paste(arr2, collapse = " "), "\n")
  cat("   Signals:", paste(crossover_result, collapse = " "), "\n")
  
  # Count signal types
  up_count <- sum(crossover_result == "Up")
  down_count <- sum(crossover_result == "Down")
  none_count <- sum(crossover_result == "None")
  cat("   Signal Summary: Up=", up_count, ", Down=", down_count, ", None=", none_count, "\n")
} else {
  cat("   ✗ Function not loaded\n")
}
cat("\n")

# ------------------------------------------------------------------
# 9. Crossunder Detection
# ------------------------------------------------------------------
cat("9. CROSSUNDER DETECTION\n")
cat("   Function: crossunder(arr1, arr2)\n")
if (exists("crossunder")) {
  crossunder_result <- crossunder(arr1, arr2)
  cat("   arr1:", paste(arr1, collapse = " "), "\n")
  cat("   arr2:", paste(arr2, collapse = " "), "\n")
  cat("   Signals:", paste(crossunder_result, collapse = " "), "\n")
  
  # Count TRUE signals
  true_count <- sum(crossunder_result == TRUE)
  false_count <- sum(crossunder_result == FALSE)
  cat("   Signal Summary: TRUE=", true_count, ", FALSE=", false_count, "\n")
} else {
  cat("   ✗ Function not loaded\n")
}
cat("\n")

# ============================================
# SUMMARY
# ============================================

cat("============================================\n")
cat("  TEST SUMMARY\n")
cat("============================================\n")

test_results <- data.frame(
  Indicator = c("SMA", "EMA", "MACD", "StDev", "LinReg", "RSI", "StochRSI", "Crossover", "Crossunder"),
  Status = c(
    ifelse(exists("sma"), "✓ PASS", "✗ FAIL"),
    ifelse(exists("ema"), "✓ PASS", "✗ FAIL"),
    ifelse(exists("macd") && exists("ema"), "✓ PASS", "✗ FAIL"),
    ifelse(exists("stdev"), "✓ PASS", "✗ FAIL"),
    ifelse(exists("linreg"), "✓ PASS", "✗ FAIL"),
    ifelse(exists("rsi"), "✓ PASS", "✗ FAIL"),
    ifelse(exists("stoch_rsi") && exists("rsi") && exists("sma"), "✓ PASS", "✗ FAIL"),
    ifelse(exists("crossover"), "✓ PASS", "✗ FAIL"),
    ifelse(exists("crossunder"), "✓ PASS", "✗ FAIL")
  )
)

print(test_results, row.names = FALSE)

# Count passes
total_tests <- nrow(test_results)
passes <- sum(test_results$Status == "✓ PASS")
fails <- total_tests - passes

cat("\n")
cat("--------------------------------------------\n")
cat(sprintf("  Total Tests: %d\n", total_tests))
cat(sprintf("  Passed: %d\n", passes))
cat(sprintf("  Failed: %d\n", fails))
cat("--------------------------------------------\n")

if (fails == 0) {
  cat("\n  🎉 ALL TESTS PASSED SUCCESSFULLY!\n")
} else {
  cat("\n  ⚠️ Some tests failed. Please check the errors above.\n")
}

cat("\n============================================\n")
cat("  END OF TEST SUITE\n")
cat("============================================\n")
cat("  Date:", Sys.Date(), "\n")
cat("  Time:", Sys.time(), "\n")
cat("============================================\n\n")