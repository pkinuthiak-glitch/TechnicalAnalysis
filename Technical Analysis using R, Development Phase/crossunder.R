crossunder <- function(arr1, arr2) {
  # Check if the length of both arrays is the same
  if (length(arr1) != length(arr2)) {
    stop("Both arrays should have the same length")
  }
  
  # Initialize a vector to store the crossunder signals
  crossunder_signals <- logical(length(arr1))
  
  # First element always has no crossunder
  crossunder_signals[1] <- FALSE
  
  # Check for crossunder signals at each data point
  for (i in 2:length(arr1)) {
    if (arr1[i] < arr2[i] && arr1[i - 1] >= arr2[i - 1]) {
      crossunder_signals[i] <- TRUE
    } else {
      crossunder_signals[i] <- FALSE
    }
  }
  
  return(crossunder_signals)
}
# Sample data for two arrays
arr1 <- c(10, 12, 15, 20, 18, 22, 25, 24, 21)
arr2 <- c(18, 20, 22, 18, 15, 12, 10, 11, 13)

# Calculate crossunder signals between arr1 and arr2
crossunder_signals <- crossunder(arr1, arr2)
print(crossunder_signals)

# Display with interpretation
for (i in 1:length(crossunder_signals)) {
  signal_text <- ifelse(crossunder_signals[i], "CROSSUNDER", "No signal")
  cat(sprintf("Index %2d: arr1[%d]=%2d, arr2[%d]=%2d, Signal: %s\n", 
              i, i, arr1[i], i, arr2[i], signal_text))
}