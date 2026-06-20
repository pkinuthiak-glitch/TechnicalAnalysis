# ============================================================
# TECHNICAL ANALYSIS ASSIGNMENT
# Stock Portfolio Analysis using R
# ============================================================
# Author: Paul Kinuthia
# Date: June 20, 2026
# ============================================================

# Load required libraries
library(quantmod)
library(TTR)

# ============================================================
# 1. STOCK DATA LOADING FUNCTION
# ============================================================

load_stock_data <- function(portfolio_file = "portfolio.txt", 
                            from_date = "2025-01-01", 
                            to_date = Sys.Date()) {
  
  # Read the portfolio file
  if (!file.exists(portfolio_file)) {
    stop("Error: portfolio.txt file not found.")
  }
  
  stocks <- readLines(portfolio_file)
  stocks <- trimws(stocks)
  stocks <- stocks[stocks != ""]
  
  cat("📊 Loading data for", length(stocks), "stocks...\n")
  cat("Stocks:", paste(stocks, collapse = ", "), "\n\n")
  
  stock_data <- list()
  
  for (symbol in stocks) {
    cat("⬇️  Downloading data for", symbol, "... ")
    
    tryCatch({
      getSymbols(symbol, from = from_date, to = to_date, 
                 warnings = FALSE, verbose = FALSE)
      stock_data[[symbol]] <- get(symbol)
      
      cat("✅ Done!\n")
      cat("   - Rows:", nrow(stock_data[[symbol]]), "\n")
      cat("   - Date range:", 
          format(index(stock_data[[symbol]][1, ]), "%Y-%m-%d"), 
          "to", 
          format(index(stock_data[[symbol]][nrow(stock_data[[symbol]]), ]), "%Y-%m-%d"), 
          "\n\n")
      
    }, error = function(e) {
      cat("❌ Failed!\n")
      cat("   Error:", e$message, "\n")
      stock_data[[symbol]] <- NULL
    })
  }
  
  cat("✅ Data loading complete!\n")
  cat("Successfully loaded", length(stock_data), "out of", length(stocks), "stocks.\n")
  
  return(stock_data)
}

# ============================================================
# 2. STATISTICS CALCULATION FUNCTION
# ============================================================

calculate_statistics <- function(stock_data, 
                                 ma_periods = c(20, 50, 200),
                                 include_volume = TRUE) {
  
  if (!is.list(stock_data) || is.data.frame(stock_data)) {
    stop("Input must be a list of stock data")
  }
  
  results <- list()
  
  for (symbol in names(stock_data)) {
    data <- stock_data[[symbol]]
    if (is.null(data) || nrow(data) == 0) next
    
    close_col <- grep("Close|Adjusted", colnames(data), value = TRUE)[1]
    if (is.na(close_col)) next
    
    prices <- as.numeric(data[, close_col])
    prices <- prices[!is.na(prices)]
    
    if (length(prices) == 0) next
    
    stats <- list(
      symbol = symbol,
      n = length(prices),
      mean = mean(prices),
      median = median(prices),
      sd = sd(prices),
      min = min(prices),
      max = max(prices),
      first_price = prices[1],
      last_price = prices[length(prices)],
      total_return = (prices[length(prices)] / prices[1] - 1) * 100
    )
    
    # Moving averages
    stats$moving_averages <- list()
    for (period in ma_periods) {
      if (length(prices) >= period) {
        ma <- SMA(prices, n = period)
        ma <- ma[!is.na(ma)]
        stats$moving_averages[[paste0("MA_", period)]] <- ma
        stats$moving_averages[[paste0("MA_", period, "_latest")]] <- tail(ma, 1)
      }
    }
    
    results[[symbol]] <- stats
    cat("📊 Calculated statistics for", symbol, "\n")
  }
  
  cat("\n✅ Statistics calculated for", length(results), "stocks.\n")
  return(results)
}

# ============================================================
# 3. DISPLAY FUNCTIONS
# ============================================================

display_stock_info <- function(data, symbol = NULL) {
  if (is.null(symbol)) {
    symbol <- deparse(substitute(data))
  }
  
  cat("\n", paste(rep("=", 60), collapse = ""), "\n")
  cat("📊 STOCK INFORMATION:", symbol, "\n")
  cat(paste(rep("=", 60), collapse = ""), "\n\n")
  
  cat("📅 Date Range:    ", format(start(data), "%Y-%m-%d"), 
      "to", format(end(data), "%Y-%m-%d"), "\n")
  cat("📈 Observations:  ", nrow(data), "trading days\n")
  cat("📊 Variables:     ", paste(colnames(data), collapse = ", "), "\n")
  
  close_col <- grep("Close|Adjusted", colnames(data), value = TRUE)[1]
  if (!is.na(close_col)) {
    prices <- as.numeric(data[, close_col])
    cat("\n💰 PRICE SUMMARY:\n")
    cat("   Latest Price:   $", sprintf("%.2f", tail(prices, 1)), "\n")
    cat("   Highest Price:  $", sprintf("%.2f", max(prices)), "\n")
    cat("   Lowest Price:   $", sprintf("%.2f", min(prices)), "\n")
    cat("   Mean Price:     $", sprintf("%.2f", mean(prices)), "\n")
    cat("   Price Change:   ", sprintf("%.2f%%", 
                                       (tail(prices, 1) / prices[1] - 1) * 100), "\n")
  }
}

display_portfolio_summary <- function(portfolio_list) {
  summary_df <- data.frame()
  
  for (symbol in names(portfolio_list)) {
    data <- portfolio_list[[symbol]]
    if (is.null(data) || nrow(data) == 0) next
    
    close_col <- grep("Close|Adjusted", colnames(data), value = TRUE)[1]
    if (is.na(close_col)) next
    
    prices <- as.numeric(data[, close_col])
    
    row <- data.frame(
      Symbol = symbol,
      Days = nrow(data),
      Latest_Price = round(tail(prices, 1), 2),
      Highest_Price = round(max(prices), 2),
      Lowest_Price = round(min(prices), 2),
      Price_Change = round((tail(prices, 1) / prices[1] - 1) * 100, 2)
    )
    summary_df <- rbind(summary_df, row)
  }
  
  cat("\n", paste(rep("=", 70), collapse = ""), "\n")
  cat("📊 PORTFOLIO SUMMARY\n")
  cat(paste(rep("=", 70), collapse = ""), "\n\n")
  print(summary_df, row.names = FALSE)
  
  if (nrow(summary_df) > 0) {
    cat("\n📈 Best Performer:  ", 
        summary_df$Symbol[which.max(summary_df$Price_Change)])
    cat("\n📉 Worst Performer: ", 
        summary_df$Symbol[which.min(summary_df$Price_Change)])
    cat("\n📊 Average Return:  ", 
        round(mean(summary_df$Price_Change), 2), "%\n")
  }
}

display_correlation <- function(portfolio_list) {
  price_matrix <- NULL
  
  for (symbol in names(portfolio_list)) {
    data <- portfolio_list[[symbol]]
    if (is.null(data) || nrow(data) == 0) next
    
    close_col <- grep("Close|Adjusted", colnames(data), value = TRUE)[1]
    if (is.na(close_col)) next
    
    prices <- as.numeric(data[, close_col])
    prices <- prices[!is.na(prices)]
    
    if (length(prices) == 0) next
    
    if (is.null(price_matrix)) {
      price_matrix <- matrix(prices, ncol = 1)
      colnames(price_matrix) <- symbol
    } else {
      price_matrix <- cbind(price_matrix, prices)
      colnames(price_matrix)[ncol(price_matrix)] <- symbol
    }
  }
  
  if (is.null(price_matrix) || ncol(price_matrix) < 2) {
    cat("❌ Not enough data for correlation\n")
    return(NULL)
  }
  
  cor_matrix <- cor(price_matrix, use = "pairwise.complete.obs")
  
  cat("\n", paste(rep("=", 60), collapse = ""), "\n")
  cat("📊 CORRELATION MATRIX\n")
  cat(paste(rep("=", 60), collapse = ""), "\n\n")
  cat("Analyzing", ncol(price_matrix), "stocks with", 
      nrow(price_matrix), "observations\n\n")
  print(round(cor_matrix, 3))
  
  return(cor_matrix)
}

# ============================================================
# END OF SCRIPT
# ============================================================