# load_data.R
# Function to load stock data from portfolio.txt using quantmod

# Load required libraries
library(quantmod)
library(TTR)

# Define the function
load_stock_data <- function(portfolio_file = "portfolio.txt", 
                            from_date = "2025-01-01", 
                            to_date = Sys.Date()) {
  
  # Step 1: Read the portfolio file
  if (!file.exists(portfolio_file)) {
    stop("Error: portfolio.txt file not found in the current directory.")
  }
  
  # Read stock symbols (one per line)
  stocks <- readLines(portfolio_file)
  stocks <- trimws(stocks)  # Remove any extra whitespace
  stocks <- stocks[stocks != ""]  # Remove empty lines
  
  cat("📊 Loading data for", length(stocks), "stocks...\n")
  cat("Stocks:", paste(stocks, collapse = ", "), "\n\n")
  
  # Step 2: Create an empty list to store data frames
  stock_data <- list()
  
  # Step 3: Loop through each stock symbol
  for (symbol in stocks) {
    cat("⬇️  Downloading data for", symbol, "... ")
    
    tryCatch({
      # Get stock data from Yahoo Finance
      getSymbols(symbol, from = from_date, to = to_date, 
                 warnings = FALSE, verbose = FALSE)
      
      # Store the data in the list
      # getSymbols creates a variable with the symbol name
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
  
  # Step 4: Return the list of stock data
  cat("✅ Data loading complete!\n")
  cat("Successfully loaded", length(stock_data), "out of", length(stocks), "stocks.\n")
  
  return(stock_data)
}