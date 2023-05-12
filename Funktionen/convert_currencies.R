convert_currencies <- function(symbols, starting_point=(Sys.Date()-30*365), end_point=Sys.Date()){
  # convert currencies
  data <- get_data(symbols=symbols, starting_point=starting_point, end_point=end_point)
  if ("GSPC.Adjusted" %in% colnames(data)){
    data$GSPC.Adjusted <- data$USDCHF.X.Adjusted * data$GSPC.Adjusted
  }
  if ("BTC.USD.Adjusted" %in% colnames(data)){
    data$BTC.USD.Adjusted <- data$USDCHF.X.Adjusted * data$BTC.USD.Adjusted
  }
  if ("TNX.Adjusted" %in% colnames(data)){
    data$TNX.Adjusted <- data$USDCHF.X.Adjusted * data$TNX.Adjusted
  }
  return(data)
}
