#Function that calculates portfolio returns
calcPortReturn = function(df, from, to, wght, rebalance, geometric = TRUE){
  
  #Cut dataframe to reflect date range
  df_range = df %>% rownames_to_column("date") %>%
    filter(as.Date(date)>=from & as.Date(date) <= to) %>% column_to_rownames("date")
  
  df_range = xts(df_range, order.by = as.Date(row.names(df_range)))
  indexClass(df_range) <- "Date"
  
  #Create repalace operator
  reb_op = ifelse(rebalance=="Never", NA,
                  ifelse(rebalance=="Annually", "years", 
                         ifelse(rebalance=="Quarterly", "quarters",
                                "months")))
  
  port_ret = Return.portfolio(df_range, weights = wght, geometric = geometric, rebalance_on = reb_op)
  print("PORT RET")
  print(port_ret)
  
  port_ret = data.frame(port_ret)
  
  colnames(port_ret) = c("RetPort")
  
  return (port_ret)
  
}