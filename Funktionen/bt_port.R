### Function to perform backtesting

bt_port = function(df, from, to, wght, rebalance){
  
  # Create a dataframe with portfolio and benchmark returns
  print("1")
  
  df_tmp = df %>% mutate(date = as.Date(row.names(df)))
  print("2")
  
  # Portfolio return
  port_ret = data.frame(calcPortReturn(df, from, to, wght, rebalance))
  print("3")
  port_ret$date = as.Date(row.names(port_ret))
  print("4")
  port_ret = rename(port_ret, Portfolio = RetPort)
  print("5")
  # 60/30/10 Portfolio
  sixty_port = data.frame(calcPortReturn(df, from, to,
                                         wght = c(0, 0.6, 0.1, 0, 0, 0.3), rebalance))
  print("6")
  sixty_port$date = as.Date(row.names(sixty_port))
  sixty_port = rename(sixty_port, R60T10C30 = RetPort)
  print("7")
  
  # Merge into one df
  port_ret = merge(port_ret, df_tmp[,c("X.GSPC","date")], by = "date", all.x = TRUE)
  port_ret = merge(port_ret, sixty_port, by = "date", all.x = TRUE)
  print("8")
  
  return(port_ret)
}
