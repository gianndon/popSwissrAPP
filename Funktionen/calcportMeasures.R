#Function that calculates portfolio performance measures

calcPortMeasures = function (port_ret, benchmark, rf){
  
  mean_rf = 0 #mean(rf)
  print("mean rf")
  print(mean_rf)
  mean_port_ret = mean(port_ret)
  print("mean port ret")
  print(mean_port_ret)
  sd_port_ret = sd(port_ret)
  print("sd port ret")
  print(sd_port_ret)
  
  #Calculate Sharpe
  sharpe = ((mean_port_ret - mean_rf) / sd_port_ret) * sqrt(250)
  
  #Calculate Beta
  mod = lm(formula = port_ret~benchmark)
  beta = summary(mod)$coefficients[2,1]
  
  #Calculate Sortino
  sortino = SortinoRatio(port_ret) * sqrt(250)
  
  #Calculate Taylor
  treynor = ((mean_port_ret - mean_rf)*250)*100/beta
  
  results = list("AvRet"=mean_port_ret * 250, "StDev" = sd_port_ret * sqrt(250),
                 "Sharpe" = sharpe, "Sortino" = sortino[1], "Beta" = beta, "Treynor" = treynor)
  
  return (results)
  
}