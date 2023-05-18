plot_efficient_indiv <- function(dat, amounts, p_year= 260){
  # definitions ----
  yearly_returns <- apply(X=dat*260, MARGIN=2, FUN=mean)
  Sigma <- cov(dat)
  
  
  # compute mvp & tp weight, returns and volatiolites ----
  # mvp
  mvp_s <- mvp_opt(assets=round(dat,3), shorting=TRUE)  # ; mvp_s <- c(mvp_s$mvp_weights, mvp_s$mvp_return, mvp_s$mvp_vola); mvp_s
  mvp_ns <- mvp_opt(assets=round(dat,3), shorting=FALSE)  # ; mvp_ns <- c(mvp_ns$mvp_weights, mvp_ns$mvp_return, mvp_ns$mvp_vola); mvp_ns
  # tp
  tp_n <- tp(assets=dat)  # ; tp_n <- c(tp_n$weights, tp_n$return, tp_n$volatility); tp_n <- unname(tp_n); tp_n
  # individual
  individ <- indiv(assets=dat, amounts=amounts)
  
  
  # generate data frame ----
  dat_cl <- data.frame(MVP_opt_short=mvp_s,
                       MVP_opt_not_short=mvp_ns,
                       TP_normal_function_short=tp_n,
                       individual=individ)
  #row.names(dat_cl) <- c(names(dat[,-2]), "return", "vola"); dat_cl
  
  
  # plot variables for efficient frontier ----
  # define an alpha sequence
  alpha <- seq(-5, 5, 0.01)
  # calculate portfolio weights with different alphas (therefore the %o% = outter product is needed)
  w_pf <- alpha %o% mvp_opt(assets=dat)[1:(nrow(dat_cl)-2)] + (1 - alpha) %o% tp(assets=dat)[1:(nrow(dat_cl)-2)]
  # define the portfolio returns
  return_pf <- w_pf %*% yearly_returns
  
  # define the portfolio volatilities
  volatility_pf <- c()
  for(i in 1:length(alpha)){volatility_pf[i] <- sqrt(t(w_pf[i,]) %*% Sigma %*% w_pf[i,]) * sqrt(p_year)}
  
  # plot of the efficient frontier ----
  plot(x=100*volatility_pf,
       y=100*return_pf,
       type="l",
       col="red",
       xlab="volatilities [%]",
       ylab="returns [%]",
       main="efficient frontier",
       xlim=c(0, (100*dat_cl$individual[nrow(dat_cl)] + 10)),
       ylim=c((100*dat_cl$MVP_opt_short[nrow(dat_cl)-1]) - 30, (100*dat_cl$MVP_opt_short[nrow(dat_cl)-1]) + 30),
       xaxs="i")
  # xlim=c(min(c(100*dat_cl$MVP_opt_short[nrow(dat_cl)],100*dat_cl$TP_normal_function_short[nrow(dat_cl)],100*dat_cl$individual[nrow(dat_cl)],100*dat_cl$TP_normal_function_short[nrow(dat_cl)] ))-5, 
  # 5+max(c(100*dat_cl$MVP_opt_short[nrow(dat_cl)],100*dat_cl$TP_normal_function_short[nrow(dat_cl)],100*dat_cl$individual[nrow(dat_cl)],100*dat_cl$TP_normal_function_short[nrow(dat_cl)] ))),
  # ylim=c(-5-min(c(100*dat_cl$MVP_opt_short[nrow(dat_cl)-1],100*dat_cl$TP_normal_function_short[nrow(dat_cl)-1],100*dat_cl$individual[nrow(dat_cl)-1],100*dat_cl$TP_normal_function_short[nrow(dat_cl)-1] )), 
  # 5+max(c(100*dat_cl$MVP_opt_short[nrow(dat_cl)-1],100*dat_cl$TP_normal_function_short[nrow(dat_cl)-1],100*dat_cl$individual[nrow(dat_cl)-1],100*dat_cl$TP_normal_function_short[nrow(dat_cl)-1] ))))
  legend("topleft", bty="n", legend=c("capital market line", "efficient frontier"), col=c("lightgrey", "red"), lty=c(2, 1))
  
  
  # mvp point shorting ----
  points(x=100*dat_cl$MVP_opt_short[nrow(dat_cl)],
         y=100*dat_cl$MVP_opt_short[nrow(dat_cl)-1],
         col="blue",
         pch=16,
         cex=1.5)
  text(x=100*dat_cl$MVP_opt_short[nrow(dat_cl)],
       y=100*dat_cl$MVP_opt_short[nrow(dat_cl)-1],
       labels=latex2exp::TeX(input="$MVP$"),
       pos=4,
       offset=1,
       col="blue")
  
  
  
  # mvp point not shorting ----
  # points(x=100*dat_cl$MVP_opt_not_short[nrow(dat_cl)],
  #        y=100*dat_cl$MVP_opt_not_short[nrow(dat_cl)-1],
  #        col="orange",
  #        pch=16,
  #        cex=0.75)
  # text(x=100*dat_cl$MVP_opt_not_short[nrow(dat_cl)],
  #      y=100*dat_cl$MVP_opt_not_short[nrow(dat_cl)-1],
  #      labels=latex2exp::TeX(input="$MVP_{not short}$"),
  #      pos=3,
  #      offset=1,
  #      col="orange")
  
  
  
  # tp point ----
  points(100*dat_cl$TP_normal_function_short[nrow(dat_cl)],
         y=100*dat_cl$TP_normal_function_short[nrow(dat_cl)-1],
         col="green",
         pch=16,
         cex=1.5)
  text(x=100*dat_cl$TP_normal_function_short[nrow(dat_cl)],
       y=100*dat_cl$TP_normal_function_short[nrow(dat_cl)-1],
       labels=c("TP"),
       pos=3,
       offset=1,
       col="green")
  
  
  
  # risk free return ----
  points(x=0, y=0.01, col="black", pch=16, cex=1.5)
  text(x=0.25, y=-0.75, col="black", pos=4, labels=latex2exp::TeX(input="$r_{rf}$"))
  # abline(v=0, col="yellow")
  
  
  # draw cpital marekt line
  # lines(x=c(0, 100*dat_cl$TP_normal_function_short[nrow(dat_cl)]),
  #       y=c(0.01, 100*dat_cl$TP_normal_function_short[nrow(dat_cl)-1]),
  #       lty=2)
  delta_y <- (100*dat_cl$TP_normal_function_short[nrow(dat_cl)-1]) - 0.01
  delta_x <- (100*dat_cl$TP_normal_function_short[nrow(dat_cl)]) - 0
  abline(a=0.01,
         b=delta_y/delta_x,
         lty=2,
         color="grey")
  
  
  
  # individual investment ----
  points(x=100*dat_cl$individual[nrow(dat_cl)],
         y=100*dat_cl$individual[nrow(dat_cl)-1],
         col="red", pch=16, cex=1.5)
  text(x=100*dat_cl$individual[nrow(dat_cl)],
       y=100*dat_cl$individual[nrow(dat_cl)-1],
       labels=latex2exp::TeX(input="$Individual$"),
       col="red", pos=1)
  
  
}
