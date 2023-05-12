plot_efficient_indiv <- function(dat, amounts){
  # definitions ----
  yearly_returns <- apply(X=dat*260, MARGIN=2, FUN=mean)
  Sigma <- cov(dat)
  
  
  # compute mvp & tp weight, returns and volatiolites ----
  # mvp
  mvp_s <- mvp_opt(assets=dat, shorting=TRUE)  # ; mvp_s <- c(mvp_s$mvp_weights, mvp_s$mvp_return, mvp_s$mvp_vola); mvp_s
  mvp_ns <- mvp_opt(assets=dat, shorting=FALSE)  # ; mvp_ns <- c(mvp_ns$mvp_weights, mvp_ns$mvp_return, mvp_ns$mvp_vola); mvp_ns
  # tp
  tp_n <- tp(assets=dat)  # ; tp_n <- c(tp_n$weights, tp_n$return, tp_n$volatility); tp_n <- unname(tp_n); tp_n
  # individual
  indiv <- individual(assets=dat, amounts=amounts)
  
  
  # generate data frame ----
  dat_cl <- data.frame(MVP_opt_short=mvp_s,
                       MVP_opt_not_short=mvp_ns,
                       TP_normal_function_short=tp_n,
                       individual=indiv)
  row.names(dat_cl) <- c(names(dat[,-2]), "return", "vola"); dat_cl
  
  
  # plot variables for efficient frontier ----
  # define an alpha sequence
  alpha <- seq(-1, 3, 0.01)
  # calculate portfolio weights with different alphas (therefore the %o% = outter product is needed)
  w_pf <- alpha %o% mvp(assets=dat)$weights + (1 - alpha) %o% tp(assets=dat)$weights
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
       xlim=c(0, 30),
       ylim=c(-25, 25))
  
  
  
  # mvp point shorting ----
  points(x=100*dat_cl$MVP_opt_short[nrow(dat_cl)],
         y=100*dat_cl$MVP_opt_short[nrow(dat_cl)-1],
         col="blue",
         pch=16,
         cex=1.5)
  text(x=100*dat_cl$MVP_opt_short[nrow(dat_cl)],
       y=100*dat_cl$MVP_opt_short[nrow(dat_cl)-1],
       labels=latex2exp::TeX(input="$MVP_{short}$"),
       pos=1,
       offset=1,
       col="blue")
  
  
  
  # mvp point not shorting ----
  points(x=100*dat_cl$MVP_opt_not_short[nrow(dat_cl)],
         y=100*dat_cl$MVP_opt_not_short[nrow(dat_cl)-1],
         col="orange",
         pch=16,
         cex=0.75)
  text(x=100*dat_cl$MVP_opt_not_short[nrow(dat_cl)],
       y=100*dat_cl$MVP_opt_not_short[nrow(dat_cl)-1],
       labels=latex2exp::TeX(input="$MVP_{not short}$"),
       pos=3,
       offset=1,
       col="orange")
  
  
  
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
  points(x=0, y=0.01, col="grey", pch=16)
  text(x=0, y=0.01, col="grey", pos=4, labels=latex2exp::TeX(input="$r_{rf}$"))
  abline(v=0, col="grey")
  
  # draw cpital marekt line
  lines(x=c(0, 100*dat_cl$TP_normal_function_short[nrow(dat_cl)]),
        y=c(0.01, 100*dat_cl$TP_normal_function_short[nrow(dat_cl)-1]),
        lty=2)
  
  
  
  # individual investment ----
  points(x=100*dat_cl$individual[nrow(dat_cl)],
         y=100*dat_cl$individual[nrow(dat_cl)-1],
         col="lightblue", pch=16, cex=2)
  text(x=100*dat_cl$individual[nrow(dat_cl)],
       y=100*dat_cl$individual[nrow(dat_cl)-1],
       labels=latex2exp::TeX(input="$Individual$"),
       col="lightblue", pos=1)
  
  
}
