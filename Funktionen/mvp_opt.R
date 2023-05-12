mvp_opt <- function(assets, shorting=TRUE, p_year=260){
  
  # import library
  library(nloptr)
  
  # yearly returns
  r <- apply(X=assets*p_year, MARGIN=2, FUN=mean)
  # covariance amtrix
  Sigma <- cov(assets)
  # number of assets
  n <- ncol(Sigma)
  
  eval_f <- function(x, Sigma, p_year){
    # !important!: x = weights
    return(sqrt(t(x) %*% Sigma %*% x))
  }
  
  eval_g_eq <- function(x, Sigma, p_year){
    # !important!: x = weights
    return(sum(x) - 1)
  }

  # lower and upper bounds, with distinction whether shorting is allowed or not
  if(shorting == TRUE){
    lb <- rep(-3, n)
    ub <- rep(3, n)
  }else{
    lb <- rep(0, n)
    ub <- rep(1, n)
  }
  
  # initial value
  x0 <- rep(1/n, n)
  
  # set optimization options
  opts <- list("algorithm"="NLOPT_GN_ISRES",
               "xtol_rel"=1.0e-15,
               "maxeval"=75000,
               "print_level"=0,
               "local_opts"=list("algorithm"="NLOPT_LD_MMA",
                                 "xtol_rel"=1.0e-15 ))
  
  # optimization
  res <- nloptr(x0=x0,
                eval_f=eval_f,
                lb=lb,
                ub=ub,
                eval_g_eq=eval_g_eq,
                opts=opts, 
                Sigma=Sigma,
                p_year=p_year)
  
  # compute mvp return and volatility and weights
  weights_scal <- as.vector(res$solution)
  mvp_return <- as.vector(abs(t(weights_scal) %*% r))
  mvp_vola <- as.vector(sqrt(t(weights_scal) %*% Sigma %*% weights_scal)*sqrt(p_year))
  
  # return
  return(c(weights_scal, abs(mvp_return), mvp_vola))
  
}
