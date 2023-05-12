v_opt <- function(assets, v_pf, shorting=TRUE, p_year=260){
  
  # import library
  library(nloptr)
  
  # yearly returns, volatility and covariance
  r <- apply(X=assets*p_year, MARGIN=2, FUN=mean)
  # compute covariance matrix
  Sigma <- p_year*cov(assets)
  # number of assets
  n <- ncol(Sigma)
  
  # Objective function
  eval_f <- function(x, Sigma, r, v_pf){
    return(t(x) %*% r)
  }
  
  # Constraint
  eval_g_eq <- function(x, Sigma, r, v_pf){
    constr <- c(sum(x) - 1,
                sqrt(t(x) %*% Sigma %*% x) - v_pf)
    return(constr)
  }
  
  # lower and upper bounds, with distinction whether shorting is allowed or not
  if(shorting == TRUE){
    lb <- rep(-3, n)
    ub <- rep(3, n)
  }else{
    lb <- rep(0, n)
    ub <- rep(1, n)
  }
  
  # Initial weights
  x0 <- rep(1/n, n)
  
  # set optimization options.
  opts <- list("algorithm"="NLOPT_GN_ISRES",
               "xtol_rel"=1.0e-15,
               "maxeval"=40000,
               "local_opts"=list("algorithm"="NLOPT_LD_MMA", 
                                 "xtol_rel"=1.0e-15),
               "print_level"=0)
  
  # optimization
  res <- nloptr (x0=x0,
                 eval_f=eval_f,
                 lb=lb,
                 ub=ub,
                 eval_g_eq=eval_g_eq,
                 opts=opts, 
                 Sigma=Sigma,
                 r=r,
                 v_pf=v_pf)
  
  # results
  weights_scal <- res$solution
  pf_return <- as.vector(abs(t(weights_scal) %*% r))
  pf_vola <- v_pf
  
  # return
  return(c(weights_scal, abs(pf_return), pf_vola))
}
