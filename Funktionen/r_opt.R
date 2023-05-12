r_opt <- function(assets, r_pf, shorting=TRUE, p_year=260){
  # import library
  library(nloptr)
  
  # yearly returns, volatility and covariance
  r <- apply(X=assets*p_year, MARGIN=2, FUN=mean)
  # compute covariance matrix
  Sigma <- cov(assets)
  # number of assets
  n <- ncol(Sigma)
  
  # minimize risk
  # objective function
  eval_f <- function(x, Sigma, r, r_pf){
    # !important!: x = weights
    return(sqrt(t(x) %*% Sigma %*% x))
  }
  
  # lower and upper bounds, with distinction whether shorting is allowed or not
  if(shorting == TRUE){
    lb <- rep(-10, n)
    ub <- rep(10, n)
  }else{
    lb <- rep(0, n)
    ub <- rep(1, n)
  }
  
  # Constraints
  eval_g_eq <- function(x, Sigma, r, r_pf){
    # !important!: x = weights
    constr <- c(sum(x) - 1,
                x %*% r - r_pf)
    return(constr)
  }
  
  # Initial weights
  x0 <- rep(1/n, n)
  
  # set optimization options
  opts <- list("algorithm"="NLOPT_GN_ISRES",
               "xtol_rel"=1.0e-15,
               "maxeval"=40000,
               "local_opts"=list("algorithm"="NLOPT_LD_MMA",
                                "xtol_rel"=1.0e-15 ),
               "print_level"=0)
  
  # optimization
  res <- nloptr(x0=x0,
                eval_f = eval_f,
                lb = lb,
                ub = ub,
                # eval_g_ineq = eval_g_ineq,
                eval_g_eq = eval_g_eq,
                opts = opts, 
                Sigma = Sigma,
                r = r,
                r_pf = r_pf
  )
  
  
  # results
  weights_scal <- res$solution
  pf_return <- r_pf
  pf_vola <- as.vector(sqrt(t(weights_scal) %*% Sigma %*% weights_scal)*sqrt(p_year))
  
  # return
  return(c(weights_scal, abs(pf_return), pf_vola))
}
