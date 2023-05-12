individual <- function(assets, amounts, p_year=260){
  Sigma <- cov(assets)
  r <- apply(X=assets*p_year, MARGIN=2, FUN=mean)
  weights <- amounts/sum(amounts)
  r_pf <- t(weights) %*% r
  v_pf <- sqrt(t(weights) %*% Sigma %*% weights)*sqrt(p_year)
  return(c(weights, r_pf, v_pf))
}
