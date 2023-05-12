mvp <- function(assets, p_year=260){
  # yearly returns, volatility and covariance
  yearly_return <- apply(X=assets*p_year, MARGIN=2, FUN=mean)
  yearly_volatility <- apply(X=assets*sqrt(p_year), MARGIN=2, FUN=sd)
  Sigma <- cov(assets); Sigma_inv <- solve(Sigma)
  # compute weights
  weights <- as.vector(Sigma_inv %*% rep(1, dim(Sigma)[1]))
  weights_scal <- weights/sum(weights); weights_scal <- as.vector(weights_scal); names(weights_scal) <- colnames(assets)
  # compute return and volatility
  return_MVP <- t(weights_scal) %*% yearly_return; colnames(return_MVP) <- "Return Portfolio"
  volatility_MVP <- sqrt(t(weights_scal) %*% Sigma %*% weights_scal)*sqrt(260); colnames(volatility_MVP) <- "Volatility Portfolio"
  # return
  return(c(weights_scal, abs(return_MVP), volatility_MVP))
}
