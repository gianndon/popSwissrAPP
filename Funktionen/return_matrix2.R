return_matrix2 <- function(dat){
  return(timeSeries::returns(dat, na.rm=TRUE)[-1,])
}