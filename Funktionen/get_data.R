get_data <- function(symbols, starting_point=Sys.Date()-10*365, end_point=Sys.Date()){
  # supress warnings (start block)
  defaultW <- getOption("warn"); options(warn = -1)
  # get data from yahoo finance
  dat <- do.call(cbind, sapply(X=symbols, FUN=function(x){
    quantmod::getSymbols(Symbols=x,
                         from=starting_point,
                         to=end_point,
                         auto.assign=FALSE)[, 6]
  }))
  #colnames(dat) <- symbols
  # drop na's and return data
  return(na.omit(dat))
  # surpress warnings end block
  options(warn = defaultW)
}
