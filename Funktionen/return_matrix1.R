return_matrix1 <- function(dat){
  return((matrixStats::colDiffs(dat)/dat[-1,]))
}
