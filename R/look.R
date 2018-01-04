#' look function
#'
#' This function allows you to see the frist 5 rows and cols of data
#' @param x dataframe or matrix.
#' @keywords look
#' @export
#' @examples
#' look()


look <- function(x) {
  cat("this is a", class(x), "\n")
  cat("the number of rows are", nrow(x), "\n")
  cat("the number of columns are ", ncol(x), "\n")
  row<-nrow(x)
  col<-ncol(x)
  if (row <5 ) {row<-2} else {row<-5}
  if(col <5){col<-2} else {col<-5}
  print(x[1:row,1:col])
}

