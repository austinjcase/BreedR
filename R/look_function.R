#' look function
#'
#' This function allows you to pull data from the oat data base or similar
#' @param x REQUIRED \code{data.frame} dataframe or matrix
#' @keywords look
#' @export
#' 
#' @examples
#'
#'df.small<-data.frame(a=rnorm(1:100), b=rnorm(1:100))
#'df.small
#'look(df.small)
#'
#'df.large<-dat <- data.frame(a=rnorm(1:100), b=rnorm(1:100), c=rnorm(1:100),
#'                            d=rnorm(1:100), e=rnorm(1:100),f=rnorm(1:100),g=rnorm(1:100), h=rnorm(1:100),
#'                            i=rnorm(1:100), j=rnorm(1:100), k=rnorm(1:100), l=rnorm(1:100))
#'head(df.large)
#'look(df.large)


look <- function(x) {
  cat("this is a", class(x), "\n")
  cat("the number of rows is", nrow(x), "\n")
  cat("the number of columns is ", ncol(x), "\n")
  row<-nrow(x)
  col<-ncol(x)
  if (row <5 ) {row<-2} else {row<-5}
  if(col <5){col<-2} else {col<-5}
  print(x[1:row,1:col])
}



