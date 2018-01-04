#' cbind.fill function
#'
#' This function allows you to cbind data with differing number of rows and fills the overlap with "NA"
#' @param ... a list of data.frames
#' @keywords cbind
#' @export
#' @examples
#' cbind.fill()



## cbind ragged cbind fill function
cbind.fill<-function(...){
  nm <- list(...)
  nm<-lapply(nm, as.matrix)
  n <- max(sapply(nm, nrow))
  do.call(cbind, lapply(nm, function (x)
    rbind(x, matrix(, n-nrow(x), ncol(x)))))
}
