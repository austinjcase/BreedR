#' data.sum function
#'
#' This function allows summarize data pulled
#' @param data REQUIRED \code{data.frame} of experient
#' @param by.loc OPTIONAL \code{T/F} true or false to give summary by location
#' @keywords data.sum
#' @export
#' @examples
#' data.sum


data.sum <- function(data=x, by.loc=F)
{
 if(by.loc == F) {with(data,table(year, experiment))} else {with(data,table(experiment, location, year ))}
}


